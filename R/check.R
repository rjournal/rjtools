#' A single article check
#'
#' @param path string, path to the directory that contains the .tex
#'     file (Ideally, this directory should contain .bib, .Rmd, and
#'     .tex with author names and two RJwrapper files:  RJwrapper.pdf
#'     and RJwrapper.tex)
#' @param file string, the file name if multiple files are detected
#'     under the \code{path} argument
#' @param dic string, the dictionary used for spelling check. See
#'     \code{dict} argument in [hunspell::hunspell()]
#' @param pkg string, optional. The name of the proposed package (if
#'     relevant), to be checked for activity on CRAN
#' @param ... additional arguments for spelling check with
#'     [hunspell::hunspell]
#' @param ask logical, if \code{TRUE} then checks may ask the user for
#'     interactive input of missing information.
#' @param logfile a connection for the output log, or a string with
#'     the filename of the output log or \code{NULL} if no log should
#'     be written
#' @details
#' Folder structure checks:
#'
#' * \code{check_filenames()}: the three files (.bib, .Rmd, and .tex) all present and have consistent names
#' * \code{check_structure()}: check validity of all filenames and depth of the directory structure
#' * \code{check_unnecessary_files()}: the template file (i.e., RJtemplate.tex) is not included in the directory
#' * \code{check_cover_letter()}: a motivational letter
#'
#' Content checks:
#'
#' * \code{check_title()}: article title is in title case
#' * \code{check_section()}: section sections are in sentence case
#' * \code{check_abstract()}: abstract should be plain text without package
#' markups (CRANpkg, BIOpkg, pkg), math notations($...$), citations, and other
#' formattings (highlight, italic, etc)
#' * \code{check_spelling()}: potential spelling mistakes
#' * \code{check_proposed_pkg()}: package proposed in the paper is on CRAN
#' * \code{check_pkg_label()}: packages marked up with \pkg{} are not available
#' on CRAN or BioConductor
#' * \code{check_packages_available()}: packages mentioned in the article are
#' available on CRAN
#' * \code{check_bib_doi}: whether bib entries have DOI or URL included, uncless
#' can't sourced online
#' * \code{check_csl}: no additional csl file should be used
#' consistent, either in sentence (preferred) or title case
#'
#' See \code{vignette("create_article", package = "rjtools")} for how to use the check functions
#' @rdname checks
#' @return list of all results (see \code{log_error} for
#'     details). You can use \code{unlist()} to get a character vector
#'     of the result statuses.
#'
#' @examples
#' your_article_path <- system.file("sample-article", package = "rjtools")
#' if (interactive()) initial_check_article(your_article_path)
#'
#' @export
initial_check_article <- function(path, dic = "en_US", pkg, ...,
                                  ask = interactive(),
                                  logfile=file.path(path, "initial_checks.log")) {
    if (missing(path))
        cli::cli_abort(
                 "The {.code path} argument is missing.
Please specify the file directory that contains the article {.field .tex} file.")

    if (is.character(logfile)) {
        logfile <- file(logfile, "a")
        on.exit(close(logfile))
    } else if (!inherits(logfile, "connection") && !is.null(logfile))
        stop("logfile must be a string or connection")

    if (!is.null(logfile)) {
        old.check.log.file <- getOption("check.log.file")
        on.exit(options(check.log.file=old.check.log.file), add=TRUE)
        options(check.log.file=logfile)
    }

    if (!"tex" %in% tools::file_ext(list.files(path = path)))
        stop("Please supply the directory that contains the .tex file")

    if (is.null(getOption("check.log.journal"))) {
        journal <- new.env(parent=emptyenv())
        options(check.log.journal=journal)
        on.exit(options(check.log.journal=NULL), add=TRUE)
    }

    if (!is.null(logfile))
        writeLines(c("Initial check results: ", ""), logfile)

    if (getOption("check.log.output", "cli") == "cli")
        cli::cli_h1(paste0("Initial check results: "))

    ## BEGIN CHECKS
    ## Folder structure checks:
    check_filenames(path)
    check_structure(path)
    check_folder_structure(path)
    check_unnecessary_files(path)
    check_cover_letter(path)

    ## Tex file checks:
    check_title(path, ...)
    check_section(path)
    check_abstract(path)
    check_spelling(path, dic, ...)
    check_proposed_pkg(pkg, ask)
    check_pkg_label(path)
    check_packages_available(path)
    check_bib_doi(path)
    check_csl(path)
    check_date(path, file)

    ## Show a numeric summary of successes, errors and notes
    journal_summary(file=logfile)

    ## return all results
    invisible(getOption("check.log.journal")$results)
}


##############################################
##############################################

#' @importFrom tools file_ext file_path_sans_ext
#' @rdname checks
#' @export
check_filenames <- function(path) {
    remaining_files <- remove_wrapper(path)
    exts <- tools::file_ext(remaining_files)

    ## do we really want to require R? We didn't use to ...
    files_exist <- c("tex", "bib", "R") %in% exts

    matching_filename <- remaining_files[exts %in% c("tex", "R")]

    single_filename <- tools::file_path_sans_ext(matching_filename)

    ## Check for all three files with matching names
    ## Find the file name that should match

    if (length(unique(single_filename)) != 1) {
        log_error("Submission does not have consistently named tex, R files")
    } else if (!all(files_exist)){
        log_error("Submission is missing a tex, bib or R file")
    } else{
        log_success("Submission has consistently named tex, bib, and R files")
    }
}

#' @rdname checks
#' @export
check_structure <- function(path) {
    all <- list.files(path, all.files=TRUE, include.dirs=TRUE, recursive=TRUE)
    all <- all[!grepl(".Rproj", all)]
    depth <- nchar(gsub("[^/]+", "", all))
    if (max(depth) > 3)
        return(log_error("There are nested subdirectories. Please use at most two directory levels for the article."))
    if (length(dot <- grep("^\\.", gsub(".*/", "", all))))
        log_warning("The archive contains hidden files which will be removed: ", paste(all[dot], collapse=", "))
    nonascii <- function(x) { r <- charToRaw(x); any(r > 127 | r <= 0x20) }
    if (any(sapply(all, nonascii)))
        log_error("File or directory names contain spaces or non-ASCII characters. For portability only use ASCII characters and no spaces in file and directory names.")
    else
        log_success("File and directory names are compliant.")
}

#' @rdname checks
#' @export
check_folder_structure <- function(path){
  files <- list.files(path)
  file_exts <- tools::file_ext(files)

  concat <- function(x)  paste0(x, collapse = ", ")

  img_exts <- c("jpeg", "jpg", "png", "gif", "tiff", "svg")
  img_files <- files[file_exts %in% img_exts] |> concat()

  data_exts <- c("csv", "rda")
  data_files <- files[file_exts %in% data_exts] |> concat()

  r_files <- files[file_exts == "R"]
  log_aux <- files[file_exts %in% c("log", "aux", "out")] |> concat()
  file_txt <- tools::file_path_sans_ext(files)
  motivation <- files[file_exts %in% c("md", "doc", "docx")] |> concat()

  valid_exts <- c("Rproj", ".sty", "bib", "Rmd", "html", "R", "tex", "pdf", "")
  exts_checked <- c(img_exts, data_exts, "R", "log", "aux")
  non_standard <- files[!file_exts %in% c(valid_exts, exts_checked)]
  non_standard <- non_standard[!grepl("motivation", non_standard)] |> concat()
  non_standard <- non_standard[!grepl("RJournal", non_standard)] |> concat()

  if ((length(img_files) > 1) | (str_length(img_files) != 0)){
    log_error("It looks like there are image(s) in the main directory: {img_files}.
              If so, they should be organised into figures/ folder.")
  } else if ((length(data_files) > 1) | (str_length(data_files) != 0)){
    log_error("It looks like there are data file(s) in the main directory: {data_files}.
              If so, they should be organised into data/ folder.")
  } else if (length(r_files) > 1){
    r_files <- concat(r_files)
    log_error(
    "Multiple R files detected: {r_files}.
    Scripts should be organised in the scripts/ folder.
    The master R file generated from rendering should still be in the main directory.")
  } else if (str_length(log_aux) != 0){
    log_note("Remove .log, .aux and .out files before submitting.")
  } else if (str_length(motivation) != 0){
    log_error("Possible motivation or cover letters detected in main folder: {motivation}.
              They should be placed in the motivation-letter/ folder.")
  } else if ((length(non_standard) > 1) | (str_length(non_standard) != 0)){
    log_warning(
      "Other non-standard file detected: {non_standard}.
      Should they be removed? ")
  } else{
    log_success("The paper is in good folder structure.")
  }

}

#' @rdname checks
#' @export
check_unnecessary_files <- function(path) {
    submission_files <- list.files(path)
    unnecessary_files <- "RJtemplate.tex"

    if (any(unnecessary_files %in% submission_files)) {
        unnecessary <- unnecessary_files[unnecessary_files %in% submission_files]
        log_error("Submission contains unnecessary files: ", unnecessary)
    } else {
        log_success("No problematic file found")
    }
}

#' @rdname checks
#' @export
check_cover_letter <- function(path){
    remaining_files <- remove_wrapper(path)
    if (!length(mot <- grep("motivation", remaining_files))) {
        log_note("Motivation letter is not detected, if applicable")
    } else {
        log_success("Possible motivation letter found: {paste0(remaining_files[mot], collapse = ', ')}")
    }
}


##############################################
##############################################
# Tex file checks:

#' @param ignore The words to ignore in title check, e.g. package name (data.table, toOoOlTiPs)
#' @importFrom tools toTitleCase
#' @rdname checks
#' @export
check_title <- function(path, ignore = ""){

  tex <- extract_tex(path)
  str <- sub(".*\\\\title\\{([^}]*)\\}.*","\\1", tex, perl = TRUE)
  res <- check_str(str, ignore)

  has_special_format <- grepl(
    "\\pkg\\{.*\\}|\\CRANpkg\\{.*\\}|\\BIOpkg\\{.*\\}", str)
  if (has_special_format){
    log_error("Article title should not contain any special format, such as the
              \\pkg, \\CRANpkg, \\BIOpkg markups used for package names.")
  }

  if (!res$result){
    log_error("Article title not in title case! Suggest title: {res$suggest}.")
  } else{
    log_success("Article title formatted in title case.")
  }

}


check_str <- function(str, ignore = ""){
  ignore <- paste0(ignore, collapse = "", sep = "|")
  str <- gsub(ignore, "", str) # remove ignored words
  str_in_title_case <- tools::toTitleCase(str)
  pass <- str_in_title_case == str

  list(result = pass, suggest = if (!pass) str_in_title_case else NULL)
}


#' @importFrom stringr str_extract
#' @importFrom utils available.packages
#' @rdname checks
#' @export
check_section <- function(path){

  tex <- extract_tex(path)

  str <- unlist(stringr::str_extract_all(tex,  "(?<=\\\\section\\{).*?(?=\\}[\\s]?[\\\\label]?)"))

  if (any(str_detect(str, "texorpdfstring"))){
    str <- unlist(stringr::str_extract_all(tex,  "(?<=\\\\section\\{).*?(?=\\}\\\\label)"))
  }

  clean_section_title <- function(str){
    if (str_detect(str, "texorpdfstring")){
      str <- str_extract(str, "(?<=\\}\\{).*?(?=\\})")
    }

    str
  }

  str <- lapply(str, clean_section_title)
  # remove the capital R
  dt <- do.call(rbind, lapply(str_remove(str, " R"), check_sentence_case))
  res <- paste0(str[!dt[["in_sentence_case"]]], collapse = ", ")
  if (nchar(res) != 0){
    log_error("Section {res} is not in sentence case!")
  } else{
    log_success("Section titles formatted in sentence case.")
  }

}

check_sentence_case <- function(str){
  remove_uppercase <- function(str){
    words <- stringr::str_split(str, " ", simplify = TRUE)
    out <- paste(words[(!stringr::str_to_upper(words) == words )| nchar(words) == 1 ],
                 collapse = " ")
    out
  }

  raw <- str
  str <- remove_uppercase(str)
  str <- strsplit(str, ": ")[[1]]

  data.frame(
    origin = raw,
    in_sentence_case = all(stringr::str_to_sentence(str) == str)
  )
}



#' @rdname checks
#' @export
check_abstract <- function(path){
  tex <- extract_tex_vec(path)
  idx_abs <- which(grepl("\\\\abstract\\{", tex))
  idx_intro <- which(grepl("\\\\section\\{Introduction\\}", tex))
  str <- paste0(tex[(idx_abs+1):(idx_intro-2)], collapse = " ")

  has_special_format <- check_abstract_str(str)

  if (has_special_format){
    log_error("Abstract should be plain text without package markups,
    mathematical notations, citation, or other formattings."
    )
  } else{
    log_success("Abstract formatted in plain text.")
  }
}

check_abstract_str <- function(str){
  #pkgs
  pkgs <- grepl("\\pkg\\{.*\\}|\\CRANpkg\\{.*\\}|\\BIOpkg\\{.*\\}", str)

  # citation
  citations <- grepl("\\cite\\{.*\\}|\\citep|\\citet", str)

  others <-  grepl("\\\\texttt|\\$.*\\$|\\\\emph|\\\\proglang", str)

  any(c(pkgs, citations, others))
}

#' @importFrom stringr str_extract str_replace_all
#' @importFrom purrr map2 map
#' @importFrom hunspell hunspell dictionary
#' @importFrom tools toTitleCase
#' @rdname checks
#' @export
check_spelling <- function(path, dic = "en_US", ...){

  tex <- extract_tex_vec(path)

  detect_abstract <- purrr::map(tex, ~stringr::str_extract(.x,  "(?<=\\\\abstract\\{).*?"))
  abstract_loc <- match(detect_abstract[!is.na(detect_abstract)], detect_abstract)

  detect_bib <- purrr::map(tex, ~stringr::str_extract(.x,  "\\\\section\\*\\{References\\}"))
  bib_loc <- match(detect_bib[!is.na(detect_bib)], detect_bib)

  # spell_to_remove is a pre-defined vector of latex commands
  # to be removed from spelling check
  to_replace <- paste(spell_to_remove, collapse = "|")
  tex2 <- stringr::str_replace_all(tex, to_replace, "")
  text_bw <- tex2[(abstract_loc + 1):(bib_loc - 1)]

  chunk_begin <- which(text_bw == "\\begin{Schunk}")
  chunk_end <- which(text_bw == "\\end{Schunk}")
  chunk_idx <- unlist(purrr::map2(chunk_begin, chunk_end, ~.x:.y))

  select_idx <- !c(1:length(text_bw)) %in% chunk_idx
  text_clean <- text_bw[select_idx]

  check_raw <- hunspell::hunspell(text_clean, format = "latex", dic = hunspell::dictionary(dic), ...)
  check <- unique(unlist(check_raw))
  check_out <- check[tolower(check) == check]

  if (length(check_out) != 0){
    log_note("A potential list of spelling to check: ", paste(check_out, collapse = ", "))
  } else{
    log_success("No spelling mistake detected")
  }

}

#' @rdname checks
#' @export
check_proposed_pkg <- function(pkg, ask=interactive()) {
    if (missing(pkg) || is.null(pkg)) {
        if (!ask)
            return(log_note("No proposed package supplied."))
        ## This is a really terrible hack ...
        pkg <- readline(prompt = "What's the name of the package being proposed in the article? Press Enter if none. ")
    }

    if (length(pkg) == 1 && nzchar(pkg)) {
        allCRANpkgs <- available.packages(type='source')[,1]
        if (!pkg %in% allCRANpkgs)
            log_note(text = "No CRAN activities detected for package {pkg}")
        else
            log_success(text = "CRAN activities have been detected for package {pkg}")
    } else
        log_success("No proposed package for the article, nothing to check.")
}

#' @rdname checks
#' @export
check_pkg_label <- function(path){
  tex <- extract_tex(path)
  with_pkg_markup <- unique(greg1("\\\\pkg\\{(.*?)\\}", tex))
  cran_idx <- which(with_pkg_markup %in% allCRANpkgs())
  cran_str <- paste0(with_pkg_markup[cran_idx],collapse = ", ")
  bio_idx <- which(with_pkg_markup %in% allBioCpkgs())
  bio_str <- paste0(with_pkg_markup[bio_idx],collapse = ", ")
  if (length(cran_idx) != 0){
    log_error("Package(s) available on CRAN: {cran_str}.
              please use \\CRANpkg rather than \\pkg.")
  } else if (length(bio_idx) != 0){
    log_error("Package(s) available on CRAN: {bio_str}.
            please use \\BIOpkg rather than \\pkg.")
  } else{
    log_success("No error with the use of \\pkg markup.")
  }


}

## get first group from all matches in all strings
greg1 <- function(pattern, strings)
  do.call(rbind, stringr::str_match_all(strings, pattern))[,2]

#' @param ignore The words to ignore in title check, use c(pkg, pkg, ...) for multiple quoted words
#' @importFrom stringr str_match_all
#' @importFrom utils available.packages
#' @importFrom BiocManager version
#' @rdname checks
#' @export
check_packages_available <- function(path, ignore) {
    if (missing(ignore)) ignore <- character()
    tex <- extract_tex(path)

    ## List of CRAN and BioC pkgs used in the text
    CRANpkgs <- unique(greg1("\\\\CRANpkg\\{(.*?)\\}", tex))
    BioCpkgs <- unique(greg1("\\\\BIOpkg\\{(.*?)\\}", tex))

    ## remove ignored ones
    CRANpkgs <- CRANpkgs[!(CRANpkgs %in% ignore)]
    BioCpkgs <- BioCpkgs[!(BioCpkgs %in% ignore)]

    allCRANpkgs <- allCRANpkgs()
    ## only bother with BioC if it is mentioned
    allBioCpkgs <- if (length(BioCpkgs)) {
        allBioCpkgs()
    } else character()

    res1 <- if (!all(CRANpkgs %in% allCRANpkgs)) {
        ## When one is missing from CRAN
        missing <- CRANpkgs[!(CRANpkgs %in% allCRANpkgs)]
        amount_missing <- length(missing)
        amount_pkgs <- length(CRANpkgs)

        log_error(text = "{amount_missing} of {amount_pkgs} package(s) not available on CRAN: {paste(missing, collapse = ', ')}")
    } else if (!all(BioCpkgs %in% allBioCpkgs)) {
        ## When one is missing from Bioconductor
        missing <- BioCpkgs[!(BioCpkgs %in% allBioCpkgs)]
        amount_missing <- length(missing)
        amount_pkgs <- length(BioCpkgs)

        log_error("{amount_missing} of {amount_pkgs} package(s) not available on Bioconductor: {paste(missing, collapse = ', ')}")
    } else {
        log_success("All packages marked-up with \\CRANpkg or \\BIOpkg are available on CRAN or Bioconductor.")
    }

    ## Check that all packages with a \pkg reference also have a \CRANpkg or \BIOpkg mention
    ## pkgs referred to in the text

    pkgs_used <-  unique(greg1("pkg\\{(.*?)\\}", tex))

    ## Start with full list of pkgs
    declared_pkgs <- pkgs_used %in% c(CRANpkgs, BioCpkgs)

    if (any(!declared_pkgs)) {
        ## Look for pkgs that were used in the text but did not have a CRANpkg{} commands
        pkgs_missing_ref <- unique(pkgs_used[!(declared_pkgs)])
        amount_missing <- length(pkgs_missing_ref)

        log_note("{amount_missing} package(s) used in the text without \\CRANpkg or \\BIOpkg commands: {paste(pkgs_missing_ref, collapse = ', ')}")
    }
    ## the last note is not passed as result but will be recorded in the journal
    res1
}

## Get CRAN list
allCRANpkgs <- function() {tools::CRAN_package_db()$Package}
allBioCpkgs <- function(){
  ## Get BioC list
  BioCver <- BiocManager::version()
  available.packages(repos = paste0("https://bioconductor.org/packages/", BioCver, "/bioc"), type='source')[,1]
}

read_bib <- function(path){
  files <- list.files(here::here(path), full.names = TRUE)
  bib_file <- files[tools::file_ext(files) == "bib"]
  a <- rmarkdown::pandoc_citeproc_convert(bib_file, type = "yaml")
  bib_list <- yaml::yaml.load(a)$reference
  return(bib_list)
}

#' @rdname checks
#' @export
check_bib_doi <- function(path){
  bib_list <- read_bib(path)
  id <- c()
  bib_tbl <- lapply(bib_list, function(x) {
    if (is.null(x$doi) && is.null(x$url)){
      id <- c(id, x$id)
    }
  })

  res <- paste0(do.call(rbind,bib_tbl), collapse = ", ")

  if (nchar(res) == 0){
    log_success("All the references contain DOI or URL")
  } else{
    log_warning("Citation should include a link to the reference, preferably a
    DOI, unless online resources cannot be found.
    References without DOI or URL: {res}.")
  }

}

#' @rdname checks
#' @export
check_csl <- function(path){
  files <- list.files(here::here(path), full.names = TRUE)
  rmd_file <- files[tools::file_ext(files) == "Rmd"]

  if (length(rmd_file) == 0) {
    csl_file <- files[tools::file_ext(files) == "csl"]
    if (length(csl_file) != 0) {res <- "has_csl"} else {res <- "good"}
  } else{
    if (length(rmd_file) > 1){
      html_basename <- basename(tools::file_path_sans_ext(
        files[tools::file_ext(files) == "html"]
      ))
      rmd_file <- rmd_file[grepl(html_basename, rmd_file)]
    }

    yaml <- rmarkdown::yaml_front_matter(rmd_file)
    yaml_nms <- names(yaml)
    if ("csl" %in% yaml_nms){ res <- "has_csl"} else {res <- "good"}

  }

  if (res == "has_csl"){
    log_error("Found CSL file {yaml[['csl']]} in the repository.
              No CSL file should be used in R Journal article.")
  } else{
    log_success("No customised csl file used. Good!")
  }

}

#' @rdname checks
#' @export
check_date <- function(path, file){
  files <- list.files(path)
  rmd_file <- files[tools::file_ext(files) == "Rmd"]
  rmd_file <- eliminate_mulitple(rmd_file, file)
  yaml <- rmarkdown::yaml_front_matter(rmd_file)
  if (as.Date(yaml[["date"]], format = "%Y-%m-%d") != Sys.Date()){
    log_error(
      "Please use a fixed article's date in the format of `%Y-%m-%d`,
      e.g. 2023-10-05. The date should match the date when the article
      is submitted. Dynamic date can cause issues on issue rendering.")
  } else{
    log_success("Article date is set fixed at the article submission date.")
  }
}


#' @importFrom stringr str_match str_count
output_summary <- function(path, file = stdout()) {

  completed_checks <- readLines(file.path(path, "initial_checks.log"))

  results <- lapply(X = c("SUCCESS", "ERROR", "NOTE"),
                    FUN = stringr::str_count,
                    string = completed_checks)

  results_tally <- lapply(results, sum)

  results_text <- paste("\n",
                        "SUCCESSES:", results_tally[[1]],"|",
                        "ERRORS:", results_tally[[2]], "|",
                        "NOTES:", results_tally[[3]], sep = " ")

  cat(results_text, "\n",
      sep = " ", file = file, append = TRUE)

  cli::cli_h3(results_text)

}

##############################################
##############################################
# helper functions

remove_wrapper <- function(path){
    submission_files <- list.files(path)
    ## remove both template and wrapper
    wrapper_files <- c("RJwrapper.tex", "RJtemplate.tex")
    submission_files[!(submission_files %in% wrapper_files)]
}

extract_tex_vec <- function(path){
    remaining <- remove_wrapper(path)
    name <- remaining[tools::file_ext(remaining) == "tex"]

    if (length(name) == 0)
        log_error(
        "Can't locate the .tex file under the current path,
        please specify the correct path that contains the .tex file")

    ## NOTE: this may match more files if there are stray ones, so we
    ##       concatenate them all
    if (length(name) > 1) {
        # this will print the msg for every check if there are multiple files
        #log_warning("Multiple .tex files found: {paste(name, collapse=', ')}")
        unlist(lapply(file.path(path, name), readLines))
    } else
        readLines(file.path(path, name))
}

extract_tex <- function(path){
  vec <- extract_tex_vec(path)
  paste0(vec , collapse = " ")
}

spell_to_remove <- c("(\\\\url\\{(.*)\\})",
                     "(\\\\href\\{(.*)\\})",
                     "(\\\\label\\{(.*)\\})",
                     "(\\\\nameref\\{(.*)\\})",
                     "(\\\\code\\{(.*)\\})",
                     "(\\\\CRANpkg\\{(.*)\\})",
                     "(\\\\pkg\\{(.*)\\})",
                     '("(.*)")',
                     "(\\\\textt\\{(.*)\\})",
                     "(\\\\emph\\{(.*)\\})",
                     "(\\\\file\\{(.*)\\})",
                     "(\\\\includegraphics\\[(.*)\\})",
                     "(emph)"
)

##############################################
## Note: this is seriously over-engineered, but the goal was to
## support the previous behavior, allow logging into files as well as
## in-session recording for automated use. We may not keep
## all of it in the end (e.g., it's unclear how useful are the
## conditions), so consider it experimental.
log_factory <- function(result = c("SUCCESS", "NOTE", "WARNING", "ERROR")) {
    result <- match.arg(result)

    function(text, ...,
             output = getOption("check.log.output", "cli"),
             file   = getOption("check.log.file", NULL),
             signal = getOption("check.log.conditions", FALSE),
             .envir = parent.frame()) {

        ## cli is brain-dead and always sends input through glue
        ## with no way to prevent that so we need a way to defer glue
        ## in that case
        cli.glue.fix <- FALSE
        output <- if (is.character(output))
                  switch(output,
                         cli = {
                             cli.glue.fix <- TRUE
                             switch(result,
                                    SUCCESS = cli::cli_alert_success,
                                    NOTE    = cli::cli_alert_info,
                                    WARNING = cli::cli_alert_warning,
                                    ERROR   = cli::cli_alert_danger) },
                         none = identity,
                         switch(result,
                                 SUCCESS =,
                                 NOTE    = function(x) cat(x, "\n", sep=''),
                                 WARNING =,
                                 ERROR   = message)
                         )
              else if (inherits(output, "connection")) {
                  function(x)
                      cat(x, "\n", sep='', file=output, append=TRUE)
              } else
                  stop("Invalid output specification")

        text <- glue::glue(result, ": ", text, ..., .envir = .envir)
        output(if (!cli.glue.fix) text else "{text}")

        ## Send output to the log file (if requested)
        if (!is.null(file))
            cat(text, "\n", sep = "", file = file, append = TRUE)

        ## we use the condition objects as result objects as well
        cond <-
            switch(result,
                   SUCCESS = structure(list(
                       message = text, call = sys.call(-1), trace = sys.calls()),
                       class = c("RJcheckSUCCESS", "RJcheckCondition",
                                 "condition")),
                   NOTE = structure(list(
                       message = text, call = sys.call(-1), trace = sys.calls()),
                       class = c("RJcheckNOTE", "RJcheckCondition",
                                 "message", "condition")),
                   WARNING = structure(list(
                       message = text, call = sys.call(-1), trace = sys.calls()),
                       class = c("RJcheckWARNING", "RJcheckCondition",
                                 "warning", "condition")),
                   ERROR = structure(list(
                       message = text, call = sys.call(-1), trace = sys.calls()),
                       class = c("RJcheckERROR", "RJcheckCondition",
                                 "error", "condition")))

        ## signal the condition only if asked
        if (isTRUE(getOption("check.log.conditions", FALSE)))
            signalCondition(cond)
        ## otherwise we just attach the info
        ## Note that with a simple c() you can drop this
        ## and just get the string with the final status.
        attr(result, "info") <- cond

        ## if the user asked for a journal, add it to the journal
        if (is.environment(ce <- getOption("check.log.journal")))
            ce$results <- c(ce$results, list(result))

        invisible(result)
    }
}

#' @title Logging functions
#'
#' @description \code{log_...} functions produce a log entry.
#'
#' @details
#' Most arguments are intended to be set with options to allow the use
#' of the checking mechanism both in interactive and automated
#' settings. There are four types of log entries: SUCCESS, NOTE,
#' WARNING and ERROR. If the \code{"check.log.journal"} option is set
#' to an environment then the entry is also added to the journal.
#'
#' @param text string, description of the error that occurred,
#'     will be passed to \code{\link[glue]{glue}}.
#' @param ... additional inputs for text passed to the \code{\link[glue]{glue}} function.
#' @param .envir the environment used to find the text string replacements
#' @param output type of the output, can either a string (\code{"cli"}
#'     to use the \code{cli} package (default), \code{"R"} for
#'     standard R facilities or \code{"none"} for no output) or a
#'     connection. It uses the \code{"check.log.output"} option if
#'     set.
#' @param file connection to log a copy of the output to or NULL
#'     (default) if no additional copy is desired. It uses the
#'     \code{"check.log.file"} option.
#' @param signal logical, if \code{TRUE} then a condition is signalled
#'     at the end of the function. All conditions have superclass
#'     \code{"RJcheckCondition"} and subclass \code{"RJCheck<result>"}
#'     where \code{<result>} is one of \code{SUCCESS}, \code{NOTE},
#'     \code{WARNING} and \code{ERROR}. They also have the
#'     corresponding standard R condition classes. Uses option
#'     \code{"check.log.conditions"}.
#'
#' @return \code{log_...} string with the result type. The
#'     corresponding condition object with a message and call is
#'     included in the \code{"info"} attribute (even if no condition
#'     is signalled).
#'
#' @keywords internal
log_error <- log_factory("ERROR")

#' @rdname log_error
log_success <- log_factory("SUCCESS")

#' @rdname log_error
log_note <- log_factory("NOTE")

#' @rdname log_error
log_warning <- log_factory("WARNING")


#' @description \code{journal_summary} prints a quick summary (status
#'     counts) based on the journal.
#'
#' @param journal environment of the journal
#' @rdname log_error
#' @return \code{journal_summary} table of the status counts
journal_summary <- function(journal=getOption("check.log.journal"),
                            file=stdout()) {
    if (!is.environment(journal))
        return(warning("No journal found."))
    res <- unlist(journal$results)
    ct <- table(factor(res, c("SUCCESS", "NOTE", "WARNING", "ERROR")))
    text <- paste(names(ct), ct, sep=": ", collapse=" | ")
    cat("", text, "", sep="\n")
    if (!is.null(file))
        cat("", text, "", sep="\n", file=file, append=TRUE)
    invisible(ct)
}

#' @description \code{simplify_journal} returns a simplified form of the
#'     results in the journal.
#'
#' @return \code{simplify_journal} string matrix with columns "result"
#'     (status), "test" (name of the calling function) and "message"
#' @rdname log_error
simplify_journal <- function(journal=getOption("check.log.journal")) {
    if (!is.environment(journal))
        return(warning("No journal found."))
    res <- journal$results
    t(sapply(res, function(o)
        c(result=c(o), test=as.character(attr(o, "info")$call[[1]]), message=c(attr(o, "info")$message))))
}


################################################################################

#' Various handy symbols to use in a command line UI
#'
#' Show symbols in console output
#'
#'
#' @name symbol
#' @aliases symbol
#' @keywords internal
symbol_utf8 <- list(
  "tick" = '\u2714',
  "cross" = '\u2716',
  "pointer" = '\u276F',
  "line" = '\u2500'
)

symbol_rstudio <- symbol_utf8
symbol_rstudio$tick <- "\u2713"
symbol_rstudio$cross <- "x"
symbol_rstudio$pointer <- ">"

symbol_win <- list(
  "tick" = '\u221A',
  "cross" = 'x',
  "line" = '-'
)

symbol_ascii <- list(
  "tick" = 'v',
  "cross" = 'x',
  "star" = '*',
  "line" = '-'
)

