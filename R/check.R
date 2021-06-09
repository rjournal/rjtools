# suppose each article is in a separate repo
# currently, there are multiple articles in the `ae-articles-testing`, supply an article id to locate which article

#' Complete initial checks for submissions.
#'
#' There are several initial checks undertaken for each submission.
#' These checks reflect the requests made in the author submission guide.
#'
#' Results will be show in the initial_checks.log file in the submission folder
#'
#' Set the directory of the articles repository using `set_articles_path`.
#'
#' @param dic the dictionary used for spelling check. See \code{dict} arguent in [hunspell::hunspell()]
#'
#' @importFrom  rj get_articles_path
#' @rdname checks
#' @export
initial_checks <- function(dic = "en_US") {

  # Find articles without initial_check logs, pass that list to do checks
  base <- file.path(rj::get_articles_path(), "Submissions")
  paths <- dir(base, full.names = TRUE)

  # Call the function for to check each submission
  lapply(paths, initial_check_article, dic = dic)

  return(cat("Checks completed"))
}


#' A single article check
#'
#' @param path Location of the article submission folder to check
#' @rdname checks
initial_check_article <- function(path, dic = "en_US") {

  # Documents:
  # Necessary files must be included in submission folder
  # Do not proceed without them, flag for manual check
  # Unnecessary files must not be included in submission folder
  # Do not proceed with them, flag for manual check

  # Create a log file for errors
  sink(file.path(path, "initial_checks.log"))

  # Display the name of the current article
  cat(paste0("Initial check results: ", basename(path), "\n"))

  cli::cli_h1(paste0("Initial check results: ", basename(path)))

  # Find all file names in the submission folder
  # for single submission in submissions folder
  submission_files <- list.files(path)

  remaining_files <- check_wrappers(submission_files)

  # BEGIN CHECKS
  # Check for one of each  "tex", "bib", "R"
  # return the name of the tex file
  filename <- check_filenames(remaining_files)
  tex_vec <- readLines(file.path(path, filename))
  tex <- paste0(tex_vec, collapse = " ")

  # Check for title case in title and sentence case in section title
  check_title(tex)
  check_section(tex)
  check_spelling(tex_vec, dic)

  # Check for unnecessary files (to avoid potential issues)
  check_unnecessary_files(submission_files)

  # Check for existence of a cover letter
  check_cover_letter(remaining_files)

  # check that packages mentioned in manuscript are available on CRAN
  # if there are no mentions, flag for manual check
  check_packages_available(tex)
  check_proposed_pkg()

  # Show a numeric summary of successes, errors and notes
  check_summary(path)

  # Return to console output
  sink()
  closeAllConnections()

}

#' Check that article title is in title case
#'
#' @param tex the tex file read in by \code{readLines}
#'
#' @importFrom stringr str_extract
#' @importFrom purrr map_chr
#' @importFrom tools toTitleCase
#' @rdname checks
check_title <- function(tex){

  str <- stringr::str_extract(tex,  "(?<=\\\\title\\{).*?(?=\\})")

  # str_to_title changes the first letter of all to capital - also for proposition
  if (tools::toTitleCase(str) != str){
    log_error("The title is not in title case!")
  } else{
    log_success("The article title is properly formatted in title case")
  }

}

#' Check that section titles are in sentence case
#'
#' @param tex the tex file read in by \code{readLines}
#'
#' @importFrom stringr str_extract
#' @importFrom utils available.packages
#' @rdname checks
check_section <- function(tex){

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

  if (!all(stringr::str_to_sentence(str) == str)){
    problem_one <- str[!stringr::str_to_sentence(str) == str]
    log_error("Section {problem_one} is not in sentence case!")
  } else{
    log_success("All sections are properly formatted in sentence case")
  }

}


#' Check that Abstract comes before the introduction section
#'
#' @param tex the tex file read in by \code{readLines}
#'
#' @importFrom stringr str_locate
#' @rdname checks
check_abstract_before_intro <- function(tex){
  abstract <- stringr::str_locate(tex, "abstract")[1]
  intro <- stringr::str_locate(tex, "introduction")[1]

  if (abstract < intro){
    log_success("Abstract comes before the introduction section")
  } else{
    log_error("Abstract doesn't come before the introduction section")
  }
}

#' Check for spelling mistakes
#' @importFrom stringr str_extract str_replace_all
#' @importFrom purrr map2
#' @importFrom hunspell hunspell dictionary
#' @importFrom tools toTitleCase
#' @rdname checks
check_spelling <- function(tex, dic = "en_US"){

  detect_abstract <- purrr::map(tex, ~stringr::str_extract(.x,  "(?<=\\\\abstract\\{).*?"))
  abstract_loc <- match(detect_abstract[!is.na(detect_abstract)], detect_abstract)

  detect_bib <- purrr::map(tex, ~stringr::str_extract(.x,  "(?<=\\\\bibliography\\{).*?(?=\\})"))
  bib_loc <- match(detect_bib[!is.na(detect_bib)], detect_bib)

  to_replace <- paste(spell_to_remove, collapse = "|")
  tex2 <- stringr::str_replace_all(tex, to_replace, "")
  text_bw <- tex2[(abstract_loc + 1):(bib_loc - 1)]

  chunk_begin <- which(text_bw == "\\begin{Schunk}")
  chunk_end <- which(text_bw == "\\end{Schunk}")
  chunk_idx <- unlist(purrr::map2(chunk_begin, chunk_end, ~.x:.y))

  select_idx <- !c(1:length(text_bw)) %in% chunk_idx
  text_clean <- text_bw[select_idx]

  check_raw <- hunspell::hunspell(text_clean, format = "latex", dic = hunspell::dictionary(dic))
  check <- unique(unlist(check_raw))
  check_out <- paste0(check[toupper(check) != check & tools::toTitleCase(check) != check], collapse = ", ") # remove acronym

  if (length(check_out) != 0){
    log_note("A potential list of spelling to check: {check_out}")
  } else{
    log_success("No spelling mistake detected")
  }

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





#' Check for the two expected RJwrapper files
#'
#' @param submission_files a vector of the file names in the submission folder
#' @rdname checks
check_wrappers <- function(submission_files) {
  # Check for RJwrapper files
  wrapper_files <- c("RJwrapper.tex",
                     "RJwrapper.pdf")

  # Immediate failure?
  # Return a warning if either "RJwrapper.tex" and/or "RJwrapper.pdf" not found
  if (!all(wrapper_files %in% submission_files)) {

    log_error("Submission is missing an RJwrapper file")

  } else {

    log_success("Submission contains both RJwrapper.tex and RJwrapper.pdf")

  }

  # Remove the two wrapper files that were checked
  remaining_files <-
    submission_files[!(submission_files %in% wrapper_files)]

  return(remaining_files)
}




#' Check for the three files that should contain the filename
#'
#' @param remaining_files a vector of the file names in the submission folder,
#' after the removal of the RJwrapper tex and pdf files.
#'
#' Returns the name of the tex file, that should be matched by the .bib and .R
#'
#' @importFrom tools file_ext
#' @importFrom tools file_path_sans_ext
#' @rdname checks
check_filenames <- function(remaining_files) {

  exts <- tools::file_ext(remaining_files)

  files_exist <- c("tex", "bib", "R") %in% exts

  # check all files are present
  if (!all(files_exist)) {

    missing_type <- exts[!( c("tex", "bib", "R") %in% exts )]

    log_error("Submission is missing a tex, bib or R file")

  } else {

    log_success("Submission has tex, bib, and R files")

  }


  matching_filename <- remaining_files[exts %in% c("tex", "bib", "R")]

  single_filename <- tools::file_path_sans_ext(matching_filename)

  # Check for all three files with matching names
  # Find the file name that should match

  if (!length(unique(single_filename)) == 1) {

    log_error("Submission does not have consistently named tex, bib, and R files")

  } else {

    log_success("Submission has consistently named tex, bib, and R files")

  }

  # return the .tex filename to allow for search for packages, even if different names are used.
  filename <- regmatches(matching_filename, regexpr("(.+)\\.tex", matching_filename))

  return(filename)
}



#' Check for the two files that may cause build errors
#'
#' @param submission_files a vector of the file names in the submission folder
#' @rdname checks
check_unnecessary_files <- function(submission_files) {

  unnecessary_files <- c("RJtemplate.tex",
                         "RJournal.sty")


  if (any(unnecessary_files %in% submission_files)) {

    log_error("Submission contains unnecessary files")

  } else {

    log_success("No problematic files have been found")

  }

}


#' Check for the two expected RJwrapper files
#'
#' @param remaining_files a vector of the file names in the submission folder
#' @rdname checks
check_cover_letter <- function(remaining_files){


  if (!any(grepl("letter", remaining_files))) {

    log_error("Check for cover letter if add-on package submission style")

  } else {

    log_note("Possible motivation letter found")

  }

}

#' Check that the proposed package is avilable on CRAN
#' @importFrom cranlogs cran_downloads
#' @rdname checks
check_proposed_pkg <- function(){

  pkg <- readline(prompt = "What's the name of package being proposed in the article? If none, please enter 0. ")

  if (pkg != 0) {
    count <- sum(cranlogs::cran_downloads(pkg, from = "2020-01-01")$count)
    if (count == 0){
      log_note(text = "No CRAN activities detected for package {pkg}")
    } else{
      log_success(text = "CRAN activities have been detected for package {pkg}")
    }

  }
}

#' Check that packages mentioned in the text are available on CRAN
#'
#' @param tex the tex file read in by readLines
#'
#' @importFrom stringr str_extract_all
#' @importFrom utils available.packages
#' @rdname checks
check_packages_available <- function(tex) {

  # List of CRAN and BIO pkgs used in the text
  pkgs_to_check <- lapply(X = c("\\\\CRANpkg\\{(.*?)\\}", "\\\\BIOpkg\\{(.*?)\\}"),
                          FUN = stringr::str_extract_all, string = tex)

  # Names of cran pkgs
  CRANpkgs <- unique(stringr::str_sub(unlist(pkgs_to_check[1]), start = 10, end = -2))

  # Run cran checks
  allCRANpkgs <- available.packages()[,1]


  if (!all(CRANpkgs %in% allCRANpkgs)) {
    # When one is missing from CRAN
    missing <- CRANpkgs[!(CRANpkgs %in% allCRANpkgs)]
    amount_missing <- length(missing)
    amount_pkgs <- length(CRANpkgs)

    log_error(text = "{amount_missing} of {amount_pkgs} package(s) not available on CRAN: {paste(missing, collapse = ', ')}")

  } else {

    log_success("All CRAN packages mentioned are available")
  }


  allBIOpkgs <- available.packages(repos = "https://bioconductor.org/packages/3.11/bioc")[,1]

  BIOpkgs <- unique(stringr::str_sub(unlist(pkgs_to_check[2]), start = 9, end = -2))

  if (!all(BIOpkgs %in% allBIOpkgs)) {
    # When one is missing from Bioconductor
    missing <- BIOpkgs[!(BIOpkgs %in% allBIOpkgs)]
    amount_missing <- length(missing)
    amount_pkgs <- length(BIOpkgs)

    log_error("{amount_missing} of {amount_pkgs} package(s) not available on Bioconductor: {paste(missing, collapse = ', ')}")

  } else {

    log_success("All Bioconductor packages mentioned are available")

  }


  # Check that all packages with a \pkg reference also have a \CRANpkg or \BIOpkg mention

  # pkgs referred to in the text

  pkgs_used <- stringr::str_sub(
    unlist(
      stringr::str_extract_all(string = tex, "pkg\\{(.*?)\\}")),
    start = 5, end = -2)

  # Start with full list of pkgs
  declared_pkgs <- pkgs_used %in% c(CRANpkgs, BIOpkgs)

  if (any(!declared_pkgs)) {
    # Look for pkgs that were used in the text but did not have a CRANpkg{} commands
    pkgs_missing_ref <- unique(pkgs_used[!(declared_pkgs)])
    amount_missing <- length(pkgs_missing_ref)

    log_note("{amount_missing} package(s) used in the text without CRANpkg or BIOpkg commands: {paste(pkgs_missing_ref, collapse = ', ')}")
  }


}

#' Produce a summary of successes, errors and notes
#'
#' @param path location of the submission being checked
#' @param file the console output directed to the log, using `stdout`
#'
#' @importFrom stringr str_match
#' @importFrom stringr str_count
#' @rdname checks
check_summary <- function(path, file = stdout()) {

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


#####################################################

log_factory <- function(prefix, .f) {

  # Guarantee definitions exist in the factory environment
  force(.f)
  force(prefix)

  function(text, ..., file = stdout(), .envir = parent.frame()) {

    text <- glue::glue(prefix, text, .envir = .envir)

    .f(text)

    # Send output to the log file
    cat(text, "\n", sep = "", file = file, append = TRUE)

  }
}

#' Produce a log file entry for an error
#'
#' Append a line in the log file that details an error
#'
#' @param text Description of the error that occurred
#' @param ... Additional inputs for text passed to the glue function
#' @param .envir The environment used to find the text string replacements
#' @param file The console output directed to the log, using `stdout`
#'
#'
log_error <- log_factory(prefix = "ERROR: ", .f = cli::cli_alert_warning)

#' Produce a log file entry for a success
#'
#' Append a line in the log file that details a success
#'
#' @param text Description of the error that occurred
#' @param ... Additional inputs for text passed to the glue function
#' @param .envir The environment used to find the text string replacements
#' @param file The console output directed to the log, using `stdout`
#'
#'
log_success <- log_factory(prefix = "SUCCESS: ", .f = cli::cli_alert_success)


#' Produce a log file entry for a note
#'
#' Append a line in the log file that details a note
#'
#' @param text Description of the error that occurred
#' @param ... Additional inputs for text passed to the glue function
#' @param .envir The environment used to find the text string replacements
#' @param file The console output directed to the log, using `stdout`
#'
#'
log_note <- log_factory(prefix = "NOTE: ", .f = cli::cli_alert_info)


################################################################################

#' Various handy symbols to use in a command line UI
#'
#' Show symbols in console output
#'
#'
#' @name symbol
#' @aliases symbol
#'
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
