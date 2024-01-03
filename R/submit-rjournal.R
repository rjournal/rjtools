#' Submit a paper to the R Journal
#'
#' This is a final self-check function, similar to
#' that runs through a checklist on your submission
#' that cannot be done automatically.
#' @importFrom utils browseURL
#' @export
submit_rjournal <- function(){

  if (!yesno(cli::cli_inform(
  "Have you checked .bib file titles? They need to be:
  1) package name should be protected, i.e. {.code ggplot2} and always in lower case,
  2) the programming language R should be protected, i.e. {.code R}, and
  3) both sentence or title case are accepteable but need to be consistent"))){
    return(invisible())
  }

  if (!yesno(cli::cli_inform(
    "Have you include the automatic check results (with {.fn rjtools::initial_check_article})
    in the motivation letter? Checks with WARNINGs and ERRORs need to be
    commented with reasons."))){
    return(invisible())
  }

  if (!yesno(cli::cli_inform(
    "Have you checked spelling with {.fn rjtools::check_spelling} ? "))){
    return(invisible())
  }

  if (!yesno(cli::cli_inform(
    "Have you added alt text to your figures as explained in
    {.url https://rjournal.github.io/rjtools/articles/format-details.html#alt-text}?"))){
    return(invisible())
  }

  if (!yesno(cli::cli_inform(
    "Have you checked the ORCID number point to the correct author?
    Try {.fn rjtools::get_orcid} and click on the link returned to check."))){
    return(invisible())
  }

  if (!yesno(cli::cli_inform(
    "The R Journal recommends the SVG format for figures since it is
    vector-based and is easier for low vision readers than PNGs.
    Have you converted your plots to SVGs?"))){
    return(invisible())
  }

  if (!yesno(cli::cli_inform(
    "Have you checked that the zip file size is less than 10 MB?"))){
    return(invisible())
  }

  # assume there is only one zip file in the folder
  zip <- list.files(pattern = "zip", recursive = TRUE)[1] |> file.size()
  if (zip/(10^6) > 10.2) {
    cli::cli_abort("The zip folder size is greater than 10 MB,
                   please fix and then submit ")
  }


  if (yesno(cli::cli_inform("Ready to submit?"))){
    browseURL("https://docs.google.com/forms/d/e/1FAIpQLSeqtHH0g9JhrNEwT4ScqVAQe6Qq4om1-EfUqBe12YsPrrB6WQ/viewform")
    return(invisible())
  }


}

#' @rdname checks
#' @export
get_orcid <- function(path, file = NULL){
  files <- list.files(here::here(path), full.names = TRUE)
  rmd_file <- files[tools::file_ext(files) == "Rmd"]
  rmd_file <- eliminate_mulitple(rmd_file, file = file)

  if (length(rmd_file) == 1) {
    # extract from .rmd
    text <- rmarkdown::yaml_front_matter(rmd_file)
    orcid <- lapply(text$author, function(x) {x[["orcid"]]}) # always extract the first author
    if (is.null(orcid)) return(cli::cli_inform("No ORCID ID found"))
  }else{
    # extract from .tex
    tex_file <- files[tools::file_ext(files) == "tex"]
    tex_file <- eliminate_mulitple(tex_file, file = file)
    text <- readLines(tex_file)
    orcid_line <- text[grep("ORCiD:", text)]
    if (length(orcid_line) == 0){
      return(cli::cli_inform("No ORCID ID found"))
    }
    orcid <- regmatches(
      orcid_line, regexpr( "\\d{4}-\\d{4}-\\d{4}-\\d{4}", orcid_line))
  }
  link <-  glue::glue("https://orcid.org/{orcid}")
  return(cli::cli_inform("{.url {link}}"))
}


eliminate_mulitple <- function(path, file){

  if (length(path) > 1){
    if (is.null(file)){
      ext <- tools::file_ext(path) |> unique()
      cli::cli_abort("Multiple {.code {ext}} file detected,
                     please specify the {.code file} argument",
                     call = parent.frame())
      }
    path <- path[grepl(file, path)]
  }

  return(path)

}
