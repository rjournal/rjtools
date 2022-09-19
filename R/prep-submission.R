#' Zip your directory for R Journal submission
#'
#' Zip up  files in the directory into a \code{paper.zip} for R Journal submission.
#'
#'
#' @param name the file name you used to create the article. See argument `file_name` in \code{create_article()}
#' @param others other files or folders to zip, this might include supplementary \code{data/} folder,
#' motivational letter for add-in packages, and \code{review/} folder for reviewer's comments and responses.
#' @importFrom utils zip
#' @importFrom yesno yesno
#' @return a zip file with items for an R Journal submission
#' @export
#'
zip_paper <- function(name, others = NULL){


  # check whether the paper folder already exist
  if (fs::dir_exists("paper") & !fs::file_exists("paper.zip")){
  delete_paper <- yesno::yesno(
  "Paper folder should be reserved for the zipped file and Rjtools is going to delete it.
  Do you want to proceed?")
  if (delete_paper) {
    fs::dir_delete("paper")
  } else{
    cli::cli_abort("Terminated by the user.")
  }

}

  # remove the paper.zip and its associated paper/ if users have unzipped before
  # regenerate a new zip
  if (fs::file_exists("paper.zip")) {
    cli::cli_inform("Removing created paper.zip and associated paper folder if detected.")
    fs::file_delete("paper.zip")
    if (fs::dir_exists("paper")) fs::dir_delete("paper")

  }

  # should find .Rmd, .tex, .bib, .html from the dual rendering,
  # and .r for the script if applicable
  basic <- list.files(pattern = name, recursive = TRUE, full.names = TRUE)
  html_files <- list.dirs(path = file.path(dirname(basic[1]), glue::glue("{name}_files")))

  # should find RJwrapper.tex
  rjwrapper <- list.files(pattern = "RJwrapper",
                          recursive = TRUE, full.names = TRUE)

  # should find style sheet
  sty <- list.files(pattern = "Rjournal.sty",
                          recursive = TRUE, full.names = TRUE)

  motivation_letter <- list.files(pattern = "motivation-letter.md",
                    recursive = TRUE, full.names = TRUE)

  # process other files to zip
  file_path <- others[fs::path_ext(others) != ""]
  folder_path <- others[fs::path_ext(others) == ""]

  # detect whether the claimed files and folders exist
  test_folders <- purrr::map_lgl(folder_path, fs::dir_exists)
  test_files <- purrr::map_lgl(file_path, fs::file_exists)
  if (!all(test_folders) & length(file_path) != 0){
    cli::cli_abort("Folder {.code {folders[!test_folder]}} {?is/are} not detected.")
  }
  if (!all(test_files) & length(file_path) != 0){
    cli::cli_abort("File {.code {files[!test_files]}} {?is/are} not detected in the top level directory.")
  }

  other_paths <- unique(c(file_path, folder_path))

  # cli::cli_inform("Copy and paste the following to the supplementary file in the submission form:
  #                 ")

  # collect everything and zip!
  to_zip <- c(basic, html_files, rjwrapper, sty, motivation_letter, other_paths)
  utils::zip(zipfile = "paper", to_zip)


}

#' Prepare pre-filled fields in the submission form
#'
#' Auto-generate answers to some fields in the R Jorunal submission form
#'
#' @param name the file name you used to create the article. See argument `file_name` in \code{create_article()}
#' @return auto-generated answers for some R Journal submission questions
#' @export
prepare_submission <- function(name){

  tex <- readLines(list.files(pattern = xfun::with_ext(name, "tex"),
                              recursive = TRUE, full.names = TRUE)) %>%
    paste0(collapse = " ")

  raw <- tex %>%
    stringr::str_extract("(?<=\\\\author\\{).*?(?=\\})")
  authors_raw <- raw[!is.na(raw)] %>% stringr::str_remove("by ")

  if (!stringr::str_detect(authors_raw, ",")){
    authors <- authors_raw %>% stringr::str_split(" and ", simplify = TRUE)
  } else{
    # more than two authors
    authors <- authors_raw %>% stringr::str_split(", ", simplify = TRUE)
  }

  leading <- authors[1]
  others <- toString(authors[2:length(authors)]) %>% stringr::str_remove("and ")

  cli::cli_alert_info("Your name: {.field {leading}}")
  cli::cli_alert_info("Names of other authors, comma separated: {.field {others}}")

  # would be nice to implement keywords

  title <- stringr::str_extract(tex,  "(?<=\\\\title\\{).*?(?=\\})")
  cli::cli_alert_info("Article title: {.field {title}}")

  cli::cli_alert_info("Please list the paths to any other supplementary files inside the zip (R scripts, data, etc.). Each file path should be separated by commas. This list will be used to construct the supplementary zip file for your article if it is accepted for publication:
                       {.field data/* ?}")
}
