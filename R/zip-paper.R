#' Zip your directory for Rjournal submission
#'
#' Zip up  files in the directory into a \code{paper.zip} for Rjournal submission.
#'
#'
#' @param name the file name you used to create the article. See argument `file_name` in \code{create_article()}
#' @param others other files or folders to zip, this might include supplementary \code{data/} folder,
#' motivational letter for add-in packages, and \code{review/} folder for reviewer's comments and responses.
#' @export
#'
zip_paper <- function(name, others = NULL){


  # check whether the paper folder already exist
  if (fs::dir_exists("paper") & !fs::file_exists("paper.zip")){
  delete_paper <- usethis::ui_yeah(
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

  # should find RJwrapper.tex and RJwrapper.pdf
  rjwrapper <- list.files(pattern = "RJwrapper",
                          recursive = TRUE, full.names = TRUE)

  # should find style sheet
  sty <- list.files(pattern = "Rjournal.sty",
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
  to_zip <- c(basic, rjwrapper, sty, other_paths)
  zip(zipfile = "paper", to_zip)


}

