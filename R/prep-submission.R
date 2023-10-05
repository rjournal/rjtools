#' Zip your directory for R Journal submission
#'
#' \code{zip_paper} will first check the folder structure with
#' [rjtools::check_folder_structure()] before zipping up everything in the
#' main directory, excluding the `.Rproj` file, if exist.
#'
#' @importFrom utils zip
#' @importFrom yesno yesno2
#' @importFrom yesno yesno
#' @return a zip file for an R Journal submission
#' @export
#'
zip_paper <- function(){

  # remove the paper.zip and its associated paper/ if users have unzipped before
  # regenerate a new zip
  if (fs::file_exists("paper.zip")) {
    cli::cli_inform("Removing the {.file paper.zip} file detected.")
    fs::file_delete("paper.zip")
    if (fs::dir_exists("paper")) fs::dir_delete("paper")
  }

  cli::cli_h1("Check for folder structure")
  res <- check_folder_structure(".")
  cli::cli_h1("Prepare paper zip")
  if (res != "SUCCESS"){
    continue <- yesno::yesno2(
    "ERRORs or WARNINGs generated from checking the folder structure.
    Please fix before creating the paper zip. Would you like to continue?")
  }

  if (continue | res == "SUCCESS"){
    files <- list.files(recursive = TRUE, full.names = TRUE)
    files <- files[!tools::file_ext(files) %in% "Rproj"]
    utils::zip(zipfile = "paper", files)
  }

}

#' Prepare pre-filled fields in the submission form
#' \code{prep_submission} generate some answers based on the .tex file to fill
#' the article submission form. You can save the answers if assigned it to an
#' object.
#'
#' @return a list
#' @export
prep_submission <- function(){

  files <- tools::file_path_sans_ext(list.files())
  tex_name <- table(files) |> which.max() |> names() |> xfun::with_ext("tex")
  tex <- readLines(tex_name) |> paste0(collapse = " ")

  raw <- tex %>% stringr::str_extract("(?<=\\\\author\\{).*?(?=\\})")
  authors_raw <- raw[!is.na(raw)] %>% stringr::str_remove("by ")

  if (!stringr::str_detect(authors_raw, ",")){
    authors <- authors_raw %>% stringr::str_split(" and ", simplify = TRUE)
  } else{
    # more than two authors
    authors <- authors_raw %>% stringr::str_split(", ", simplify = TRUE)
  }

  leading <- authors[1]
  others <- toString(authors[2:length(authors)]) %>% stringr::str_remove("and ")
  title <- stringr::str_extract(tex,  "(?<=\\\\title\\{).*?(?=\\})")

  res <- list(
    `Your name` =  leading,
    `Names of other authors, comma separated` = others,
    `Article title` = title)

  cli::cli_alert_info("Your name: {.field {leading}}")
  cli::cli_alert_info("Names of other authors, comma separated: {.field {others}}")
  cli::cli_alert_info("Article title: {.field {title}}")
  invisible(res)
}

