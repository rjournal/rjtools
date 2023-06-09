#' Create an R Journal article with the existing template
#'
#' @details
#' Outputs an R Journal paper template set of files in the directory specified
#' by `dir_path` or in the project directory, if nothing is specified.
#' See \code{vignette("create_article", package = "rjtools")} for more details
#' on how to create and format an article.
#'
#' @param name the name of the Rmd, will default to "test"
#' @inheritParams rmarkdown::draft
#'
#' @importFrom stringr str_extract
#' @importFrom fs dir_create file_copy file_move
#' @export
create_article <- function(name="test", file = xfun::with_ext(name, "Rmd"), create_dir = FALSE, edit = TRUE){
  rmarkdown::draft(
    file,
    template = "rjournal",
    package = "rjtools",
    create_dir = create_dir,
    edit = FALSE
  )

  if(edit) {
    path <- if(create_dir) {
      file.path(xfun::sans_ext(file), file)
    } else {
      file
    }
    path <- normalizePath(path)
    has_rstudio <- if(requireNamespace("rstudioapi", quietly = TRUE)) {
      rstudioapi::hasFun("navigateToFile")
    } else {
      FALSE
    }
    if(has_rstudio) {
      rstudioapi::navigateToFile(path)
    } else {
      utils::file.edit(path)
    }
  }

  message("Success: your paper is ready to edit!")
}
