#' Create an R Journal article with the existing template
#'
#' @details
#' See \code{vignette("create_article", package = "rjtools")} for how to create an article
#'
#' @param name the name of the Rmd
#' @inheritParams rmarkdown::draft
#'
#' @return a created R Journal template in the directory specified by `dir_path`
#'
#' @importFrom stringr str_extract
#' @importFrom fs dir_create file_copy file_move
#' @export
create_article <- function(name, file = xfun::with_ext(name, "Rmd"), create_dir = FALSE, edit = TRUE){
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
}
