#' R Markdown output formats for R Journal articles
#'
#' The R Journal is built upon the distill framework with some modifications.
#' This output format behaves almost identically to the
#' `distill::distill_article()` format, with some formatting and structural
#' changes.
#'
#' @param ... Arguments passed to `distill::distill_article()` for web articles,
#'   and `rticles::rjournal_article()` for pdf articles.
#' @rdname rjournal_article
#' @export
rjournal_web_article <- rjdistill::rjournal_web_article

#' @rdname rjournal_article
#' @export
rjournal_pdf_article <- function(...) {
  rticles::rjournal_article(...)
}

#' Create an R journal article with the existing template
#'
#' @details
#' See \code{vignette("create_article", package = "rjtools")} for how to create an article
#'
#' @param dir_path the directory name that houses the template files
#' @param file_name the name of the bib/Rmd file
#' @importFrom usethis use_directory use_template
#' @importFrom stringr str_remove str_extract
#' @importFrom fs path
#' @export
create_article <- function(dir_path = "paper", file_name = "article"){
  templates <- c("rjournal/resources/RJwrapper.tex",
                 "rjournal/skeleton/RJreferences.bib",
                 "rjournal/skeleton/skeleton.Rmd",
                 "rjournal/skeleton/Rjournal.sty")
  names <- stringr::str_extract(templates, "([^\\/]+$)")
  rmd_idx <- stringr::str_detect(templates, "Rmd")
  names[rmd_idx] <- paste0(file_name, ".", fs::path_ext(names)[rmd_idx])
  bib_idx <- stringr::str_detect(templates, "bib")
  names[bib_idx] <- "skeleton.bib"

  usethis::use_directory(dir_path)

  for (i in 1: length(templates)){
    usethis::use_template(templates[i], package = "rjtools", save_as = fs::path(dir_path, names[i]))
  }

  invisible(TRUE)

}
