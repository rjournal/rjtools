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
#' @inheritParams rjdistill::rjournal_web_article
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
#' @param file_name the name of the Rmd file
#' @param dir_path the directory name that houses the template files
#' @importFrom stringr str_extract
#' @importFrom fs dir_create file_copy file_move
#' @export
create_article <- function(file_name = "article", dir_path = here::here()){
  tmeplates <- c(
    "skeleton/skeleton.Rmd",
    "skeleton/RJreferences.bib",
    "skeleton/Rjournal.sty",
    "resources/RJwrapper.tex",
    "skeleton/motivation-letter.md",
    "skeleton/penguins.png")

  template_full <- system.file(glue::glue("rmarkdown/templates/rjournal/{tmeplates}"),
                               package = "rjtools")

  file_names <- stringr::str_extract(template_full, "([^\\/]+$)")
  to <- glue::glue("{dir_path}/{file_names}")

  fs::dir_create(path = dir_path)
  fs::dir_create(path = glue::glue("{dir_path}/figures"))
  fs::file_copy(template_full, dir_path, overwrite = TRUE)
  fs::file_move(to[str_detect(to, "Rmd")], glue::glue("{dir_path}/{file_name}.Rmd"))
  fs::file_move(to[str_detect(to, "bib")], glue::glue("{dir_path}/skeleton.bib"))
  fs::file_move(to[str_detect(to, "png")], glue::glue("{dir_path}/figures/penguins.png"))

  cli::cli_alert_info(glue::glue("The initial files for your article have been created under the {dir_path} folder"))
  cli::cli_alert_info(glue::glue("{file_name}.Rmd, Rjournal.sty, RJwrapper.tex, skeleton.bib, and motivation-letter.md."))
  cli::cli_alert_info(glue::glue("Don't forget to change the name of the .bib file, and change its name in the {file_name}.Rmd YAML too"))
}
