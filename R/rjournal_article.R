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
rjournal_web_article <- function(...) {
  fmt <- distill::distill_article(...)
  post_knit <- fmt$post_knit
  fmt$post_knit <- function(...) {
    args <- post_knit(...)
    is_html <- which(str_detect(args, "html$"))
    lapply(args[is_html], function(x) {
      any(str_detect(readLines(x), "misc"))
    })
    args
  }

  fmt
}

#' @rdname rjournal_article
#' @export
rjournal_pdf_article <- function(...) {
  rticles::rjournal_article(...)
}

#' Create an article template
#' @param dir_path the directory name that hold the .bib, .tex, and .Rmd
#' @param file_name the name of teh .Rmd file
#' @importFrom usethis use_directory use_template
#' @importFrom stringr str_remove str_extract
#' @importFrom fs path
#' @export
create_article <- function(dir_path = "paper", file_name = "article"){

  # it seems that usethis::use_template requires the template to be under inst/templates/...
  templates <- c("rjournal/resources/RJwrapper.tex",
                    "rjournal/skeleton/RJreferences.bib",
                    "rjournal/skeleton/skeleton.Rmd")
  names <- stringr::str_extract(templates, "([^\\/]+$)")
  name_idx <- stringr::str_detect(templates, "skeleton")
  ext <- fs::path_ext(names)
  names[name_idx] <- paste0(file_name, ".", ext[name_idx])
  usethis::use_directory(dir_path)

  for (i in 1: length(templates)){
    usethis::use_template(templates[i], package = "rjtools", save_as = fs::path(dir_path, names[i]))
  }

  message("Remember to change the .bib & .Rmd to the same name!")

  invisible(TRUE)

}
