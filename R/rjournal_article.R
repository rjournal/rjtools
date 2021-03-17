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
