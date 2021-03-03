#' R Markdown output format for R Journal articles
#'
#' The R Journal is built upon the distill framework with some modifications.
#' This output format behaves almost identically to the
#' `distill::distill_article()` format, with some formatting and structural
#' changes.
#'
#' @param ... Arguments passed to `distill::distill_article()`.
#' @export
rjournal_article <- function(...) {
  distill::distill_article(...)
}
