#' R Markdown site generator for the R Journal
#'
#' @inherit  distill::distill_website
#'
#' @keywords internal
rjournal_website <- function(input, ...) {
  distill::distill_website(input = input, ...)
}
