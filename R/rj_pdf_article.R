#' @export
rjournal_pdf_article <- function(..., self_contained = FALSE) {
  fmt <- rticles::rjournal_article(...)
  post_process <- fmt$post_processor
  fmt$post_processor <- function(...) {
    sty_origin <- list.files(system.file("tex", package = "rjtools"),
                             full.names = TRUE)
    sty_dest <- file.path(".", basename(sty_origin))
    copied <- file.copy(sty_origin, sty_dest)
    on.exit(unlink(sty_dest[copied]))
    post_process(...)
  }
  fmt
}
