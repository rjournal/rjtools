#' @export
#' @rdname article
rjournal_pdf_article <- function(..., self_contained = FALSE) {
  fmt <- rticles::rjournal_article(...)
  post_process <- fmt$post_processor
  fmt$post_processor <- function(metadata, utf8_input, output_file, clean, verbose) {
    # Replace \@ref(***) with \ref{***}
    file <- xfun::with_ext(output_file, '.tex')
    lines <- xfun::read_utf8(file)
    lines <- resolve_refs_latex(lines)
    xfun::write_utf8(lines, file)

    sty_origin <- list.files(system.file("tex", package = "rjtools"),
                             full.names = TRUE)
    sty_dest <- file.path(".", basename(sty_origin))
    copied <- file.copy(sty_origin, sty_dest)
    on.exit(unlink(sty_dest[copied]))
    post_process(metadata, utf8_input, output_file, clean, verbose)
    out_pdf <- xfun::with_ext(basename(output_file), ".pdf")
    file.rename("RJwrapper.pdf", out_pdf)
    out_pdf
  }

  fmt
}
