# Adapted from rticles::rjournal_article

#' @export
#' @rdname rjournal_article
rjournal_pdf_article <- function(..., self_contained = FALSE) {
  fmt <- rticles::rjournal_article(...)
  post_process <- fmt$post_processor
  fmt$post_processor <- function(metadata, utf8_input, output_file, clean, verbose) {
    filename <- basename(output_file)

    temp_tex <- xfun::read_utf8(output_file)
    # Add author line
    temp_tex <- rticles:::post_process_authors(temp_tex)
    # Replace \@ref(***) with \ref{***}
    temp_tex <- resolve_refs_latex(temp_tex)
    xfun::write_utf8(temp_tex, output_file)
    if(!is.null(metadata$volume)) {
      issue_months <- if(metadata$volume < 14) {
        c("June", "December")
      } else {
        c("March", "June", "September", "December")
      }
    }
    metadata$journal$section <- metadata$journal$section %||% "Contributed research article"
    m <- list(
      filename = xfun::sans_ext(filename),
      volume = metadata$volume %||% "XX",
      issue = metadata$issue %||% "YY",
      issueyear = if(is.null(metadata$volume)) "20ZZ" else 2008 + metadata$volume,
      issuemonth = if(is.null(metadata$volume)) "AAAA" else issue_months[metadata$issue],
      journal = metadata$journal
    )

    get_field <- function(x, field) {
      if(!is.list(x)) return(NULL)
      if(length(field) == 1L) return(x[[field]])
      get_field(x[[field[1]]], field[-1])
    }
    pdf_header <- get_field(metadata, c("output", "rjtools::rjournal_pdf_article", "includes", "in_header"))
    pdf_header <- c(pdf_header, if (length(preamble <- unlist(metadata[c("preamble", "header-includes")]))) {
      f <- tempfile(fileext = ".tex")
      on.exit(unlink(f), add = TRUE)
      xfun::write_utf8(preamble, f)
      f
    })
    wrapper_template <- system.file(
      file.path("tex", "RJwrapper.tex"),
      package = "rjtools"
    )

    wrapper_md <- tempfile(fileext = ".md")
    on.exit(unlink(wrapper_md), add = TRUE)
    xfun::write_utf8(c("---", yaml::as.yaml(m), "---\n"), wrapper_md)

    rmarkdown::pandoc_convert(
      wrapper_md, output = "RJwrapper.tex",
      verbose = verbose, wd = ".",
      options = c(
        "--template", rmarkdown::pandoc_path_arg(wrapper_template),
        rmarkdown::pandoc_include_args(pdf_header)
      )
    )

    # Replace style file
    sty_origin <- list.files(system.file("tex", package = "rjtools"),
                             full.names = TRUE)
    sty_dest <- file.path(".", basename(sty_origin))
    copied <- file.copy(sty_origin, sty_dest)
    on.exit(unlink(sty_dest[copied]), add = TRUE)

    tinytex::latexmk(
      "RJwrapper.tex", fmt$pandoc$latex_engine,
      pdf_file = xfun::with_ext(basename(output_file), ".pdf"),
      clean = clean
    )
  }

  fmt
}


