#' R Markdown output formats for R Journal articles
#'
#' The R Journal is built upon the distill framework with some modifications.
#' This output format behaves almost identically to the
#' `distill::distill_article()` format, with some formatting and structural
#' changes.
#'
#' @param ... Arguments passed to `distill::distill_article()` for web articles,
#'   and `rticles::rjournal_article()` for pdf articles.
#' @inheritParams distill::distill_article
#' @param legacy_pdf whether an article is from the past and only have pdf version
#' @param web_only additional param for embedding PDF or using Rmd to produce HTML
#' @importFrom rlang caller_env env_poke
#' @return the rendered R Journal article
#' @export
#' @rdname rjournal_article
rjournal_web_article <- function(toc = FALSE, self_contained = FALSE,
                                 legacy_pdf = FALSE, web_only = !legacy_pdf, ...) {
  args <- c()
  base_format <- distill::distill_article(
    self_contained = self_contained, toc = toc, ...
  )

  # Remove distill RStudio validation checks
  base_format$on_exit <- function(){}

  distill_post_knit <- base_format$post_knit

  rmd_path <- NULL
  render_pdf <- NULL
  article_metadata <- NULL

  base_format$post_knit <- function(metadata, input_file, runtime, ...) {
    # Modify YAML metadata for pre-processor
    render_env <- rlang::caller_env(n = 2)
    metadata <- replace_names(metadata, c("abstract" = "description"))
    metadata$title <- strip_macros(metadata$title)
    metadata$description <- strip_macros(metadata$description %||% paste0('"', metadata$title, '" published in The R Journal.'))
    for(i in seq_along(metadata$author)) {
      metadata$author[[i]] <- replace_names(metadata$author[[i]], c("orcid" = "orcid_id"))
    }

    metadata$journal <- list(
      title = metadata$journal$title %||% "The R Journal",
      issn = metadata$journal$issn %||% "2073-4859",
      firstpage = metadata$journal$firstpage %||% metadata$pages[1] %||% 1,
      lastpage = metadata$journal$lastpage %||% metadata$pages[2]
    )
    metadata$slug <- metadata$slug %||% xfun::sans_ext(basename(input_file))
    metadata$pdf_url <- xfun::with_ext(metadata$slug, "pdf")
    if(metadata$journal$title == "The R Journal") {
      has_parent_dir <- function(path, nm){
        if(basename(path) == nm) return(TRUE)
        if(path == dirname(path)) return(FALSE)
        has_parent_dir(dirname(path), nm)
      }
      is_repo <- has_parent_dir(normalizePath(input_file), "rjournal.github.io")
      if(is_repo) {
        if(has_parent_dir(normalizePath(input_file), "_articles")) {
          # Use article DOIs
          metadata$citation_url <- paste0("https://doi.org/10.32614/", metadata$slug)
          metadata$doi <- paste0("10.32614/", metadata$slug)
        } else {
          # News don't have DOIs
          metadata$citation_url <- paste0("https://journal.r-project.org/news/", metadata$slug)
        }
      }
    }
    metadata$creative_commons <- metadata$creative_commons %||% "CC BY"
    if(is.null(metadata$date)) {
      if(!is.null(metadata$volume) && !is.null(metadata$issue)) {
        issue_freq <- if(metadata$volume < 14) 6 else 3
        metadata$date <- paste0(2008 + metadata$volume, "-", issue_freq * metadata$issue, "-01")
      } else {
        warning("A date must be provided for your article. Defaulting to today's date.")
        metadata$date <- format(Sys.Date())
      }
    }
    if(is.null(metadata$packages)) {
      input <- xfun::read_utf8(input_file)
      pkgs <- gregexpr("\\\\(CRAN|BIO)pkg\\{.+?\\}", input)
      pkgs <- mapply(
        function(pos, line) {
          if(pos[1] == -1) return(NULL)
          substr(rep_len(line, length(pos)), pos, pos + pos%@%"match.length" - 1)
        },
        pkgs, input,
        SIMPLIFY = FALSE
      )
      pkgs <- unique(do.call(c, pkgs))
      pkg_is_cran <- grepl("^\\\\CRAN", pkgs)
      pkgs <- sub("\\\\(CRAN|BIO)pkg\\{(.+?)\\}$", "\\2", pkgs)
      message(paste0(
        "Detected the following packages from article:\n  ",
        "CRAN: ", paste0(pkgs[pkg_is_cran], collapse = ", "), "\n  ",
        "Bioconductor: ", paste0(pkgs[!pkg_is_cran], collapse = ", ")
      ))
      metadata$packages <- list(
        cran = pkgs[pkg_is_cran],
        bioc = pkgs[!pkg_is_cran]
      )
    }
    if(is.null(metadata$CTV)) {
      if(local_cache$exists("ctv")){
        ctvs <- local_cache$get("ctv")
      } else {
        ctvs <- readRDS(
          gzcon(url("https://cran.r-project.org/src/contrib/Views.rds", open = "rb"))
        )
        local_cache$add(ctvs, "ctv")
      }
      ctvs <- Filter(
        function(taskview) {
          any(metadata$packages$cran %in% taskview$packagelist$name)
        },
        ctvs
      )
      metadata$CTV <- vapply(ctvs, function(x) x[["name"]], character(1L))
    }

    if(!is.null(metadata$csl)) warning("Please do not use custom CSL formatting, if there is an issue with the default styling please contact r-journal@r-project.org")
    metadata$csl <- metadata$csl %||% system.file("rjournal.csl", package = "rjtools", mustWork = TRUE)

    metadata$output <- replace_names(metadata$output, c("rjtools::rjournal_web_article" = "distill::distill_article"))

    # Replace metadata with modified copy
    article_metadata <<- metadata
    rlang::env_poke(
      render_env, nm = "front_matter", value = metadata,
      inherit = TRUE, create = TRUE
    )

    # save Rmd path for later use
    rmd_path <<- normalizePath(input_file)
    render_pdf <<- !is.null(metadata$author)

    # Pass updated metadata to distill's post_knit()
    distill_post_knit(metadata, input_file, runtime, ...)
  }

  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                            output_dir) {

    # Add embedded PDF
    embed_pdf <- if(! web_only){
      whisker::whisker.render(
'<div class="l-page">
  <embed src="{{slug}}.pdf" type="application/pdf" height="955px" width="100%">
</div>', data = list(slug = metadata$slug)
      )
    } else {
      NULL
    }

    # Add custom appendix
    data <- list()
    if(file.exists(suppl <- xfun::with_ext(metadata$slug, ".zip"))) {
    # if (!is.null(metadata$supplementary_materials)) {
      data <- c(data, list(supp = suppl))
    }
    if (!is.null(metadata$CTV)) {
      if (length(metadata$packages$cran) > 0) {
        CTV <- sprintf("[%s](https://cran.r-project.org/view=%s)", metadata$CTV, metadata$CTV)
        CTV <- paste(CTV, collapse = ", ")
        data <- c(data, list(CTV = CTV))
      }
    }
    if (!is.null(metadata$packages)) {
      if (length(metadata$packages$cran) != 0) {
        CRAN <- sprintf("[%s](https://cran.r-project.org/package=%s)", metadata$packages$cran, metadata$packages$cran)
        CRAN <- paste(CRAN, collapse = ", ")
        data <- c(data, list(CRAN = CRAN))
      }
      if (length(metadata$packages$bioc) != 0) {
        BIOC <- sprintf("[%s](https://www.bioconductor.org/packages/%s)", metadata$packages$bioc, metadata$packages$bioc)
        BIOC <- paste(BIOC, collapse = ", ")
        data <- c(data, list(BIOC = BIOC))
      }
    }

    template <- xfun::read_utf8(system.file("appendix.md", package = "rjtools"))
    appendix <- whisker::whisker.render(template, data)

    input <- xfun::read_utf8(input_file)
    front_matter_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input)

    xfun::write_utf8(
      c(
        "---",
        yaml::as.yaml(metadata),
        "---",
        "",
        if(! web_only) embed_pdf else input[(front_matter_delimiters[2]+1):length(input)],
        "",
        appendix
      ),
      input_file
    )

    # Custom args
    args <- c(
      "--number-sections",
      rmarkdown::pandoc_include_args(
        in_header = system.file("rjdistill.html", package = "rjtools")
      )
    )

    args
  }

  on_exit <- function() {
    # TODO: This should be done in a temp directory
    # and files produced moved back into the main dir.

    # Skip rendering pdf for non-article pages
    if(is.null(render_pdf)) return()

    # Update legacy PDF metadata just by changing the wrapper
    if (legacy_pdf) {
      wrapper_path <- file.path(dirname(rmd_path), "RJwrapper.tex")
      if(!file.exists(wrapper_path)) {
        warning("Could not find wrapper for this legacy article, so the PDF could not be updated.")
        return()
      }

      # Update wrapper with new metadata
      wrapper <- xfun::read_utf8(wrapper_path)
      has_parent_dir <- function(path, nm){
        if(basename(path) == nm) return(TRUE)
        if(path == dirname(path)) return(FALSE)
        has_parent_dir(dirname(path), nm)
      }
      is_repo <- has_parent_dir(normalizePath(wrapper_path), "rjournal.github.io")
      article_section <- if(is_repo) {
        if(has_parent_dir(normalizePath(wrapper_path), "_articles")) {
          "Contributed research article"
        } else {
          "News and notes"
        }
      } else {
        "Contributed research article"
      }

      issue_months <- if(article_metadata$volume < 14) {
        c("June", "December")
      } else {
        c("March", "June", "September", "December")
      }
      wrapper[str_which(wrapper, "^\\s*\\\\sectionhead")] <- sprintf("\\sectionhead{%s}", article_section)
      wrapper[str_which(wrapper, "^\\s*\\\\volume")] <- sprintf("\\volume{%s}", article_metadata$volume)
      wrapper[str_which(wrapper, "^\\s*\\\\volnumber")] <- sprintf("\\volnumber{%s}", article_metadata$issue)
      wrapper[str_which(wrapper, "^\\s*\\\\year")] <- sprintf("\\year{%s}", 2008 + article_metadata$volume)
      wrapper[str_which(wrapper, "^\\s*\\\\month")] <- sprintf("\\month{%s}", issue_months[article_metadata$issue])

      # Set page count
      wrapper_page_counter <- which(str_detect(wrapper, "^\\s*\\\\setcounter\\{page\\}\\{.+\\}"))
      if(length(wrapper_page_counter) == 0) {
        wrapper_page_counter <- which(str_detect(wrapper, "^\\s*\\\\month\\{.+\\}")) + 1
        wrapper <- append(wrapper, "", wrapper_page_counter - 1)
      }
      wrapper[wrapper_page_counter] <- paste0("\\setcounter{page}{", article_metadata$journal$firstpage, "}")
      if(identical(wrapper, xfun::read_utf8(wrapper_path))) return()
      message("Detected changes to the article metadata, re-building PDF.")
      xfun::write_utf8(wrapper, wrapper_path)

      oldwd <- getwd()
      on.exit(setwd(oldwd))
      setwd(dirname(wrapper_path))
      file.copy(
        system.file("tex/RJournal.sty", package = "rjtools"),
        "RJournal.sty"
      )
      on.exit(
        file.remove("RJournal.sty"),
        add = TRUE
      )
      pdf_path <- xfun::with_ext(article_metadata$slug, ".pdf")
      tinytex::latexmk(
        wrapper_path,
        base_format$pandoc$latex_engine,
        pdf_file = pdf_path,
        clean = TRUE
      )

      # Update metadata with new page numbers
      if(requireNamespace("pdftools", quietly = TRUE)) {
        yml <- rmarkdown::yaml_front_matter(rmd_path)
        yml$journal$lastpage <- yml$journal$firstpage + pdftools::pdf_length(pdf_path)
        update_front_matter(yml, rmd_path)
      }
    } else {
      callr::r(function(input){
        rmarkdown::render(
          input,
          output_format = "rjtools::rjournal_pdf_article"
        )
      }, args = list(input = rmd_path))
    }
  }

  rmarkdown::output_format(
    knitr = NULL, # use base one
    pandoc = list(
      args = args,
      lua_filters = c(
        system.file("latex-pkg.lua", package = "rjtools"),
        system.file("sec-depth.lua", package = "rjtools")
      )
    ),
    keep_md = NULL, # use base one
    clean_supporting = NULL, # use base one
    pre_knit = NULL,
    # post_knit = post_knit, # passed directly to base_format
    pre_processor = pre_processor,
    on_exit = on_exit,
    base_format = base_format
  )
}
