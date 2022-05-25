#' R Markdown output formats for R Journal issues
#'
#' The R Journal is built upon the distill framework with some modifications.
#' This output format produces the PDF for an R Journal issue.
#'
#' @param ... Arguments passed to `rmarkdown::pdf_document()`.
#' @export
#' @rdname rjournal_issue
rjournal_pdf_issue <- function(...) {

  editorial_slug <- NULL
  article_slugs <- NULL
  news_slugs <- NULL

  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                            output_dir) {
    file.copy(
      list.files(system.file("tex", package = "rjtools"), full.names = TRUE),
      dirname(input_file)
    )

    input <- xfun::read_utf8(input_file)
    front_matter_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input)

    issue_articles <- list_issue_articles(metadata$volume, metadata$issue)
    article_slugs <<- vapply(issue_articles, function(art) art[["slug"]], character(1L))

    special_slugs <- unlist(metadata$articles, use.names = FALSE)
    if(any(!special_slugs %in% article_slugs)) {
      warning(
        sprintf(
          "Could not find these special articles for this issue: %s",
          glue::glue_collapse(setdiff(special_slugs, article_slugs), sep = ", ")
        )
      )
    }
    metadata$articles$`Contributed Research Articles` <- c(
      # Manually declared articles (prioritised to top)
      intersect(metadata$articles$`Contributed Research Articles`, article_slugs),
      # All remaining articles
      setdiff(article_slugs, unlist(metadata$articles))
    )

    articles <- lapply(metadata$articles, function(slugs) {
      issue_articles[match(slugs, article_slugs)]
    })

    # Organise news metadata
    issue_news <- list_issue_news(metadata$volume, metadata$issue)
    news_slugs <- vapply(issue_news, function(art) art[["slug"]], character(1L))
    editorial_idx <- which(grepl("editorial$", news_slugs))
    if(length(editorial_idx) != 1) {
      stop("Could not uniquely identify the editorial article. Ensure that the editorial's slug ends with 'editorial'.")
    }
    editorial_slug <<- news_slugs[editorial_idx]
    editorial <- issue_news[editorial_idx]
    issue_news <- issue_news[-editorial_idx]
    news_slugs <- news_slugs[-editorial_idx]
    news_specified <- unlist(metadata$news, use.names = FALSE)
    if(any(!news_specified %in% news_slugs)) {
      warning(
        sprintf(
          "Could not find these news articles for this issue: %s",
          glue::glue_collapse(setdiff(news_specified, news_slugs), sep = ", ")
        )
      )
    }
    news_slugs <<- metadata$news <- c(
      # Manually specified news (prioritised to top)
      intersect(metadata$news, news_slugs),
      # All remaining articles
      setdiff(news_slugs, metadata$news)
    )
    news <- issue_news[match(metadata$news, news_slugs)]

    articles <- c(
      list(editorial),
      articles,
      list(`News and Notes` = news)
    )

    toc <- mapply(function(articles, toc_section) {
      c(
        if(toc_section=="")
          NULL
        else
          paste0("\\addtocontents{toc}{\\protect\\subsection*{", toc_section, "}\\protect}"),
        vapply(articles, function(x) {
          start_page <- x$journal$firstpage %||% x$pages[1]
          paste0(
            "\\addtocontents{toc}{\\protect\\contentsline{chapter}{\\protect\\numberline{}",
            x$title, "}{", start_page, "}{}}"
          )
        }, character(1L))
      )
    }, articles, names(articles))

    metadata$issue_year <- 2008 + metadata$volume
    metadata$issue_month <- c("June", "December")[metadata$issue]

    xfun::write_utf8(
      c(
        "---",
        yaml::as.yaml(metadata),
        "---",
        input[(front_matter_delimiters[2]+1):length(input)],
        "",
        unlist(toc),
        # Add empty content to ensure toc is generated
        "\\empty"
      ),
      input_file
    )

    NULL

  }

  post_processor <- function(metadata, input, output_file, ...) {
    # Join article PDFs to single issue
    editorial_pdf <- normalizePath(
      file.path("..", "..", "_news", editorial_slug, xfun::with_ext(editorial_slug, "pdf"))
    )
    article_pdf <- normalizePath(
      file.path("..", "..", "_articles", article_slugs, xfun::with_ext(article_slugs, "pdf"))
    )
    news_pdf <- normalizePath(
      file.path("..", "..", "_news", news_slugs, xfun::with_ext(news_slugs, "pdf"))
    )
    system(
      sprintf(
        "pdftk %s cat output %s",
        paste(c(output_file, editorial_pdf, article_pdf, news_pdf), collapse = " "),
        tmp <- tempfile(fileext = "pdf")
      )
    )

    file.rename(tmp, output_file)
    output_file
  }

  template <- system.file("issue.tex", package = "rjtools")

  rmarkdown::output_format(
    knitr = NULL, # use base one
    pandoc = list(),
    pre_processor = pre_processor,
    post_processor = post_processor,
    base_format = rmarkdown::pdf_document(toc = TRUE, template = template, ...)
  )
}
