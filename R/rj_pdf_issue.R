#' R Markdown output formats for R Journal issues
#'
#' The R Journal is built upon the distill framework with some modifications.
#' This output format produces the PDF for an R Journal issue.
#'
#' @param ... Arguments passed to `rmarkdown::pdf_document()`.
#' @param render_all Re-render all articles in the issue, even if the page numbers have not changed.
#' @return an generated R Journal issue
#' @export
#' @rdname rjournal_issue
rjournal_pdf_issue <- function(..., render_all = FALSE) {
  require_package("pdftools")

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
    article_slugs <- vapply(issue_articles, function(art) art[["slug"]], character(1L))

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

    get_article_data <- function(slugs) {
      if(is.list(slugs)) return(lapply(slugs, get_article_data))
      issue_articles[match(slugs, article_slugs)]
    }
    articles <- lapply(metadata$articles, get_article_data)
    # Update order of PDF article slugs
    article_slugs <<- unlist(metadata$articles, use.names = FALSE)

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
      articles$before,
      articles["Contributed Research Articles"],
      articles$after,
      list(`News and Notes` = news)
    )

    # Check and update article page numbers
    # 1. Obtain page length of toc
    # 2. Obtain page length of article pdfs
    # 3. Using article order, compute and assign page numbers
    # 4. If the page numbers have changed for an article, re-render it.
    # 5. Generate toc with the appropriate page numbers for articles
    make_toc <- function(articles, toc_section, skip_updates = FALSE) {
      c(
        if(toc_section=="")
          NULL
        else
          paste0("\\addtocontents{toc}{\\protect\\subsection*{", toc_section, "}\\protect}"),
        vapply(articles, function(x) {
          start_page <- as.integer(x$journal$firstpage %||% x$pages[1] %||% 1L)
          end_page <- as.integer(x$journal$lastpage %||% x$pages[2] %||% 1L)

          art_type <- if(grepl("^RJ-\\d{4}-\\d{3}$", x$slug)) "_articles" else "_news"
          art_rmd <- file.path("..", "..", art_type, x$slug, xfun::with_ext(x$slug, ".Rmd"))
          art_pdf <- xfun::with_ext(art_rmd, ".pdf")

          # Render the PDF if it doesn't exist yet
          if(!xfun::file_exists(art_pdf)) {
            message(sprintf("Rendering PDF for '%s' article.", x$slug))
            callr::r(function(input){
              rmarkdown::render(
                input,
                output_format = "rjtools::rjournal_pdf_article"
              )
            }, args = list(input = art_rmd))
          }

          # Calculate and update page ranges
          pdf_pages <- pdftools::pdf_length(art_pdf)
          if(!skip_updates) {
            pages_match <- identical(current_page, start_page) && identical(current_page + pdf_pages - 1L, end_page)
            if(!pages_match || render_all) {
              end_page <- x$journal$lastpage <- current_page + pdf_pages - 1L
              start_page <- x$journal$firstpage <- current_page
              x$draft <- FALSE
              update_front_matter(x, art_rmd)
              message(sprintf("Updating page numbers for '%s' article.", x$slug))
              callr::r(function(input){
                rmarkdown::render(
                  input,
                  output_format = "rjtools::rjournal_article"
                )
              }, args = list(input = art_rmd))
            }
          }
          current_page <<- end_page + 1L
          paste0(
            "\\addtocontents{toc}{\\protect\\contentsline{chapter}{\\protect\\numberline{}",
            x$title, "}{", start_page, "}{}}"
          )
        }, character(1L))
      )
    }

    toc <- mapply(make_toc, articles, names(articles), skip_updates = TRUE)
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
      toc_file <- file.path(tempdir(), "toc.md")
    )
    file.copy(
      c(
        # Style files
        list.files(system.file("tex", package = "rjtools"), full.names = TRUE),
        # R Logo
        system.file("Rlogo-5.png", package = "rjtools")
      ),
      dirname(toc_file)
    )
    rmarkdown::pandoc_convert(
      toc_file, to = "pdf", output = toc_pdf <- xfun::with_ext(toc_file, ".pdf"),
      options = c(
        "--template", system.file("issue.tex", package = "rjtools")
      )
    )

    current_page <- pdftools::pdf_length(toc_pdf) + 1L
    #as.integer(articles[[1]][[1]]$journal$firstpage %||% articles[[1]]$pages[1] %||% 1)
    toc <- mapply(make_toc, articles, names(articles))

    metadata$issue_year <- 2008 + metadata$volume
    issue_months <- if(metadata$volume < 14) {
      c("June", "December")
    } else {
      c("March", "June", "September", "December")
    }
    metadata$issue_month <- issue_months[metadata$issue]

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
    file.copy(
      list.files(system.file("tex", package = "rjtools"), full.names = TRUE),
      dirname(input_file)
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
        tmp <- tempfile(fileext = ".pdf")
      )
    )

    file.copy(tmp, output_file, overwrite = TRUE)
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
