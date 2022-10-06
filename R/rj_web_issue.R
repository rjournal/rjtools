#' R Markdown output formats for R Journal issues
#'
#' The R Journal is built upon the distill framework with some modifications.
#' This output format behaves almost identically to the
#' `distill::distill_article()` format, with some formatting and structural
#' changes.
#'
#' @param ... Arguments passed to `distill::distill_article()`.
#' @param rnews This issue is from R News.
#' @inheritParams distill::distill_article
#' @export
#' @rdname rjournal_issue
rjournal_web_issue <- function(toc = FALSE, self_contained = FALSE, rnews = FALSE, ...) {
  base_format <- distill::distill_article(
    self_contained = self_contained, toc = toc, ...
  )
  distill_post_knit <- base_format$post_knit

  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                            output_dir) {

    input <- xfun::read_utf8(input_file)
    front_matter_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input)

    issue_slug <- paste(metadata$volume + if(rnews) 2000 else 2008, metadata$issue, sep = "-")

    issue_articles <- list_issue_articles(metadata$volume, metadata$issue, rnews)
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

    articles <- lapply(
      c(metadata$articles$before, metadata$articles["Contributed Research Articles"], metadata$articles$after),
      function(slugs) {
        issue_articles[match(slugs, article_slugs)]
      }
    )

    issue_news <- list_issue_news(metadata$volume, metadata$issue, rnews)
    news_slugs <- vapply(issue_news, function(art) art[["slug"]], character(1L))
    editorial_idx <- which(grepl("editorial$", news_slugs))
    if(length(editorial_idx) != 1) {
      stop("Could not uniquely identify the editorial article. Ensure that the editorial's slug ends with 'editorial'.")
    }
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
    metadata$news <- c(
      # Manually specified news (prioritised to top)
      intersect(metadata$news, news_slugs),
      # All remaining news articles
      setdiff(news_slugs, metadata$news)
    )
    news <- issue_news[match(metadata$news, news_slugs)]

    svg_pdf <- '<svg viewBox="0 0 384 512" style="height:1em;position:relative;display:inline-block;top:.1em;" xmlns="http://www.w3.org/2000/svg">
      <path d="M181.9 256.1c-5-16-4.9-46.9-2-46.9 8.4 0 7.6 36.9 2 46.9zm-1.7 47.2c-7.7 20.2-17.3 43.3-28.4 62.7 18.3-7 39-17.2 62.9-21.9-12.7-9.6-24.9-23.4-34.5-40.8zM86.1 428.1c0 .8 13.2-5.4 34.9-40.2-6.7 6.3-29.1 24.5-34.9 40.2zM248 160h136v328c0 13.3-10.7 24-24 24H24c-13.3 0-24-10.7-24-24V24C0 10.7 10.7 0 24 0h200v136c0 13.2 10.8 24 24 24zm-8 171.8c-20-12.2-33.3-29-42.7-53.8 4.5-18.5 11.6-46.6 6.2-64.2-4.7-29.4-42.4-26.5-47.8-6.8-5 18.3-.4 44.1 8.1 77-11.6 27.6-28.7 64.6-40.8 85.8-.1 0-.1.1-.2.1-27.1 13.9-73.6 44.5-54.5 68 5.6 6.9 16 10 21.5 10 17.9 0 35.7-18 61.1-61.8 25.8-8.5 54.1-19.1 79-23.2 21.7 11.8 47.1 19.5 64 19.5 29.2 0 31.2-32 19.7-43.4-13.9-13.6-54.3-9.7-73.6-7.2zM377 105L279 7c-4.5-4.5-10.6-7-17-7h-6v128h128v-6.1c0-6.3-2.5-12.4-7-16.9zm-74.1 255.3c4.1-2.7-2.5-11.9-42.8-9 37.1 15.8 42.8 9 42.8 9z"></path>
      </svg>'
    xfun::write_utf8(
      c(
        "---",
        yaml::as.yaml(metadata),
        "---",
        input[(front_matter_delimiters[2]+1):length(input)],
        "",
        paste0("[Complete issue ", svg_pdf, "](", xfun::with_ext(issue_slug, "pdf"), ")"),
        "",
        "## Table of contents",
        news_entries(editorial),
        articles_toc(articles),
        news_toc(list("News and Notes" = news))
      ),
      input_file
    )

    # Create DOI
    xfun::write_utf8(
      article_doi(metadata, issue_articles),
      "doi.xml"
    )

    # Custom args
    args <- rmarkdown::pandoc_include_args(
      in_header = system.file("rjissue.html", package = "rjtools")
    )

    args
  }

  on_exit <- function() {
    # Deactivate for now as I am not sure to understand what should be built
    # if (!is.null(render_pdf) && !legacy_pdf) {
    #   callr::r(function(input){
    #     rmarkdown::render(
    #       input,
    #       output_format = "rjtools::rjournal_pdf_article"
    #     )
    #   }, args = list(input = rmd_path))
    # }
  }

  rmarkdown::output_format(
    knitr = NULL, # use base one
    pandoc = list(),
    keep_md = NULL, # use base one
    clean_supporting = NULL, # use base one
    pre_knit = NULL,
    # post_knit = post_knit, # passed directly to base_format
    pre_processor = pre_processor,
    on_exit = on_exit,
    base_format = base_format
  )
}


list_issue_articles <- function(volume, issue, rnews = FALSE) {
  prefix <- if(rnews) "RN" else "RJ"
  articles <- list.files(
    list.dirs("../../_articles", recursive = FALSE),
    paste0(prefix, "-\\d{4}-\\d{3}\\.(r|R)md"),
    full.names = TRUE
  )

  front_matter <- lapply(articles, rmarkdown::yaml_front_matter)

  issue_articles <- Filter(
    function(yml) yml$volume == volume && yml$issue == issue,
    front_matter
  )

  # Sort by received date
  issue_article_sort <- as.Date(vapply(issue_articles, date_received_from_metadata, character(1L)))
  issue_articles[order(issue_article_sort)]
}

list_issue_news <- function(volume, issue, rnews = FALSE) {
  base_year <- if(rnews) 2000 else 2008
  news_pattern <- paste0("R(J|N)-", base_year + volume, "-", issue, "-.+")
  news <- list.files(
    dir("../../_news", news_pattern, full.names = TRUE),
    paste0("\\.(r|R)md$"),
    full.names = TRUE
  )
  news <- news[basename(dirname(news)) == xfun::sans_ext(basename(news))]

  # Sort by page number
  news <- lapply(news, rmarkdown::yaml_front_matter)
  news_pages <- vapply(news, function(x) as.integer(x$journal$firstpage %||% 1L), integer(1L))
  news[order(news_pages)]
}

news_toc <- function(contents) {
  unlist(
    mapply(
      function(header, articles) {
        if(length(articles) == 0) return(NULL)
        c(paste("###", header), news_entries(articles))
      },
      names(contents), contents,
      SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
  )
}

news_entries <- function(news) {
  vapply(news, news_entry, character(1L))
}
news_entry <- function(art) {
  if(is.null(art$author)) art$author <- ""
  if(!is.character(art$author)) {
    art$author <- vapply(art$author, function(z) z$name %||% paste(z$first_name, z$last_name), character(1L))
  }
  stringr::str_glue(
    "[{art$title}](../../news/{art$slug})<br>{glue::glue_collapse(art$author, sep = ', ', last = ' and ')} {art$journal$firstpage %||% art$pages[1] %||% \"\"}\n\n"
  )
}

articles_toc <- function(contents) {
  contents <- Filter(function(x) length(x)>0, contents)
  unlist(
    mapply(
      function(header, articles) c(paste("###", header), article_entries(articles)),
      names(contents), contents,
      SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
  )
}

article_entries <- function(articles) {
  vapply(articles, article_entry, character(1L))
}

article_entry <- function(art) {
  if(!is.character(art$author)) {
    art$author <- vapply(art$author, function(z) z$name %||% paste(z$first_name, z$last_name), character(1L))
  }

  stringr::str_glue(
    "[{art$title}](../../articles/{art$slug})<br>{glue::glue_collapse(art$author, sep = ', ', last = ' and ')} {art$journal$firstpage %||% art$pages[1] %||% \"\"}\n\n"
  )
}

date_received_from_metadata <- function(metadata) {
  metadata$date_received %||% metadata$date %||% paste0(2008 + metadata$volume, "-", 6 * metadata$issue, "-01")
}
