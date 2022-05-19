#' R Markdown output formats for R Journal issues
#'
#' The R Journal is built upon the distill framework with some modifications.
#' This output format behaves almost identically to the
#' `distill::distill_article()` format, with some formatting and structural
#' changes.
#'
#' @param ... Arguments passed to `distill::distill_article()`.
#' @inheritParams distill::distill_article
#' @export
#' @rdname article
rjournal_web_issue <- function(toc = FALSE, self_contained = FALSE, ...) {
  base_format <- distill::distill_article(
    self_contained = self_contained, toc = toc, ...
  )
  distill_post_knit <- base_format$post_knit

  rmd_path <- NULL
  render_pdf <- NULL

  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                            output_dir) {

    input <- xfun::read_utf8(input_file)
    front_matter_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input)

    issue_articles <- list_issue_articles(metadata$volume, metadata$issue)
    issue_article_sort <- as.Date(vapply(issue_articles, date_received_from_metadata, character(1L)))
    issue_articles <- issue_articles[order(issue_article_sort)]
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

    articles <- lapply(metadata$articles, function(slugs) {
      issue_articles[match(slugs, article_slugs)]
    })

    issue_news <- list_issue_news(metadata$volume, metadata$issue)
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
      # All remaining articles
      setdiff(news_slugs, metadata$news)
    )
    news <- issue_news[match(metadata$news, news_slugs)]

    xfun::write_utf8(
      c(
        "---",
        yaml::as.yaml(metadata),
        "---",
        input[(front_matter_delimiters[2]+1):length(input)],
        "",
        "## Table of contents",
        news_entries(editorial),
        articles_toc(articles),
        news_toc(list("News and Notes" = news))
      ),
      input_file
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


list_issue_articles <- function(volume, issue) {
  articles <- list.files(
    list.dirs("~/github/rjournal.github.io/_articles", recursive = FALSE),
    "RJ-\\d{4}-\\d{3}\\.(r|R)md",
    full.names = TRUE
  )

  front_matter <- lapply(articles, rmarkdown::yaml_front_matter)

  issue_articles <- Filter(
    function(yml) yml$volume == volume && yml$issue == issue,
    front_matter
  )
}

list_issue_news <- function(volume, issue) {
  news_pattern <- paste0("RJ-", 2008 + volume, "-", issue, "-.+")
  news <- list.files(
    dir("~/github/rjournal.github.io/_news", news_pattern, full.names = TRUE),
    paste0("\\.(r|R)md$"),
    full.names = TRUE
  )

  lapply(news, rmarkdown::yaml_front_matter)
}

news_toc <- function(contents) {
  unlist(
    mapply(
      function(header, articles) c(paste("###", header), news_entries(articles)),
      names(contents), contents,
      SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
  )
}

news_entries <- function(news) {
  vapply(news, news_entry, character(1L))
}
news_entry <- function(art) {
  if(!is.character(art$author)) {
    art$author <- vapply(art$author, function(z) z$name, character(1L))
  }
  stringr::str_glue(
    "[{art$title}](../../news/{art$slug})<br>{glue::glue_collapse(art$author, sep = ', ', last = ' and ')} {art$journal$firstpage %||% art$pages[1]}\n\n"
  )
}

articles_toc <- function(contents) {
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
    art$author <- vapply(art$author, function(z) z$name, character(1L))
  }


  stringr::str_glue(
    "[{art$title}](../../articles/{art$slug})<br>{glue::glue_collapse(art$author, sep = ', ', last = ' and ')} {art$journal$firstpage %||% art$pages[1]}\n\n"
  )
}

date_received_from_metadata <- function(metadata) {
  metadata$date_received %||% metadata$date %||% paste0(2008 + metadata$volume, "-", 6 * metadata$issue, "-01")
}
