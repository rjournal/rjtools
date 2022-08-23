article_doi <- function(issue_metadata, article_metadata) {
  timestamp <- format(Sys.time(), format = "%Y%m%d%H%M%S")
  doi_template <- xfun::read_utf8(system.file("doi_template.xml", package = "rjtools"))

  articles_metadata <- lapply(article_metadata, function(x) {
    published_date <- stringr::str_match(x$date, "(\\d{4})-(\\d{2})-(\\d{2})")
    list(
      title = x$title,
      authors = mapply(function(z, i) {
        if(!is.list(z)) {
          z <- list(name = z)
        }
        if(!is.null(z$name)) {
          name <- stringr::str_match(z$name, "([^\\s]+)\\s*(.*)")
          z$first_name <- name[,2]
          z$last_name <- name[,3]
        }
        list(
          family = z$last_name,
          given = z$first_name,
          affiliation = z$affiliation,
          orcid_id = z$orcid_id,
          author_order = if(i == 1L) "first" else "additional"
        )
      }, x$author, seq_along(x$author), SIMPLIFY = FALSE),
      slug = x$slug,
      pub_day = published_date[,4],
      pub_month = published_date[,3],
      pub_year = published_date[,2],
      journal = x$journal
    )
  })

  volume <- issue_metadata$volume
  issue <- issue_metadata$issue
  issue_date <- as.Date(issue_metadata$date)
  # issue_year <- 2008 + volume
  # issue_month <- if(volume < 14) c("06", "12")[issue] else c("03", "06", "09", "12")[issue]

  whisker::whisker.render(
    # Template from schema examples here:
    # https://gitlab.com/crossref/schema/-/blob/master/best-practice-examples/journal_article_4.8.0.xml
    # https://gitlab.com/crossref/schema/-/blob/master/best-practice-examples/journal.vol.issue5.3.0.xml
    doi_template,
    data = list(
      # Constants
      doi_prefix = "10.32614",
      rjournal_url = "https://journal.r-project.org",

      depositor_name = "R Foundation",
      depositor_email = "R-foundation@r-project.org",

      journal_title = "The R Journal",
      journal_abbreviated_title = "The R Journal",
      coden = NULL, # JSS is "JSSOBK", we don't have one?
      issn = "2073-4859", # 1548-7660",

      batch_id = timestamp,
      timestamp = timestamp,

      issue_year = format(issue_date, "%Y"),
      issue_month = format(issue_date, "%m"),
      issue_volume = volume,
      issue_num = issue,

      articles = articles_metadata
    )
  )
}
