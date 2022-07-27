article_doi <- function(metadata) {
  timestamp <- format(Sys.time(), format = "%Y%m%d%H%M%S")
  doi_template <- xfun::read_utf8(system.file("doi_template.xml", package = "rjtools"))

  articles_metadata <- lapply(metadata, function(x) {
    published_date <- stringr::str_match(x$date, "(\\d{4})-(\\d{2})-(\\d{2})")
    list(
      title = x$title,
      authors = mapply(function(z, i) {
        name <- stringr::str_match(z$name, "([^\\s]+)\\s*(.*)")
        list(
          family = name[,3],
          given = name[,2],
          suffix = z$suffix,
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

  volume <- metadata[[1]]$volume
  issue <- metadata[[1]]$issue
  issue_year <- 2008 + volume
  issue_month <- if(volume < 14) c("06", "12")[issue] else c("03", "06", "09", "12")[issue]

  whisker::whisker.render(
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

      issue_year = issue_year,
      issue_month = issue_month,
      issue_volume = volume,
      issue_num = issue,

      articles = articles_metadata
    )
  )
}
