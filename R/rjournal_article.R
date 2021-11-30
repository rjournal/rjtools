#' Create an R journal article with the existing template
#'
#' @details
#' See \code{vignette("create_article", package = "rjtools")} for how to create an article
#'
#' @param file_name the name of the Rmd file
#' @param dir_path the directory name that houses the template files
#' @importFrom stringr str_extract
#' @importFrom fs dir_create file_copy file_move
#' @export
create_article <- function(file_name = "article", dir_path = here::here()){

  # read in all the template files
  folder <- c("skeleton", "resources")
  templates <- fs::dir_ls(
    system.file("rmarkdown", "templates","rjournal", glue::glue("{folder}"),
                package = "rjtools"))

  # create directory
  fs::dir_create(path = dir_path)
  fs::dir_create(path = file.path(dir_path, "figures/"))

  # move rmd as well as change the bib parameter as file_name
  template_rmd <- xfun::read_utf8(templates[str_detect(templates, "Rmd")])
  template_rmd <- whisker::whisker.render(
    template = template_rmd,
    data = list(bibfile = glue::glue(file_name, ".bib")))
  template_rmd <- str_split(template_rmd, "\n")[[1]]
  usethis::write_over(file.path(dir_path, glue::glue("{file_name}.Rmd")),
                      template_rmd, quiet = TRUE)

  # move all others templates
  all_others <- templates[!str_detect(templates, "Rmd")]
  fs::file_copy(all_others, dir_path, overwrite = TRUE)

  # rename/re-move bib file and penguins png
  fs::file_move(file.path(dir_path, "RJreferences.bib"),
                file.path(dir_path, glue::glue("{file_name}.bib")))
  fs::file_move(file.path(dir_path, "penguins.png"),
                file.path(dir_path, "figures", "penguins.png"))


  cli::cli_alert_success("Article created :)")

}
