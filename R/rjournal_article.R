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
create_article <- function(file_name = "article", dir_path = "paper"){

  force(dir_path)
  # read in all the template files
  folder <- c("skeleton", "resources")
  templates <- fs::dir_ls(system.file("rmarkdown", "templates", "rjournal", folder, package = "rjtools"))

  # create directory
  fs::dir_create(path = dir_path)

  # move rmd as well as change the bib parameter as file_name
  template_rmd <- xfun::read_utf8(templates[str_detect(templates, "Rmd")])
  xfun::write_utf8(template_rmd,
                   file.path(dir_path, xfun::with_ext(file_name, "Rmd")))

  # move all others templates
  all_others <- templates[!str_detect(templates, "Rmd")]
  fs::file_copy(all_others, dir_path, overwrite = TRUE)


  cli::cli_alert_success(glue::glue("Article created in the directory called ", dir_path, " :)"))

}
