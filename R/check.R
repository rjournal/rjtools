# suppose each article is in a separate repo
# currently, there are multiple articles in the `ae-articles-testing`, supply an article id to locate which article

check <- function(article = NULL){

  # find .tex file of the article

  tex <- readLines(here::here("Submission", article), pattern = ".tex")

  check_title(tex)
  check_section(tex)

}


check_title <- function(tex){

  str_raw <- purrr::map_chr(tex, ~stringr::str_extract(.x,  "(?<=\\\\title\\{).*?(?=\\})"))
  str <- str_raw[!is.na(str_raw)]

  # str_to_title changes the first letter of all to capital - also for proposition
  if (tools::toTitleCase(str) != str){
    log_error("The title is not in title case!")
  } else{
    log_success("The article title is properly formatted in title case")
  }

}


check_section <- function(tex){

  str_raw <- purrr::map_chr(tex, ~stringr::str_extract(.x,  "(?<=\\\\section\\{).*?(?=\\})"))
  str <- str_raw[!is.na(str_raw)]

  if (!all(str_to_sentence(str) == str)){
    problem_one <- str[!str_to_sentence(str) == str]
    log_error(" Section {problem_one} is not in sentence case!")
  } else{
    log_success("All sections is properly formatted in sentence case")
  }

}

check_spelling <- function(text, dic = "en_US"){

  detect_abstract <- purrr::map_chr(tex, ~stringr::str_extract(.x,  "(?<=\\\\abstract\\{).*?"))
  abstract_loc <- match(detect_abstract[!is.na(detect_abstract)], detect_abstract)

  detect_bib <- purrr::map_chr(tex, ~stringr::str_extract(.x,  "(?<=\\\\bibliography\\{).*?(?=\\})"))
  bib_loc <- match(detect_bib[!is.na(detect_bib)], detect_bib)

  to_replace <- paste(spell_to_remove, collapse = "|")
  tex2 <- stringr::str_replace_all(tex, to_replace, "")
  text_bw <- tex2[(abstract_loc + 1):(bib_loc - 1)]

  chunk_begin <- which(text_bw == "\\begin{Schunk}")
  chunk_end <- which(text_bw == "\\end{Schunk}")
  chunk_idx <- unlist(purrr::map2(chunk_begin, chunk_end, ~.x:.y))

  select_idx <- !c(1:length(text_bw)) %in% chunk_idx
  text_clean <- text_bw[select_idx]

  check_raw <- hunspell::hunspell(text_clean, format = "latex", dic = dictionary(dic))
  check <- unique(unlist(check_raw))
  check_out <- check[toupper(check) != check & tools::toTitleCase(check) != check] # remove acronym

  check_out

}

spell_to_remove <- c("(\\\\url\\{(.*)\\})",
                     "(\\\\href\\{(.*)\\})",
                     "(\\\\label\\{(.*)\\})",
                     "(\\\\nameref\\{(.*)\\})",
                     "(\\\\code\\{(.*)\\})",
                     "(\\\\CRANpkg\\{(.*)\\})",
                     "(\\\\pkg\\{(.*)\\})",
                     '("(.*)")',
                     "(\\\\textt\\{(.*)\\})",
                     "(\\\\emph\\{(.*)\\})",
                     "(\\\\file\\{(.*)\\})",
                     "(\\\\includegraphics\\[(.*)\\})",
                     "(emph)"
                     )


#####################################################

log_factory <- function(prefix, .f) {

  # Guarantee definitions exist in the factory environment
  force(.f)
  force(prefix)

  function(text, ..., file = stdout(), .envir = parent.frame()) {

    text <- glue::glue(prefix, text, .envir = .envir)

    .f(text)

    # Send output to the log file
    cat(text, "\n", sep = "", file = file, append = TRUE)

  }
}

#' Produce a log file entry for an error
#'
#' Append a line in the log file that details an error
#'
#' @param text Description of the error that occurred
#' @param ... Additional inputs for text passed to the glue function
#' @param .envir The environment used to find the text string replacements
#' @param file The console output directed to the log, using `stdout`
#'
#'
log_error <- log_factory(prefix = "ERROR: ", .f = cli::cli_alert_warning)

#' Produce a log file entry for a success
#'
#' Append a line in the log file that details a success
#'
#' @param text Description of the error that occurred
#' @param ... Additional inputs for text passed to the glue function
#' @param .envir The environment used to find the text string replacements
#' @param file The console output directed to the log, using `stdout`
#'
#'
log_success <- log_factory(prefix = "SUCCESS: ", .f = cli::cli_alert_success)


#' Produce a log file entry for a note
#'
#' Append a line in the log file that details a note
#'
#' @param text Description of the error that occurred
#' @param ... Additional inputs for text passed to the glue function
#' @param .envir The environment used to find the text string replacements
#' @param file The console output directed to the log, using `stdout`
#'
#'
log_note <- log_factory(prefix = "NOTE: ", .f = cli::cli_alert_info)
