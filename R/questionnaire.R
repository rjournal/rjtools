#' Submit a paper to the R Journal
#'
#' This is a last-minute self-serve  check function, similar to
#' [devtools::release()], for handling R Journal styling that can be difficult
#'  with automatic checks.
#'
#' @export
submit_rjournal <- function(){

  if (!yesno(
  "Have you checked .bib file titles? They need to be:
  1) package name should be protected, i.e. {ggplot2} and always in lower case,
  2) the programming language R should be protected, i.e. {R}, and
  3) both sentence or title case are accepteable but need to be consistent")){
    return(invisible())
  }

  if (!yesno(
    "Have you include the automatic check results (with `initial_check_article`)
    in the motivation letter? Checks with WARNINGs and ERRORs need to be
    commented with reasons.")){
    return(invisible())
  }

  if (yesno("Ready to submit?")){
    browseURL("https://docs.google.com/forms/d/e/1FAIpQLSeqtHH0g9JhrNEwT4ScqVAQe6Qq4om1-EfUqBe12YsPrrB6WQ/viewform")
    return(invisible())
  }


}
