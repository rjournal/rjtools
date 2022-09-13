replace_names <- function(x, replacements) {
  i <- match(names(x), names(replacements))
  nm_replace <- which(!is.na(i))
  names(x)[nm_replace] <- unname(replacements[i[nm_replace]])
  x
}

strip_macros <- function(x){
  gsub("\\\\(CRAN|BIO)?pkg\\{(.+?)\\}", "\\2", x)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

`%@%` <- function(x, attribute) {
  attr(x, attribute)
}

local_cache <- function() {
  table <- new.env(parent = emptyenv())
  list(
    add = function(x, nm) {
      table[[nm]] <- x
      table[[nm]]
    },
    get = function(nm) {
      table[[nm]]
    },
    exists = function(nm) {
      nm %in% names(table)
    }
  )
}

local_cache <- local_cache()


update_front_matter <- function(yml, file) {
  input <- xfun::read_utf8(file)
  front_matter_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input)

  xfun::write_utf8(
    c(
      "---",
      yaml::as.yaml(yml),
      "---",
      "",
      input[(front_matter_delimiters[2]+1):length(input)]
    ),
    file
  )
}

post_process_authors <- function (text)
{
  i1 <- grep("^\\\\author\\{", text)
  if (length(i1) == 0L) {
    return(text)
  }
  if (length(i1) > 1L) {
    warning("There should be only one instance of '\\author{}' in the tex file. ",
            "Post-processing \\author{} is cancelled.", call. = FALSE)
    return(text)
  }
  i2 <- grep("\\}$", text)
  i2 <- i2[i2 >= i1][1]
  i <- i1:i2
  x1 <- paste0(text[i], collapse = "\n")
  x2 <- knitr::combine_words(strsplit(x1, split = ", ")[[1]])
  text[i] <- xfun::split_lines(x2)
  text
}

require_package <- function (pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    rlang::abort(sprintf("The `%s` package must be installed to use this functionality. It can be installed with install.packages(\"%s\")",
                  pkg, pkg))
  }
}
