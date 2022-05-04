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
