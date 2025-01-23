## some tests require CRAN access - make sure a mirror is set
if ("@CRAN@" %in% getOption("repos"))
    chooseCRANmirror(FALSE, 1, TRUE)

test_that("different styles for titles are ok", {
  # check_str is the function used in check_title

  # base
  str <- "An R Package for Customizable Tooltips in Interactive Graphics"
  expect_true(rjtools:::check_str(str)$result)

  # title should not include special format: \\pkg{}
  str <- "\\pkg{toOoOlTiPs}: An R Package for Customizable Tooltips in Interactive Graphics"
  expect_false(rjtools:::check_str(str)$result)

  # title should not include special format: multiple \\pkgs{}
  str <- "\\pkg{toOoOlTiPs}: An R Package for Customizable Tooltips in \\pkg{leaflet}"
  expect_false(rjtools:::check_str(str)$result)

  # title should not include special format: with dot in the pkg name
  str <- "\\pkg{toOoOlTiPs}: An R Package for Customizable Tooltips in \\pkg{data.table}"
  expect_false(rjtools:::check_str(str)$result)

  # use the ignore parameter
  str <- "toOoOlTiPs: An R Package for Customizable Tooltips in Interactive Graphics"
  expect_true(rjtools:::check_str(str, ignore = "toOoOlTiPs")$result)

  # use the ignore parameter with multiple packages
  str <- "toOoOlTiPs: An R Package for Customizable Tooltips in data.table"
  expect_true(rjtools:::check_str(str, ignore = c("toOoOlTiPs", "data.table"))$result)
})

test_that("check abstract works", {

  # pick up BIOpkg, CRANpkg, and pkg markups
  str <- "The package \\BIOpkg{toOoOlTiPs} for the R language is great."
  expect_true(check_abstract_str(str))
  str <- "The package \\CRANpkg{toOoOlTiPs} for the R language is great."
  expect_true(check_abstract_str(str))
  str <- "The package \\pkg{toOoOlTiPs} for the R language is great."
  expect_true(check_abstract_str(str))

  # pick up citation
  str <- "It is an \\cite{xxx} package "
  expect_true(check_abstract_str(str))
  str <- "It uses different forms of citation: \\citet{fox2009}"
  expect_true(check_abstract_str(str))
  str <- "Also, \\citep{bayesassurance}"
  expect_true(check_abstract_str(str))

  # others
  str <- "Highlight with \\texttt{person(2025)} is not allowed"
  expect_true(check_abstract_str(str))
  str <- "neither is \\emph{sdkfjls} allowed"
  expect_true(check_abstract_str(str))
  str <- "but emphasize that this is allowed"
  expect_false(check_abstract_str(str))
  str <- "You should generally not cite \\proglang{R}. "
  expect_true(check_abstract_str(str))

# pick up mathematics
  str <- "and great equations $\\mathcal{O}$"
  expect_true(check_abstract_str(str))

})


expect_SUCCESS <- function(expr) expect_equal(c(expr), "SUCCESS")
expect_NOTE <- function(expr) expect_equal(c(expr), "NOTE")
expect_ERROR <- function(expr) expect_equal(c(expr), "ERROR")

article_path <- system.file("sample-article", package = "rjtools")
bad_article_path <- "../bad-article"

#test_that("filenames check works", {
#    expect_SUCCESS(check_filenames(article_path))
#})

test_that("structure check works", {
    expect_SUCCESS(check_structure(article_path))
    ## we cannot ship a file with spaces since that would trigger
    ## check failure, so we have to create it here
    writeLines("foo", fws <- file.path(bad_article_path, "file with spaces"))
    #expect_ERROR(check_structure(bad_article_path))
    unlink(fws)
    ## create invalid nesting
    # dir.create(file.path(bad_article_path, "a", "b", "c", "d"), TRUE, TRUE)
    # expect_ERROR(check_structure(bad_article_path))
    # unlink(file.path(bad_article_path, "a"), TRUE)
    ## create unicode file name
    # writeLines("foo", fwu <- file.path(bad_article_path, "\u010cesko"))
    # expect_ERROR(check_structure(bad_article_path))
    # unlink(fwu)
    expect_SUCCESS(check_structure(bad_article_path))
})

test_that("title check works", {
    expect_SUCCESS(check_title(article_path))
    expect_ERROR(check_title(bad_article_path))
})

test_that("section check works", {
    expect_SUCCESS(check_section(article_path))
})

test_that("cover letter check works", {
    expect_SUCCESS(check_cover_letter(article_path))
    expect_NOTE(check_cover_letter(bad_article_path))
})

test_that("unnecessary check works", {
    expect_SUCCESS(check_unnecessary_files(article_path))
    expect_ERROR(check_unnecessary_files(bad_article_path))
})

#test_that("spelling check works", {
#    expect_NOTE(check_spelling(article_path))
#})

test_that("proposed package check works", {
    expect_SUCCESS(check_proposed_pkg("ggplot2"))
    expect_NOTE(check_proposed_pkg("nonexistent"))
})

test_that("available packages check works", {
    expect_SUCCESS(check_packages_available(article_path, ignore="ToOoOlTiPs"))
})
