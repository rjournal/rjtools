test_that("different styles for titles are ok", {
  # check_str is the function used in check_title

  # base
  str <- "An R Package for Customizable Tooltips in Interactive Graphics"
  expect_true(rjtools:::check_str(str)$result)

  # use \\pkg{} to mark up pkg name
  str <- "\\pkg{toOoOlTiPs}: An R Package for Customizable Tooltips in Interactive Graphics"
  expect_true(rjtools:::check_str(str)$result)

  # multiple pkgs
  str <- "\\pkg{toOoOlTiPs}: An R Package for Customizable Tooltips in \\pkg{leaflet}"
  expect_true(rjtools:::check_str(str)$result)

  # with dot in the pkg name
  str <- "\\pkg{toOoOlTiPs}: An R Package for Customizable Tooltips in \\pkg{data.table}"
  expect_true(rjtools:::check_str(str)$result)

  # use the ignore parameter
  str <- "toOoOlTiPs: An R Package for Customizable Tooltips in Interactive Graphics"
  expect_true(rjtools:::check_str(str, ignore = "toOoOlTiPs")$result)

  # use the ignore parameter with multiple packages
  str <- "toOoOlTiPs: An R Package for Customizable Tooltips in data.table"
  expect_true(rjtools:::check_str(str, ignore = c("toOoOlTiPs", "data.table"))$result)
})

test_that("title check works", {
  article_path <- system.file("sample-article", package = "rjtools")
  expect_true(is.null(check_title(article_path)))
})

test_that("section check works", {
  article_path <- system.file("sample-article", package = "rjtools")
  expect_true(is.null(check_section(article_path)))
})

test_that("cover letter check works", {
  article_path <- system.file("sample-article", package = "rjtools")
  expect_true(is.null(check_cover_letter(article_path)))
})

test_that("unnecessary check works", {
  article_path <- system.file("sample-article", package = "rjtools")
  expect_true(is.null(check_unnecessary_files(article_path)))
})

test_that("spelling check works", {
  article_path <- system.file("sample-article", package = "rjtools")
  expect_true(is.null(check_spelling(article_path)))
})

test_that("proposed package check works", {
  expect_true(is.null(check_proposed_pkg("ggplot2")))
})

#test_that("available packages check works", {
#  article_path <- system.file("sample-article", package = "rjtools")
#  expect_true(is.null(check_packages_available(article_path, ignore=c("ToOoOlTiPs"))))
#})
