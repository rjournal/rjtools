test_that("title check works", {
  # base
  str <- "An R Package for Customizable Tooltips in Interactive Graphics"
  expect_true(check_string(str))

  # use \\pkg{} to mark up pkg name
  str <- "\\pkg{toOoOlTiPs}: An R Package for Customizable Tooltips in Interactive Graphics"
  expect_true(check_string(str))

  # multiple pkgs
  str <- "\\pkg{toOoOlTiPs}: An R Package for Customizable Tooltips in \\pkg{leaflet}"
  expect_true(check_string(str))

  # with dot in the pkg name
  str <- "\\pkg{toOoOlTiPs}: An R Package for Customizable Tooltips in \\pkg{data.table}"
  expect_true(check_string(str))

  # use the ignore parameter
  str <- "toOoOlTiPs: An R Package for Customizable Tooltips in Interactive Graphics"
  expect_true(check_string(str, ignore = "toOoOlTiPs"))

  # use the ignore parameter with multiple packages
  str <- "toOoOlTiPs: An R Package for Customizable Tooltips in data.table"
  expect_true(check_string(str, ignore = c("toOoOlTiPs", "data.table")))
})
