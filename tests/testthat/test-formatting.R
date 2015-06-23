library("testthat")

test_that("Roxygen wrapping", {
  code <- c("#' This is a test", "#' of text wrapping")
  code <- formatRText(code)
  expect_equal(code, "#' This is a test of text wrapping")
})


test_that("Roxygen itemize", {
  code <- c("#' \\itemize{ \\item{ item 1}", "#'\\item{ item 2 }}")
  code <- formatRText(code)
  expect_equal(code,
               c("#' \\itemize{", "#'   \\item { item 1}", "#'   \\item { item 2 }", "#' }", "#'"))
})
