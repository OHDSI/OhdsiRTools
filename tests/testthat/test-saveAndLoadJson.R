library("testthat")

test_that("data.frame restore", {
  settings <- list(a = "a",
                   b = data.frame(x=c(1,2,3), y=c("p","q","r"), stringsAsFactors = FALSE))
  saveSettingsToJson(settings, "temp.json")
  settings2 <- loadSettingsFromJson("temp.json")
  unlink("temp.json")
  expect_equal(class(settings$b), class(settings2$b))
  expect_equivalent(settings, settings2)
})
