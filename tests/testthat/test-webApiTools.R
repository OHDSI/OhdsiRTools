library("testthat")

test_that("Invalid source key", {
  expect_error(object = getCohortGenerationStatuses(baseUrl = "http://api.ohdsi.org:80/WebAPI", definitionIds = c(1234), sourceKeys = c("blah")))
})

test_that("Invalid base Url", {
  expect_error(object = getCohortDefinitionName(baseUrl = "blah", definitionId = 1234))
})