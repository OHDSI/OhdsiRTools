library("testthat")

test_that("Invalid source key", {
  expect_error(object = getCohortGenerationStatuses(baseUrl = "http://api.ohdsi.org:80/WebAPI", definitionIds = c(1234), sourceKeys = c("blah")))
  expect_error(object = invokeCohortSetGeneration(baseUrl = "http://api.ohdsi.org:80/WebAPI", definitionIds = c(1234), sourceKeys = c("blah")))
})

test_that("Invalid base Url", {
  expect_error(object = getCohortDefinitionName(baseUrl = "blah", definitionId = 1234))
  expect_error(object = getConceptSetConceptIds(baseUrl = "blah", setId = 1234))
  expect_error(object = insertCohortDefinitionSetInPackage(baseUrl = "blah", fileName = "conceptsetids.csv"))
  expect_error(object = getCohortGenerationStatuses(baseUrl = "blah", definitionIds = c(1234), sourceKeys = c("CDM")))
  expect_error(object = invokeCohortSetGeneration(baseUrl = "blah", definitionIds = c(1234), sourceKeys = c("CDM")))
})
