library("testthat")

validBaseUrl <- "http://api.ohdsi.org:80/WebAPI"

test_that("Invalid source key", {
  expect_error(object = getCohortGenerationStatuses(baseUrl = validBaseUrl, definitionIds = c(1234), sourceKeys = c("blah")))
  expect_error(object = invokeCohortSetGeneration(baseUrl = validBaseUrl, definitionIds = c(1234), sourceKeys = c("blah")))
})

test_that("Invalid base Url", {
  baseUrls <- c("blah", "256.256.256.256:80/WebAPI", "1.1:80/WebAPI", "1.1.1.255/webapi")
  for (baseUrl in baseUrls) {
    expect_error(object = getCohortDefinitionName(baseUrl = baseUrl, definitionId = 1234))
    expect_error(object = getConceptSetConceptIds(baseUrl = baseUrl, setId = 1234))
    expect_error(object = insertCohortDefinitionSetInPackage(baseUrl = baseUrl, fileName = "conceptsetids.csv"))
    expect_error(object = getCohortGenerationStatuses(baseUrl = baseUrl, definitionIds = c(1234), sourceKeys = c("CDM")))
    expect_error(object = invokeCohortSetGeneration(baseUrl = baseUrl, definitionIds = c(1234), sourceKeys = c("CDM")))
    expect_error(object = getConceptSetExpression(baseUrl = baseUrl, setId = 1234))
    expect_error(object = getCohortDefinitionExpression(baseUrl = baseUrl, definitionId = 1234))
    expect_error(object = getSetExpressionConceptIds(baseUrl = baseUrl, expression = '{"items": []}'))
    expect_error(object = getCohortDefinitionSql(baseUrl = baseUrl, definitionId = 1234))
    expect_error(object = getConceptSetsAndConceptsFromCohort(baseUrl = baseUrl, definitionId = 1234))
  }
})
