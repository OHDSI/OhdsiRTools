library(testthat)

test_that("takeEnvironmentSnapshot", {
  snapshot <- takeEnvironmentSnapshot("OhdsiRTools")
  expect_true("OhdsiRTools" %in% snapshot$package)
  expect_equal(as.character(packageVersion("OhdsiRTools")), 
               as.character(snapshot$version[snapshot$package == "OhdsiRTools"]))
})

test_that("createRenvLockFile description mode", {
  lockFile <- tempfile(pattern = "renv", fileext = ".lock")
  createRenvLockFile(rootPackage = "OhdsiRTools",
                     mode = "description",
                     fileName = lockFile)
  contents <- RJSONIO::fromJSON(lockFile)
  node <- contents$Packages$OhdsiRTools
  expect_equal(node["Source"], "GitHub", check.attributes = FALSE)
  unlink(lockFile)
})

