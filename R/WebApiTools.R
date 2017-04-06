# @file WebApiTools.R
#
# Copyright 2017 Observational Health Data Sciences and Informatics
#
# This file is part of OhdsiRTools
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Load a cohort definition and insert it into this package
#'
#' @details
#' Load a cohort definition from a WebApi instance and insert it into this package. This will fetch the
#' json object and store it in the 'inst/cohorts' folder, and fetch the template SQL and store it in the
#' 'inst/sql/sql_server' folder. Both folders will be created if they don't exist.
#' 
#' When using generateStats = TRUE, the following tables are required to exist when executing the SQL:
#' cohort_inclusion, cohort_inclusion_result, cohort_inclusion_stats, and cohort_summary_stats. Also note
#' that the cohort_inclusion table should be populated with the names of the rules prior to executing
#' the cohort definition SQL. 
#'
#' @param definitionId   The number indicating which cohort definition to fetch.
#' @param name           The name that will be used for the json and SQL files. If not provided, the
#'                       name in cohort will be used, but this may not lead to valid file names.
#' @param baseUrl        The base URL for the WebApi instance.
#' @param generateStats  Should the SQL include the code for generating inclusion rule statistics?
#'                       Note that if TRUE, several additional tables are expected to exists as described
#'                       in the details
#'
#' @examples
#' \dontrun{
#' # This will create 'inst/cohorts/Angioedema.json' and 'inst/sql/sql_server/Angioedema.sql':
#'
#' insertCohortDefinitionInPackage(282, "Angioedema")
#' }
#'
#' @export
insertCohortDefinitionInPackage <- function(definitionId,
                                            name = NULL,
                                            baseUrl = "http://hix.jnj.com:8080/WebAPI",
                                            generateStats = FALSE) {
  
  ### Fetch JSON object ###
  url <- paste(baseUrl, "cohortdefinition", definitionId, sep = "/")
  json <- RCurl::getURL(url)
  parsedJson <- RJSONIO::fromJSON(json)
  if (is.null(name)) {
    name <- parsedJson$name
  }
  expression <- parsedJson$expression
  if (!file.exists("inst/cohorts")) {
    dir.create("inst/cohorts", recursive = TRUE)
  }
  fileConn <- file(file.path("inst/cohorts", paste(name, "json", sep = ".")))
  writeLines(expression, fileConn)
  close(fileConn)
  
  ### Fetch SQL by posting JSON object ###
  parsedExpression <- RJSONIO::fromJSON(parsedJson$expression)
  if (generateStats) {
    jsonBody <- RJSONIO::toJSON(list(expression = parsedExpression, options = list(generateStats = TRUE)), digits = 23)  
  } else {
    jsonBody <- RJSONIO::toJSON(list(expression = parsedExpression), digits = 23)  
  }
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
  url <- paste(baseUrl, "cohortdefinition", "sql", sep = "/")
  
  cohortSqlJson <- RCurl::postForm(url,
                                   .opts = list(httpheader = httpheader, postfields = jsonBody))
  sql <- RJSONIO::fromJSON(cohortSqlJson)
  if (!file.exists("inst/sql/sql_server")) {
    dir.create("inst/sql/sql_server", recursive = TRUE)
  }
  
  fileConn <- file(file.path("inst/sql/sql_server", paste(name, "sql", sep = ".")))
  writeLines(sql, fileConn)
  close(fileConn)
}

#' Load a Circe definition and insert it into this package
#'
#' @details
#' Deprecated. Use \code{\link{insertCohortDefinitionInPackage}} instead.
#' 
#' @param definitionId   The number indicating which Circe definition to fetch.
#' @param name           The name that will be used for the json and SQL files. If not provided, the
#'                       name in Circe will be used, but this may not lead to valid file names.
#' @param baseUrl        The base URL for the WebApi instance.
#'
#' @export
insertCirceDefinitionInPackage <- function(definitionId,
                                           name = NULL,
                                           baseUrl = "http://hix.jnj.com:8080/WebAPI") {
  .Deprecated("insertCohortDefinitionInPackage")
  insertCohortDefinitionInPackage(definitionId, name, baseUrl)
}

#' Get cohort inclusion rules
#' 
#' @details 
#' Parses all cohort definition JSON files in the inst/cohorts folder and extracts the name
#' of all inclusion rules.
#'
#' @return
#' A data frame with the names of all inclusion rules.
#'
#' @export
getCohortInclusionRules <- function() {
  rules <- data.frame()
  for (file in list.files(path = "inst/cohorts", pattern = ".*\\.json")) {
    writeLines(paste("Parsing", file, "for inclusion rules"))
    definition <- RJSONIO::fromJSON(file.path("inst/cohorts", file))
    if (!is.null(definition$InclusionRules)) {
      cohortName <- sub(".json", "", file)
      nrOfRules <- length(definition$InclusionRules)
      for (i in 1:nrOfRules) {
        rules <- rbind(rules, data.frame(cohortName = cohortName, 
                                         ruleSequence = i - 1,
                                         name = definition$InclusionRules[[i]]$name))
      }
    }
  }
  return(rules)
}

#' Insert SQL for creating a cohort table in the package
#'
#' @details 
#' Creates a SQL file called inst/sql/sql_server/CreateCohortTable.sql that
#' will create an empty cohort table.
#'
#' @param statsTables   If TRUE, the SQL will also create the four tables needed
#'                      when computing inclusion statistics.
#' 
#' @export
insertSqlForCohortTableInPackage <- function(statsTables = FALSE) {
  fileName <- system.file("cohortTable.sql", package = "OhdsiRTools")
  sql <- readChar(fileName, file.info(fileName)$size)
  if (statsTables) {
    fileName <- system.file("inclusionStatsTables.sql", package = "OhdsiRTools")
    sql <- paste(sql, readChar(fileName, file.info(fileName)$size), sep = "\n")
  }
  if (!file.exists("inst/sql/sql_server")) {
    dir.create("inst/sql/sql_server", recursive = TRUE)
  }
  fileConn <- file("inst/sql/sql_server/CreateCohortTable.sql")
  writeLines(sql, fileConn)
  close(fileConn)
  invisible(sql)
}

