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


#' Insert a set of cohort definitions into package
#'
#' @param fileName               Name of a CSV file in the inst/settings folder of the package specifying
#'                               the cohorts to insert. See details for the expected file format.
#' @param baseUrl                The base URL for the WebApi instance. 
#' @param insertTableSql         Should the SQL for creating the cohort table be inserted into the 
#'                               package as well? This file will be called CreateCohortTable.sql.
#' @param insertCohortCreationR  Insert R code that will create the cohort table and instantiate
#'                               the cohorts? This will create a file called R/CreateCohorts.R containing
#'                               a function called .createCohorts.
#' @param generateStats          Should cohort inclusion rule statistics be created?
#' @param packageName            The name of the package (only needed when inserting the R code as well).
#' 
#' @details 
#' The CSV file should have at least the following fields:
#' \describe{
#' \item{atlasId}{The cohort ID in ATLAS.} 
#' \item{cohortId}{The cohort ID that will be used when instantiating the cohort (can be different from atlasId).}
#' \item{name}{The name to be used for the cohort. This name will be used to generate file names, so please use letters and numbers only (no spaces).}
#' }
#' 
#' @export
insertCohortDefinitionSetInPackage <- function(fileName,
                                               baseUrl = "http://hix.jnj.com:8080/WebAPI",
                                               insertTableSql = TRUE,
                                               insertCohortCreationR = TRUE,
                                               generateStats = FALSE,
                                               packageName) {
  if (insertCohortCreationR && !insertTableSql)
    stop("Need to insert table SQL in order to generate R code")
  cohortsToCreate <- read.csv(file.path("inst/settings", fileName))
  for (i in 1:nrow(cohortsToCreate)) {
    writeLines(paste("Inserting cohort:", cohortsToCreate$name[i]))
    OhdsiRTools::insertCohortDefinitionInPackage(definitionId = cohortsToCreate$atlasId[i], 
                                                 name = cohortsToCreate$name[i], 
                                                 baseUrl = baseUrl,
                                                 generateStats = generateStats)
  }
  if (insertTableSql) {
    .insertSqlForCohortTableInPackage(statsTables = generateStats)
  }
  if (generateStats) {
    rules <- .getCohortInclusionRules()
    rules <- merge(rules, data.frame(cohortId = cohortsToCreate$cohortId, 
                                     cohortName = cohortsToCreate$name))
    write.csv(rules, "inst/cohorts/InclusionRules.csv", row.names = FALSE)
  }
  if (insertCohortCreationR) {
    templateFileName <- system.file("CreateCohorts.R", package = "OhdsiRTools")
    rCode <- readChar(templateFileName, file.info(templateFileName)$size)
    rCode <- gsub("#CopyrightYear#",  format(Sys.Date(), "%Y"), rCode)
    rCode <- gsub("#packageName#",  packageName, rCode)
    rCode <- gsub("#fileName#",  fileName, rCode)
    if (generateStats) {
      rCode <- gsub("#stats_start#",  "", rCode)
      rCode <- gsub("#stats_end#",  "", rCode)
    } else {
      rCode <- gsub("#stats_start#.*?#stats_end#",  "", rCode)
    }
    fileConn <- file("R/CreateCohorts.R")
    writeChar(rCode, fileConn, eos = NULL)
    close(fileConn)
  }
}

.getCohortInclusionRules <- function() {
  rules <- data.frame()
  for (file in list.files(path = "inst/cohorts", pattern = ".*\\.json")) {
    writeLines(paste("Parsing", file, "for inclusion rules"))
    definition <- RJSONIO::fromJSON(file.path("inst/cohorts", file))
    if (!is.null(definition$InclusionRules)) {
      nrOfRules <- length(definition$InclusionRules)
      if (nrOfRules > 0) {
        cohortName <- sub(".json", "", file)
        for (i in 1:nrOfRules) {
          rules <- rbind(rules, data.frame(cohortName = cohortName, 
                                           ruleSequence = i - 1,
                                           ruleName = definition$InclusionRules[[i]]$name))
        }
      }
    }
  }
  return(rules)
}

.insertSqlForCohortTableInPackage <- function(statsTables = FALSE) {
  fileName <- system.file("CohortTable.sql", package = "OhdsiRTools")
  sql <- readChar(fileName, file.info(fileName)$size)
  if (statsTables) {
    fileName <- system.file("InclusionStatsTables.sql", package = "OhdsiRTools")
    sql <- paste(sql, readChar(fileName, file.info(fileName)$size), sep = "\n")
  }
  if (!file.exists("inst/sql/sql_server")) {
    dir.create("inst/sql/sql_server", recursive = TRUE)
  }
  fileConn <- file("inst/sql/sql_server/CreateCohortTable.sql")
  writeChar(sql, fileConn, eos = NULL)
  close(fileConn)
  invisible(sql)
}
