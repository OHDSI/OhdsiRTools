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
#' @param baseUrl        The base URL for the WebApi instance, for example: "http://api.ohdsi.org:8080/WebAPI"
#' 
#' @param generateStats  Should the SQL include the code for generating inclusion rule statistics?
#'                       Note that if TRUE, several additional tables are expected to exists as described
#'                       in the details
#' @param opts           List of options that can be passed to the RCurl methods for specifing additional 
#'                       options for connecting to REST end-points
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
                                            baseUrl,
                                            generateStats = FALSE,
                                            opts = list()) {
  
  ### Fetch JSON object ###
  url <- paste(baseUrl, "cohortdefinition", definitionId, sep = "/")
  json <- RCurl::getURL(url, .opts = opts)
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
  
  postFormOpts <- append(list(httpheader = httpheader, postfields = jsonBody), opts)
  cohortSqlJson <- RCurl::postForm(url,
                                   .opts = postFormOpts)
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
#' @param baseUrl        The base URL for the WebApi instance, for example: "http://api.ohdsi.org:8080/WebAPI"
#'
#' @export
insertCirceDefinitionInPackage <- function(definitionId,
                                           name = NULL,
                                           baseUrl) {
  .Deprecated("insertCohortDefinitionInPackage")
  insertCohortDefinitionInPackage(definitionId, name, baseUrl)
}


#' Insert a set of cohort definitions into package
#'
#' @param fileName               Name of a CSV file in the inst/settings folder of the package specifying
#'                               the cohorts to insert. See details for the expected file format.
#' @param baseUrl                The base URL for the WebApi instance, for example: "http://api.ohdsi.org:8080/WebAPI"
#' @param insertTableSql         Should the SQL for creating the cohort table be inserted into the 
#'                               package as well? This file will be called CreateCohortTable.sql.
#' @param insertCohortCreationR  Insert R code that will create the cohort table and instantiate
#'                               the cohorts? This will create a file called R/CreateCohorts.R containing
#'                               a function called .createCohorts.
#' @param generateStats          Should cohort inclusion rule statistics be created?
#' @param opts                   List of options that can be passed to the RCurl methods for specifing additional 
#'                               options for connecting to REST end-points
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
                                               baseUrl,
                                               insertTableSql = TRUE,
                                               insertCohortCreationR = TRUE,
                                               generateStats = FALSE,
                                               opts = list(),
                                               packageName) {
  if (insertCohortCreationR && !insertTableSql)
    stop("Need to insert table SQL in order to generate R code")
  cohortsToCreate <- read.csv(file.path("inst/settings", fileName))
  for (i in 1:nrow(cohortsToCreate)) {
    writeLines(paste("Inserting cohort:", cohortsToCreate$name[i]))
    OhdsiRTools::insertCohortDefinitionInPackage(definitionId = cohortsToCreate$atlasId[i], 
                                                 name = cohortsToCreate$name[i], 
                                                 baseUrl = baseUrl,
                                                 generateStats = generateStats,
                                                 opts = opts)
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


#' Get a cohort definition's name from WebAPI
#' 
#' @details                     Obtains the name of a cohort
#' @param baseUrl               The base URL for the WebApi instance
#' @param definitionId          The cohort definition id in Atlas
#' @param formatName            Should the name be formatted to remove prefixes and underscores?
#' @return                      The name of the cohort
#' 
#' @export
getCohortDefinitionName <- function(baseUrl, 
                                    definitionId, 
                                    formatName = FALSE)
{
  url <- SqlRender::renderSql(sql = "@baseUrl/WebAPI/cohortdefinition/@definitionId",
                              baseUrl = baseUrl,
                              definitionId = definitionId)$sql
  
  # don't verify SSL chain. work-around for self-certified certificates.
  json <- RJSONIO::fromJSON(RCurl::getURL(url, .opts = list(ssl.verifypeer = FALSE)))

    if (formatName)
  {
    return(stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(json$name, " ", "_"), "\\[(.*?)\\]_", ""), "_", " "))
  }
  return(json$name)
}


#' Get a concept set's name from WebAPI
#' 
#' @details                     Obtains the name of a concept set
#' @param baseUrl               The base URL for the WebApi instance
#' @param setId                 The concept set id in Atlas
#' @param formatName            Should the name be formatted to remove prefixes and underscores?
#' @return                      The name of the concept set
#' 
#' @export
getConceptSetName <- function(baseUrl, 
                              setId,
                              formatName = FALSE)
{
  url <- SqlRender::renderSql(sql = "@baseUrl/WebAPI/conceptset/@setId",
                              baseUrl = baseUrl,
                              setId = setId)$sql
  
  # don't verify SSL chain. work-around for self-certified certificates.
  json <- RJSONIO::fromJSON(RCurl::getURL(url, .opts = list(ssl.verifypeer = FALSE)))

  if (formatName)
  {
    return(stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(json$name, " ", "_"), "\\[(.*?)\\]_", ""), "_", " "))
  }
  return(json$name)
}

#' Get Vocab Source Key
#' 
#' @details               Obtains the source key of the default OMOP Vocab in Atlas
#' @param baseUrl         The base URL for the WebApi instance
#' @return                A string with the source key of the default OMOP Vocab in Atlas
#' 
#' @export 
getVocabSourceKey <- function(baseUrl)
{
  url <- SqlRender::renderSql(sql = "@baseUrl/WebAPI/source/priorityVocabulary",                     
                              baseUrl = baseUrl)$sql 
  
  # don't verify SSL chain, work-around for self-certified certificates.
  json <- RJSONIO::fromJSON(RCurl::getURL(url = url, .opts = list(ssl.verifypeer = FALSE)))
  return (json$sourceKey)
}


#' Get Concept Set Concept Ids
#' 
#' @details                 Obtains the full list of concept Ids in a concept set
#' @param baseUrl           The base URL for the WebApi instance
#' @param setId             The concept set id in Atlas
#' @return                  A list of concept Ids
#' 
#' @export
getConceptSetConcepts <- function(baseUrl, 
                                  setId, 
                                  vocabSourceKey = NULL)
{
  if (missing(vocabSourceKey))
  {
    vocabSourceKey <- getVocabSourceKey(baseUrl = baseUrl)
  }
  
  url <- SqlRender::renderSql(sql = "@baseUrl/WebAPI/conceptset/@setId/expression",
                              baseUrl = baseUrl,
                              setId = setId)$sql
  
  # don't verify SSL chain. work-around for self-certified certificates.
  json <- RJSONIO::fromJSON(RCurl::getURL(url, .opts = list(ssl.verifypeer = FALSE)))
  
  url <- SqlRender::renderSql(sql = "@baseUrl/WebAPI/vocabulary/@vocabSourceKey/resolveConceptSetExpression",
                              baseUrl = baseUrl,
                              vocabSourceKey = vocabSourceKey)$sql
  

  # don't verify SSL chain. work-around for self-certified certificates.
  httpheader <- c(Accept="application/json; charset=UTF-8", "Content-Type" = "application/json")
  
  body <- as.character(RJSONIO::toJSON(x = json, digits = 50))
  req <- RCurl::postForm(uri = url,
                         .opts = list(ssl.verifypeer = FALSE, 
                                      httpheader = httpheader,
                         postfields = body))
  concepts <- gsub(pattern = "\\[|\\]", replacement = "", x = req[1])
  
  return (as.integer(unlist((stringr::str_split(string = concepts, pattern = ",")))))
}



