# @file WebApiTools.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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
#' Load a cohort definition from a WebApi instance and insert it into this package. This will fetch
#' the json object and store it in the 'inst/cohorts' folder, and fetch the template SQL and store it
#' in the 'inst/sql/sql_server' folder. Both folders will be created if they don't exist.
#' When using generateStats = TRUE, the following tables are required to exist when executing the SQL:
#' cohort_inclusion, cohort_inclusion_result, cohort_inclusion_stats, and cohort_summary_stats. Also
#' note that the cohort_inclusion table should be populated with the names of the rules prior to
#' executing the cohort definition SQL.
#'
#' @param definitionId    The number indicating which cohort definition to fetch.
#' @param name            The name that will be used for the json and SQL files. If not provided, the
#'                        name in cohort will be used, but this may not lead to valid file names.
#' @param baseUrl         The base URL for the WebApi instance, for example:
#'                        "http://api.ohdsi.org:80/WebAPI".
#'
#' @param generateStats   Should the SQL include the code for generating inclusion rule statistics?
#'                        Note that if TRUE, several additional tables are expected to exists as
#'                        described in the details.
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
                                            generateStats = FALSE) {
  if (!.checkBaseUrl(baseUrl)) {
    stop("Base URL not valid, should be like http://api.ohdsi.org:80/WebAPI")
  }

  ### Fetch JSON object ###
  url <- paste(baseUrl, "cohortdefinition", definitionId, sep = "/")
  json <- httr::GET(url)
  json <- httr::content(json)
  # expression <- RJSONIO::fromJSON(json$expression)
  if (is.null(name)) {
    name <- json$name
  }
  if (!file.exists("inst/cohorts")) {
    dir.create("inst/cohorts", recursive = TRUE)
  }
  fileConn <- file(file.path("inst/cohorts", paste(name, "json", sep = ".")))
  writeLines(json$expression, fileConn)
  close(fileConn)

  ### Fetch SQL by posting JSON object ###
  parsedExpression <- RJSONIO::fromJSON(json$expression)
  if (generateStats) {
    jsonBody <- RJSONIO::toJSON(list(expression = parsedExpression,
                                     options = list(generateStats = TRUE)), digits = 23)
  } else {
    jsonBody <- RJSONIO::toJSON(list(expression = parsedExpression), digits = 23)
  }
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
  url <- paste(baseUrl, "cohortdefinition", "sql", sep = "/")
  cohortSqlJson <- httr::POST(url, body = jsonBody, config = httr::add_headers(httpheader))
  cohortSqlJson <- httr::content(cohortSqlJson)
  sql <- cohortSqlJson$templateSql
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
#' @param baseUrl        The base URL for the WebApi instance, for example:
#'                       "http://api.ohdsi.org:80/WebAPI".
#'
#' @export
insertCirceDefinitionInPackage <- function(definitionId, name = NULL, baseUrl) {
  .Deprecated("insertCohortDefinitionInPackage")
  insertCohortDefinitionInPackage(definitionId, name, baseUrl)
}


#' Insert a set of cohort definitions into package
#'
#' @param fileName                Name of a CSV file in the inst/settings folder of the package
#'                                specifying the cohorts to insert. See details for the expected file
#'                                format.
#' @param baseUrl                 The base URL for the WebApi instance, for example:
#'                                "http://api.ohdsi.org:80/WebAPI".
#' @param insertTableSql          Should the SQL for creating the cohort table be inserted into the
#'                                package as well? This file will be called CreateCohortTable.sql.
#' @param insertCohortCreationR   Insert R code that will create the cohort table and instantiate the
#'                                cohorts? This will create a file called R/CreateCohorts.R containing
#'                                a function called \code{.createCohorts}.
#' @param generateStats           Should cohort inclusion rule statistics be created?
#' @param packageName             The name of the package (only needed when inserting the R code as
#'                                well).
#'
#' @details
#' The CSV file should have at least the following fields: \describe{ \item{atlasId}{The cohort ID in
#' ATLAS.} \item{cohortId}{The cohort ID that will be used when instantiating the cohort (can be
#' different from atlasId).} \item{name}{The name to be used for the cohort. This name will be used to
#' generate file names, so please use letters and numbers only (no spaces).} }
#'
#' @export
insertCohortDefinitionSetInPackage <- function(fileName,
                                               baseUrl,
                                               insertTableSql = TRUE,
                                               insertCohortCreationR = TRUE,
                                               generateStats = FALSE,
                                               packageName) {
  if (!.checkBaseUrl(baseUrl)) {
    stop("Base URL not valid, should be like http://api.ohdsi.org:80/WebAPI")
  }
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
    rCode <- gsub("#CopyrightYear#", format(Sys.Date(), "%Y"), rCode)
    rCode <- gsub("#packageName#", packageName, rCode)
    rCode <- gsub("#fileName#", fileName, rCode)
    if (generateStats) {
      rCode <- gsub("#stats_start#", "", rCode)
      rCode <- gsub("#stats_end#", "", rCode)
    } else {
      rCode <- gsub("#stats_start#.*?#stats_end#", "", rCode)
    }
    fileConn <- file("R/CreateCohorts.R")
    writeChar(rCode, fileConn, eos = NULL)
    close(fileConn)
  }
}


#' Insert a set of concept sets' concept ids into package
#'
#' @param fileName                Name of a CSV file in the inst/settings folder of the package
#'                                specifying the concept sets to insert. See details for the expected file
#'                                format.
#' @param baseUrl                 The base URL for the WebApi instance, for example:
#'                                "http://api.ohdsi.org:80/WebAPI".
#'
#' @details
#' The CSV file should have: \describe{ \item{atlasId}{The concept set Id in
#' ATLAS.} }
#'
#' @export
insertConceptSetConceptIdsInPackage <- function(fileName,
                                                baseUrl)
{
  if (!.checkBaseUrl(baseUrl)) {
    stop("Base URL not valid, should be like http://api.ohdsi.org:80/WebAPI")
  }
  
  conceptSetsToCreate <- read.csv(file.path("inst/settings", fileName))
  if (!file.exists("inst/conceptsets")) {
    dir.create("inst/conceptsets", recursive = TRUE)
  }
  
  for (i in 1:nrow(conceptSetsToCreate)) {
    writeLines(paste("Inserting concept set:", conceptSetsToCreate$atlasId[i]))
    df <- as.data.frame(getConceptSetConceptIds(baseUrl = baseUrl, setId = conceptSetsToCreate$atlasId[i]))
    names(df) <- c("CONCEPT_ID")
    fileConn <- file(file.path("inst/conceptsets", paste(conceptSetsToCreate$atlasId[i], "csv", sep = ".")))
    write.csv(x = df, file = fileConn, row.names = FALSE, quote = FALSE)
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

.formatName <- function(name) {
  return(gsub("_", " ", gsub("\\[(.*?)\\]_", "", gsub(" ", "_", name))))
}

.checkBaseUrl <- function(baseUrl)
{
  return(grepl(pattern = "https?:\\/\\/[a-z0-9]+([\\-\\.]{1}[a-z0-9]+)*\\.[a-z]{2,5}(:[0-9]{1,5})+(\\/.*)?\\/WebAPI$", 
                x = baseUrl, 
                ignore.case = FALSE))
}


#' Get a cohort definition's name from WebAPI
#'
#' @details
#' Obtains the name of a cohort.
#' 
#' @param baseUrl        The base URL for the WebApi instance, for example:
#'                       "http://api.ohdsi.org:80/WebAPI".
#' @param definitionId   The cohort definition id in Atlas.
#' @param formatName     Should the name be formatted to remove prefixes and underscores?
#' 
#' @return
#' The name of the cohort.
#'
#' @export
getCohortDefinitionName <- function(baseUrl, definitionId, formatName = FALSE) {
  if (!.checkBaseUrl(baseUrl)) {
    stop("Base URL not valid, should be like http://api.ohdsi.org:80/WebAPI")
  }
  url <- gsub("@baseUrl",
              baseUrl,
              gsub("@definitionId", definitionId, "@baseUrl/cohortdefinition/@definitionId"))
  json <- httr::GET(url)
  json <- httr::content(json)

  if (formatName) {
    return(.formatName(json$name))
  } else {
    return(json$name)
  }
}


#' Get a concept set's name from WebAPI
#'
#' @details
#' Obtains the name of a concept set.
#' 
#' @param baseUrl      The base URL for the WebApi instance, for example:
#'                     "http://api.ohdsi.org:80/WebAPI".
#' @param setId        The concept set id in Atlas.
#' @param formatName   Should the name be formatted to remove prefixes and underscores?
#' 
#' @return
#' The name of the concept set.
#'
#' @export
getConceptSetName <- function(baseUrl, setId, formatName = FALSE) {
  if (!.checkBaseUrl(baseUrl)) {
    stop("Base URL not valid, should be like http://api.ohdsi.org:80/WebAPI")
  }
  
  url <- gsub("@baseUrl", baseUrl, gsub("@setId", setId, "@baseUrl/conceptset/@setId"))
  json <- httr::GET(url)
  json <- httr::content(json)

  if (formatName) {
    return(.formatName(json$name))
  } else {
    return(json$name)
  }
}

#' Get Priority Vocab Source Key
#'
#' @details
#' Obtains the source key of the default OMOP Vocab in Atlas.
#' 
#' @param baseUrl   The base URL for the WebApi instance, for example:
#'                  "http://api.ohdsi.org:80/WebAPI".
#'                  
#' @return
#' A string with the source key of the default OMOP Vocab in Atlas.
#'
#' @export
getPriorityVocabKey <- function(baseUrl) {
  if (!.checkBaseUrl(baseUrl)) {
    stop("Base URL not valid, should be like http://api.ohdsi.org:80/WebAPI")
  }
  url <- gsub("@baseUrl", baseUrl, "@baseUrl/source/priorityVocabulary")
  json <- httr::GET(url)
  json <- httr::content(json)
  return(json$sourceKey)
}


#' Get Concept Set Concept Ids
#'
#' @details
#' Obtains the full list of concept Ids in a concept set.
#' 
#' @param baseUrl          The base URL for the WebApi instance, for example:
#'                         "http://api.ohdsi.org:80/WebAPI".
#' @param setId            The concept set id in Atlas.
#' @param vocabSourceKey   The source key of the Vocabulary. By default, the priority Vocabulary is
#'                         used.
#'                         
#' @return
#' A list of concept Ids.
#'
#' @export
getConceptSetConceptIds <- function(baseUrl, setId, vocabSourceKey = NULL) {
  if (!.checkBaseUrl(baseUrl)) {
    stop("Base URL not valid, should be like http://api.ohdsi.org:80/WebAPI")
  }
  
  if (missing(vocabSourceKey) || is.null(vocabSourceKey)) {
    vocabSourceKey <- OhdsiRTools::getPriorityVocabKey(baseUrl = baseUrl)
  }

  url <- gsub("@baseUrl", baseUrl, gsub("@setId", setId, "@baseUrl/conceptset/@setId/expression"))
  json <- httr::GET(url)
  json <- httr::content(json)

  url <- gsub("@baseUrl",
              baseUrl,
              gsub("@vocabSourceKey",
                   vocabSourceKey,
                   "@baseUrl/vocabulary/@vocabSourceKey/resolveConceptSetExpression"))
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
  body <- as.character(RJSONIO::toJSON(x = json, digits = 50))  # disables scientific notation
  req <- httr::POST(url, body = body, config = httr::add_headers(httpheader))
  req <- httr::content(req)
  concepts <- unlist(req)
  return(concepts)
}

#' Get Cohort Generation Statuses
#' 
#' @details 
#' Obtains cohort generation statuses for a collection of cohort definition Ids and CDM sources.
#' Useful if running multiple cohort generation jobs that are long-running.
#' 
#' @param baseUrl          The base URL for the WebApi instance, for example:
#'                         "http://api.ohdsi.org:80/WebAPI".            
#' @param definitionIds    A list of cohort definition Ids
#' @param sourceKeys       A list of CDM source keys. These can be found in Atlas -> Configure.
#' 
#' @return 
#' A data frame of cohort generation statuses, start times, and execution durations per definition id and source key.
#' 
#' @export
getCohortGenerationStatuses <- function(baseUrl, 
                                        definitionIds, 
                                        sourceKeys)
{
  checkSourceKeys <- function(baseUrl, sourceKeys)
  {
    sourceIds <- lapply(X = sourceKeys, .getSourceIdFromKey, baseUrl = baseUrl)
    return (!(-1 %in% sourceIds))
  }
  
  if (!checkSourceKeys(baseUrl = baseUrl, sourceKeys = sourceKeys)) {
    stop("One or more source keys is invalid, please check Atlas -> Configure page.")
  }
  
  tuples <- list(definitionIds, sourceKeys)
  df <- expand.grid(tuples, KEEP.OUT.ATTRS = FALSE)
  colnames(df) <- c("definitionId", "sourceKey")
  
  statuses <- apply(X = df, MARGIN = 1, function(row)
  {
    result <- .getCohortGenerationStatus(baseUrl = baseUrl, 
                                         definitionId = row["definitionId"],
                                         sourceKey = row["sourceKey"])
    
    status <- list(sourceKey = row["sourceKey"], 
                   definitionId = row["definitionId"], 
                   definitionName = getCohortDefinitionName(baseUrl = baseUrl, 
                                                            definitionId = row["definitionId"], 
                                                            formatName = FALSE),
                   status = result$status,
                   startTime = result$startTime,
                   executionDuration = result$executionDuration,
                   personCount = result$personCount)
  })
  
  return (do.call(rbind, lapply(statuses, data.frame, stringsAsFactors = FALSE)))
}

.getSourceIdFromKey <- function(baseUrl,
                                sourceKey)
{
  if (!.checkBaseUrl(baseUrl)) {
    stop("Base URL not valid, should be like http://api.ohdsi.org:80/WebAPI")
  }
  
  url <- gsub("@baseUrl", baseUrl,
              gsub("@sourceKey", sourceKey, "@baseUrl/source/@sourceKey"))
  json <- httr::GET(url)
  json <- httr::content(json)
  if (is.null(json$sourceId))
    json$sourceId <- -1
  json$sourceId
}

.getCohortGenerationStatus <- function(baseUrl,
                                       definitionId,
                                       sourceKey)
{
  millisecondsToDate = function(milliseconds) 
  {
    sec = milliseconds / 1000
    as.character(as.POSIXct(sec, origin = "1970-01-01", tz = Sys.timezone()))
  }
  
  if (!.checkBaseUrl(baseUrl)) {
    stop("Base URL not valid, should be like http://api.ohdsi.org:80/WebAPI")
  }
  
  url <- gsub("@baseUrl", baseUrl,
              gsub("@definitionId", definitionId, 
                   "@baseUrl/cohortdefinition/@definitionId/info"))
  json <- httr::GET(url)
  json <- httr::content(json)
  sourceId <- .getSourceIdFromKey(baseUrl = baseUrl, sourceKey = sourceKey)

  json <- json[sapply(json, function(j) j$id$sourceId == sourceId)]
  if (length(json) == 0) 
  { 
    return (list(status = "NA", startTime = "NA", executionDuration = "NA", personCount = "NA"))
  }
  return (list(status = json[[1]]$status, 
               startTime = millisecondsToDate(milliseconds = json[[1]]$startTime),
               executionDuration = json[[1]]$executionDuration,
               personCount = json[[1]]$personCount))
}

