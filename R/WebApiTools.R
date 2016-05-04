# @file WebApiTools.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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

#' Load a Circe definition and insert it into this package
#'
#' @details
#' Load a Circe definition from a WebApi instance and insert it into this package. This will fetch the
#' json object and store it in the 'inst/circe' folder, and fetch the template SQL and store it in the
#' 'inst/sql/sql_server' folder. Both folders will be created if they don't exist.
#'
#' @param definitionId   The number indicating which Circe definition to fetch.
#' @param name           The name that will be used for the json and SQL files. If not provided, the
#'                       name in Circe will be used, but this may not lead to valid file names.
#' @param baseUrl        The base URL for the WebApi instance.
#'
#' @examples
#' \dontrun{
#' # This will create 'inst/circe/Angioedema.json' and
#' # 'inst/sql/sql_server/Angioedema.sql':
#'
#' insertCirceDefinitionInPackage(2132, "Angioedema")
#' }
#'
#' @export
insertCirceDefinitionInPackage <- function(definitionId,
                                           name = NULL,
                                           baseUrl = "http://hix.jnj.com:8080/WebAPI") {

  ### Fetch JSON object ###
  url <- paste(baseUrl, "cohortdefinition", definitionId, sep = "/")
  json <- RCurl::getURL(url)
  parsedJson <- RJSONIO::fromJSON(json)
  if (is.null(name)) {
    name <- parsedJson$name
  }
  expression <- parsedJson$expression
  if (!file.exists("inst/circe")) {
    dir.create("inst/circe", recursive = TRUE)
  }

  fileConn <- file(file.path("inst/circe", paste(name, "json", sep = ".")))
  writeLines(expression, fileConn)
  close(fileConn)

  ### Fetch SQL by posting JSON object ###
  parsedExpression <- RJSONIO::fromJSON(parsedJson$expression)
  jsonBody <- RJSONIO::toJSON(list(expression = parsedExpression), digits = 23)
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
