# Copyright 2022 Observational Health Data Sciences and Informatics
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

#' Create a results schema stub
#' 
#' @details 
#' Scans an export folder for CSV files, and created an initial results schema CSV
#' file as used in Strategus modules. This schema CVS must be manually edited for 
#' correctness, but should make a good start.
#'
#' @param folder      The export folder containing example CSV files.
#' @param outputFile  The name of the CSV file to be created.
#'
#' @return
#' Invisibly returns the results schema. Is usually executed for the side-effect
#' of creating the results schema CSV file.
#' 
#' @export
createResultsSchemaStub <- function(folder, outputFile = file.path(folder, "resultsDataModelSpecification.csv")) {
  ensure_installed("readr")
  ensure_installed("SqlRender")
  ensure_installed("DatabaseConnector")
  
  csvFiles <- list.files(folder, ".*\\.csv")
  csvFiles <- csvFiles[csvFiles != basename(outputFile)]
  stub <- lapply(csvFiles, createTableStub, folder = folder)
  stub <- do.call(rbind, stub)
  stub <- SqlRender::camelCaseToSnakeCaseNames(stub)
  readr::write_csv(stub, outputFile)
  invisible(stub)
}

# csvFile <- 'cm_follow_up_dist.csv' 
createTableStub <- function(csvFile, folder) {
  data <- readr::read_csv(file.path(folder, csvFile), show_col_types = FALSE)
  tableName <- gsub("\\.csv$", "", csvFile)
  message(sprintf("Analysing '%s'", csvFile))
  rows <- vector("list", ncol(data))
  for (i in seq_len(ncol(data))) {
    message(sprintf("- Column '%s'", colnames(data)[i]))
    if (DatabaseConnector::isSqlReservedWord(colnames(data)[i])) {
      warning(sprintf("Column name '%s' in the '%s' table is a SQL reserved word", colnames(data)[i], tableName))
    }
    rows[[i]] <- data.frame(tableName = tableName,
                            columnName = colnames(data)[i],
                            dataType = guessDataType(data[, i]),
                            isRequired = guessRequired(data[, i]),
                            primaryKey = "No",
                            emptyIsNa = "No",
                            minCellCount = guessMinCellCount(data[, i]),
                            description = "")
  }
  rows <- do.call(rbind, rows)
  return(rows)
}

# column = data[, 1]
guessDataType <- function(column) {
  columnName <- names(column)
  column <- as.vector(column)[[1]]
  if (is.character(column)) {
    if (length(column) == 0) {
      maxWidth <- Inf
    } else {
      maxWidth <- max(nchar(column))
    }
    if (maxWidth > 25) {
      return("varchar")
    } else {
      return(sprintf("varchar(%d)", maxWidth))
    }
  } else if (is(column, "POSIXt")) {
    return("Timestamp")
  } else if (is(column, "Date")) {
    return("Date") 
  } else if (is.numeric(column)) {
    if (all(column == round(column), na.rm = TRUE)) {
      if (max(abs(column), na.rm = TRUE) > 2^31) {
        return("bigint")
      } else {
        return("int")
      }
    } else {
      return("float")
    }
  } else if (is.logical(column)) {
    return("varchar(5)")
  }
  stop(sprintf("Unknown data type for column '%s'", columnName))
}

guessRequired <- function(column) {
  column <- as.vector(column)[[1]]
  if (all(!is.na(column) & (!is.character(column) | column != ""))) {
    return("Yes")
  } else {
    return("No")
  }
}

guessMinCellCount <- function(column) {
  columnName <- names(column)
  column <- as.vector(column)[[1]]
  if (is.numeric(column) && grepl("(_subjects)|(_count)|(_outcomes)", columnName)) {
    return("Yes")
  } else {
    return("No")
  }
}

# Borrowed from devtools:
# https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L44
is_installed <- function(pkg, version = 0) {
  installed_version <-
    tryCatch(
      utils::packageVersion(pkg),
      error = function(e) {
        NA
      }
    )
  !is.na(installed_version) && installed_version >= version
}

# Borrowed and adapted from devtools:
# https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L74
ensure_installed <- function(pkg) {
  if (!is_installed(pkg)) {
    msg <-
      paste0(sQuote(pkg), " must be installed for this functionality.")
    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (menu(c("Yes", "No")) == 1) {
        install.packages(pkg)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    }
  }
}
