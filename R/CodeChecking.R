# @file OhdsiRTools.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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

#' Check R file for code errors
#'
#' @details
#' This function uses the lintr package to check the code.
#'
#' @param file   The R file to check
#'
#' @export
ohdsiLintrFile <- function(file) {
  lintr::lint(file, linters = list(assignment_linter = lintr::assignment_linter,
                                   object_snake_case_linter = lintr::object_snake_case_linter,
                                   commas_linter = lintr::commas_linter,
                                   infix_spaces_linter = lintr::infix_spaces_linter,
                                   no_tab_linter = lintr::no_tab_linter,
                                   object_usage_linter = lintr::object_usage_linter,
                                   object_multiple_dots_linter = lintr::object_multiple_dots_linter,
                                   object_length_linter = lintr::object_length_linter,
                                   open_curly_linter = lintr::open_curly_linter,
                                   single_quotes_linter = lintr::single_quotes_linter,
                                   spaces_inside_linter = lintr::spaces_inside_linter,
                                   trailing_blank_lines_linter = lintr::trailing_blank_lines_linter,
                                   trailing_whitespace_linter = lintr::trailing_whitespace_linter))
}

#' Check all R files in a folder
#'
#' @details
#' This function uses the lintr package to check the code.
#'
#' @param path        Path to the folder containing the files to check. Only files with the .R
#'                    extension will be checked.
#' @param recursive   Include all subfolders?
#'
#' @export
ohdsiLintrFolder <- function(path = ".", recursive = TRUE) {
  flist <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE, recursive = recursive)
  for (f in flist) {
    message("Checking code in ", f)
    ohdsiLintrFile(f)
  }
}

.getFunctionDefinitionFromMem <- function(note) {
  funcPos <- regexpr("^.*: ", note)
  func <- substr(note, funcPos, funcPos + attr(funcPos, "match.length") - 3)
  func <- gsub("\\s", "", func)
  funcDef <- capture.output(getFunction(func, mustFind = FALSE))
  if (funcDef[1] == "NULL")
    return(NULL) else return(funcDef)
}

.getVariableName <- function(note) {
  varPos <- regexpr("(variable|parameter) .[a-zA-Z0-9_.-]*.", note)
  var <- substr(note, varPos + 10, varPos + attr(varPos, "match.length") - 2)
  return(var)
}

#' Check all code in a package
#'
#' @details
#' This function uses the codetools package to check the code from problems. Heuristics are used to
#' elimite false positives due to non-standard evaluation.
#'
#' @param package                   The name of the package to check.
#' @param ignoreHiddenFunctions     Ignore functions for which the definition cannot be retrieved?
#' @param suppressBindingKeywords   A set of keywords that are indicative of non-standard evaluation.
#'
#' @export
checkUsagePackage <- function(package,
                              ignoreHiddenFunctions = TRUE,
                              suppressBindingKeywords = c("ggplot2",
                                                          "ffwhich",
                                                          "subset.ffdf",
                                                          "glm")) {
  require(package, character.only = TRUE)
  notes <- capture.output(codetools::checkUsagePackage(package,
                                                       suppressLocal = FALSE,
                                                       suppressParamAssigns = TRUE,
                                                       suppressParamUnused = FALSE,
                                                       suppressFundefMismatch = FALSE,
                                                       suppressLocalUnused = FALSE,
                                                       suppressNoLocalFun = FALSE,
                                                       skipWith = TRUE,
                                                       suppressUndefined = FALSE,
                                                       suppressPartialMatchArgs = FALSE))
  if (length(notes) == 0) {
    writeLines("No problems found")
    return(notes)
  }
  newNotes <- c()
  for (i in 1:length(notes)) {
    if (regexpr("no visible binding for global variable", notes[i]) != -1) {
      filePos <- regexpr(" \\(.*\\.R:", notes[i])
      if (filePos != -1) {
        # Option 1: use file name and line number to get offending text:
        file <- substr(notes[i], filePos + 2, filePos + attr(filePos, "match.length") - 2)
        linePos <- regexpr("\\.R:.*\\)", notes[i])
        line <- substr(notes[i], linePos + 3, linePos + attr(linePos, "match.length") - 2)
        line <- strsplit(line, "-")[[1]]
        if (length(line) == 1) {
          line <- as.integer(line)
        } else {
          line <- as.integer(line[1]):as.integer(line[2])
        }
        text <- readLines(file)[line]
      } else {
        # Option 2: Get function definition from memory, and select lines with variable name
        funcDef <- .getFunctionDefinitionFromMem(notes[i])
        variableName <- .getVariableName(notes[i])
        text <- funcDef[grep(paste("(^|[^$])", variableName, sep = ""), funcDef)]
      }
      hasKeyword <- FALSE
      for (keyword in suppressBindingKeywords) {
        if (length(grep(keyword, text)) != 0)
          hasKeyword <- TRUE
      }
      if (!hasKeyword)
        newNotes <- c(newNotes, notes[i])
    } else if (regexpr("assigned but may not be used", notes[i]) != -1) {
      funcDef <- .getFunctionDefinitionFromMem(notes[i])
      if (is.null(funcDef)) {
        if (ignoreHiddenFunctions) {
          warning(paste("Ignoring problem in hidden function '", notes[i], "'", sep = ""))
        } else {
          newNotes <- c(newNotes, notes[i])
        }
      } else {
        variableName <- .getVariableName(notes[i])
        text <- funcDef[grep(paste("(^|[^$])", variableName, sep = ""), funcDef)]
        hasKeyword <- FALSE
        for (keyword in suppressBindingKeywords) {
          if (length(grep(keyword, text)) != 0)
          hasKeyword <- TRUE
        }
        if (!hasKeyword)
          newNotes <- c(newNotes, notes[i])
      }
    } else if (regexpr("parameter .* may not be used", notes[i]) != -1) {
      funcDef <- .getFunctionDefinitionFromMem(notes[i])
      if (is.null(funcDef)) {
        if (ignoreHiddenFunctions)
          warning(paste("Ignoring problem in hidden function '",
                        notes[i],
                        "'",
                        sep = "")) else newNotes <- c(newNotes, notes[i])
      } else {
        if (length(grep("UseMethod\\(", funcDef)) == 0)
          newNotes <- c(newNotes, notes[i])
      }
    } else {
      newNotes <- c(newNotes, notes[i])
    }
  }
  if (length(newNotes) == 0)
    writeLines("No problems found") else writeLines(newNotes)
  return(newNotes)
}
