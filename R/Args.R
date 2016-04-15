# @file Args.R
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

#' Create an argument function
#'
#' @details
#' This function can be used to create a function that has (almost) the same interface as the
#' specified function, and the output of this function will be a list of argument values.
#'
#' @param functionName   The name of the function for which we want to create an args function.
#' @param excludeArgs    Exclude these arguments from appearing in the args function.
#' @param includeArgs    Include these arguments in the args function.
#' @param addArgs        Add these arguments to the args functions. Defined as a list with format name = default.
#' @param rCode          A character vector representing the R code where the new function should be
#'                       appended to.
#' @param newName        The name of the new function. If not specified, the new name will be
#'                       automatically derived from the old name.
#'
#' @return
#' A character vector with the R code including the new function.
#' 
#' @examples 
#' createArgFunction("read.csv", addArgs = list(exposureId = "exposureId"))
#'
#' @export
createArgFunction <- function(functionName,
                              excludeArgs = c(),
                              includeArgs = NULL,
                              addArgs = list(),
                              rCode = c(),
                              newName) {
  args <- formals(functionName)
  if (!is.null(includeArgs)) {
    args <- args[names(args) %in% includeArgs]
  }
  args <- args[!(names(args) %in% excludeArgs)]
  args <- append(args, addArgs)
  toChar <- function(x) {
    if (is.null(x)) {
      "NULL"
    } else if (class(x) == "call") {
      paste(capture.output(x), collapse = "")
    } else if (class(x) == "character") {
      paste("\"", x, "\"", sep = "")
    } else {
      as.character(x)
    }
  }
  args <- sapply(args, toChar)
  argInfo <- data.frame(name = names(args))
  argInfo$default <- NULL
  for (i in 1:length(args)) {
    argInfo$default[argInfo$name == names(args)[[i]]] <- args[[i]]
  }
  html <- capture.output(tools::Rd2HTML(utils:::.getHelpFile(help(functionName))))
  parameterHelp <- XML::xpathApply(XML::htmlParse(html),
                                   "//table[@summary=\"R argblock\"]//tr//td",
                                   XML::xmlValue)
  parameterHelp <- iconv(unlist(parameterHelp), from = "UTF-8", to = "ASCII")
  argInfo$help <- ""
  for (i in 1:(length(parameterHelp)/2)) {
    argInfo$help[argInfo$name == parameterHelp[i * 2 - 1]] <- gsub("\n", "", parameterHelp[i * 2])
  }

  if (length(rCode) != 0) {
    rCode <- c(rCode, "")
  }
  rCode <- c(rCode, paste("#' Create a parameter object for the function", functionName))
  rCode <- c(rCode, "#'")
  rCode <- c(rCode, "#' @details")
  rCode <- c(rCode, "#' Create an object defining the parameter values.")
  rCode <- c(rCode, "#'")
  for (i in 1:nrow(argInfo)) {
    rCode <- c(rCode, paste("#' @param", argInfo$name[i], argInfo$help[i]))
  }
  rCode <- c(rCode, "#'")
  rCode <- c(rCode, "#' @export")
  if (missing(newName)) {
    createFunArgsName <- paste("create",
                               toupper(substr(functionName, 1, 1)),
                               substr(functionName, 2, nchar(functionName)),
                               "Args",
                               sep = "")
  } else {
    createFunArgsName <- newName
  }
  header <- paste(createFunArgsName, "<- function(")
  for (i in 1:nrow(argInfo)) {
    if (i == 1) {
      start <- header
    } else {
      start <- paste(rep(" ", nchar(header)), collapse = "")
    }
    if (argInfo$default[i] == "") {
      end <- ""
    } else {
      end <- paste(" = ", argInfo$default[i], sep = "")
    }
    if (i == nrow(argInfo)) {
      end <- paste(end, ") {", sep = "")
    } else {
      end <- paste(end, ",", sep = "")
    }

    rCode <- c(rCode, paste(start, argInfo$name[i], end, sep = ""))
  }
  rCode <- c(rCode, "  # First: get default values:")
  rCode <- c(rCode, "  analysis <- list()")
  rCode <- c(rCode, paste0("  for (name in names(formals(",createFunArgsName,"))) {"))
  rCode <- c(rCode, "    analysis[[name]] <- get(name)")
  rCode <- c(rCode, "  }")
  rCode <- c(rCode, "  # Second: overwrite defaults with actual values:")
  rCode <- c(rCode, "  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))")
  rCode <- c(rCode, "  for (name in names(values)) {")
  rCode <- c(rCode, "    if (name %in% names(analysis))")
  rCode <- c(rCode, "      analysis[[name]] <- values[[name]]")
  rCode <- c(rCode, "  }")
  rCode <- c(rCode, "  class(analysis) <- \"args\"")
  rCode <- c(rCode, "  return(analysis)")
  rCode <- c(rCode, "}")
  return(rCode)
}

.recursivePrettyPrint <- function(object, name, indent) {
  name <- paste(paste(rep(" ", indent), collapse = ""), name, sep = "")
  if (is.list(object)) {
    writeLines(name)
    for (i in 1:length(object)) {
      .recursivePrettyPrint(object[[i]], names(object)[i], indent + 2)
    }
  } else if (is.character(object)) {
    writeLines(paste(name, paste("\"", object, "\"", sep = ""), sep = " = "))
  } else if (is.vector(object)) {
    writeLines(substr(paste(name, paste(object, collapse = ", "), sep = " = "), 1, 100))
  } else {
    writeLines(paste(name, object, sep = " = "))
  }
}

#' Print a list of objects
#'
#' @details
#' Will print nested lists using indentation.
#'
#' @param object   The list to print.
#'
#' @export
prettyPrint <- function(object) {
  .recursivePrettyPrint(object, "", 0)
}

#' Select variables from a list of objects of the same type
#'
#' @param x        A list of objects of the same type.
#' @param select   A character vector of names of variables to select.
#'
#' @export
selectFromList <- function(x, select) {
  return(sapply(x, function(x) {
    x[names(x)[names(x) %in% select]]
  }, simplify = FALSE))
}

#' Exclude variables from a list of objects of the same type
#'
#' @param x         A list of objects of the same type.
#' @param exclude   A character vector of names of variables to exclude.
#'
#' @export
excludeFromList <- function(x, exclude) {
  return(sapply(x, function(x) {
    x[names(x)[!(names(x) %in% exclude)]]
  }, simplify = FALSE))
}

#' In a list of object of the same type, find those that match the input
#'
#' @details
#' Typically, toMatch will contain a subset of the variables that are in the objects in the list. Any
#' object matching all variables in \code{toMatch} will be included in the result.
#'
#' @param x         A list of objects of the same type.
#' @param toMatch   The object to match.
#'
#' @return
#' A list of objects that match the \code{toMatch} object.
#'
#' @export
matchInList <- function(x, toMatch) {
  selected <- selectFromList(x, names(toMatch))
  result <- list()
  for (i in 1:length(x)) {
    if (identical(selected[[i]], toMatch)) {
      result[[length(result) + 1]] <- x[[i]]
    }
  }
  return(result)
}

#' Deprecated: Convert arguments used in call to a list
#'
#' @details
#' Takes the argument values (both default and user-specified) and store them in a list. 
#' 
#' This function is deprecated because it fails when used in a function that is called using ::.
#'
#' @param matchCall     The result of \code{match.call()}.
#' @param resultClass   The class of the resulting object.
#'
#' @return
#' An object of the class specified in \code{resultClass}.
#'
#' @examples
#' myFun <- function(x = 1, y = 2) {
#'   return(convertArgsToList(match.call()))
#' }
#'
#' @export
convertArgsToList <- function(matchCall, resultClass = "list") {
  # First: get default values:
  result <- list()
  for (name in names(formals(as.character(matchCall[[1]])))) {
    result[[name]] <- get(name, envir = parent.frame(n = 1))
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(matchCall)[-1], function(x) eval(x, envir = sys.frame(-4)))
  for (name in names(values)) {
    if (name %in% names(result))
      result[[name]] <- values[[name]]
  }
  class(result) <- resultClass
  return(result)
}