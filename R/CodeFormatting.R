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

.mySplit <- function(fullLine,
                     anchors = c("\\("),
                     separators = c(","),
                     offsets = c(-3),
                     width.cutoff) {
  bestSolution <- c(fullLine)
  minLength <- nchar(fullLine)
  minLines <- 1

  for (i in 1:length(anchors)) {
    anchor <- anchors[i]
    separator <- separators[i]
    offset <- offsets[i]
    potentialAnchors <- gregexpr(anchor, fullLine)
    if (potentialAnchors[[1]][1] != -1) {
      quotationMark <- regexpr("\"", fullLine)
      for (potentialAnchor in potentialAnchors[[1]]) {
        if (quotationMark[[1]][1] == -1 || quotationMark[[1]][1] > potentialAnchor) {
          indent <- potentialAnchor + 1
          newX <- c()
          depth <- 0
          quote <- FALSE
          start <- 1
          for (j in indent:nchar(fullLine)) {
          char <- substr(fullLine, j, j)
          if (char == "\"") {
            quote <- !quote
          } else if (!quote) {
            if (char == "(") {
            depth <- depth + 1
            } else if (char == ")") {
            if (depth == 0)
              break
            depth <- depth - 1
            } else if (depth == 0 & char == separator) {
            part <- substr(fullLine, start, j)
            if (start != 1) {
              part <- paste(paste(rep(" ", indent + offset), collapse = ""), part)
            }
            newX <- c(newX, part)
            start <- j + 1
            }
          }
          }
          part <- substr(fullLine, start, nchar(fullLine))
          if (start != 1) {
          part <- paste(paste(rep(" ", indent + offset), collapse = ""), part)
          }
          newX <- c(newX, part)
          maxLength <- 0
          for (x in newX) if (nchar(x) > maxLength)
          maxLength <- nchar(x)
          lines <- length(newX)
          better <- FALSE
          if (maxLength <= width.cutoff && minLength > width.cutoff) {
          better <- TRUE
          } else if (maxLength <= width.cutoff && minLength <= width.cutoff) {
          if (lines < minLines)
            better <- TRUE
          } else {
          if (maxLength < minLength)
            better <- TRUE
          }
          if (better) {
          bestSolution <- newX
          minLength <- maxLength
          minLines <- lines
          }
        }
      }
    }
  }
  return(bestSolution)
}

.reWrapLines <- function(x, width.cutoff) {
  # Unwrap lines:
  newX <- c()
  i <- 1
  while (i <= length(x)) {
    if (regexpr("[,+-] $", x[i]) != -1 && regexpr("^\\s*#", x[i]) == -1) {
      fullLine <- c(x[i])
      i <- i + 1
      while (regexpr("[,+-] $", x[i]) != -1) {
        fullLine <- c(fullLine, substr(x[i], regexpr("[^ ]", x[i]), nchar(x[i])))
        i <- i + 1
      }
      fullLine <- c(fullLine, substr(x[i], regexpr("[^ ]", x[i]), nchar(x[i])))
      fullLine <- paste(fullLine, collapse = "")
      newX <- c(newX, fullLine)
    } else {
      newX <- c(newX, x[i])
    }
    i <- i + 1
  }
  x <- newX

  # Rewrap lines:
  newX <- c()
  i <- 1
  while (i <= length(x)) {
    if (nchar(x[i]) > width.cutoff && regexpr("^\\s*#", x[i]) == -1) {
      newSplit <- .mySplit(x[i],
                           anchors = c("<- ", "\\("),
                           separators = c("+", ","),
                           offsets = c(-1, -3),
                           width.cutoff)
      for (j in 1:length(newSplit)) {
        if (nchar(newSplit[j]) <= width.cutoff) {
          newX <- c(newX, newSplit[j])
        } else {
          newerSplit <- .mySplit(newSplit[j],
                                 anchors = c("\\("),
                                 separators = c(","),
                                 offsets = c(-3),
                                 width.cutoff)
          newX <- c(newX, newerSplit)
        }
      }
    } else {
      newX <- c(newX, x[i])
    }
    i <- i + 1
  }
  return(newX)
}

.findStartOfBody <- function(text) {
  i <- 1
  while (i <= length(text)) {
    if (regexpr("^ *(#([^']|$)|$)", text[i]) == -1)
      return(i)
    i <- i + 1
  }
  return(-1)
}
.tidyExample <- function(text, width.cutoff) {
  start <- 1
  newText <- c()
  snippet <- c()
  inDontRun <- FALSE
  level <- 0
  for (i in 1:length(text)) {
    if (!inDontRun && regexpr("\\\\dontrun *\\{", text[i]) != -1) {
      if (i > start) {
        dontrun <- regexpr("\\\\dontrun *\\{", text[i])
        snippet <- c(snippet, text[start:(i - 1)])
        if (dontrun[[1]] > 1)
          snippet <- c(snippet, substr(text[i], 1, dontrun[[1]] - 1))
        newText <- c(newText, .myTidy(snippet, width.cutoff))
        snippet <- c(substr(text[i], dontrun[[1]] + attr(dontrun, "match.length"), nchar(text[i])))
        if (gsub("\\s", "", snippet[1]) == "")
          snippet <- c()
      }
      newText <- c(newText, "\\dontrun{")
      start <- i + 1
      inDontRun <- TRUE
    } else if (inDontRun) {
      for (j in 1:nchar(text[i])) {
        if (substr(text[i], j, j) == "{") {
          level <- level + 1
        } else if (substr(text[i], j, j) == "}") {
          level <- level - 1
          if (level == -1) {
          snippet <- c(snippet, text[start:(i - 1)])
          if (j > 2)
            snippet <- c(snippet, substr(text[i], 1, j - 1))
          newText <- c(newText, .myTidy(snippet, width.cutoff), "}")
          snippet <- c(substr(text[i], j + 1, nchar(text[i])))
          if (gsub("\\s", "", snippet[1]) == "")
            snippet <- c()
          inDontRun <- FALSE
          start <- i + 1
          level <- 0
          break
          }
        }
      }
    }
  }
  if (length(text) >= start) {
    snippet <- c(snippet, text[start:length(text)])
  }
  if (length(snippet) != 0) {
    newText <- c(newText, .myTidy(snippet, width.cutoff))
  }
  return(newText)
}

.wrapRoxygenLine <- function(line, width.cutoff) {
  newText <- c()
  start <- 1
  i <- 1
  level <- 0
  itemize <- FALSE
  while (i <= nchar(line)) {
    if (!itemize && substr(line, i, i + nchar("\\itemize{") - 1) == "\\itemize{") {
      if (i > start) {
        newText <- c(newText, strwrap(substr(line, start, i - 1), width.cutoff))
      }
      newText <- c(newText, "\\itemize{")
      start <- i + nchar("\\itemize{")
      # start = start + regexpr('[^ ]', substr(line,start, nchar(line))) - 1
      i <- start
      level <- 0
      itemize <- TRUE
    }
    if (itemize) {
      if (substr(line, i, i + nchar("\\item") - 1) == "\\item") {
        if (i > start && regexpr("[^ ]", substr(line, start, i - 1)) != -1) {
          text <- strwrap(substr(line, start, i - 1), width.cutoff - nchar("  \\item"))
          text[1] <- paste("  \\item", text[1])
          if (length(text) > 1) {
          text[2:length(text)] <- paste(paste(rep(" ", nchar("  \\item")), collapse = ""),
                                        text[2:length(text)])
          }
          newText <- c(newText, text)
        }
        start <- i + nchar("\\item")
        i <- start
      }
      if (substr(line, i, i) == "{") {
        level <- level + 1
      }
      if (substr(line, i, i) == "}") {
        level <- level - 1
        if (level == -1) {
          text <- strwrap(substr(line, start, i - 1), width.cutoff - nchar("  \\item"))
          text[1] <- paste("  \\item", text[1])
          if (length(text) > 1) {
          text[2:length(text)] <- paste(paste(rep(" ", nchar("  \\item")), collapse = ""),
                                        text[2:length(text)])
          }
          newText <- c(newText, text)
          newText <- c(newText, "}")
          start <- i + 1
          itemize <- FALSE
        }
      }
    }
    i <- i + 1
  }
  if (i >= start) {
    newText <- c(newText, strwrap(substr(line, start, i - 1), width.cutoff))
  }
  return(newText)
}

.tidyRoxygenBlock <- function(text, width.cutoff) {
  # Remove #' and unwrap lines:
  newText <- c()
  line <- ""
  examples <- FALSE
  for (i in 1:length(text)) {
    chunk <- sub("^\\s*#'\\s*", "", text[i])  # Remove leading spaces and #'
    chunk <- sub("\\s*$", "", chunk)  # Remove trailing spaces

    if (regexpr("^@|$)", chunk) != -1)
      examples <- FALSE

    if (chunk == "" || regexpr("^@|$)", chunk) != -1 || examples) {
      if (!(length(newText) == 0 && line == ""))
        newText <- c(newText, line)
      line <- ""
    }
    if (regexpr("^@examples", chunk) != -1) {
      examples <- TRUE
    }
    if (chunk != "")
      line <- paste(line, chunk, " ", sep = "")
  }
  newText <- c(newText, line)
  text <- newText

  # Put most keywords on their own line:
  newText <- c()
  for (i in 1:length(text)) {
    if ((regexpr("^@",
                 text[i]) != -1) && (regexpr("^(@param|@template|@export|@keyword|@docType|@importFrom|@import|@useDynLib|@name)",
                                             text[i]) == -1)) {
      keyword <- regexpr("^@[a-zA-Z0-9]*", text[i])
      newText <- c(newText, substr(text[i], 1, attr(keyword, "match.length")))
      if (attr(keyword, "match.length") + 2 < nchar(text[i]))
        newText <- c(newText, substr(text[i], attr(keyword, "match.length") + 2, nchar(text[i])))
    } else {
      newText <- c(newText, text[i])
    }
  }
  text <- newText

  # Perform wrapping
  maxParamLength <- 0
  for (i in 1:length(text)) {
    keyword <- regexpr("^@param\\s+[a-zA-Z0-9.]+", text[i])
    if (attr(keyword, "match.length") > maxParamLength)
      maxParamLength <- attr(keyword, "match.length")
  }
  examples <- FALSE
  example <- c()
  newText <- c()
  for (i in 1:length(text)) {
    if (examples) {
      if (regexpr("^@", text[i]) == -1) {
        example <- c(example, text[i])
      } else {
        examples <- FALSE
        if (length(example) != 0) {
          example <- .tidyExample(example, width.cutoff = width.cutoff - 3)
          newText <- c(newText, example)
        }
        example <- c()
      }
    }
    if (!examples) {
      if (regexpr("^@param", text[i]) == -1) {
        newText <- c(newText, .wrapRoxygenLine(text[i], width.cutoff = width.cutoff))
      } else {
        param <- regexpr("^@param\\s+[a-zA-Z0-9._]+", text[i])
        definition <- regexpr("^@param\\s+[a-zA-Z0-9._]+\\s+", text[i])
        part1 <- substr(text[i], 1, attr(param, "match.length"))
        part2 <- substr(text[i], attr(definition, "match.length") + 1, nchar(text[i]))
        part2Wrapped <- .wrapRoxygenLine(part2, width.cutoff = width.cutoff - maxParamLength - 2)
        line1 <- paste(part1, paste(rep(" ", 3 + maxParamLength - attr(param, "match.length")),
                                    collapse = ""), part2Wrapped[1], sep = "")
        newText <- c(newText, line1)
        if (length(part2Wrapped) > 1) {
          otherLines <- paste(paste(rep(" ", 2 + maxParamLength), collapse = ""),
                              part2Wrapped[2:length(part2Wrapped)])
          newText <- c(newText, otherLines)
        }
      }
      if (regexpr("^@examples", text[i]) != -1) {
        examples <- TRUE
      }
    }
  }
  if (length(example) != 0) {
    example <- .tidyExample(example, width.cutoff = width.cutoff - 3)
    newText <- c(newText, example)
  }
  text <- paste("#'", newText)
  return(text)
}

.roxygenTidy <- function(text, width.cutoff) {
  start <- -1
  toAdd <- 1
  newText <- c()
  i <- 1
  while (i <= length(text)) {
    if (regexpr("^ *#'", text[i]) != -1) {
      if (start == -1)
        start <- i
    } else {
      if (start != -1) {
        if (start > toAdd)
          newText <- c(newText, text[toAdd:(start - 1)])
        newText <- c(newText, .tidyRoxygenBlock(text[start:(i - 1)], width.cutoff = width.cutoff))
        toAdd <- i
      }
      start <- -1
    }
    i <- i + 1
  }
  if (length(text) >= toAdd) {
    if (start != -1) {
      newText <- c(newText,
                   .tidyRoxygenBlock(text[start:length(text)], width.cutoff = width.cutoff))
    } else {
      newText <- c(newText, text[toAdd:length(text)])
    }
  }
  return(newText)
}

.trimTrailingWhiteSpace <- function(text) {
  text <- sub("\\s+$", "", text)
  return(text)
}

.formatRblock <- function(text, width.cutoff) {
  formatROutput <- capture.output(formatR::tidy_source(text = text,
                                                       width.cutoff = width.cutoff,
                                                       arrow = TRUE,
                                                       indent = 2,
                                                       output = TRUE))
  if (length(grep("^\\$text.tidy$", formatROutput)) == 0) {
    return(formatROutput)
  } else {
    return(text)
  }
}

.applyFormatR <- function(text, width.cutoff) {
  # Skip all roxygen lines, apply formatR to the rest. Reason: formatR changes the Roxygen blocks,
  # replacing double quotes with single quotes
  newText <- c()
  start <- 1
  for (i in 1:length(text)) {
    if (regexpr("^ *#'", text[i]) != -1) {
      if (i > start) {
        newText <- c(newText, .formatRblock(text[start:(i - 1)], width.cutoff))
      }
      newText <- c(newText, text[i])
      start <- i + 1
    }
  }
  if (length(text) >= start) {
    newText <- c(newText, .formatRblock(text[start:length(text)], width.cutoff))
  }
  return(newText)
}

.myTidy <- function(text, width.cutoff) {
  text <- gsub("\\t", "", text)  # Remove all tabs
  text <- .applyFormatR(text, width.cutoff = width.cutoff)
  text <- .reWrapLines(text, width.cutoff = width.cutoff)
  text <- .roxygenTidy(text, width.cutoff = width.cutoff)
  text <- .trimTrailingWhiteSpace(text)
}

#' Format R code
#'
#' @param text           A character vector with the R code to be formatted.
#' @param width.cutoff   Number of characters that each line should be limited to.
#'
#' @return
#' A character vector with formatted R code.
#'
#' @export
formatRText <- function(text, width.cutoff = 100) {
  startOfBody <- .findStartOfBody(text)
  if (startOfBody == 1) {
    header <- c()
  } else {
    header <- text[1:(startOfBody - 1)]
  }
  body <- text[startOfBody:length(text)]
  body <- .myTidy(body, width.cutoff = width.cutoff)
  text <- c(header, body)
  return(text)
}

#' Format an R file
#'
#' @param file           The path to the file.
#' @param width.cutoff   Number of characters that each line should be limited to.
#'
#' @export
formatRFile <- function(file, width.cutoff = 100) {
  # Note: Github code window width is 130 characters, but 100 fits better on my laptop
  text <- readLines(file)
  text <- formatRText(text, width.cutoff)
  writeLines(text, con = file)
}

#' Format all R files in a folder
#'
#' @param path                Path to the folder containing the files to format. Only files with the .R
#'                            extension will be formatted.
#' @param recursive           Include all subfolders?
#' @param skipAutogenerated   Skip autogenerated files such as RcppExports.R?
#' @param ...                 Parameters to be passed on the the formatRFile function
#'
#' @examples
#' \dontrun{
#' formatRFolder()
#' }
#' @export
formatRFolder <- function(path = ".", recursive = TRUE, skipAutogenerated = TRUE, ...) {
  flist <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE, recursive = recursive)
  for (f in flist) {
    if (skipAutogenerated && regexpr("RcppExports.R$", f) != -1) {
      message("Skipping ", f)
    } else {
      message("Auto code formatting ", f)
      formatRFile(f)
    }
  }
}
