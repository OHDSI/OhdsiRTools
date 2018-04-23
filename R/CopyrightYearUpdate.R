# @file OhdsiRTools.R
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


#' Update the copyright year in a R or SQL file
#'
#' @param file   The path to the file.
#'
#' @export
updateCopyrightYearFile <- function(file) {
  text <- readLines(file)
  lineNr <- grep("Copyright 20[0-9][0-9]", text)
  if (length(lineNr) > 0) {
    year <- format(Sys.Date(), "%Y")
    text[lineNr] <- gsub("Copyright 20[0-9][0-9]", paste("Copyright", year), text[lineNr])
  }
  writeLines(text, con = file)
}

#' Update the package name in a R or SQL file
#'
#' @param file          The path to the file.
#' @param packageName   The replacement package name
#'
#' @export
updatePackageName <- function(file, packageName) {
  text <- readLines(file)
  matchString <- "# This file is part of [A-Za-z]+"
  lineNr <- grep(matchString, text)
  if (length(lineNr) > 0) {
    text[lineNr] <- gsub(matchString, paste("# This file is part of", packageName), text[lineNr])
  }
  writeLines(text, con = file)
}

#' Update the copyright year in all R and SQL files in a folder
#'
#' @param path        Path to the folder containing the files to update. Only files with the .R and
#'                    .SQL extension will be updated.
#' @param recursive   Include all subfolders?
#'
#' @examples
#' \dontrun{
#' updateCopyrightYearFolder()
#' }
#' @export
updateCopyrightYearFolder <- function(path = ".", recursive = TRUE) {
  flist <- list.files(path,
                      pattern = "\\.[Rr]$|\\.[Ss][Qq][Ll]$|\\.[Jj][Aa][Vv][Aa]$|\\.[Cc][Pp][Pp]$|\\.[Hh]$",
                      full.names = TRUE,
                      recursive = recursive)
  flist <- flist[!grepl("/packrat/", flist)]
  for (f in flist) {
    message("Checking copyright year in ", f)
    updateCopyrightYearFile(f)
  }
}

#' Update the package name in all R and SQL files in a folder
#'
#' @param path          Path to the folder containing the files to update. Only files with the .R and
#'                      .SQL extension will be updated.
#' @param packageName   The replacement package name
#' @param recursive     Include all subfolders?
#'
#' @examples
#' \dontrun{
#' updateCopyrightYearFolder()
#' }
#' @export
updatePackageNameFolder <- function(path = ".", packageName, recursive = TRUE) {
  flist <- list.files(path,
                      pattern = "\\.[Rr]$|\\.[Ss][Qq][Ll]$|\\.[Jj][Aa][Vv][Aa]$|\\.[Cc][Pp][Pp]$|\\.[Hh]$",
                      full.names = TRUE,
                      recursive = recursive)
  flist <- flist[!grepl("/packrat/", flist)]
  for (f in flist) {
    message("Checking package name in ", f)
    updatePackageName(f, packageName)
  }
}
