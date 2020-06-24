# @file PackageTools.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
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


#' Checks if a pacakge is installed
#' 
#' @details 
#' Borrowed from devtools:
#' https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L44
#' 
#' @param pkg           The package name
#' @param version       The minimum version of the package required. Default is 0
#'
#' @return 
#' TRUE if the package is installed and at or above the version specified.
#' 
#' @examples
#' \dontrun{
#' # This will check if devtools is installed:
#'
#' isPackageInstalled(pkg = "devtools")
#'                    
#' }
#'                        
#' @export
isPackageInstalled <- function(pkg, version = 0) {
  installedVersion <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  !is.na(installedVersion) && installedVersion >= version
}

#' Ensures a package is installed.
#' 
#' @details 
#' Checks to see if a package is installed. If it is not, this function
#' will attempt to install it.
#' 
#' Borrowed and adapted from devtools:
#' https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L74
#' 
#' @param pkg           The package name to ensure is installed
#' 
#' @examples
#' \dontrun{
#' # This will ensure ggplot is installed:
#'
#' ensurePackageInstalled(pkg = "ggplot")
#'                    
#' }
#'                        
#' @export
ensurePackageInstalled <- function(pkg) {
  if (!isPackageInstalled(pkg)) {
    msg <- paste0(sQuote(pkg), " must be installed for this functionality.")
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