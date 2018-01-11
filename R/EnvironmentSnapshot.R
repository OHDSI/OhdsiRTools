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

#' Take a snapshot of the R environment
#'
#' @details
#' This function records all versions used in the R environment that are used by one root package.
#' This can be used for example to restore the environment to the state it was when a particular study
#' package was run using the \code{\link{restoreEnvironment}} function.
#'
#' @param rootPackage   The name of the root package
#'
#' @return
#' A data frame listing all the dependencies of the root package and their version numbers, in the
#' order in which they should be installed.
#'
#' @examples
#' snapshot <- takeEnvironmentSnapshot("OhdsiRTools")
#' snapshot
#'
#' @export
takeEnvironmentSnapshot <- function(rootPackage) {

  splitPackageList <- function(packageList) {
    if (is.null(packageList)) {
      return(c())
    } else {
      return(strsplit(gsub("\\([^)]*\\)", "", gsub(" ", "", gsub("\n", "", packageList))),
                      ",")[[1]])
    }
  }

  fetchDependencies <- function(package, recursive = TRUE, level = 0) {
    description <- packageDescription(package)
    packages <- splitPackageList(description$Depends)
    packages <- c(packages, splitPackageList(description$Imports))
    # Note: if we want to include suggests, we'll need to consider circular references packages <-
    # c(packages, splitPackageList(description$Suggests))
    packages <- packages[packages != "R"]
    packages <- data.frame(name = packages, level = rep(level,
                                                        length(packages)), stringsAsFactors = FALSE)
    if (recursive && nrow(packages) > 0) {
      all <- lapply(packages$name, fetchDependencies, recursive = TRUE, level = level + 1)
      dependencies <- do.call("rbind", all)
      if (nrow(dependencies) > 0) {
        packages <- rbind(packages, dependencies)
        packages <- aggregate(level ~ name, packages, max)
      }
    }
    return(packages)
  }

  packages <- fetchDependencies(rootPackage, recursive = TRUE)
  packages <- packages[order(-packages$level), ]
  getVersion <- function(package) {
    return(packageDescription(package)$Version)
  }
  versions <- sapply(c(packages$name, rootPackage), getVersion)
  snapshot <- data.frame(package = names(versions), version = as.vector(versions))
  s <- sessionInfo()
  rVersion <- data.frame(package = "R",
                         version = paste(s$R.version$major, s$R.version$minor, sep = "."))
  snapshot <- rbind(rVersion, snapshot)
  return(snapshot)
}

#' Restore the R environment to a snapshot
#'
#' @details
#' This function restores the R environment to a previous snapshot, meaning all the packages will be
#' restored to the versions they were at at the time of the snapshot. Note: on Windows you will very
#' likely need to have RTools installed to build the various packages.
#'
#' @param snapshot              The snapshot data frame as generated using the
#'                              \code{\link{takeEnvironmentSnapshot}} function.
#' @param stopOnWrongRVersion   Should the function stop when the wrong version of R is installed? Else
#'                              just a warning will be thrown when the version doesn't match.
#'
#'
#' @examples
#' \dontrun{
#' snapshot <- takeEnvironmentSnapshot("OhdsiRTools")
#' write.csv(snapshot, "snapshot.csv")
#'
#' # 5 years later
#'
#' snapshot <- read.csv("snapshot.csv")
#' restoreEnvironment(snapshot)
#' }
#' @export
restoreEnvironment <- function(snapshot, stopOnWrongRVersion = FALSE) {
  s <- sessionInfo()
  rVersion <- paste(s$R.version$major, s$R.version$minor, sep = ".")
  if (rVersion != as.character(snapshot$version[snapshot$package == "R"])) {
    message <- paste0("Wrong R version: need version ",
                      as.character(snapshot$version[snapshot$package ==
      "R"]), ", found version ", rVersion)
    if (stopOnWrongRVersion) {
      stop(message)
    } else {
      warning(message)
    }
  }
  snapshot <- snapshot[snapshot$package != "R", ]
  for (i in 1:nrow(snapshot)) {
    package <- as.character(snapshot$package[i])
    version <- as.character(snapshot$version[i])
    if (package %in% c("grDevices", "graphics", "utils", "stats", "methods", "tools")) {
      writeLines(paste0("Skipping ", package, " (", version, ") because part of R itself"))
    } else if (package %in% installed.packages() && packageDescription(package)$Version == version) {
      writeLines(paste0("Skipping ",
                        package,
                        " (",
                        version,
                        ") because correct version already installed"))
    } else {
      devtools::install_version(package = package, version = version, type = "source")
    }
  }
  invisible(NULL)
}


#' Store snapshot of the R environment in the package
#'
#' @details
#' This function records all versions used in the R environment that are used by one root package, and
#' stores them in the R package that is currently being developed in a file called
#' \code{inst/settings/rEnvironmentSnapshot.csv}.This can be used for example to restore the
#' environment to the state it was when a particular study package was run using the
#' \code{\link{restoreEnvironment}} function.
#'
#' @param rootPackage   The name of the root package
#'
#' @examples
#' \dontrun{
#' insertEnvironmentSnapshotInPackage("OhdsiRTools")
#' }
#'
#' @export
insertEnvironmentSnapshotInPackage <- function(rootPackage) {
  snapshot <- takeEnvironmentSnapshot(rootPackage)
  if (!file.exists("inst/settings")) {
    dir.create("inst/settings", recursive = TRUE)
  }
  fileName <- file.path("inst/settings", "rEnvironmentSnapshot.csv")
  write.csv(snapshot, fileName, row.names = FALSE)
}
