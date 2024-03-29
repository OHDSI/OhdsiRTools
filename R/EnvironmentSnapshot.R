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
#' @export
takeEnvironmentSnapshot <- function(rootPackage) {
  if (is.na(packageDescription(rootPackage, fields = "Package"))) {
    stop(sprintf("Root package %s not found. Did you forget to build it?", rootPackage))
  }

  splitPackageList <- function(packageList) {
    if (is.null(packageList)) {
      return(c())
    } else {
      return(strsplit(
        gsub("\\([^)]*\\)", "", gsub(" ", "", gsub("\n", "", packageList))),
        ","
      )[[1]])
    }
  }

  fetchDependencies <- function(package, recursive = TRUE, level = 0) {
    description <- packageDescription(package)
    packages <- splitPackageList(description$Depends)
    packages <- c(packages, splitPackageList(description$Imports))
    packages <- c(packages, splitPackageList(description$LinkingTo))
    # Note: if we want to include suggests, we'll need to consider circular references packages <-
    # c(packages, splitPackageList(description$Suggests))
    packages <- packages[packages != "R"]
    packages <- data.frame(name = packages, level = rep(
      level,
      length(packages)
    ), stringsAsFactors = FALSE)
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
  if (length(packages) > 0) {
    packageNames <- c(packages$name, rootPackage)
  } else {
    packageNames <- rootPackage
  }
  versions <- sapply(packageNames, getVersion)
  snapshot <- data.frame(
    package = names(versions),
    version = as.vector(versions),
    stringsAsFactors = FALSE
  )
  s <- sessionInfo()
  rVersion <- data.frame(
    package = "R",
    version = paste(s$R.version$major, s$R.version$minor, sep = "."),
    stringsAsFactors = FALSE
  )
  snapshot <- rbind(rVersion, snapshot)
  return(snapshot)
}

comparable <- function(installedVersion, requiredVersion) {
  parts1 <- strsplit(as.character(installedVersion), "[^0-9]")[[1]]
  parts2 <- strsplit(as.character(requiredVersion), "[^0-9]")[[1]]
  if (parts1[1] != parts2[1]) {
    return(FALSE)
  }
  parts1 <- as.numeric(parts1)
  parts2 <- as.numeric(parts2)
  if (length(parts1) > 1 && parts1[2] > parts2[2]) {
    return(TRUE)
  } else if (length(parts1) > 2 && parts1[2] == parts2[2] && parts1[3] > parts2[3]) {
    return(TRUE)
  } else if (length(parts1) > 3 && parts1[2] == parts2[2] && parts1[3] == parts2[3] && parts1[4] > parts2[4]) {
    return(TRUE)
  }
  return(FALSE)
}

#' Create a renv lock file
#'
#' @details
#' Create a lock file that allows reconstruction of the R environment using the \code{renv} package.
#' This function will include the root file and all of its dependencies in the lock file, requiring
#' the same package versions as currently installed on this computer.
#'
#' If \code{mode = "auto"}, this function will invoke \code{renv::init()}, which in turn will scan the
#' project folders for any dependencies that are referenced. Afterwards, references to OHDSI packages will
#' be altered so the correct GitHub tags are used for the installed versions.
#'
#' If \code{mode = "description"}, this function will assume the project is a full-fledged R package with
#' up-to-date DESCRIPTION, and will only install the dependencies listed in the DESCRIPION.
#'
#' The second option tends to lead to smaller lock files, but requires all dependencies are accurately listed
#' in the DESCRIPTION file of the study package.
#'
#' @param rootPackage                  The name of the root package, the package that we'd like to be
#'                                     able to run in the end.
#' @param mode                         Can be "auto" or "description". See details.
#' @param includeRootPackage           Include the root package in the renv file?
#' @param additionalRequiredPackages   Additional packages we want to have installed (with their
#'                                     dependencies), such as 'keyring'. Ignored if \code{mode = "auto"}.
#' @param ohdsiGitHubPackages          Names of R packages that need to be installed from the OHDSI
#'                                     GitHub.
#' @param ohdsiStudiesGitHubPackages   Names of R packages that need to be installed from the
#'                                     OHDSI-Studies GitHub.
#' @param fileName                     Name of the lock file to be generated. Ignored if \code{mode = "auto"}.
#' @param restart                      Boolean; attempt to restart the R session after initializing the project? 
#'
#' @return
#' Does not return a value. Is executed for the side-effect of creating the lock file.
#'
#' @export
createRenvLockFile <- function(rootPackage,
                               mode = "auto",
                               includeRootPackage = TRUE,
                               additionalRequiredPackages = NULL,
                               ohdsiGitHubPackages = getOhdsiGitHubPackages(),
                               ohdsiStudiesGitHubPackages = rootPackage,
                               fileName = "renv.lock",
                               restart = (mode == "auto")) {
  if (is.na(tryCatch(utils::packageVersion("renv"), error = function(e) NA))) {
    stop("The renv package must be installed to use this function")
  }
  if (mode == "auto") {
    createRenvLockFileAuto(
      rootPackage = rootPackage,
      includeRootPackage = includeRootPackage,
      ohdsiGitHubPackages = ohdsiGitHubPackages
    )
  } else {
    createRenvLockFileManual(
      rootPackage = rootPackage,
      includeRootPackage = includeRootPackage,
      additionalRequiredPackages = additionalRequiredPackages,
      ohdsiGitHubPackages = ohdsiGitHubPackages,
      ohdsiStudiesGitHubPackages = ohdsiStudiesGitHubPackages,
      fileName = fileName
    )
  }
  if (restart) {
    renv:::renv_restart_request(project = getwd(), reason = "renv activated")
  }
}

#' Fix HADES packages in lock file
#'
#' @description 
#' For all HADES packages that must be installed from GitHub, ensures the lock file points to the 
#' git tag corresponding to the installed version. If no corresponding tag exists, a warning is raised.
#' 
#' @param lockFile             The name of the lock file to fix.
#' @param ohdsiGitHubPackages  Names of R packages that need to be installed from the OHDSI GitHub.
#'
#' @return
#' Returns nothing. Fixes the lock file.
#' 
#' @export
fixHadesPackagesInLockFile <- function(lockFile = "renv.lock", ohdsiGitHubPackages = getOhdsiGitHubPackages()) {
  lock <- RJSONIO::fromJSON(lockFile)
  for (i in 1:length(lock$Packages)) {
    if (lock$Packages[[i]]["Source"] == "GitHub") {
      if (!lock$Packages[[i]]["Package"] %in% ohdsiGitHubPackages) {
        warning(sprintf("Found GitHub package '%s' that is not an OHDSI GitHub package.", lock$Packages[[i]]["Package"]))
      } else {
        remoteRef <- sprintf("v%s", lock$Packages[[i]]["Version"])
        if (!tagExists(lock$Packages[[i]]["Package"], remoteRef)) {
          warning(sprintf(
            "Tag '%s' does not exist for package '%s'. Did you install a develop version? Please only use released package versions.",
            remoteRef,
            lock$Packages[[i]]["Package"]
          ))
        } else {
          message(sprintf("Settings remote ref for package '%s' to '%s'", lock$Packages[[i]]["Package"], remoteRef))
          toDelete <- which(names(lock$Packages[[i]]) %in% c("RemoteSha", "Hash"))
          lock$Packages[[i]] <- lock$Packages[[i]][-toDelete]
          lock$Packages[[i]]["RemoteRef"] <- remoteRef
        }
      }
    }
  }
  json <- RJSONIO::toJSON(lock, pretty = TRUE)
  sink(lockFile)
  cat(json)
  sink()
  invisible(NULL)
}

createRenvLockFileAuto <- function(rootPackage,
                                   includeRootPackage = TRUE,
                                   ohdsiGitHubPackages) {
  if (includeRootPackage && is.na(packageDescription(rootPackage, fields = "Package"))) {
    stop(sprintf("Root package %s not found. Did you forget to build it?", rootPackage))
  }

  renv::init(restart = FALSE)
  fixHadesPackagesInLockFile(lockFile = "renv.lock", ohdsiGitHubPackages = ohdsiGitHubPackages)
  
  lock <- RJSONIO::fromJSON("renv.lock")
  if (includeRootPackage) {
    version <- packageDescription(rootPackage)$Version
    lock$Packages[[length(lock$Packages) + 1]] <- list(
      Package = rootPackage,
      Version = version,
      Source = "GitHub",
      RemoteType = "github",
      RemoteHost = "api.github.com",
      RemoteRepo = rootPackage,
      RemoteUsername = "ohdsi-studies",
      RemoteRef = "main"
    )
    names(lock$Packages)[length(lock$Packages)] <- rootPackage
  }
  json <- RJSONIO::toJSON(lock, pretty = TRUE)
  sink("renv.lock")
  cat(json)
  sink()
}

tagExists <- function(repo, tag) {
  url <- sprintf("https://github.com/OHDSI/%s/tree/%s", repo, tag)
  response <- httr::GET(url)
  return(response$status_code != "404")
}

createRenvLockFileManual <- function(rootPackage,
                                     includeRootPackage,
                                     additionalRequiredPackages,
                                     ohdsiGitHubPackages,
                                     ohdsiStudiesGitHubPackages,
                                     fileName) {
  snapShot <- takeEnvironmentSnapshot(rootPackage)
  rVersion <- snapShot[snapShot$package == "R", ]

  if (!is.null(additionalRequiredPackages)) {
    for (additionalRequiredPackage in additionalRequiredPackages) {
      snapShot <- rbind(snapShot, takeEnvironmentSnapshot(additionalRequiredPackage))
    }
    snapShot <- snapShot[!duplicated(snapShot$package), ]
  }
  snapShot <- snapShot[!snapShot$package %in% c("R", getCorePackages()), ]

  if (!includeRootPackage) {
    snapShot <- snapShot[snapShot$package != rootPackage, ]
  }

  cranPackages <- snapShot[!snapShot$package %in% c(
    ohdsiGitHubPackages,
    ohdsiStudiesGitHubPackages
  ), ]
  cranPackages <- rbind(cranPackages, data.frame(
    package = "renv",
    version = packageDescription("renv")$Version,
    stringsAsFactors = FALSE
  ))
  ohdsiGitHubPackages <- snapShot[snapShot$package %in% ohdsiGitHubPackages, ]
  ohdsiStudiesGitHubPackages <- snapShot[snapShot$package %in% ohdsiStudiesGitHubPackages, ]


  createRNode <- function() {
    list(
      Version = rVersion$version,
      Repositories = list(list(Name = "CRAN", URL = "https://cloud.r-project.org"))
    )
  }

  createCranNode <- function(i) {
    list(
      Package = cranPackages$package[i],
      Version = cranPackages$version[i],
      Source = "Repository",
      Repository = "CRAN"
    )
  }

  createOhdsiGitHubNode <- function(i) {
    list(
      Package = ohdsiGitHubPackages$package[i],
      Version = ohdsiGitHubPackages$version[i],
      Source = "GitHub",
      RemoteType = "github",
      RemoteHost = "api.github.com",
      RemoteRepo = ohdsiGitHubPackages$package[i],
      RemoteUsername = "ohdsi",
      RemoteRef = sprintf("v%s", ohdsiGitHubPackages$version[i])
    )
  }

  createOhdsiStudiesGitHubNode <- function(i) {
    list(
      Package = ohdsiStudiesGitHubPackages$package[i],
      Version = ohdsiStudiesGitHubPackages$version[i],
      Source = "GitHub",
      RemoteType = "github",
      RemoteHost = "api.github.com",
      RemoteRepo = ohdsiStudiesGitHubPackages$package[i],
      RemoteUsername = "ohdsi-studies",
      RemoteRef = "main"
    )
  }

  createPackagesNode <- function() {
    if (nrow(cranPackages) == 0) {
      cranNodes <- list()
    } else {
      cranNodes <- lapply(1:nrow(cranPackages), createCranNode)
      names(cranNodes) <- cranPackages$package
    }

    if (nrow(ohdsiGitHubPackages) == 0) {
      ohdsiGitHubNodes <- list()
    } else {
      ohdsiGitHubNodes <- lapply(1:nrow(ohdsiGitHubPackages), createOhdsiGitHubNode)
      names(ohdsiGitHubNodes) <- ohdsiGitHubPackages$package
    }

    if (nrow(ohdsiStudiesGitHubPackages) == 0) {
      ohdsiStudiesGitHubNodes <- list()
    } else {
      ohdsiStudiesGitHubNodes <- lapply(
        1:nrow(ohdsiStudiesGitHubPackages),
        createOhdsiStudiesGitHubNode
      )
      names(ohdsiStudiesGitHubNodes) <- ohdsiStudiesGitHubPackages$package
    }
    return(append(append(cranNodes, ohdsiGitHubNodes), ohdsiStudiesGitHubNodes))
  }

  lock <- list(R = createRNode(), Packages = createPackagesNode())
  json <- RJSONIO::toJSON(lock, pretty = TRUE)
  write(json, fileName)
}

#' Get a list of packages in the OHDSI GitHub.
#'
#' @details
#' Returns names of packages that need to be installed from https://github.com/ohdsi. Connects to
#' GitHub to get the latest list.
#'
#' @return
#' A character vector.
#'
#' @export
getOhdsiGitHubPackages <- function() {
  packageListUrl <- "https://raw.githubusercontent.com/OHDSI/Hades/main/extras/packages.csv"
  hadesPackageList <- read.table(packageListUrl, sep = ",", header = TRUE)
  hadesPackages <- hadesPackageList$name[!hadesPackageList$inCran]
  return(hadesPackages)
}

#' Get a list of R core packages
#'
#' @details
#' Returns names of packages that are part of the R code, and can therefore not be installed.
#'
#' @return
#' A character vector.
#'
#' @export
getCorePackages <- function() {
  return(rownames(installed.packages(priority = "base")))
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
#' @param strict                If TRUE, the exact version of each package will installed. If FALSE, a
#'                              package will only be installed if (a) a newer version is required than
#'                              currently installed, or (b) the major version number is different.
#' @param skipLast              Skip last entry in snapshot? This is usually the study package that
#'                              needs to be installed manually.
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
restoreEnvironment <- function(snapshot,
                               stopOnWrongRVersion = FALSE,
                               strict = FALSE,
                               skipLast = TRUE) {
  start <- Sys.time()
  # R core packages that cannot be installed:
  corePackages <- c("devtools", "remotes", getCorePackages())

  # OHDSI packages not in CRAN:
  ohdsiPackages <- getOhdsiGitHubPackages()

  s <- sessionInfo()
  rVersion <- paste(s$R.version$major, s$R.version$minor, sep = ".")
  if (rVersion != as.character(snapshot$version[snapshot$package == "R"])) {
    message <- sprintf(
      "Wrong R version: need version %s, found version %s",
      as.character(snapshot$version[snapshot$package ==
        "R"]), rVersion
    )
    if (stopOnWrongRVersion) {
      stop(message)
    } else {
      warning(message)
    }
  }

  snapshot <- snapshot[snapshot$package != "R", ]
  if (skipLast) {
    snapshot <- snapshot[1:(nrow(snapshot) - 1), ]
  }
  for (i in 1:nrow(snapshot)) {
    package <- as.character(snapshot$package[i])
    requiredVersion <- as.character(snapshot$version[i])
    isInstalled <- package %in% installed.packages()
    if (isInstalled) {
      installedVersion <- packageDescription(package)$Version
    }
    if (package %in% corePackages) {
      writeLines(sprintf("Skipping %s (%s) because part of R core", package, requiredVersion))
    } else if (isInstalled && requiredVersion == installedVersion) {
      writeLines(sprintf(
        "Skipping %s (%s) because correct version already installed",
        package,
        requiredVersion
      ))
    } else if (!strict && isInstalled && comparable(installedVersion, requiredVersion)) {
      writeLines(sprintf(
        "Skipping %s because installed version (%s) is newer than required version (%s), and major version number is the same",
        package,
        installedVersion,
        requiredVersion
      ))
    } else if (package %in% ohdsiPackages) {
      if (isInstalled) {
        writeLines(sprintf(
          "Installing %s because version %s needed but version %s found",
          package,
          requiredVersion,
          installedVersion
        ))
      } else {
        writeLines(sprintf("Installing %s (%s)", package, requiredVersion))
      }
      url <- sprintf(
        "https://github.com/OHDSI/drat/raw/gh-pages/src/contrib/%s_%s.tar.gz",
        package,
        requiredVersion
      )
      remotes::install_url(url, dependencies = FALSE)
    } else {
      if (package %in% installed.packages()) {
        writeLines(sprintf(
          "Installing %s because version %s needed but version %s found",
          package,
          requiredVersion,
          installedVersion
        ))
      } else {
        writeLines(sprintf("Installing %s (%s)", package, requiredVersion))
      }
      remotes::install_version(
        package = package,
        version = requiredVersion,
        type = "source",
        dependencies = FALSE
      )
    }
  }
  delta <- Sys.time() - start
  writeLines(paste("Restoring environment took", delta, attr(delta, "units")))
  invisible(NULL)
}


#' Store snapshot of the R environment in the package
#'
#' @details
#' This function records all versions used in the R environment that are used by one root package, and
#' stores them in a CSV file in the R package that is currently being developed. The default location
#' is \code{inst/settings/rEnvironmentSnapshot.csv}.This can be used for example to restore the
#' environment to the state it was when a particular study package was run using the
#' \code{\link{restoreEnvironment}} function.
#'
#' @param rootPackage   The name of the root package
#' @param pathToCsv     The path for saving the snapshot (as CSV file).
#'
#' @examples
#' \dontrun{
#' insertEnvironmentSnapshotInPackage("OhdsiRTools")
#' }
#'
#' @export
insertEnvironmentSnapshotInPackage <- function(rootPackage,
                                               pathToCsv = "inst/settings/rEnvironmentSnapshot.csv") {
  snapshot <- takeEnvironmentSnapshot(rootPackage)
  folder <- dirname(pathToCsv)
  if (!file.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  write.csv(snapshot, pathToCsv, row.names = FALSE)
}

#' Restore environment stored in package
#'
#' @details
#' This function restores all packages (and package versions) described in the environment snapshot
#' stored in the package currently being developed. The default location is
#' \code{inst/settings/rEnvironmentSnapshot.csv}.
#'
#' @param pathToCsv             The path for saving the snapshot (as CSV file).
#' @param stopOnWrongRVersion   Should the function stop when the wrong version of R is installed? Else
#'                              just a warning will be thrown when the version doesn't match.
#' @param strict                If TRUE, the exact version of each package will installed. If FALSE, a
#'                              package will only be installed if (a) a newer version is required than
#'                              currently installed, or (b) the major version number is different.
#' @param skipLast              Skip last entry in snapshot? This is usually the study package that
#'                              needs to be installed manually.
#'
#' @examples
#' \dontrun{
#' restoreEnvironmentFromPackage()
#' }
#'
#' @export
restoreEnvironmentFromPackage <- function(pathToCsv = "inst/settings/rEnvironmentSnapshot.csv",
                                          stopOnWrongRVersion = FALSE,
                                          strict = FALSE,
                                          skipLast = TRUE) {
  snapshot <- read.csv(pathToCsv)
  restoreEnvironment(
    snapshot = snapshot,
    stopOnWrongRVersion = stopOnWrongRVersion,
    strict = strict,
    skipLast = skipLast
  )
}

#' Restore environment stored in package
#'
#' @details
#' This function restores all packages (and package versions) described in the environment snapshot
#' stored in the package currently being developed. The default location is
#' \code{inst/settings/rEnvironmentSnapshot.csv}.
#'
#' @param githubPath            The path for the GitHub repo containing the package (e.g.
#'                              'OHDSI/StudyProtocols/AlendronateVsRaloxifene').
#' @param pathToCsv             The path for the snapshot inside the package.
#' @param stopOnWrongRVersion   Should the function stop when the wrong version of R is installed? Else
#'                              just a warning will be thrown when the version doesn't match.
#' @param strict                If TRUE, the exact version of each package will installed. If FALSE, a
#'                              package will only be installed if (a) a newer version is required than
#'                              currently installed, or (b) the major version number is different.
#' @param skipLast              Skip last entry in snapshot? This is usually the study package that
#'                              needs to be installed manually.
#'
#' @examples
#' \dontrun{
#' restoreEnvironmentFromPackageOnGithub("OHDSI/StudyProtocols/AlendronateVsRaloxifene")
#' }
#'
#' @export
restoreEnvironmentFromPackageOnGithub <- function(githubPath,
                                                  pathToCsv = "inst/settings/rEnvironmentSnapshot.csv",
                                                  stopOnWrongRVersion = FALSE,
                                                  strict = FALSE,
                                                  skipLast = TRUE) {
  parts <- strsplit(githubPath, "/")[[1]]
  if (length(parts) > 2) {
    githubPath <- paste(c(parts[1:2], "main", parts[3:length(parts)]), collapse = "/")
  } else {
    githubPath <- paste(c(parts[1:2], "main"), collapse = "/")
  }
  url <- paste(c("https://raw.githubusercontent.com", githubPath, pathToCsv), collapse = "/")
  snapshot <- read.csv(url)
  restoreEnvironment(
    snapshot = snapshot,
    stopOnWrongRVersion = stopOnWrongRVersion,
    strict = strict,
    skipLast = skipLast
  )
}
