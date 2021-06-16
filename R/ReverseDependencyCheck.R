# Copyright 2021 Observational Health Data Sciences and Informatics
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

reverseDependencyCheck <- function(rootPackage) {
  packageListUrl <- "https://raw.githubusercontent.com/OHDSI/Hades/master/extras/packages.csv"
  gitHubOrganization <- "ohdsi"
  
  hadesPackageList <- read.table(packageListUrl, sep = ",", header = TRUE) 
  
  dependencies <- lapply(hadesPackageList$name, getPackageDependenciesFromGitHub)
  dependencies <- do.call(rbind, dependencies)
  hadesDependencies <- dependencies[dependencies$dependency %in% hadesPackageList$name & 
                                      dependencies$type != "Suggests", ]
  
  getReverseDependencies <- function(package, maxDepth = 10) {
    # print(package)
    if (maxDepth == 0) {
      warning("Maximum recursion depth reached. Are there circular dependencies?")
      return(c())
    }
    reverseDependencies <- hadesDependencies$package[hadesDependencies$dependency == package]
    recursiveReverseDependencies <- lapply(reverseDependencies, getReverseDependencies, maxDepth = maxDepth - 1)
    recursiveReverseDependencies <- do.call(c, recursiveReverseDependencies)
    return(unique(c(reverseDependencies, recursiveReverseDependencies)))
  }
  reverseDependencies <- getReverseDependencies(rootPackage)
  
  # Includes suggests of packages to check, but not suggests of deeper dependencies:
  packagesToInstall <- unique(c(reverseDependencies,
                                dependencies$dependency[dependencies$package %in% reverseDependencies]))
  
  packagesToInstallFromGitHub <- packagesToInstall[packagesToInstall %in% hadesPackageList$name[!hadesPackageList$inCran]]
  packagesToInstallFromCran <- packagesToInstall[!packagesToInstall %in% packagesToInstallFromGitHub]
  packagesToInstallFromCran <- c(packagesToInstallFromCran, "formatR") # Required for some vignettes
  
  # Does not work from within RStudio because it keeps packages loaded:
  install.packages(packagesToInstallFromCran)
  for (package in packagesToInstallFromGitHub) {
    remotes::install_github(sprintf("%s/%s", gitHubOrganization, package), upgrade = FALSE)
  }
  
  for (package in reverseDependencies) {
    if (hadesPackageList$inCran[hadesPackageList$name == package]) {
      source <- "CRAN"
    } else {
      source <- "GitHub"
    }
    checkPackage(package, source)
  }
}

checkPackage <- function(package, source) {
  writeLines(sprintf("*** Checking package %s ***", package))
  gitHubOrganization <- "ohdsi"
  if (source == "CRAN") {
    sourcePackage <- remotes::download_version(package, type = "source")
    on.exit(unlink(sourcePackage))
  } else if (source == "GitHub") {
    sourcePackage <- remotes::remote_download(remotes::github_remote(sprintf("%s/%s", gitHubOrganization, package)))
    on.exit(unlink(sourcePackage))
  } else {
    stop(sprintf("Unknown source '%s'", source))
  }
  sourceFolder <- tempfile(pattern = package)
  dir.create(sourceFolder)
  on.exit(unlink(sourceFolder, recursive = TRUE), add = TRUE)
  untar(sourcePackage, exdir = sourceFolder)
  sourcePath <- list.dirs(sourceFolder, full.names = TRUE, recursive = FALSE)
  docDir <- file.path(sourcePath, "inst", "doc")
  if (dir.exists(docDir)) {
    unlink(docDir, recursive = TRUE)
  }
  x <- rcmdcheck::rcmdcheck(path = sourcePath, args = c("--no-manual", "--as-cran", "--no-multiarch"), error_on = "warning")
}

getPackageDependenciesFromGitHub <- function(package) {
  # print(package)
  descriptionUrlTemplate <- "https://raw.githubusercontent.com/OHDSI/%s/master/DESCRIPTION"
  
  description <- scan(sprintf(descriptionUrlTemplate, package), what = character(), sep = "|", quiet = TRUE) 
  dependencies <- lapply(X = c("Depends", "Imports", "LinkingTo", "Suggests"), 
                         FUN = extractDependenciesFromDescriptionSection, 
                         description = description)
  dependencies <- do.call(rbind, dependencies)
  dependencies <- dependencies[dependencies$dependency != "R", ]
  dependencies <- dependencies[!dependencies$dependency %in% getCorePackages(), ]
  dependencies$package <- rep(package, nrow(dependencies))
  return(dependencies)
}

extractDependenciesFromDescriptionSection <- function(section, description) {
  tagsPos <- grep(":", description)
  sectionPos <- grep(sprintf("%s:", section), description)
  if (length(sectionPos) != 0) {
    endOfSection <- ifelse(sectionPos < max(tagsPos), min(tagsPos[tagsPos > sectionPos]), length(description) + 1)
    dependencies <- gsub("[\t ,]|(\\(.*\\))", "", description[(sectionPos + 1):(endOfSection - 1)])
    if (length(section) > 0) {
      return(data.frame(type = section,
                        dependency = dependencies))
    } 
  }
  return(NULL)
}
