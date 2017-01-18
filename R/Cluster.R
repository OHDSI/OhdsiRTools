# @file Cluster.R
#
# Copyright 2017 Observational Health Data Sciences and Informatics
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

.computeFfMemPerCluster <- function(nClusters) {
  # memory.limit is windows specific
  if (.Platform$OS.type == "windows") {
    if (getRversion() >= "2.6.0")
      ffmaxbytes <- 0.5 * memory.limit() * (1024^2) else ffmaxbytes <- 0.5 * memory.limit()
  } else {
    # some magic constant (2GB)
    ffmaxbytes <- 2 * 1024^3
  }
  ffmaxbytes <- ffmaxbytes/nClusters
  # Limit size on machines with a lot of memory to prevent integer overflows in ff:
  ffmaxbytes <- min(ffmaxbytes, .Machine$integer.max * 12)

  ffbatchbytes <- ffmaxbytes/50
  return(c(round(ffmaxbytes), round(ffbatchbytes)))
}

#' Create a cluster of nodes for parallel computation
#'
#' @param numberOfThreads      Number of parallel threads.
#' @param singleThreadToMain   If \code{numberOfThreads} is 1, should we fall back to running the
#'                             process in the main thread?
#'
#' @return
#' An object representing the cluster.
#'
#' @export
makeCluster <- function(numberOfThreads, singleThreadToMain = TRUE) {
  if (numberOfThreads == 1 && singleThreadToMain) {
    cluster <- list()
    class(cluster) <- "noCluster"
  } else {
    cluster <- snow::makeCluster(numberOfThreads, type = "SOCK")
  }
  return(cluster)
}

#' Require a package in the cluster
#'
#' @param cluster   The cluster object.
#' @param package   The name of the package to load in all nodes.
#'
#' @export
clusterRequire <- function(cluster, package) {
  if (class(cluster)[1] == "noCluster") {
    do.call("require", list(package = package))
  } else {
    requirePackage <- function(package) {
      do.call("require", list(package = package))
    }
    for (i in 1:length(cluster)) {
      snow::sendCall(cluster[[i]], requirePackage, list(package = package))
    }
    for (i in 1:length(cluster)) {
      snow::recvOneResult(cluster)
    }
  }
}

#' Stop the cluster
#'
#' @param cluster   The cluster to stop
#'
#' @export
stopCluster <- function(cluster) {
  if (class(cluster)[1] != "noCluster") {
    snow::stopCluster.default(cluster)
  }
}

setFfMem <- function(values) {
  options(ffmaxbytes = values[1])
  options(ffbatchbytes = values[2])
  return(c(getOption("ffmaxbytes"), getOption("ffbatchbytes")))
}

setFfDir <- function(fftempdir) {
  options(fftempdir = fftempdir)
}

#' Apply a function to a list using the cluster
#'
#' @details
#' The function will be executed on each element of x in the threads of the cluster. If there are more
#' elements than threads, the elements will be queued. The progress bar will show the number of
#' elements that have been completed.
#' It can sometimes be important to realize that the context in which a function is created is also
#' transmitted to the worker node. If a function is defined inside another function, and that outer
#' function is called with a large argument, that argument will be transmitted to the worker node each
#' time the function is executed. It can therefore make sense to define the function to be called at
#' the package level rather than inside a function, to save overhead.
#'
#' @param cluster          The cluster of threads to run the function.
#' @param x                The list on which the function will be applied.
#' @param fun              The function to apply. Note that the context in which the function is
#'                         specifies matters (see details).
#' @param ...              Additional parameters for the function.
#' @param stopOnError      Stop when one of the threads reports an error? If FALSE, all errors will be
#'                         reported at the end.
#' @param progressBar      Show a progress bar?
#' @param divideFfMemory   When TRUE, the memory available for processing ff and ffdf objects will be
#'                         equally divided over the threads.
#' @param setFfTempDir     When TRUE, the ffTempDir option will be copied to each thread.
#'
#' @return
#' A list with the result of the function on each item in x.
#'
#' @export
clusterApply <- function(cluster,
                         x,
                         fun,
                         ...,
                         stopOnError = FALSE,
                         progressBar = TRUE,
                         divideFfMemory = TRUE,
                         setFfTempDir = TRUE) {
  if (class(cluster)[1] == "noCluster") {
    lapply(x, fun, ...)
  } else {
    if (divideFfMemory) {
      values <- .computeFfMemPerCluster(length(cluster))
      for (i in 1:length(cluster)) {
        snow::sendCall(cluster[[i]], setFfMem, list(values = values))
      }
      for (i in 1:length(cluster)) {
        if (min(snow::recvOneResult(cluster)$value == values) == 0)
          warning("Unable to set ffmaxbytes and/or ffbatchbytes on worker")
      }
    }
    if (setFfTempDir) {
      for (i in 1:length(cluster)) {
        snow::sendCall(cluster[[i]], setFfDir, list(fftempdir = options("fftempdir")$fftempdir))
      }
      for (i in 1:length(cluster)) {
        snow::recvOneResult(cluster)
      }
    }

    n <- length(x)
    p <- length(cluster)
    if (n > 0 && p > 0) {
      if (progressBar)
        pb <- txtProgressBar(style = 3)

      for (i in 1:min(n, p)) {
        snow::sendCall(cluster[[i]], fun, c(list(x[[i]]), list(...)), tag = i)
      }

      val <- vector("list", n)
      errors <- c(paste("Error(s) when calling function ",
                        substitute(fun, parent.frame(1)),
                        ":",
                        sep = ""))
      formatError <- function(error, args) {
        paste("\nError \"", error[[1]], "\" when using argument(s): ",
              paste(args, collapse = ","),
              sep = "")
      }
      for (i in 1:n) {
        d <- snow::recvOneResult(cluster)
        if (inherits(d$value, "try-error")) {
          val[d$tag] <- NULL
          errors <- c(errors, formatError(d$value, c(list(x[[i]]), list(...))))
          if (stopOnError)
          stop(paste(errors, collapse = ""), call. = FALSE)
        }
        if (progressBar)
          setTxtProgressBar(pb, i/n)
        j <- i + min(n, p)
        if (j <= n) {
          snow::sendCall(cluster[[d$node]], fun, c(list(x[[j]]), list(...)), tag = j)
        }
        val[d$tag] <- list(d$value)
      }
      if (progressBar)
        close(pb)
      if (length(errors) != 1)
        stop(paste(errors, collapse = ""), call. = FALSE)
      return(val)
    }
  }
}
