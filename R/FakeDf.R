# @file FakeDf.R
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

#' Create a subset of an ffdf object as a fakeDf object
#'
#' @param ffdf    The ffdf object to subset.
#' @param index   An integer vector or ri object identifying the rows to extract.
#'
#' @return
#' An object of type \code{fakeDf}, which is a list of vectors.
#'
#' @export
subsetFfdfToFakeDf <- function(ffdf, index) {
  fakeDf <- vector(length = length(ffdf), mode = "list")
  for (i in 1:length(ffdf)) {
    fakeDf[[i]] <- ffdf[index, i]
  }
  names(fakeDf) <- names(ffdf)
  class(fakeDf) <- "fakeDf"
  return(fakeDf)
}

#' Return number of rows in fakeDf
#'
#' @param fakeDf   The fakeDf object
#'
#' @return
#' Number of rows.
#'
#' @export
nrowOfFakeDf <- function(fakeDf) {
  return(length(fakeDf[[1]]))
}

#' Bind two fakeDf objects by rows
#'
#' @param fakeDf1   The first fakeDf object.
#' @param fakeDf2   The second fakeDf object.
#'
#' @return
#' A fakeDf object with the rows of both fakeDf1 and fakeDf2.
#'
#' @export
rbindFakeDfs <- function(fakeDf1, fakeDf2) {
  for (i in 1:length(fakeDf1)) {
    if (is.factor(fakeDf1[[i]])) {
      stop("R cannot concatenate factors")
    }
    fakeDf1[[i]] <- c(fakeDf1[[i]], fakeDf2[[i]])
  }
  return(fakeDf1)
}

#' Convert a fakeDf object to ffdf
#'
#' @param  fakeDf   An object of type \code{fakeDf}.
#'
#' @return
#' An ffdf object.
#'
#' @export
fakeDfToFfdf <- function(fakeDf) {
  fun <- function(...) {
    ff::ffdf(...)
  }
  return(do.call("fun", sapply(fakeDf, ff::ff)))
}

#' Create a subset of fakeDf object
#'
#' @param fakeDf   The fakeDf object to subset.
#' @param index    An integer vector identifying the rows to extract.
#'
#' @return
#' An object of type \code{fakeDf}, which is a list of vectors.
#'
#' @export
subsetFakeDf <- function(fakeDf, index) {
  result <- vector(length = length(fakeDf), mode = "list")
  for (i in 1:length(fakeDf)) {
    result[[i]] <- fakeDf[[i]][index]
  }
  names(result) <- names(fakeDf)
  class(result) <- "fakeDf"
  return(result)
}

#' Convert a fakeDf object to a data.frame
#'
#' @param  fakeDf   An object of type \code{fakeDf}.
#'
#' @return
#' A data.frame.
#'
#' @export
convertFakeDfToDataFrame <- function(fakeDf) {
  class(fakeDf) <- "data.frame"
  row.names(fakeDf) <- seq_len(nrowOfFakeDf(fakeDf))
  return(fakeDf)
}
