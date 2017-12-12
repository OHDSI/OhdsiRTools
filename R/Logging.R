# @file Logging.R
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


#' Layout for futile.logger for parellel computing
#' 
#' @description 
#' A layout function to be used with the \code{flog.layout} function in the \code{futile.logger} package. The layout is 
#' identical to \code{layout.simple}, except that the thread idenftifier is also included.
#'
#' @param level  The level of the message (e.g. "INFO")
#' @param msg    The message to layout.
#' @param ...    Values to be used in the message.
#'
#' @export
layoutParallel <- function (level, msg, ...) {
  the.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (length(list(...)) > 0) {
    parsed <- lapply(list(...), function(x) ifelse(is.null(x), 
                                                   "NULL", x))
    msg <- do.call(sprintf, c(msg, parsed))
  }
  threadNumber <- getOption("threadNumber")
  if (is.null(threadNumber)) {
    threadLabel <- "Main thread"
  } else {
    threadLabel <- paste("Thread", threadNumber)
  }
    
  sprintf("%s\t%s\t[%s]\t%s\n", threadLabel, names(level), the.time, msg)
}
