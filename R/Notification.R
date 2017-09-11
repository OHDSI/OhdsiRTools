# @file Notification.R
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


#' Run code and send e-mail notification on error, warning, or completion
#'
#' @param expression      The expression to run.
#' @param mailSettings    Arguments to be passed to the send.mail function in the mailR package
#'                        (except subject and body).
#' @param label           A label to be used in the subject to identify a run.
#'
#' @return                The ouput of \code{expression}.
#'
#' @examples
#' \dontrun{
#' mailSettings <- list(from = "someone@gmail.com",
#'                      to = c("someone_else@gmail.com"),
#'                      smtp = list(host.name = "smtp.gmail.com", 
#'                                  port = 465, 
#'                                  user.name = "someone@gmail.com", 
#'                                  passwd = "super_secret!", 
#'                                  ssl = TRUE),
#'                      authenticate = TRUE,
#'                      send = TRUE)
#' runAndNotify({a <- 1 + 2 + 3}, mailSettings = mailSettings, label = "My fancy R code")                      
#' }
#' 
#' @export
runAndNotify <- function(expression, mailSettings, label = "R") {
  ev <- new.env()
  start <- Sys.time()
  result <- tryCatch({
    eval(expression)
  }, warning = function(w) {
    assign("warningMessage", w$message, envir = ev)
  }, error = function(e) {
    assign("errorMessage", e$message, envir = ev)
  }
  )
  delta <- Sys.time() - start
  timing <- paste("Code ran for", signif(delta, 3), attr(delta, "units"))
  subject <- NULL
  body <- NULL
  if (exists("warningMessage", envir = ev)) {
    subject <- paste0("[", label, "] ", "Warning")
    body <- paste(get("warningMessage", envir = ev), timing, sep = "\n\n")
    warning(get("warningMessage", envir = ev), call. = FALSE)
  } else if (exists("errorMessage", envir = ev)) {
    subject <- paste0("[", label, "] ", "Error")
    body <- paste(get("errorMessage", envir = ev), timing, sep = "\n\n")
    warning(get("errorMessage", envir = ev), call. = FALSE)
  } else {
    subject <- paste0("[", label, "] ", "Done")
    body <- timing
  }
  mailSettings$subject <- subject
  mailSettings$body <- body
  myfun <- get("send.mail", asNamespace("mailR"))
  do.call(myfun, mailSettings)
  writeLines(paste("Message sent to", mailSettings$to))
  invisible(result)
}


