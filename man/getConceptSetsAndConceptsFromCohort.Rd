% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WebApiTools.R
\name{getConceptSetsAndConceptsFromCohort}
\alias{getConceptSetsAndConceptsFromCohort}
\title{Get a list of concept sets and concepts from a cohort definition}
\usage{
getConceptSetsAndConceptsFromCohort(
  baseUrl,
  definitionId,
  vocabSourceKey = NULL
)
}
\arguments{
\item{baseUrl}{The base URL for the WebApi instance, for example:
"http://server.org:80/WebAPI".}

\item{definitionId}{The cohort id to fetch concept sets and concepts from}

\item{vocabSourceKey}{A mysterious parameter.}
}
\value{
A list of concept sets, set names, and concepts
}
\description{
Get a list of concept sets and concepts from a cohort definition
}
\details{
For a given cohort definition id, get all concept sets and resolve all concepts from each
}
\examples{
\dontrun{
# This will obtain a list of concept sets and concepts from a cohort id:

getConceptsFromCohortId(baseUrl = "http://server.org:80/WebAPI", definitionId = 123)
}

}
