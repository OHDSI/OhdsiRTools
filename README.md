OhdsiRTools
===========

Introduction
============
An R package with tools to be used in the other OHDSI R packages

Features
========
- Auto code formatting
- Auto checking of R code
- Functions for parallel computation
- Functions for logging
- Functions used for automating analyses
- Functions for retrieving cohort definition and concept set metadata, status, and composition (SQL/JSON/CSV) from [ATLAS](https://github.com/OHDSI/Atlas)

Examples
===========

```r
# Auto-format all R files in a package:
formatRFolder()

# Identify problems in R code in a package:
checkUsagePackage("OhdsiRTools")

# Insert CIRCE JSON and SQL into a study package:
insertCohortDefinitionInPackage(123, "MyocardialInfarction")

# Insert concept set concept Ids into a study package:
insertConceptSetConceptIdsInPackage(baseUrl = "http://api.ohdsi.org:80/WebAPI", fileName = "conceptsetids.csv")

# Get a formatted cohort definition name (no bracketed prefixes) from Atlas:
getCohortDefinitionName(baseUrl = "http://api.ohdsi.org:80/WebAPI", definitionId = 123, formatName = TRUE)

# Get a formatted concept set name (no bracketed prefixes) from Atlas:
getConceptSetName(baseUrl = "http://api.ohdsi.org:80/WebAPI", setId = 123, formatName = TRUE)

# Get all concept Ids from a concept set from Atlas:
getConceptSetConceptIds(baseUrl = "http://api.ohdsi.org:80/WebAPI", setId = 123)

# Get a data frame filled with generation statuses of multiple cohort definitions across multiple CDM sources in Atlas:
getCohortGenerationStatuses(baseUrl = "http://api.ohdsi.org:80/WebAPI", definitionIds = c(1234), sourceKeys = c("blah"))
```

Technology
============
OhdsiRTools is an R package.

System Requirements
============
Requires R (version 3.1.0 or higher)

Dependencies
============
None

Getting Started
===============
1. In R, use the following commands to download and install OhdsiRTools:

  ```r
  install.packages("devtools")
  library(devtools)
  install_github("ohdsi/OhdsiRTools")
  ```

Getting Involved
=============
* Vignette: [Logging using OhdsiRTools](https://raw.githubusercontent.com/OHDSI/OhdsiRTools/master/inst/doc/Logging.pdf)
* Package manual: [OhdsiRTools.pdf](https://raw.githubusercontent.com/OHDSI/OhdsiRTools/master/extras/OhdsiRTools.pdf)
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="../../issues">GitHub issue tracker</a> for all bugs/issues/enhancements

License
=======
OhdsiRTools is licensed under Apache License 2.0

Development
===========
OhdsiRTools is being developed in R Studio.

### Development status
[![Build Status](https://travis-ci.org/OHDSI/OhdsiRTools.svg?branch=master)](https://travis-ci.org/OHDSI/OhdsiRTools)
[![codecov.io](https://codecov.io/github/OHDSI/OhdsiRTools/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/OhdsiRTools?branch=master)

Ready for use

# Acknowledgements
- This project is supported in part through the National Science Foundation grant IIS 1251151.
