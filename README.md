OhdsiRTools
===========

[![Build Status](https://travis-ci.org/OHDSI/OhdsiRTools.svg?branch=master)](https://travis-ci.org/OHDSI/OhdsiRTools)
[![codecov.io](https://codecov.io/github/OHDSI/OhdsiRTools/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/OhdsiRTools?branch=master)

Introduction
============
An R package with tools to help write and maintain other OHDSI R packages. Other OHDSI R packages should not depend on or import OhdsiRTools.

Features
========
- Auto code formatting.
- Auto checking of R code.
- Generate renv lock files with correct references to OHDSI repos.

Examples
========

```r
# Auto-format all R files in a package:
formatRFolder()

# Identify problems in R code in a package:
checkUsagePackage("OhdsiRTools")
```

Technology
============
OhdsiRTools is an R package.

System Requirements
============
Requires R.


Installation
=============
1. In R, use the following commands to download and install OhdsiRTools:

  ```r
  install.packages("remotes")
  library(remotes)
  install_github("ohdsi/OhdsiRTools")
  ```

User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/OhdsiRTools).

PDF versions of the documentation are also available:
* Package manual: [OhdsiRTools.pdf](https://raw.githubusercontent.com/OHDSI/OhdsiRTools/master/extras/OhdsiRTools.pdf)

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/OhdsiRTools/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/MethodsLibrary/contribute.html) how you can contribute to this package.

License
=======
OhdsiRTools is licensed under Apache License 2.0

Development
===========
OhdsiRTools is being developed in R Studio.

### Development status

Ready for use

# Acknowledgements
- This project is supported in part through the National Science Foundation grant IIS 1251151.
