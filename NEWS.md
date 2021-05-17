OhdsiRTools v1.10.0
==================

Changes:

1.Adding `includeRootPackage` and `additionalRequiredPackages` arguments to `createRenvLockFile()` function


OhdsiRTools v1.9.2
==================

Changes:

1. Better detection of R base packages when creating renv lock files.


OhdsiRTools v1.9.1
==================

Changes:

1. Dropped `runAndNotify()` function.

Bugfixes:

1. `takeEnvironmentSnapshot()` and `createRenvLockFile()` now include dependencies in 'LinkingTo' section.

2. `splines` now also recognized as R core package in `createRenvLockFile()`.



OhdsiRTools v1.9.0
==================

Changes:

1. Added findNonAsciiStringsInFolder, a function for finding non-ASCII strings in a folder.

2. Added createRenvLockFile, a function for creating an renv lock file for OHDSI study packages.

3. Added fixHadesLogo function, needed when generating the documentation websites for HADES packages.


OhdsiRTools v1.8.0
==================

Changes: 

1. Deprecating WebAPI functions, which have been moved to ROhdsiWebApi

2. Deleting deprecated logging and parallel computation functions, which have been moved to ParallelLogger.


OhdsiRTools v1.7.0
==================

Changes: 

1. Restoring R environment now fetches OHDSI packages from drat repository. 

2. Restoring R environment has option (default) to be less strict, only installing packages if installed version is older than required, or has different major version number.

2. Added function for storing R environments from packages (including on GitHub)


OhdsiRTools v1.6.0
==================

Changes: Deprecating logging and parallel computation functions, which have been moved to ParallelLogger.


OhdsiRTools v1.5.5
==================

Changes: initial submission to CRAN.
