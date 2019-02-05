
insertCohortDefinitionInPackage(definitionId = 5021, 
                                name = "Test", 
                                baseUrl = Sys.getenv("baseUrl"))






# WebAPI functions -----------------------------------------------------------

getCohortDefinitionName(baseUrl = Sys.getenv("baseUrl"), definitionId = 5021)

getConceptSetName(baseUrl = Sys.getenv("baseUrl"), setId = 12)

getPriorityVocabKey(baseUrl = Sys.getenv("baseUrl"))

getConceptSetConceptIds(baseUrl = Sys.getenv("baseUrl"), setId = 12)


# R environment snapshot ------------------------------------------------------
# snapshot <- read.csv("c:/temp/rEnvironmentSnapshot.csv")
# restoreEnvironment(snapshot)

restoreEnvironmentFromPackageOnGithub("OHDSI/Legend")


