insertCohortDefinitionInPackage(definitionId = 3876, name = "Alendronate", generateStats = TRUE)




insertCohortDefinitionSetInPackage(fileName = "cohorts.csv",
                                   insertTableSql = TRUE,
                                   insertCohortCreationR = TRUE,
                                   generateStats = FALSE,
                                   packageName = "OhdsiRTools")




# Test mail notifications -------------------------------------------------
mailSettings <- list(from = Sys.getenv("mailAddress"),
                     to = c(Sys.getenv("mailAddress")),
                     smtp = list(host.name = "smtp.gmail.com",
                                 port = 465,
                                 user.name = Sys.getenv("mailAddress"),
                                 passwd = Sys.getenv("mailPassword"),
                                 ssl = TRUE),
                     authenticate = TRUE,
                     send = TRUE)

runAndNotify({
  a <- b
}, mailSettings = mailSettings, label = "Fancy code")



# WebAPI functions -----------------------------------------------------------


getCohortDefinitionName(baseUrl = "http://api.ohdsi.org:80/WebAPI", definitionId = 3289)

getConceptSetName(baseUrl = "http://api.ohdsi.org:80/WebAPI", setId = 525)

getPriorityVocabKey(baseUrl = "http://api.ohdsi.org:80/WebAPI")

getConceptSetConcepts(baseUrl = "http://api.ohdsi.org:80/WebAPI", setId = 525)
