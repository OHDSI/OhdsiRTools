insertCohortDefinitionInPackage(definitionId = 3876, name = "Alendronate", generateStats = TRUE)


insertCohortDefinitionInPackage(5021, "Test", "https://epi.jnj.com:8443/WebAPI")

url <- "https://epi.jnj.com:8443/WebAPI/cohortdefinition/5021"
x <- httr::GET(url)
definitionId <- 5021
baseUrl <- "https://epi.jnj.com:8443/WebAPI"

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


getCohortDefinitionName(baseUrl = "https://epi.jnj.com:8443/WebAPI", definitionId = 5021)

getConceptSetName(baseUrl = "https://epi.jnj.com:8443/WebAPI", setId = 12)

getPriorityVocabKey(baseUrl = "https://epi.jnj.com:8443/WebAPI")

getConceptSetConceptIds(baseUrl = "https://epi.jnj.com:8443/WebAPI", setId = 12)


# Logging -----------------------------------------------------------------
library(OhdsiRTools)
unlink("c:/temp/logFile.txt")
futile.logger::flog.appender(futile.logger::appender.tee("c:/temp/logFile.txt"))
futile.logger::flog.layout(layoutParallel)
futile.logger::flog.info("Hello world")
cluster <- makeCluster(3)
fun <- function(x) {
  futile.logger::flog.info("Value of x is %s", x)
  if (x == 6)
    x <- a
  return(NULL)
}
clusterApply(cluster, 1:10, fun)

stopCluster(cluster)

launchLogViewer("c:/temp/logFile.txt")
