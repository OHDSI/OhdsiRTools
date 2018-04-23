
insertCohortDefinitionInPackage(definitionId = 5021, 
                                name = "Test", 
                                baseUrl = Sys.getenv("baseUrl"))



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


# Args functions --------------------------------------------------------------


createArgFunction(functionName = "getCohortDefinitionName", rCode = "")
