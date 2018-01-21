# This may become a vignette later on

library(OhdsiRTools)

# By default there is one logger with threshold = "INFO" that writes to the console:

logInfo("Hello world")

# We can replace this with a fancier logger, for example with threshold = "DEBUG":

clearLoggers()
registerLogger(createLogger(threshold = "DEBUG",
                            appenders = list(createConsoleAppender(layout = layoutTimestamp))))
logDebug("Hello world")

# We can add a second logger that logs to a file:

unlink("c:/temp/logFile.txt")
registerLogger(createLogger(threshold = "TRACE", 
                            appenders = list(createFileAppender(layout = layoutParallel, 
                                                                fileName = "c:/temp/logFile.txt"))))
logInfo("Hello world")

# Messages from separate threads will also be logged to the same file:
cluster <- makeCluster(3)
fun <- function(x) {
  OhdsiRTools::logInfo("Value of x is ", x)
  if (x == 6)
    x <- a
  return(NULL)
}
clusterApply(cluster, 1:10, fun)

stopCluster(cluster)

# A convenient way to view the log file is with the log viewer:
launchLogViewer("c:/temp/logFile.txt")




