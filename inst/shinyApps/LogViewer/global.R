# logFileName <- "c:/temp/logFile.txt"
eventLog <- read.table(logFileName, header = FALSE, sep = "\t")
colnames(eventLog) <- c("Thread", "Level", "Timestamp", "Message")

levels <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR")
threads <- as.character(unique(eventLog$Thread))
threads <- threads[order(threads)]
threads <- c("All", threads)