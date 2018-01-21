# logFileName <- "c:/temp/logFile.txt"
eventLog <- read.table(logFileName, header = FALSE, sep = "\t")
colnames(eventLog) <- c("Timestamp", "Thread", "Level","Package", "Function", "Message")

levels <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR")
threads <- as.character(unique(eventLog$Thread))
threads <- threads[order(threads)]
threads <- c("All", threads)
packages <- as.character(unique(eventLog$Package))
packages <- c("All", packages)