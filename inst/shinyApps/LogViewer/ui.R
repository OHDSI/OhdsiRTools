library(shiny)

shinyUI(
  fluidPage(
    titlePanel(paste("Log File Viewer -", logFileName)),
    fluidRow(
      column(2,
             selectInput("level", label = "Level", choices = levels, selected = "INFO"),
             selectInput("thread", label = "Thread", choices = threads)),
      column(10,
             tableOutput("logTable"))
    )
  )
)
