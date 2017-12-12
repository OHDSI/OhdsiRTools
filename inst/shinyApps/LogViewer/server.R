library(shiny)

shinyServer(function(input, output, session) {
  output$logTable <- renderTable({
    visibleLevels <- levels[which(levels == input$level):length(levels)]
    if (input$thread == "All") {
      return(eventLog[eventLog$Level %in% visibleLevels, ])
    } else {
      return(eventLog[eventLog$Level %in% visibleLevels & eventLog$Thread == input$thread, ])
    }
  })
})

