library(shiny)

shinyServer(function(input, output, session) {
  output$logTable <- renderTable({
    visibleLevels <- levels[which(levels == input$level):length(levels)]
    idx <- eventLog$Level %in% visibleLevels
    if (input$thread != "All") {
      idx <- idx & eventLog$Thread == input$thread
    }
    if (input$package != "All") {
      idx <- idx & eventLog$Package == input$package
    }
    return(eventLog[idx, ])
  })
})

