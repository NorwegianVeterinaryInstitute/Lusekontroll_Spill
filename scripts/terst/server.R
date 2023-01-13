library(shiny)

shinyServer(function(input, output, session) {
  
  observe({
    if(input$tv == "All, but C") {
      updateCheckboxGroupInput(session, "check2", label = NULL, choices = NULL, 
                               selected = c("A", "B", "D", "E"))
    }
    if(input$tv == "Only C or D") {
      updateCheckboxGroupInput(session, "check2", label = NULL, choices = NULL, 
                               selected = c("C", "D"))
    }
    if(input$tv == "All") {
      updateCheckboxGroupInput(session, "check2", label = NULL, choices = NULL, 
                               selected = c("A", "B","C", "D", "E"))
    }
    
    
  })
  
  output$values <- renderPrint({
    list(radio = input$radio, checkbox = input$check)
  })
  output$values2 <- renderPrint({
    list(radio = input$tv, checkbox = input$check2)
  })
})