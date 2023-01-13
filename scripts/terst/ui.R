library(shiny)

shinyUI(
  fluidPage(
    
    tagList(
      tags$head(
        tags$script(type="text/javascript", src = "disable.js")
      )
    ),
    
    sidebarLayout(
      sidebarPanel(
        radioButtons("radio",
                     h5("Uncheck all:"),
                     c("All", "Custom"),
                     inline = T
        ),
        checkboxGroupInput("check",
                           "",
                           c(1,2,3,4,5),
                           inline = T
        ),
        
        radioButtons("tv",
                     h5("Uncheck custom:"),
                     c("All", "All, but C" ,"Only C or D"),
                     inline = T
        ),
        checkboxGroupInput("check2",
                           "",
                           c("A","B","C","D","E"),
                           c("A","B","C","D","E"),
                           inline = T
        )
        
      ),
      mainPanel(
        h5("Uncheck all"),
        verbatimTextOutput('values'),
        h5("Uncheck custom:"),
        verbatimTextOutput('values2')
      )
    )
  )
)