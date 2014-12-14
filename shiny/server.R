require(shiny)
require(Matrix)

source("shiny_lib.R")

# option to think: load optional dictionaries? preload everything?

shinyServer(
  function (input, output) {

    words <- reactive({unlist(strsplit(input$phrase, " "))})
    wordLen <- reactive({length(words())})
    Pred <- reactive({if (wordLen()>0) predWord(input$phrase)})
    
    # You can access the value of the widget with input$text, e.g.
    output$len <- renderText({ wordLen() })
    output$phrase <- renderText({input$phrase})
    output$wordPred <- renderText({ Pred() })
  }  
)