require(shiny)

source("shiny_lib.R")
load("~/GitHub/jh_capstone/training/070/probidxC_700.RData")

# DONE: load default dictionaries and probability tables
# option to think: load optional dictionaries? preload everything?

# TODO: function to calculate words (i.e. vocab id) probability

shinyServer(
  function (input, output) {
    #words <- reactive({unlist(strsplit(input$phrase), " ")})
    #wordPred <- reactive({length(words)
    #})
    words <- reactive({unlist(strsplit(input$phrase, " "))})
    #wordPred <- length(words)

    
    # You can access the value of the widget with input$text, e.g.
    output$value <- renderText({ length(words()) })
    output$wordPred <- renderText({ vocab[listNext(decompose(words()))]})
  }  
)