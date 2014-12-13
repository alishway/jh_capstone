require(shiny)

# TODO: load default dictionaries and probability tables
# option to think: load optional dictionaries? preload everything?

# DONE: function to convert phrases into term indices for uni, bi, trigram

# TODO: function to generate potential (i.e. non-zero) post-term, in terms of vocab id

# TODO: function to calculate words (i.e. vocab id) probability

shinyServer(
  function (input, output) {
    #words <- reactive({unlist(strsplit(input$phrase), " ")})
    #wordPred <- reactive({length(words)
    #})
    words <- reactive({unlist(strsplit(input$phrase, " "))})
    #wordPred <- length(words)
    #cat(wordPred)
    
    # You can access the value of the widget with input$text, e.g.
    output$value <- renderText({ input$phrase })
    output$wordPred <- renderText({ length(words())})
  }  
)