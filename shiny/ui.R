shinyUI(fluidPage(
  #Application title
  titlePanel("Word predictor"),
  sidebarLayout(position="right",
    sidebarPanel(
      p("Place holder for sliders"),
      br(),
      h4("Choose speed / accuracy of prediction:")
      #TODO: sliders
      
    ),
    mainPanel(
      fluidRow(
        textInput("phrase", label = h3("Text input"), value = "Enter text..."),
        submitButton('Submit'),
        hr()
      ),
      #verbatimTextOutput("value"),

      fluidRow(
        verbatimTextOutput("value"),
        p("Predicted next word:"),
        br(),
        h3(textOutput("wordPred"))
      )
    )
  )
))