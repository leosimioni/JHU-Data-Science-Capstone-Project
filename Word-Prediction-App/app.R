library(shiny)
library(tidyverse)
library(stringr)

source("prediction-model.R")

ui <- fluidPage(
  
  titlePanel("Word Prediction App"),
  p("This app makes a word prediction after an input phrase is written in the text box"),
  p("Author: Leonardo Simioni"),
  p(img(src = "https://raw.githubusercontent.com/leosimioni/JHU-Data-Science-Capstone-Project/main/Word-Prediction-App/data/headers.png", height=62, width=383)),
  
  sidebarLayout(
    sidebarPanel(
      h2("Instructions:"), 
      h5("1. Enter an input phrase in the text box."),
      h5("2. The app automatically predicts the next word below."),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("App",
                 textInput("user_input", h3("Text Box"), 
                  value = "Write here"),
                 h3("Word Prediction"),
                 h4(em(span(textOutput("ngram_output"), style="color:red")))),
        
        tabPanel("About",
                 br(),
                 div("Word Prediction App is a Shiny application that uses a text
                     prediction algorithm to predict the next word after an input
                     phrase is written by the user.",
                     br(),
                     br(),
                     "This app was developed as part of the capstone project for the
                     Data Science Specialization course of Johns Hopkins University.
                     The dataset used was provided in collaboration with SwiftKey.",
                     br(),
                     br(),
                     a("Source Code", href = "https://github.com/leosimioni/JHU-Data-Science-Capstone-Project")
                  )),
      )   
    )
  )
)

server <- function(input, output) {
  
  output$ngram_output <- renderText({
    ngrams(input$user_input)
  })
  
}

shinyApp(ui = ui, server = server)
