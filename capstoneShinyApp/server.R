#Coursera data project
#Akshaya Padhi
#server.R

library(shinythemes)
library(shiny)

source("./nextWord.R")
shinyServer(function(input, output) {
	
	wordPrediction <- reactive({
		text <- input$text
		nextWord <- predictNextWord(text)})
	
	output$nextWord <- renderPrint(wordPrediction())
	output$enteredWords <- renderText({ input$text }, quoted = FALSE)
})
