#Coursera data project
#Akshaya Padhi
#ui.R
library(shiny)
library(shinythemes)

shinyUI(navbarPage(
	"Capstone Project - Next Word Prediction",
	
	theme = shinytheme("paper"),
	
	tabPanel("Akshaya Padhi",
					 tags$head(includeScript("./point_to_inputbox.js")),
					 
					 fluidRow(
					 	column(3),
					 	column(6,
					 				 tags$div(textInput("text", 
					 				 									 label = h4("Enter your text here:"),
					 				 									 value = ),
					 				 				 br(),
					 				 				 tags$hr(),
					 				 				 h4("Predicted next word:"),
					 				 				 tags$span(style="color:brown",
					 				 				 					tags$em(tags$h5(textOutput("nextWord")))),
					 				 				 br(),
					 				 				align="center")
					 	),
					 	column(3)
					 )
	)
)
)
