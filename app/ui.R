#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    tags$head(includeCSS("styles.css")),

    # Application title
    fluidRow(
        column(12, align = "center",
               titlePanel("Predict Next Phrase Using N-Gram Model")
    )),
    
    fluidRow(
        column(12, align = "center",
               h6("by Szymon Lipiński")
        )
    ),
    
    # Main input
    
    fluidRow(
        column(12, align = "center",
               h4("Enter A Phrase", width = "100%")
        )
    ),
    
    fluidRow(
        column(12, align = "center",
               textInput("phrase", "", width = "100%")
        )
    ),
    
    fluidRow(
        column(12, align = "center",
               h4("You can also click the buttons to insert text.")
    )),
    
    fluidRow(
        column(12, align = "center",
               actionButton("button1", "", width = "20%"),
               actionButton("button2", "", width = "20%"),
               actionButton("button3", "", width = "20%"),
               actionButton("button4", "", width = "20%"),
        )
    ),
    
    fluidRow(    
        column(12, align = "center",
               actionButton("button5", "", width = "20%"),
               actionButton("button6", "", width = "20%"),
               actionButton("button7", "", width = "20%"),
               actionButton("button8", "", width = "20%")
        )
    )

))
