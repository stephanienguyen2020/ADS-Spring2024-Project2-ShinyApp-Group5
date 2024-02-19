#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Disaster Data Frequency Visualization"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput("column", "Select Column:", choices = list("incidentType", "state", "declarationType"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("freqPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # disaster data pulled from github
    # Read CSV file
    data <- reactive({
      df <- read.csv("~/Desktop/Columbia/Applied Data Science/Project 1 - Disaster data/DisasterDeclarationsSummaries.csv")
      return(df)
    })
    
    output$table <- renderTable(data())
    
    # Create the frequency plot
    output$freqPlot <- renderPlot({
      req(data())
      
      # Count frequency of values in selected column
      freq_table <- table(data()[[input$column]])
      
      # Convert frequency table to data frame
      freq_df <- as.data.frame(freq_table)
      names(freq_df) <- c(input$column, "Frequency")
      
      # Plot frequency
      ggplot(freq_df, aes_string(x = input$column, y = "Frequency")) +
        geom_bar(stat = "identity") +
        labs(title = paste("Frequency of", input$column), x = input$column, y = "Frequency") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
