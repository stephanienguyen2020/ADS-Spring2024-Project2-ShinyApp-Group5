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
library(bslib)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Disaster Data Frequency Visualization"),

    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     sidebarPanel(
    #       
    #         
    #         
    #     ),

    navset_card_underline(
      
      # Show a plot of the generated distribution for all
      nav_panel("National Frequencies" , 
                sidebarLayout(
                  
                  sidebarPanel(
                    selectInput("columnNational", "Select Data:", choices = list("incidentType", "state", "declarationType")),
                  ),
                  
                  mainPanel(
                    plotOutput("freqPlot"))
                  ),
                ),
      
      # selected state/region frequency
      nav_panel("Selected Regional Frequency", 
                
                sidebarLayout(
                  
                  sidebarPanel(
                    selectInput("region", "Select Region: ", choices = list("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                                                                            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                                                                            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                                                                            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
                                                                            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
                                                                            "DC", "AS", "GU", "MP", "PR", "VI")),
                    selectInput("columnRegional", "Select Data:", choices = list("incidentType", "declarationType")),
                    
                  ),
                  
                  mainPanel(
                    plotOutput("regionPlot"))
                  ),
                ),
      # pie chart comparisons
      nav_panel("Pie Chart Comparisons", 
                
                sidebarLayout(
                  
                  sidebarPanel(
                    selectInput("pieRegion", "Select Region: ", choices = list("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                                                                            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                                                                            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                                                                            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
                                                                            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
                                                                            "DC", "AS", "GU", "MP", "PR", "VI")),
                    selectInput("pieColumn", "Select Data:", choices = list("incidentType", "declarationType")),
                    
                  ),
                  
                  mainPanel(
                    plotOutput("pieNational", width = "600px", height = "600px"),
                    plotOutput("pieRegional", width = "600px", height = "600px"))
                ),
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
    
    filtered_data <- reactive({
      df <- read.csv("~/Desktop/Columbia/Applied Data Science/Project 1 - Disaster data/DisasterDeclarationsSummaries.csv")
      df <- df[df$state == input$region, ]
      return(df)
    })
    
    filtered_data_pie <- reactive({
      df <- read.csv("~/Desktop/Columbia/Applied Data Science/Project 1 - Disaster data/DisasterDeclarationsSummaries.csv")
      df <- df[df$state == input$pieRegion, ]
      return(df)
    })
    
    # Create the frequency plot
    output$freqPlot <- renderPlot({
      req(data())
      
      # Count frequency of values in selected column
      freq_table <- table(data()[[input$columnNational]])
      
      # Convert frequency table to data frame
      freq_df <- as.data.frame(freq_table)
      
      category <- as.character(input$columnNational)
      names(freq_df) <- c(category, "Frequency")
      # frew_df$input$columnNationa<- reorder(freq_df$input$columnNationa, -freq_df[,"Frequency"])
      #freq_df <- freq_df[order(as.numeric(freq_df$Frequency),decreasing = TRUE), ]
      freq_df <- freq_df[order(freq_df$Frequency),]
      #print(freq_df)
      
      # Plot frequency
      # ggplot(freq_df, aes_string(x = input$columnNational, y = "Frequency")) +
      #   geom_bar(stat = "identity") +
      #   #labs(title = paste("Frequency of", input$columnNational), x = input$columnNational, y = "Frequency") +
      #   theme_minimal() +
      #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
      colors <- colorRampPalette(c("darkblue", "lightblue"))(nrow(freq_df))
      
      bp <- barplot(freq_df$Frequency,#names.arg = freq_df[[category]],
              col = colors, xlab = category, ylab = "Frequency")
      text(x = bp, y = 0, labels = freq_df[[category]], srt = 45, adj = c(1, 1), xpd = TRUE)
     
    })
    
    # Create the frequency plot for specified region
    output$regionPlot <- renderPlot({
      req(filtered_data())
      
      # Count frequency of values in selected column
      freq_table <- table(filtered_data()[[input$columnRegional]])
      
      # Convert frequency table to data frame
      freq_df <- as.data.frame(freq_table)

      category <-as.character(input$columnRegional)
      
      names(freq_df) <- c(category, "Frequency")
      freq_df <- freq_df[order(freq_df$Frequency),]
      
      colors <- colorRampPalette(c("darkblue", "lightblue"))
      
      # Plot frequency
      ggplot(freq_df, aes_string(x = reorder(freq_df[[category]], freq_df$Frequency), y = "Frequency")) +
        geom_bar(stat = "identity", fill = colors(nrow(freq_df))) +
        labs(title = paste("Frequency of Regional", input$columnRegional), x = input$columnRegional, y = "Frequency") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    })
    
    
    output$pieNational <- renderPlot({
      req(data())
      
      # Count frequency of values in selected column
      freq_table <- table(data()[[input$pieColumn]])
      
      # Convert frequency table to data frame
      freq_df <- as.data.frame(freq_table)
      
      category <- as.character(input$pieColumn)
      names(freq_df) <- c(category, "Frequency")
      print(freq_df)
      freq_df <- freq_df[order(freq_df$Frequency),]
      
      # Convert "frequency" column to numeric
      freq_df$Frequency <- as.numeric(as.character(freq_df$Frequency))
      
      # Filter out rows with NA values or values less than 0 in "frequency" column
      freq_df <- freq_df[complete.cases(freq_df) & freq_df$Frequency >= 0, , drop = FALSE]
      
      print(freq_df)
      
      colors <- colorRampPalette(c("yellow", "lightblue"))(nrow(freq_df))
      pie(freq_df$Frequency, labels = freq_df[[category]], col = colors)
      
    })
    
    output$pieRegional <- renderPlot({
      req(filtered_data_pie())
      
      # Count frequency of values in selected column
      freq_table <- table(data()[[input$pieColumn]])
      
      # Convert frequency table to data frame
      freq_df <- as.data.frame(freq_table)
      
      category <- as.character(input$pieColumn)
      names(freq_df) <- c(category, "Frequency")
      print(freq_df)
      freq_df <- freq_df[order(freq_df$Frequency),]
      
      # Convert "frequency" column to numeric
      freq_df$Frequency <- as.numeric(as.character(freq_df$Frequency))
      
      # Filter out rows with NA values or values less than 0 in "frequency" column
      freq_df <- freq_df[complete.cases(freq_df) & freq_df$Frequency >= 0, , drop = FALSE]
      
      print(freq_df)
      
      colors <- colorRampPalette(c("yellow", "lightblue"))(nrow(freq_df))
      pie(freq_df$Frequency, labels = freq_df[[category]], col = colors)
      
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
