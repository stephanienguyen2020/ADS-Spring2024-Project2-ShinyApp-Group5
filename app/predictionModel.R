#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fire Trend Predictions"),

    # Sidebar with selection for which region to predict
    sidebarLayout(
        sidebarPanel(
          selectInput("region", "Select Region: ", choices = list("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                                                                     "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                                                                     "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                                                                     "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
                                                                     "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
                                                                     "DC", "AS", "GU", "MP", "PR", "VI"),
                      selected = "CA"),
          
          # Prediction value based off curve
          # Render the output text
          htmlOutput("output_text"),
          
          ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data <- reactive({
    df <- read.csv("DisasterDeclarationsSummaries.csv")
    
    # filter for only region and only fire
    df <- df[df$state == input$region, ]
    df <- df[df$incidentType == "Fire", ]
    
    #drop unnecesary colums
    df <- df[, "incidentBeginDate", drop = FALSE]
    return(df)
  })

  output$distPlot <- renderPlot({
    # load in data
    req(data())
    
    #check if empty
    if (nrow(data()) == 0) {
      return(NULL)  # Return NULL to output a blank graph
    }
    
    # frequency of fire by month
    data_df <- data()
    # data_df$incidentBeginDate <- as.Date(data_df$incidentBeginDate)
    # freqMonth <- table(format(data_df$incidentBeginDate,"%b-%Y"))
    # print(freqMonth)
    # 
    # print(data())
    
    # Convert date column to datetime if needed
    data_df$incidentBeginDate <- as.Date(data_df$incidentBeginDate)
    
    # Extract month from date
    data_df$month <- format(data_df$incidentBeginDate, "%Y-%m")
    
    #print(data_df)
    
    # Count frequency of rows for each month
    freq_by_month <- data_df %>%
      group_by(month) %>%
      summarise(frequency = n())
    
    # Convert month to Date class
    #freq_by_month$month <- as.Date(freq_by_month$month)
    freq_by_month <- freq_by_month %>%
      mutate(month = paste0(month, "-01"),  # Append a day value
             month = as.Date(month))         # Convert to Date object
    
    # linear regression fit prediction
    # Fit a linear model
    lm_model <- lm(frequency ~ month, data = freq_by_month)
    
    # Predict frequencies using the linear model
    freq_by_month$predicted_frequency <- predict(lm_model, newdata = freq_by_month)
    
    #curve prediction
    # Fit a loess curve
    loess_model <- loess(frequency ~ as.numeric(month), data = freq_by_month)
    freq_by_month$predicted_frequency_loess <- predict(loess_model, newdata = freq_by_month)
    
    
    # # Plot time series
    # ggplot(freq_by_month, aes(x = month, y = frequency)) +
    #   geom_line(color = "orange") +
    #   geom_point(color = "red") +
    #   geom_line(aes(y = predicted_frequency), color = "lightblue") + 
    #   geom_line(aes(y = predicted_frequency_loess), color = "blue") +  # Fitted curve
    #   scale_color_manual(values = c("Linear" = "lightblue", "Loess" = "blue", "Actual" = "red")) +
    #   labs(x = "Month", y = "Frequency", title = "Monthly Fire Frequencies") +
    #   theme(legend.position = "bottom")
    
    # Plot time series
    ggplot(freq_by_month, aes(x = month, y = frequency)) +
      geom_line(aes(color = "Actual"), size = 1.2) +
      geom_point(color = "red") +
      geom_line(aes(y = predicted_frequency, color = "Linear Fit"), size = 1.2) + 
      geom_line(aes(y = predicted_frequency_loess, color = "Loess Fit"), size = 1.2) +
      labs(x = "Month", y = "Frequency", title = "Monthly Fire Frequencies") +
      scale_color_manual(values = c("Actual" = "orange", "Linear Fit" = "lightblue", "Loess Fit" = "blue")) +
      guides(color = guide_legend(title = "Lines", override.aes = list(size = 2))) 
      
  })
  
  # Render the output text
  output$output_text <- renderText({
    
    # load in data
    req(data())
    
    #check if empty
    if (nrow(data()) == 0) {
      return("No fire data available for this region")  
    }
    
    # data processing
    data_df <- data()
    
    # Convert date column to datetime if needed
    data_df$incidentBeginDate <- as.Date(data_df$incidentBeginDate)
    
    # Extract month from date
    data_df$month <- format(data_df$incidentBeginDate, "%Y-%m")
    
    #print(data_df)
    
    # Count frequency of rows for each month
    freq_by_month <- data_df %>%
      group_by(month) %>%
      summarise(frequency = n())
    
    # Convert month to Date class
    #freq_by_month$month <- as.Date(freq_by_month$month)
    freq_by_month <- freq_by_month %>%
      mutate(month = paste0(month, "-01"),  # Append a day value
             month = as.Date(month))         # Convert to Date object
    
    # Fit models
    lm_model <- lm(frequency ~ month, data = freq_by_month)
    loess_model <- loess(frequency ~ as.numeric(month), data = freq_by_month)
  
    lm_model_summary <- summary(lm_model)
    loess_model_summary <- summary(loess_model)
    
    prediction <- "\nPrediction: Fires may increase. You may want to upgrade insurance. "
    
    if(lm_model_summary$coefficients[, 1][2] < 0){
      prediction <- "\nPrediction: Fires may decrease You may want to downgrade insurance. "
    }
    
    fullStatement <- paste("Linear Model: <br>",
                           "<br> Intercept: ", lm_model_summary$coefficients[, 1][1],
                           " Slope: ", lm_model_summary$coefficients[, 1][2],
                           "<br>R-squared: ", lm_model_summary$r.squared,
                           "<br><br>", prediction)

    return(fullStatement)  # Display the dynamically updated text
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
