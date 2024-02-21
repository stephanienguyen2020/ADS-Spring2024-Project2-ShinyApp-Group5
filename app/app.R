###############################Install Related Packages #######################
required_packages <- c("bslib", "shiny", "ggplot2", "readr", "dplyr", "tidyr",
                       "tidyverse", "plotly", "scales", "leaflet",
                       "leaflet.extras", "shinythemes")

# Loop through the list and install any packages that are not already installed
for(package in required_packages) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

###################### Load the Data #######################
df <- read.csv("DisasterDeclarationsSummaries.csv")
non_df <- read_csv("NonDisasterAssistanceFirefighterGrants.csv")

FemaWebSum <- read_csv('FemaWebDisasterSummaries.csv')
FemaWebDec <- read_csv('FemaWebDisasterDeclarations.csv')

merged_df <- inner_join(FemaWebDec %>% 
                          select(disasterNumber, stateCode, incidentType, declarationType),
                        FemaWebSum %>% 
                          select(-paLoadDate, -iaLoadDate, -hash, -lastRefresh, -id),
                        by = "disasterNumber")
ApprAmount <- merged_df %>%
  mutate(
    totalNumberIaApproved = as.numeric(totalNumberIaApproved),
    totalAmountIhpApproved = as.numeric(totalAmountIhpApproved),
    totalAmountHaApproved = as.numeric(totalAmountHaApproved),
    totalAmountOnaApproved = as.numeric(totalAmountOnaApproved)
  )

write_csv(ApprAmount, "Approved_Amount.csv")

#===============================================Shiny UI=========================================================
ui <- fluidPage(
  tags$head(
    tags$h1("Geospatial Insights and Forecasting for Fire Incident Preparedness", style = "text-align: center; margin-top: 20px;")
  ),
  navbarPage(title = "",
    theme = shinytheme("flatly"),
    #################### Tab 1: Introduction Page ####################
    
    tabPanel("Introduction",
             # Background image with reduced opacity
             tags$div(style = "position: relative; text-align: center; color: white;",
                      tags$img(src = "https://firesystems.net/wp-content/uploads/2023/07/AdobeStock_603209092-scaled.jpeg", style = "opacity: 0.90; width: 100%;"),
                      # White box with text, centered over the image
                      tags$div(style = "position: absolute; top: 40%; left: 25%; right: 25%; background-color: white; padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);",
                               tags$h3("Geospatial Insights and Forecasting for Fire Incident Preparedness", style = "color: black;"),
                               tags$p("The increasing occurrence of fire incidents, driven by factors such as climate change and human encroachment into vulnerable areas, presents a significant challenge for effective management and mitigation efforts. Current approaches to understanding and preparing for these incidents often rely on static, historical data, lacking the capacity for real-time analysis and spatial awareness.", style = "text-align: justify; color: black;"),
                               tags$p("The absence of accessible, user-friendly tools that offer geospatial visualization of fire risk areas based on historical data limits proactive planning and community engagement in fire preparedness strategies. There is a clear need for a straightforward, data-driven solution that leverages existing datasets to provide intuitive, geospatial visualizations of fire incident risks, enhancing the overall preparedness and awareness of potential fire threats among various stakeholders.", style = "text-align: justify; color: black;")
                      )
             )
    ),
    #################### Tab 2: Disaster Incident Archive (freqPlot) ####################
    tabPanel("Disaster Incident Archive",
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
  
),
    #################### Tab 3: Fire Map Visualization (FM_Map) ####################
    tabPanel("Geographical Overview"),
    #################### Tab 4: Chronological Overview ####################
    tabPanel("Chronological Overview", fluidPage(
      titlePanel("Chronological Overview"),
      sidebarLayout(
        sidebarPanel(
          selectInput("plotType", "Choose a plot:",
                      choices = c("Number of Fire Incidents by State" = "fireIncidents",
                                  "Yearly Overview of Fire Disaster Incidences" = "yearlyOverview")),
        ),
        mainPanel(
          plotlyOutput("DIA_Plot") # Output area for the plot
        )
      )
    )
    ),

    #################### Tab 5: Government Assistance ####################
    tabPanel("Government Assistance", value = "Government Assistance",
             sidebarLayout(
               sidebarPanel(
                 h1("Exploring the Actual Approved Assistance Amount"),
                 p("FEMA provides several progems to assists individuals and households on disasters: Individual Assistance(IA), Individual and Households Program(IHP), Public Assistance(PA), and Hazard Mitigation Grant Program(HMGP). On this page, you can look into the number of disaster assistance applications that were approved for IA program, and the total amount approved dollars for the IHP, PA, and HMGP under each state."),
                 selectInput(
                   inputId = "State", 
                   label = "Select State:",
                   choices = unique(ApprAmount$stateCode),
                   selected = 'CA',
                   multiple = FALSE),
                 selectInput(
                   inputId = "IncidentType", 
                   label = "Select Incident Type:",
                   choices = unique(ApprAmount$incidentType),
                   selected = 'Fire'),
                 checkboxGroupInput("DeclarationType", "Declaration Type:",
                                    choices = unique(ApprAmount$declarationType)),
                 selected = c("Major Disaster", "Emergency", "Fire Management", "Fire Suppression")
               ),
               mainPanel(
                 plotOutput("ApprovedAmountPlot")
               )
             )
             ),
    #################### Tab 6: Fire Map Visualization (FM_Map) ####################
    
    ########## Tab 7: Fire Trend Predictions (FT_Plot) ####################
    tabPanel("Fire Trend Predictions", value = "Fire Trend Predictions",
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("regionPrediction", "Select Region: ", choices = list("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
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
                 plotOutput("distPlotPred")
               )
             )
             
             ),
    #################### Tab 8: Conclusions ####################
    tabPanel("Conclusions"),
    #################### Tab 9: Reference ####################
    tabPanel("Reference")
  )
)

server <- function(input, output) {
  
  #################### Disaster Incident Archive (freqPlot) ####################
  output$DIA_Plot <- renderPlotly({
    
    if (input$plotType == "fireIncidents") {
      df_fire <- df %>%
        filter(incidentType == "Fire") %>%
        group_by(state) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count))
      
      # Create a ggplot object with a custom tooltip
      p <- ggplot(df_fire, aes(x=reorder(state, -Count), y=Count, text = paste(Count))) +
        geom_bar(stat="identity", fill="tomato") +
        theme_minimal() +
        labs(title="Number of Fire Incidents by State", x="State", y="Number of Incidents") +
        theme(axis.text.x = element_text(angle=45, hjust=1))
      
      # Use ggplotly and specify to use the 'text' aesthetic for the tooltip
      ggplotly(p, tooltip = "text")
      
    } else if (input$plotType == "yearlyOverview") {
      fire_disasters_by_year_updated <- df %>%
        filter(incidentType == "Fire") %>%
        group_by(fyDeclared) %>%
        summarise(TotalFireDisasterNumber = n_distinct(disasterNumber))
      
      ggplot(fire_disasters_by_year_updated, aes(x=fyDeclared, y=TotalFireDisasterNumber)) +
        geom_line() +
        geom_point() +
        theme_minimal() +
        labs(title="Yearly Overview of Fire Disaster Incidences", x="Fiscal Year", y="Total Fire Disaster Number") +
        theme(axis.text.x = element_text(angle=45, hjust=1))
    }
  })
  
  #################### FM_Map ####################
  
  
  
  #################### Approved Amount Plot ####################
  output$ApprovedAmountPlot <- renderPlot({
    filtered_data <- ApprAmount %>%
      filter(stateCode == input$State,
             incidentType == input$IncidentType,
             declarationType %in% input$DeclarationType)
    
    plot_data <- data.frame(
      Program = c("IA", "IHP", "PA", "HMGP"),
      Amount = c(sum(filtered_data$totalNumberIaApproved, na.rm = TRUE),
                 sum(filtered_data$totalAmountHaApproved, na.rm = TRUE),
                 sum(filtered_data$totalObligatedAmountPa, na.rm = TRUE),
                 sum(filtered_data$totalObligatedAmountHmgp, na.rm = TRUE))
    )
    
    ggplot(plot_data, aes(x = Program, y = Amount, fill = Program)) +
      geom_bar(stat = "identity") +
      labs(title = "Approved Assistance Amount from FEMA Assisstance Programs",
           x = "Programs",
           y = "Approved Amount (in different units)") +
      theme_classic() +
      theme(legend.position = 'right',
            legend.title = element_blank(),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14, face = 'bold'),
            plot.title = element_text(size = 16, face = 'bold', hjust = 0.5)) +
      scale_fill_brewer(palette = "Set3")
  })
  
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
  
  #################### DIA ####################
  data <- reactive({
    df <- read.csv("DisasterDeclarationsSummaries.csv")
    return(df)
  })
  
  filtered_data <- reactive({
    df <- read.csv("DisasterDeclarationsSummaries.csv")
    df <- df[df$state == input$region, ]
    return(df)
  })
  
  filtered_data_pie <- reactive({
    df <- read.csv("DisasterDeclarationsSummaries.csv")
    df <- df[df$state == input$pieRegion, ]
    return(df)
  })
  
  predictionData <- reactive({
    df <- read.csv("DisasterDeclarationsSummaries.csv")
    #print(df)
    
    # filter for only region and only fire
    df <- df[df$state == input$regionPrediction, ]
    df <- df[df$incidentType == "Fire", ]
    
    #drop unnecesary colums
    df <- df[, "incidentBeginDate", drop = FALSE]
    
    
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
  
  ### FT_Plot
  output$distPlotPred <- renderPlot({
    # load in data
    req(predictionData())
    
    #check if empty
    if (nrow(predictionData()) == 0) {
      return(NULL)  # Return NULL to output a blank graph
    }
    
    # frequency of fire by month
    data_df <- predictionData()
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
    req(predictionData())
    
    #check if empty
    if (nrow(predictionData()) == 0) {
      return("No fire data available for this region")  
    }
    
    # data processing
    data_df <- predictionData()
    
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
                           #"<br>R-squared: ", lm_model_summary$r.squared,
                           "<br><br>", prediction)
    
    return(fullStatement)  # Display the dynamically updated text
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
