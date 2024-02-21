library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(leaflet)

#################### Data Cleaning #######################

disasterDecSum <- read_csv('./data/DisasterDeclarationsSummaries.csv')

# Function to categorize declarationTitle into groups
categorize_fire_type <- function(title) {
  if (str_detect(title, "\\d")) {
    return(NA)
  }
  title_lower <- tolower(title)
  if (str_detect(title_lower, "creek|river")) {
    return('Creek/River Fire')
  } else if (str_detect(title_lower, "explosion")) {
    return('Explosion')
  } else if (str_detect(title_lower, "forest|mountain")) {
    return('Forest/Mountain Fire')
  } else if (str_detect(title_lower, "road")) {
    return('Road Fire')
  } else if (str_detect(title_lower, "fl-s")) {
    return('FL-S Fire')
  } else if (str_detect(title_lower, "canyon")) {
    return('Canyon Fire')
  } else if (str_detect(title_lower, "and|with|&")) {
    return('Fire came with other incidents')
  } else {
    return(NA)
  }
}

# Apply the function to create a new column for the fire type
fire_data <- disasterDecSum %>%
  filter(incidentType == "Fire") %>%
  mutate(FireType = sapply(declarationTitle, categorize_fire_type))

# Count the number of incidents in each FireType group, including NA
fire_type_counts <- fire_data %>%
  group_by(FireType) %>%
  summarise(Count = n(), .groups = 'drop')

################ Define UI #####################
ui <- fluidPage(
  
  titlePanel("Fire Incident Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("stateInput", "Select State:", 
                  choices = unique(fire_data$state)),
      selectInput("yearInput", "Select Fiscal Year:", 
                  choices = unique(fire_data$fyDeclared)),
      actionButton("goButton", "Go")
      ),
      
    mainPanel(
      plotOutput("firePlot")
    )
  )
)

################# Define server logic ###############
server <- function(input, output) {
  
  observeEvent(input$goButton, {
    # Filter the data based on the selected state and fiscal year, and exclude null FireType
    filteredData <- reactive({
      fire_data %>%
        filter(state == input$stateInput, fyDeclared == input$yearInput, !is.na(FireType)) %>%
        mutate(cleanedCounty = gsub("\\s*\\(.*?\\)\\s*", "", designatedArea)) %>% # Remove text in parentheses
        group_by(cleanedCounty, FireType) %>%
        summarise(Count = n(), .groups = 'drop')
    })
    
    # Generate the plot
    output$firePlot <- renderPlot({
      data <- filteredData()
      ggplot(data, aes(x = reorder(cleanedCounty, Count), y = Count, color = FireType)) +
        geom_point() +
        theme_minimal() +
        labs(x = "County", y = "Number of Fire Types", title = "Fire Types by County in Selected State and Fiscal Year") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
