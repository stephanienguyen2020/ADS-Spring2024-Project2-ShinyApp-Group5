install.packages("shiny")
library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

###################### Load the Data #######################

FemaWebSum <- read_csv('./data/FemaWebDisasterSummaries.csv')
FemaWebDec <- read_csv('./data/FemaWebDisasterDeclarations.csv')

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

write_csv(ApprAmount, "./output/Approved_Amount.csv")


########################## Define UI #############################


ui <- fluidPage(
  
  tabsetPanel(
    id = "tabs",
    type = "pills",
    
  # Tab 2
  tabPanel("Approved Assistance", value = "Approved Assistance",
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
        )
  )
)


##################### Define Server ########################


server <- function(input, output) {
  
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
}



################## Run the Application ###################
shinyApp(ui = ui, server = server)

