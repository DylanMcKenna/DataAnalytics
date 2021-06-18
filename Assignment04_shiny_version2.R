
# load libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)
library(lubridate)

# Read in Static Data
ohs2019 <- read.csv("OHS_2019.csv") 

# Clean Names
ohs2019 <- ohs2019 %>%
  clean_names()


# Select desired variables
ohs_useful <- ohs2019 %>%
  select(field_visit_date, field_visit_type, case_type, naics_description, contravener_role, order_type, order_status, reg_name )
          
#str(ohs_useful)


# Update UI - origin
ui <- fluidPage(
  titlePanel("OHS - Site Visits for 2019"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "contravener_role",
        label = "Select Contravener Role",
        choices = c("Co-owner of", "Constructor", "Employer", "Employer Representative", "Licensee", "Other", "Owner", "Primary Employer", "Project Management", "Property Management", "Secondary Employer", "Supplier", "Union", "NA")
      )), dateRangeInput(
        inputId = "date_range",
        label = "Select date range",
        start = min(ohs_useful$field_visit_date),
        end = max(ohs_useful$field_visit_date)
      )
  ),
      mainPanel(plotlyOutput(outputId = "contravener_plot")
  ))


# Update Server with the Plot
server <- function(input, output) {
  output$contravener_plot <- renderPlotly({
    ohs_ggplot <- ohs_useful %>%
      filter(contravener_role == input$contravener_role) %>%
      filter(between(field_visit_date, input$field_visit_date[1], input$field_visit_date[2])) %>%
      count(contravener_role) %>%
      ggplot(aes(x = n, y = contravener_role)) +
      geom_col() +
      #labs(
      #  title = glue("Daily sentiment score, {input$contravener_role} COVID-19 briefings"),
      #  x = "Date", y = "Sentiment score (positive - negative)"
      #) +
      theme_minimal() +
      theme(legend.position = "none")
    
    #ggplotly(ohs_ggplot, tooltip = c("x", "y"))
  })
}




shinyApp(ui, server)

