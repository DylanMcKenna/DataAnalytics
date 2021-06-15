# load libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)

# Read in Static Data
ohs2019 <- read.csv("OHS_2019.csv") 

# Clean Names
ohs2019 <- ohs2019 %>%
  clean_names()

# Select desired variables
ohs_useful <- ohs2019 %>%
  select(field_visit_date, field_visit_type, case_type, naics_description, contravener_role, order_type, order_status, reg_name )



ui <- fluidPage(
  titlePanel("Ontario Health & Safety - Site Visits - 2019"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "contravener_role",
        label = "Select Contravener Role",
        choices = c("Co-owner","Constructor", "Employer", "Employer Representative", "Licensee", "Other", "Owner", "Primary Employer", "Project Management", "Property Management", "Secondary Employer", "Supplier", "Union")), 
      dateRangeInput(
        inputId = "field_visit_date",
        label = "Select date range",
        start = min(ohs_useful$field_visit_date),
        end = max(ohs_useful$field_visit_date)
      )
  )))
  mainPanel(plotlyOutput(outputId = "sentiment_plot")
  )



# Update Server with the Plot
server <- function(input, output) {
  output$sentiment_plot <- renderPlotly({
    p <- covid_speeches_words %>%
      filter(origin == input$origin) %>%
      filter(between(date, input$date_range[1], input$date_range[2])) %>%
      anti_join(stop_words) %>%
      filter(word != "positive") %>%
      inner_join(get_sentiments("bing"), by = "word") %>%
      count(date, sentiment) %>%
      pivot_wider(names_from = sentiment, values_from = n) %>%
      mutate(sentiment = positive - negative) %>%
      ggplot(aes(x = date, y = sentiment)) +
      geom_line(color = "gray") +
      geom_point(aes(color = sentiment > 0), size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "lightgray") +
      labs(
        title = glue("Daily sentiment score, {input$origin} COVID-19 briefings"),
        x = "Date", y = "Sentiment score (positive - negative)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
}


shinyApp(ui, server)