# load libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)
library(janitor)


# Read in Static Data
ohs2019 <- read.csv("OHS_2019.csv") 

# Clean Names
ohs2019 <- ohs2019 %>%
  clean_names()

# Select desired variables
ohs_useful <- ohs2019 %>%
  select(field_visit_date, field_visit_type, case_type, naics_description, contravener_role, order_type, order_status, reg_name )

header <- dashboardHeader(title = "OHS - Site Visits",
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Site Inspector",
                                         message = "How many inspections occurred in February?",
                                         icon = icon("question-circle")
                                       )),
                          dropdownMenu(type = "notifications",
                                       notificationItem(
                                         text = "Site Visits: 2 New Requests",
                                         icon("radiation-alt")
                                       )),
                          dropdownMenu(type = "tasks", badgeStatus = "primary",
                                       taskItem(value = 25, color = "blue",
                                                "Site Assessment Report")
))
                          

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Industry Classifications", tabName = "industry_class", icon = icon("bars")),
    menuItem("Site Visit Chart", tabName = "plot_visits", icon = icon("industry"))
  
  ))


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "industry_class",
            h2())
  )
)


ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body)



# Update Server with the Plot
server <- function(input, output) {
  output$ <- renderMenu({})
  
}


shinyApp(ui, server)