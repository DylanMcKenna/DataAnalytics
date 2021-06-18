# load libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)
library(janitor)


ui <- dashboardPage(
  dashboardHeader(title = "Dynamic sidebar"),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  dashboardBody()
)

server <- function(input, output) {
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu item", icon = icon("calendar"))
    )
  })
}

shinyApp(ui, server)