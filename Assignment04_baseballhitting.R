# load libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)
library(janitor)
library(gghighlight)

master <- read.csv("Master.csv")

batting <- read.csv("Batting.csv")

master_name <- master %>%
  select(playerID, nameFirst, nameLast, bats, throws, birthCountry)

batting <- batting %>%
  mutate( GIDP = as.double(GIDP),
          Avg. = H / AB)

batting_full <- left_join(batting, master_name, join = "playerID")

batting_full <- batting_full %>%
  rename( 
    Year = yearID, 
    Team = teamID,
    League = lgID
  )

batting_full <- batting_full %>%
  mutate(Avg. = H / AB)

batting_full <- relocate(batting_full,  playerID, nameFirst, nameLast, bats, birthCountry )

batting_full <- relocate(batting_full, Avg., .after = G)

batting_new <- batting_full %>%
  filter(Year %in% (2000:2015))

batting_new <- batting_new %>%
  replace_na( list(HR = 0, RBI= 0, SB = 0, CS = 0,  BB = 0,  SO = 0, IBB = 0, HBP  = 0, SH = 0, SF  = 0, GIDP = 0)) %>%
  select(-GIDP)

batting_new <- batting_new %>%
  group_by(Team, Year) %>%
  summarise(HR_count = sum(HR))



ui <- fluidPage(
  titlePanel("MLB - Homeruns for 2000 to 2015"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "Year",
        label = "Select Season",
        choices = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011",
                    "2012", "2013", "2014", "2015")
      )),
  mainPanel(plotlyOutput(outputId = "homerun_plot"))),
  
  downloadButton("downloadData","Download Data",
                 icon = icon("arrow-circle-down"))
  )


# Update Server with the Plot
server <- function(input, output) {
  output$homerun_plot <- renderPlotly({
    chart <- batting_new %>%
      filter(Year == input$Year) %>%
      ggplot(aes(x = HR_count, y = Team, color = Team)) +
      geom_col(width = 0.5, position = "dodge") +
      labs(
        title = glue("Homeruns per Team during {input$Year} Season"),
        x = "Homeruns", y = "Team"
      ) +
      theme_classic() +
      gghighlight(max(HR_count)) +
      geom_text(size = 3, aes(color= Team, label = HR_count, ),
                nudge_x = 10,
                vjust = -0.4)
    
  output$downloadData <- downloadHandler(
      filename = function() {
        paste(Sys.Date(),"Batting", sep="_")
      },
      content = function(file) {
        write.csv(batting_full, file)
      }
  
    )
   
    
    ggplotly(chart, tooltip = c("x", "y"))
  })
}


shinyApp(ui, server)

