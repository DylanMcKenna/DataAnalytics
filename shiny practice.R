
# load libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)


# read data
covid_speeches_words <- read_rds("covid-speeches-words.rds")

 # Transform Data
csw <- covid_speeches_words %>%
  filter(origin == "Scotland") %>%
  # stop words is from tidytext library
  # ignores certain words that are common
  anti_join(stop_words) %>%
  # != means not equal to
  filter(word != "positive") %>%
  # bing is a lexicon - a collection of words
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(date, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n) %>%
  mutate(sentiment = positive - negative)


# Plot Sentiment Chart
csw %>%
  ggplot(aes(x = date, y = sentiment)) +
  geom_line(color = "gray") +
  geom_point(aes(color = sentiment > 0), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "lightgray") +
  labs(title = glue("Daily sentiment score, Scotland COVID-19 briefings"),
       x = "Date", y = "Sentiment score (positive - negative)") +
  theme_minimal() + theme(legend.position = "none")



 #  SET SHINY APP TEMPLATE


# ui User Interface - how it is rendered.

# ui

ui <- fluidPage(
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
                    )
                )


# Server

server <- function(input, output) {
      
                                      }


# Shiny App
shinyApp(ui = ui, server = server)


# Update UI - origin

ui <- fluidPage(
  titlePanel("Scotland and UK COVID-19 Speeches"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "origin",
        label = "Select origin",
        choices = c("Scotland", "UK")
      )),
    mainPanel()
  )
)


# Update Server with the Plot
server <- function(input, output) {
  output$sentiment_plot <- renderPlot({
    csw <- covid_speeches_words %>%
      filter(origin == "Scotland") %>%
      # stop words is from tidytext library
      # ignores certain words that are common
      anti_join(stop_words) %>%
      # != means not equal to
      filter(word != "positive") %>%
      # bing is a lexicon - a collection of words
      inner_join(get_sentiments("bing"), by = "word") %>%
      count(date, sentiment) %>%
      pivot_wider(names_from = sentiment, values_from = n) %>%
      mutate(sentiment = positive - negative) %>%
      ggplot(aes(x = date, y = sentiment)) +
      geom_line(color = "gray") +
      geom_point(aes(color = sentiment > 0), size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "lightgray") +
      labs(title = glue("Daily sentiment score, Scotland COVID-19 briefings"),
           x = "Date", y = "Sentiment score (positive - negative)") +
      theme_minimal() + theme(legend.position = "none")
  })
}

