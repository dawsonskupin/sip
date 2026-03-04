
library(shiny)
library(tidyverse)
library(lubridate)

sip <- read_csv("data/sip_data.csv") |>
  mutate(
    Date = mdy(Date),
    Season = year(Date),
    Total_Yards = Off_Pass_YDS + Off_Rush_YDS,
    Margin = Points_for - Points_against,
    Win = Points_for > Points_against
  )

ui <- fluidPage(
  titlePanel("Football Analytics Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "season",
        "Select Season:",
        choices = c("All Seasons", sort(unique(sip$Season))),
        selected = "All Seasons"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Offense", plotOutput("scatter")),
        tabPanel("Defense", plotOutput("defense")),
        tabPanel("Margins", plotOutput("margin"))
      )
    )
  )
)

server <- function(input, output) {
  
  filtered <- reactive({
    if (input$season == "All Seasons") {
      sip
    } else {
      sip |> filter(Season == as.integer(input$season))
    }
  })
  
  output$scatter <- renderPlot({
    filtered() |>
      ggplot(aes(Total_Yards, Points_for, color = Win)) +
      geom_point(size = 3)
  })
  
  output$defense <- renderPlot({
    filtered() |>
      mutate(Takeaways = FR + INT) |>
      ggplot(aes(Takeaways, fill = Win)) +
      geom_bar(position = "dodge")
  })
  
  output$margin <- renderPlot({
    filtered() |>
      mutate(Game = row_number()) |>
      ggplot(aes(Game, Margin, fill = Win)) +
      geom_col() +
      labs(x = "Game (chronological)", y = "Margin")
  })
}

shinyApp(ui, server)
