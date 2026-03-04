
library(shiny)
library(tidyverse)
library(lubridate)

# Load data
sip <- read_csv("data/sip_data.csv", show_col_types = FALSE) |>
  mutate(
    Date = mdy(Date),
    Season = year(Date),
    Total_Yards = Off_Pass_YDS + Off_Rush_YDS,
    Margin = Points_for - Points_against,
    Win = Points_for > Points_against,
    Outcome = if_else(Win, "Win", "Loss")
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
        tabPanel("Offense",
                 plotOutput("scatter")),
        tabPanel("Defense",
                 plotOutput("defense")),
        tabPanel("Margins",
                 plotOutput("margin"))
      )
    )
  )
)

server <- function(input, output) {
  
  filtered <- reactive({
    if (input$season == "All Seasons") {
      sip
    } else {
      sip |> filter(Season == input$season)
    }
  })
  
  # Offensive Scatter Plot
  output$scatter <- renderPlot({
    filtered() |>
      ggplot(aes(Total_Yards, Points_for, color = Outcome)) +
      geom_point(size = 3) +
      scale_color_manual(
        values = c("Loss" = "red",  # clean red
                   "Win" = "dodgerblue")   # clean blue
      ) +
      labs(
        title = "Total Offensive Yards vs Points Scored",
        x = "Total Offensive Yards",
        y = "Points Scored",
        color = "Outcome"
      ) +
      theme_minimal()
  })
  
  # Defensive Takeaways Plot
  output$defense <- renderPlot({
    filtered() |>
      mutate(Takeaways = FR + INT) |>
      ggplot(aes(Takeaways, fill = Outcome)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(
        values = c("Loss" = "red",
                   "Win" = "dodgerblue")
      ) +
      labs(
        title = "Takeaways and Game Outcome",
        x = "Total Takeaways",
        y = "Number of Games",
        fill = "Outcome"
      ) +
      theme_minimal()
  })
  
  # Margin Plot
  output$margin <- renderPlot({
    filtered() |>
      mutate(Game = row_number()) |>
      ggplot(aes(Game, Margin, fill = Outcome)) +
      geom_col() +
      scale_fill_manual(
        values = c("Loss" = "red",
                   "Win" = "dodgerblue")
      ) +
      labs(
        title = "Game Margin Over Time",
        x = "Game (Chronological)",
        y = "Point Differential",
        fill = "Outcome"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)

