
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
    Outcome = if_else(Win, "Win", "Loss"),
    Takeaways = FR + INT,
    Pressure = Sacks + TFL
  )

off_vars <- c(
  "Total Yards" = "Total_Yards",
  "Points Scored" = "Points_for",
  "Points Allowed" = "Points_against",
  "Pass Yards" = "Off_Pass_YDS",
  "Rush Yards" = "Off_Rush_YDS",
  "Pass TDs" = "Off_Pass_TD",
  "Rush TDs" = "Off_Rush_TD",
  "Pass Long" = "Off_Pass_Long",
  "Rush Long" = "Off_Rush_Long",
  "Receiving Attempts" = "Off_Rec_ATT",
  "Receiving Yards" = "Off_Rec_YDS",
  "Receiving TDs" = "Off_Rec_TD",
  "Margin" = "Margin"
)

def_vars <- c(
  "Points Allowed" = "Points_against",
  "Takeaways" = "Takeaways",
  "Interceptions" = "INT",
  "Fumble Recoveries" = "FR",
  "Forced Fumbles" = "FF",
  "Sacks" = "Sacks",
  "TFL" = "TFL",
  "Pressure (Sacks + TFL)" = "Pressure",
  "Total Tackles" = "Tot",
  "Solo Tackles" = "Solo",
  "Assisted Tackles" = "Ast",
  "Sack Yards" = "Sack_YDS",
  "TFL Yards" = "TFL_YDS",
  "Opponent Pass Yards" = "Opp_Pass_YDS",
  "Opponent Rush Yards" = "Opp_Rush_YDS",
  "Opponent Pass TDs" = "Opp_Pass_TD",
  "Opponent Rush TDs" = "Opp_Rush_TD",
  "Margin" = "Margin"
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
        tabPanel(
          "Offense",
          br(),
          fluidRow(
            column(
              4,
              selectInput(
                "off_plot_type",
                "Offensive Plot Type:",
                choices = c("Scatterplot", "Histogram"),
                selected = "Scatterplot"
              )
            ),
            column(
              4,
              selectInput(
                "off_xvar",
                "Offensive X-axis:",
                choices = off_vars,
                selected = "Total_Yards"
              )
            ),
            column(
              4,
              selectInput(
                "off_yvar",
                "Offensive Y-axis:",
                choices = off_vars,
                selected = "Points_for"
              )
            )
          ),
          plotOutput("scatter")
        ),
        
        tabPanel(
          "Defense",
          br(),
          fluidRow(
            column(
              4,
              selectInput(
                "def_plot_type",
                "Defensive Plot Type:",
                choices = c("Scatterplot", "Histogram"),
                selected = "Scatterplot"
              )
            ),
            column(
              4,
              selectInput(
                "def_xvar",
                "Defensive X-axis:",
                choices = def_vars,
                selected = "Takeaways"
              )
            ),
            column(
              4,
              selectInput(
                "def_yvar",
                "Defensive Y-axis:",
                choices = def_vars,
                selected = "Points_against"
              )
            )
          ),
          plotOutput("defense")
        ),
        
        tabPanel(
          "Margins",
          plotOutput("margin")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  filtered <- reactive({
    if (input$season == "All Seasons") {
      sip
    } else {
      sip |> filter(Season == as.numeric(input$season))
    }
  })
  
  # Offensive Plot
  output$scatter <- renderPlot({
    df <- filtered()
    
    if (input$off_plot_type == "Scatterplot") {
      
      ggplot(
        df,
        aes(
          x = .data[[input$off_xvar]],
          y = .data[[input$off_yvar]],
          color = Outcome
        )
      ) +
        geom_point(size = 3) +
        scale_color_manual(
          values = c("Loss" = "#231F20", #k-college black
                     "Win" = "#FF6900")  #k-college orange
        ) +
        labs(
          title = paste(names(off_vars[off_vars == input$off_xvar]),
                        "vs",
                        names(off_vars[off_vars == input$off_yvar])),
          x = names(off_vars[off_vars == input$off_xvar]),
          y = names(off_vars[off_vars == input$off_yvar]),
          color = "Outcome"
        ) +
        theme_minimal()
      
    } else if (input$off_plot_type == "Histogram") {
      
      ggplot(
        df,
        aes(
          x = .data[[input$off_xvar]],
          fill = Outcome
        )
      ) +
        geom_histogram(alpha = 0.7, bins = 10) +
        scale_fill_manual(
          values = c("Loss" = "#231F20",
                     "Win" = "#FF6900")
        ) +
        labs(
          title = paste("Distribution of",
                        names(off_vars[off_vars == input$off_xvar])),
          x = names(off_vars[off_vars == input$off_xvar]),
          y = "Count",
          fill = "Outcome"
        ) +
        theme_minimal()
    }
  })
  
  # Defensive Plot
  output$defense <- renderPlot({
    df <- filtered()
    
    if (input$def_plot_type == "Scatterplot") {
      
      ggplot(
        df,
        aes(
          x = .data[[input$def_xvar]],
          y = .data[[input$def_yvar]],
          color = Outcome
        )
      ) +
        geom_point(size = 3) +
        scale_color_manual(
          values = c("Loss" = "#231F20",
                     "Win" = "#FF6900")
        ) +
        labs(
          title = paste(names(def_vars[def_vars == input$def_xvar]),
                        "vs",
                        names(def_vars[def_vars == input$def_yvar])),
          x = names(def_vars[def_vars == input$def_xvar]),
          y = names(def_vars[def_vars == input$def_yvar]),
          color = "Outcome"
        ) +
        theme_minimal()
      
    } else if (input$def_plot_type == "Histogram") {
      
      ggplot(
        df,
        aes(
          x = .data[[input$def_xvar]],
          fill = Outcome
        )
      ) +
        geom_histogram(alpha = 0.7, bins = 10) +
        scale_fill_manual(
          values = c("Loss" = "#231F20",
                     "Win" = "#FF6900")
        ) +
        labs(
          title = paste("Distribution of",
                        names(def_vars[def_vars == input$def_xvar])),
          x = names(def_vars[def_vars == input$def_xvar]),
          y = "Count",
          fill = "Outcome"
        ) +
        theme_minimal()
    }
  })
  
  # Margin Plot
  output$margin <- renderPlot({
    
    filtered() |>
      mutate(Game = row_number()) |>
      ggplot(aes(Game, Margin, fill = Outcome)) +
      geom_col() +
      scale_fill_manual(
        values = c("Loss" = "#231F20",
                   "Win" = "#FF6900")
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