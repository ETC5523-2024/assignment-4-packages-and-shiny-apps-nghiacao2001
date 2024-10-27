library(EPLstats2122)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(forcats)
library(grid)
library(DT)
library(bslib)  # For theming

# Load the datasets from the package
data("full_league_table", package = "EPLstats2122")
data("half_time_results", package = "EPLstats2122")

# Define a custom Bootstrap theme
my_theme <- bs_theme(
  version = 4,
  bootswatch = "cosmo",
  primary = "blue",
  secondary = "red",
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto Slab")
)

# Define UI with theme
ui <- dashboardPage(
  dashboardHeader(title = "EPL 2021-2022 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",  # Ensure the ID is correctly set for tab tracking
      menuItem("Summary", tabName = "summary", icon = icon("chart-bar")),
      menuItem("Full-Time Analysis", tabName = "full_time", icon = icon("clock")),
      menuItem("Halftime Analysis", tabName = "half_time", icon = icon("clock"))
    )
  ),
  dashboardBody(
    bs_theme_dependencies(my_theme),
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f0f4f8; }
        .box { background-color: #ffffff; border-radius: 8px; border: 1px solid #e0e0e0; }
        .main-sidebar { background-color: #2c3e50; }
        .skin-blue .main-header .navbar { background-color: #ff69b4; }
      "))
    ),
    tabItems(
      # Summary Plot Tab
      tabItem(
        tabName = "summary",
        fluidRow(
          box(
            title = "League Position Plot", width = 12, solidHeader = TRUE, status = "primary",
            plotOutput("league_plot"),
            h4("Plot Description"),
            p("The plot shows position changes with arrows representing team movements. Red and blue lines indicate threshold positions for relegation and top-tier placements."),
            h4("Plot Interpretation"),
            p("From the plot, you can interpret which teams gained or lost the most if games ended at halftime versus full-time.")
          )
        )
      ),
      # Full-Time Analysis Tab
      tabItem(
        tabName = "full_time",
        fluidRow(
          box(
            title = "Full-Time Output Display", width = 12, solidHeader = TRUE, status = "primary",
            DTOutput("full_time_table"),
            h4("Dataset Description"),
            p("Pos.: The position of the team in the league table."),
            p("Team: Name of the team."),
            p("MP, W, D, L, GF, GA, GD, Points: Stats representing Matches Played, Wins, Draws, Losses, Goals For, Goals Against, Goal Difference, and Points respectively.")
          )
        )
      ),
      # Halftime Analysis Tab
      tabItem(
        tabName = "half_time",
        fluidRow(
          box(
            title = "Halftime Output Display", width = 12, solidHeader = TRUE, status = "primary",
            DTOutput("half_time_table"),
            h4("Dataset Description"),
            p("Pos.: The position of the team in the league table."),
            p("Team: Name of the team."),
            p("MP, W, D, L, GF, GA, GD, Points: Stats representing Matches Played, Wins, Draws, Losses, Goals For, Goals Against, Goal Difference, and Points respectively.")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # No additional logic required for tabs
  # Render Full-Time Results Table
  output$full_time_table <- renderDT({
    datatable(full_league_table)
  })

  # Render Halftime Results Table
  output$half_time_table <- renderDT({
    datatable(half_time_results)
  })

  # Render Plot
  output$league_plot <- renderPlot({
    plot_data <- full_league_table %>%
      bind_rows(half_time_results) %>%
      mutate(Team = fct_reorder(factor(Team), as.numeric(Pos.), first, .desc = TRUE))

    ggplot(plot_data) +
      geom_vline(xintercept = -17.5, colour = '#e74c3c', size = .6, alpha = 0.7) +
      geom_vline(xintercept = -4.5, colour = '#3498db', size = .6, alpha = 0.7) +
      geom_path(aes(x = -as.numeric(Pos.), y = Team), colour = '#2c3e50',
                arrow = arrow(length = unit(0.15, "cm"), type = 'closed')) +
      theme_minimal() +
      theme(text = element_text(family = 'Arial', color = 'gray30'),
            plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = 12, margin = margin(0, 0, 10, 0)),
            axis.text.x = element_blank(), panel.grid = element_blank(),
            plot.caption = element_text(hjust = 0.5, size = 10, color = 'grey40')) +
      labs(x = "Position change", y = "Team") +
      geom_text(aes(x = -as.numeric(Pos.), y = Team, label = Pos.),
                nudge_y = -.4, size = 3.5, family = 'Arial', color = 'grey25') +
      scale_y_discrete(expand = expansion(add = c(1.4, .6)))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

