
setwd("C:/Users/13038/Downloads/NBA_COMBINED")

library(shiny)
library(tidyverse)
library(scales)
library(stringr)

# Load and prep data
stats <- read_csv("nba_advanced_all_seasons.csv")
salaries <- read_csv("final_nba_salaries.csv")

# Clean and merge
stats <- stats %>% mutate(Player_clean = tolower(trimws(Player)))
salaries <- salaries %>% mutate(Player_clean = tolower(trimws(Player)))

combined <- stats %>%
  inner_join(salaries, by = c("Player_clean", "season")) %>%
  mutate(
    Salary_M = Salary / 1e6,
    value_score = (0.6 * WS + 0.4 * VORP) / Salary_M,
    salary_tier = case_when(
      Salary_M >= 30 ~ "Max ($30M+)",
      Salary_M >= 20 ~ "High ($20M–29M)",
      Salary_M >= 10 ~ "Mid ($10M–19M)",
      TRUE ~ "Low (<$10M)"
    )
  )

# UI
ui <- fluidPage(
  titlePanel("NBA Value Score Explorer (2022–2025)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Season", choices = unique(combined$season), selected = "2024-25"),
      selectInput("tier", "Salary Tier", choices = unique(combined$salary_tier)),
      sliderInput("numPlayers", "Number of Players to Show", min = 5, max = 30, value = 15)
    ),
    
    mainPanel(
      plotOutput("valuePlot", height = "600px"),
      br(),
      dataTableOutput("playerTable")
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    combined %>%
      filter(season == input$season, salary_tier == input$tier) %>%
      arrange(desc(value_score)) %>%
      slice_head(n = input$numPlayers) %>%
      mutate(display_name = str_wrap(Player.x, width = 20))
  })
  
  output$valuePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = reorder(display_name, value_score), y = value_score, fill = salary_tier)) +
      geom_col() +
      geom_text(aes(label = round(value_score, 2)), hjust = -0.1, size = 3) +
      coord_flip() +
      labs(
        title = paste("Top", input$numPlayers, input$tier, "Value Players (", input$season, ")"),
        x = "",
        y = "Value Score (WS + VORP per $1M)"
      ) +
      theme_minimal(base_size = 13) +
      scale_y_continuous(labels = label_number(accuracy = 0.1), expand = expansion(mult = c(0, 0.25)))
  })
  
  output$playerTable <- renderDataTable({
    filtered_data() %>%
      select(Player = Player.x, Salary_M, WS, VORP, value_score) %>%
      arrange(desc(value_score))
  })
}

# Run the app
shinyApp(ui = ui, server = server)




