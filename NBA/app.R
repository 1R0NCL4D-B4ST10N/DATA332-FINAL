library(shiny)
library(tidyverse)
library(scales)
library(stringr)
library(ggimage)
library(ggtext)
library(RCurl)

read_csv_url <- function(url) {
  text <- getURL(url, ssl.verifypeer = FALSE, httpheader = c("User-Agent" = "R"))
  read.csv(text = text, stringsAsFactors = FALSE)
}

stats <- read_csv_url("https://raw.githubusercontent.com/1R0NCL4D-B4ST10N/DATA332-FINAL/main/NBA/NBA_COMBINED/nba_advanced_all_seasons.csv")
salaries <- read_csv_url("https://raw.githubusercontent.com/1R0NCL4D-B4ST10N/DATA332-FINAL/main/NBA/NBA_COMBINED/final_nba_salaries.csv")
pergamestats <- read_csv_url("https://raw.githubusercontent.com/1R0NCL4D-B4ST10N/DATA332-FINAL/main/NBA/NBA_COMBINED/combined_nba_per_game_stats.csv")

# Clean and prepare columns
stats <- stats %>% mutate(Player_clean = tolower(trimws(Player)))
salaries <- salaries %>% mutate(Player_clean = tolower(trimws(Player)))
pergamestats <- pergamestats %>%
  mutate(Player_clean = tolower(trimws(Player))) %>%
  distinct(Player_clean, season, .keep_all = TRUE)

combined <- stats %>%
  inner_join(salaries, by = c("Player_clean", "season")) %>%
  left_join(pergamestats, by = c("Player_clean", "season")) %>%
  filter(!Tm %in% c("2TM", "3TM")) %>%   # ✅ remove multi-team aggregates
  group_by(Player_clean, season) %>%
  summarise(
    Player = first(Player.x),
    Tm = first(Tm),
    Pos = first(Pos),
    Salary = mean(Salary, na.rm = TRUE),
    WS = mean(WS, na.rm = TRUE),
    VORP = mean(VORP, na.rm = TRUE),
    PTS = mean(PTS, na.rm = TRUE),
    G = mean(G, na.rm = TRUE),
    Win_Shares = mean(Win_Shares, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Salary_M = Salary / 1e6,
    value_score = (0.6 * WS + 0.4 * VORP) / Salary_M,
    salary_tier = case_when(
      Salary_M >= 30 ~ "Max ($30M+)",
      Salary_M >= 20 ~ "High ($20M–29M)",
      Salary_M >= 10 ~ "Mid ($10M–19M)",
      TRUE ~ "Low (<$10M)"
    ),
    Pos_full = case_when(
      Pos == "PG" ~ "Point Guard",
      Pos == "SG" ~ "Shooting Guard",
      Pos == "SF" ~ "Small Forward",
      Pos == "PF" ~ "Power Forward",
      Pos == "C"  ~ "Center",
      TRUE        ~ "Other"
    ),
    logo_path = paste0("www/logos/", Tm, ".png")
  )
# UI
ui <- fluidPage(
  titlePanel("NBA Value Score Explorer (2022–2025)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Season", choices = unique(combined$season), selected = "2024-25"),
      selectInput("tier", "Salary Tier", choices = unique(combined$salary_tier)),
      sliderInput("numPlayers", "Number of Players to Show", min = 5, max = 30, value = 15),
      selectInput("position", "Position",
                  choices = c("All", "Point Guard", "Shooting Guard", "Small Forward", "Power Forward", "Center"),
                  selected = "All"),
      helpText("GitHub Link: https://github.com/1R0NCL4D-B4ST10N/DATA332-FINAL")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Value Score",
                 plotOutput("valuePlot", height = "600px"),
                 br(),
                 dataTableOutput("playerTable")
        ),
        tabPanel("Top Scorers",
                 plotOutput("topScorersPlot", height = "600px"),
                 br(),
                 dataTableOutput("scorerTable")
        ),
        tabPanel("Most Games Played",
                 plotOutput("mostGamesPlot", height = "600px"),
                 br(),
                 dataTableOutput("gamesTable")
        ),
        tabPanel("Value vs Salary", 
                 plotOutput("valueVsSalaryPlot", height = "600px"),
                 br(),
                 dataTableOutput("valueVsSalaryTable")
        ),
        
        tabPanel("Position Boxplot", 
                 plotOutput("positionBoxplot", height = "600px")
        ),
        
        tabPanel("Team × Position Heatmap", 
                 plotOutput("heatmapPlot", height = "600px")
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    combined %>%
      filter(
        season == input$season,
        salary_tier == input$tier,
        (input$position == "All" | Pos_full == input$position),
        !is.na(value_score),
        is.finite(value_score),
        value_score > 0,
        value_score < 3,
        !is.na(Player)
      ) %>%
      mutate(
        display_name = paste0(
          "<img src='", logo_path, "' width='20' style='vertical-align:middle;'> ",
          Player
        )
      )
  })
  
  
  output$valuePlot <- renderPlot({
    top_players <- filtered_data() %>%
      slice_head(n = input$numPlayers)
    
    ggplot(top_players, aes(x = reorder(display_name, value_score), y = value_score, fill = Pos_full)) +
      geom_col() +
      geom_text(aes(label = round(value_score, 2)), hjust = -0.1, size = 3) +
      coord_flip() +
      labs(
        title = paste("Top", input$numPlayers, input$tier, "Value Players (", input$season, ")"),
        x = "",
        y = "Value Score (WS + VORP per $1M)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.y = element_markdown(size = 10),  # ← this does the magic
        plot.title = element_text(size = 15, face = "bold")
      ) +
      scale_y_continuous(
        labels = scales::label_number(accuracy = 0.1),
        expand = expansion(mult = c(0, 0.25))
      )
  })
  
  output$playerTable <- renderDataTable({
    filtered_data() %>%
      arrange(desc(value_score)) %>%
      slice_head(n = input$numPlayers) %>%
      select(Player, Tm, Salary_M, WS, VORP, value_score)
  })
  
  output$topScorersPlot <- renderPlot({
    top_pts <- filtered_data() %>%
      arrange(desc(PTS)) %>%
      slice_head(n = input$numPlayers) %>%
      mutate(
        display_name = paste0(
          "<img src='", logo_path, "' width='20' style='vertical-align:middle;'> ",
          Player
        )
      )
    
    ggplot(top_pts, aes(x = reorder(display_name, PTS), y = PTS, fill = Pos_full)) +
      geom_col() +
      geom_text(aes(label = round(PTS, 1)), hjust = -0.1, size = 3) +
      coord_flip() +
      labs(
        title = paste("Top", input$numPlayers, input$tier, "Players by Points Per Game -", input$season),
        x = "", y = "Points Per Game"
      ) +
      theme_minimal(base_size = 13) +
      theme(axis.text.y = ggtext::element_markdown())
  })
  
  output$scorerTable <- renderDataTable({
    filtered_data() %>%
      arrange(desc(PTS)) %>%
      slice_head(n = input$numPlayers) %>%
      select(Player, Pos, Tm, PTS)
  })
  
  output$mostGamesPlot <- renderPlot({
    top_games <- filtered_data() %>%
      arrange(desc(G)) %>%
      slice_head(n = input$numPlayers) %>%
      mutate(
        display_name = paste0(
          "<img src='", logo_path, "' width='20' style='vertical-align:middle;'> ",
          Player
        )
      )
    
    ggplot(top_games, aes(x = reorder(display_name, G), y = G, fill = Pos_full)) +
      geom_col() +
      geom_text(aes(label = G), hjust = -0.1, size = 3) +
      coord_flip() +
      labs(
        title = paste("Top", input$numPlayers, input$tier, "Players by Games Played -", input$season),
        x = "", y = "Games Played"
      ) +
      theme_minimal(base_size = 13) +
      theme(axis.text.y = ggtext::element_markdown())
  })
  
  output$gamesTable <- renderDataTable({
    filtered_data() %>%
      arrange(desc(G)) %>%
      slice_head(n = input$numPlayers) %>%
      select(Player, Tm, G, Win_Shares)
  })
  
  output$valueVsSalaryPlot <- renderPlot({
    filtered_data() %>%
      slice_head(n = input$numPlayers) %>%
      ggplot(aes(x = Salary_M, y = value_score, label = Player)) +
      geom_point(aes(color = Pos_full), size = 3, alpha = 0.7) +
      labs(
        title = paste("Value Score vs. Salary (Top", input$numPlayers, "players)"),
        x = "Salary (in Millions)",
        y = "Value Score"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 11)
      )
  })
  
  output$valueVsSalaryTable <- renderDataTable({
    filtered_data() %>%
      slice_head(n = input$numPlayers) %>%
      arrange(desc(value_score)) %>%
      select(Player, Pos = Pos_full, Tm, Salary_M, value_score)
  })
  
  team_trend_data <- combined %>%
    group_by(Tm, season) %>%
    summarise(avg_value = mean(value_score, na.rm = TRUE), .groups = "drop")
  
  output$teamTrendPlot <- renderPlot({
    ggplot(team_trend_data, aes(x = season, y = avg_value, group = Tm, color = Tm)) +
      geom_line() +
      labs(title = "Average Value Score per Team Over Time",
           x = "Season", y = "Average Value Score") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 11)
      )
  })
  
  output$positionBoxplot <- renderPlot({
    filtered_data() %>%
      slice_head(n = input$numPlayers) %>%
      ggplot(aes(x = Pos_full, y = value_score, fill = Pos_full)) +
      geom_boxplot() +
      labs(title = paste("Value Score Distribution by Position (Top", input$numPlayers, "players)" ), y = "Value Score", x = "") +
      theme_minimal(base_size = 13)
  })
  
  output$heatmapPlot <- renderPlot({
    heatmap_data <- filtered_data() %>%
      slice_head(n = input$numPlayers) %>%
      group_by(Tm, Pos_full, logo_path) %>%
      summarise(avg_score = mean(value_score, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        TeamLogo = paste0(
          "<img src='", logo_path, "' width='20' style='vertical-align:middle;'>"
        )
      )
    
    ggplot(heatmap_data, aes(x = Pos_full, y = TeamLogo, fill = avg_score)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightyellow", high = "red", na.value = "gray90") +
      labs(
        title = paste("Avg Value Score by Team and Position (Top", input$numPlayers, "players)"),
        x = "Position",
        y = "Team",
        fill = "Avg Score"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.y = ggtext::element_markdown(size = 11),
        plot.title = element_text(size = 15, face = "bold"),
        panel.grid = element_blank()
      )
  })
}


# Run the app
shinyApp(ui = ui, server = server)




