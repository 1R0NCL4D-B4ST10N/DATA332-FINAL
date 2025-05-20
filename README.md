# NBA Value Analysis Shiny App

## Project Overview

This project analyzes NBA player data from the 2022-2025 seasons, specifically examining player salaries and performance metrics sourced primarily from Basketball Reference. The aim is to determine player value relative to their cost and leverage data analytics techniques to gain actionable insights into player efficiency and team composition.

## Motivation

Our goal was to explore the intersection of sports analytics and economic value, providing NBA teams and analysts with tools to optimize their salary cap strategies by identifying undervalued players.

## Shiny Application Structure

* **Value Score Tab**: Visualizes players ranked by a calculated value score.
* **Top Scorers Tab**: Shows leading scorers by salary tier and season.
* **Most Games Played Tab**: Highlights players with consistent game participation.
* **Value vs Salary Tab**: Analyzes the relationship between salary and performance value.
* **Position Boxplot Tab**: Depicts value score distributions across player positions.
* **Team × Position Heatmap Tab**: Displays average team performance by position.

## Code Documentation

### Libraries Used

```r
library(shiny)
library(tidyverse)
library(scales)
library(stringr)
library(ggimage)
library(ggtext)
library(RCurl)
```

### Function for Reading CSV from URL

```r
read_csv_url <- function(url) {
  text <- getURL(url, ssl.verifypeer = FALSE, httpheader = c("User-Agent" = "R"))
  read.csv(text = text, stringsAsFactors = FALSE)
}
```

### Data Loading and Cleaning

```r
stats <- stats %>% mutate(Player_clean = tolower(trimws(Player)))
salaries <- salaries %>% mutate(Player_clean = tolower(trimws(Player)))
pergamestats <- pergamestats %>%
  mutate(Player_clean = tolower(trimws(Player))) %>%
  distinct(Player_clean, season, .keep_all = TRUE)
```

### Dataset Merging

```r
combined <- stats %>%
  inner_join(salaries, by = c("Player_clean", "season")) %>%
  left_join(pergamestats, by = c("Player_clean", "season")) %>%
  filter(!Tm %in% c("2TM", "3TM")) %>%
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
  )
```

### Value Score Calculation

```r
combined <- combined %>%
  mutate(
    Salary_M = Salary / 1e6,
    value_score = (0.6 * WS + 0.4 * VORP) / Salary_M
  )
```

## Shiny Application

[View the Shiny Application](https://1r0ncl4d-b4st10n.shinyapps.io/NBA_Statistics/)

---

*Last updated: May 2025*
