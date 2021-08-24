# Make an interactive table for the Scores:
library(tidyverse)
library(reactable)
library(htmltools)
library(sparkline)
library(htmlwidgets)
library(reactablefmtr)
library(tippy)

source("src/Oura_api.R")
source("src/reactable/Make_reactable.R")

Scores <- bind_rows(map(oura, function(df){
  is.score <- grep("score", colnames(df))
  
  df %>%
    pivot_longer(., cols = contains("score"), names_to = "Score") %>%
    group_by(Score) %>%
    summarise(firstdate = first(Date),
              lastdate = last(Date),
              LastVal = last(value),
              Mean = round(mean(value, na.rm = TRUE)),
              All = list(value)) %>%
    mutate(Score = str_to_sentence(ifelse(grepl("score_", Score) == TRUE, 
                                          gsub("_", " ", str_extract(Score, pattern = "(?<=\\.score_).*$")), 
                                          gsub("\\.", " ", Score))),
           unit = "%",
           limit = 60)
}), .id = "Category")

score_tbl <- make.reactable.trend(Scores, name = "Score",
                     title = "Sleep, activity and readiness scores", 
                     tracker = "Oura Ring")

# save without logo:
# save_reactable(score_tbl, "src/reactable/output/OuraRing_scores.html")

# save with logo -> save Viewer pane
tbl_logo <- appendContent(score_tbl,
              div(a(
                img(src = knitr::image_uri("data/logo_grey.png"), width = 40, 
                    alt = "Logo Je bent wat je Meet"), 
                href="https://www.jebentwatjemeet.nl"),
                style="text-align: center; padding-bottom:10px;"),
              div("\u00A9 JeBentWatJeMeet 2021", 
                  style="text-align:center; font-size:10px;
                  padding-bottom:10px; font-family:Catamaran; color:#434d80"))

tbl_logo


