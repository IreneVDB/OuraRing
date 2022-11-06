# Make an interactive table for the Scores:
make.reactable.trend <- function(df, name = "Value", groupBy = "Category", title,
                                 hue = 0.63,
                                 pagination = FALSE, highlight = TRUE, 
                                 striped = FALSE, bordered = FALSE,
                                 borderless = TRUE, outlined  = TRUE){
  
  col_dark <- hsv(h = hue, s = 0.6, v = 0.6)
  col_mid <- hsv(h = hue, s = 0.48, v = 0.97)
  col_low <- hsv(h = hue, s = 0.1, v = 1)
  contrast <- hsv(h = (hue + 0.45) %% 1, s = 0.6, v = 0.9)
  
  df_tbl <- df %>%
    rowwise() %>%
    mutate(Last14 = list(rev(All)[14:1]),
           color_pal = case_when(LastVal < limit ~ contrast,
                                 TRUE ~ col_mid)) %>%
    mutate(mean7 = mean(rev(Last14)[1:7], na.rm = TRUE),
           sd7 = sd(rev(Last14)[1:7], na.rm = TRUE),
           icon = case_when(LastVal < mean7 - sd7 ~ "circle-down",
                            LastVal <= mean7 + sd7 ~ "arrows-left-right",
                            TRUE ~ "circle-up"),
           icon_color = case_when(LastVal < mean7 - sd7 ~ contrast,
                                  LastVal <= mean7 + sd7 ~ col_dark,
                                  TRUE ~ col_mid)) %>%
    select(any_of(c("Category", name, "LastVal", "mean7", "Last14",  "All", 
                    "Mean", "unit", "color_pal", "icon", "icon_color")))
  
  with_tooltip <- function(value, tooltip) {
    div(style = "text-decoration: underline;",
        tippy(value, 
              tooltip = paste0("<span style='font-family: Catamaran; background-color:", 
                               col_mid, "; border-radius: 5px; padding: 5px; margin-left:-10px; margin-right:-10px;'>", 
                               tooltip, "<span>"), 
              allowHTML = TRUE))
  }
  
  columns <- list(
    name = colDef(name = name, style = list(fontSize = 14, alignItems = "center")),
    LastVal = colDef(name = "Last value",
                     header = with_tooltip("Last value", paste0(
                       "Last update: ", df$lastdate[1])),
                     aggregate = JS("function(values) {return values[0] + '%'}"),
                     cell = data_bars(df_tbl, 
                                      text_position = "outside-end",
                                      fill_color_ref = "color_pal", 
                                      background = "#ffffff",
                                      text_color =  col_dark, 
                                      max_value = 120, bar_height = 20, 
                                      number_fmt = scales::label_percent(scale=1)),
                     minWidth = 200),
    Last14 = colDef(name = "Last 14 days", 
                   cell = function(value, index) {
                     sparkline(df_tbl$Last14[[index]], type = "bar", chartRangeMin = 0, 
                               chartRangeMax = 100, height = 25,
                               colorMap = case_when(df_tbl$Last14[[index]] < 60 ~ contrast,
                                                    TRUE ~ col_dark))
                   }, 
                   align = "center",  
                   style = list(alignItems = "center")),
    mean7 = colDef(name = "Trend", 
                   header = with_tooltip("Trend", 
                                         "Above, below or within 7 day mean +/- 1 SD"),
                   align = "center", maxWidth = 68,
                   cell = icon_sets(df_tbl, icon_ref = "icon", icon_size = 24, 
                                    icon_position = "over", icon_color_ref = "icon_color")),
    All = colDef(name = "All \U00AA",
                 header = with_tooltip("All \U00AA", 
                                       paste0("n = ", length(df_tbl$All[[1]]))),
                 cell = function(value, index) {
                   sparkline(df_tbl$All[[index]][!is.na(df_tbl$All[[index]])], type = "box", 
                             chartRangeMin = 0, chartRangeMax = 100,
                             showOutliers = TRUE, 
                             height=24 , width = "100%", lineColor = col_dark,
                             boxLineColor = col_dark, whiskerColor = col_dark,
                             medianColor = col_dark, outlierLineColor = col_dark,
                             boxFillColor = col_mid, outlierFillColor = "#fff")
                 }, 
                 align = "center"),
    Mean = colDef(name = "Mean \U00AA",
                  header = with_tooltip("Mean \U00AA", 
                                        paste0("n = ", length(df_tbl$All[[1]]))),
                  aggregate = JS("function(values) {return values[0] + '%'}"),
                  cell = function(value){paste0(value, "%")}, 
                  style = list(fontSize = 14),
                  align = "center", maxWidth = 72),
    color_pal = colDef(show = FALSE),
    icon = colDef(show = FALSE),
    icon_color = colDef(show = FALSE),
    unit = colDef(show = FALSE))
  
  if(groupBy == "Category"){
    columns[["Category"]] <- colDef(minWidth = 136, maxWidth = 140)
  }
  
  table <- reactable(df_tbl, 
                     pagination = pagination, 
                     highlight = highlight,
                     striped = striped, 
                     bordered = bordered,
                     borderless = borderless, 
                     outlined = outlined,
                     groupBy = groupBy, 
                     fullWidth = TRUE,
                     defaultColDef = colDef(align = "left"),
                     columns = columns,
                     theme = reactableTheme(
                       color = col_dark,
                       borderColor = col_dark,
                       highlightColor = col_low,
                       stripedColor = col_low,
                       cellPadding = "8px 12px",
                       style = list(fontFamily = "Catamaran", align = "right"),
                       headerStyle =  list(backgroundColor = col_dark, color="#fff")
                     )) %>%
    add_title(title = title,
              font_size = 36,
              font_color = col_dark) %>%
    add_subtitle(subtitle = paste0("Measured with Oura Ring between ",
                                  df$firstdate[1], " and ", df$lastdate[1],
                                  " (", length(Scores$All[[1]]), " days)"),
                 font_size = 20,
                 font_weight = "normal",
                 font_color = col_dark,
                 margin = margin(t=5, r=0, b=5, l=0))
  
  return(table)
}


