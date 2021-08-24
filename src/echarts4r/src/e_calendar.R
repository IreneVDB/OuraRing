make.hm.cal <- function(df, column, min = 0, max, hue, title, subtitle){
  df %>%
    select(Date, all_of(column)) %>%
    mutate(Year = year(Date)) %>%
    group_by(Year) %>%
    e_charts(Date) %>%
    e_calendar(range = "2020",
               top = "15%",
               height = "30%",
               itemStyle = list(borderColor = 'fff'),
               dayLabel = list(color = "#808080"),
               monthLabel = list(color =  "#808080",
                                 fontSize = 16)) %>%
    e_calendar(range = "2021", 
               top = "55%",
               height = "30%",
               itemStyle = list(borderColor = 'fff'),
               dayLabel = list(color = "#808080"),
               monthLabel = list(color =  "#808080",
                                 fontSize = 16)) %>%
    e_visual_map(min = min, 
                 max = max, 
                 itemWidth = 25,
                 itemHeight = 240,
                 left = "center",
                 bottom = "5%",
                 orient = "horizontal",
                 inRange = list(
                   color = c(hsv(hue, 0.1, 1), 
                             hsv(hue, 0.7, 0.95),
                             hsv(hue, 1, 0.5)))) %>%
    e_title(text = title,
            subtext = paste0(subtitle, " (", nrow(df), " days)"),
            textStyle = list(fontSize = 36, height = "50%"),
            subtextStyle = list(fontSize = 24)) %>%
    e_tooltip(trigger = "item",
              formatter = htmlwidgets::JS(
                "function(params){
                return(params.value[0] + ':' + '<br>' + '<strong>' + 
                params.value[1] + '</strong>' +  ' steps')
}"
            )) %>%
    e_text_style(fontFamily = "Catamaran") 
  
  
}

make.hm.cal(oura[["Activity"]], "activity.steps", hue = 0.65, max = 25000,
            title = "Daily Steps over the Years",
            subtitle = "Measured with Oura Ring") %>%
  e_heatmap(activity.steps, coord_system = "calendar") %>%
  saveWidget(., selfcontained = TRUE,
             file = file.path("output", "echarts_oura", "e_calendar_steps.html"),
             title = "StepsCalendar")
  
make.hm.cal(oura[["Sleep"]], "sleep.rmssd", hue = 0.55, 
            min = 75, max = 125,
            title = "Mean HRV over the Years",
            subtitle = "Measured with Oura Ring") %>%
  e_heatmap(sleep.rmssd, coord_system = "calendar") %>%
  saveWidget(., selfcontained = TRUE,
             file = file.path("output", "echarts_oura", "e_calendar_HRV.html"),
             title = "HRVCalendar")