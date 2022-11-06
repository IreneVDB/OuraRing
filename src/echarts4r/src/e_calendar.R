make.hm.cal <- function(df, column, min = 0, max, hue, title, subtitle){
  df %>%
    select(Date, all_of(column)) %>%
    mutate(Year = year(Date)) %>%
    group_by(Year) %>%
    e_charts(Date) %>%
    e_calendar(range = "2020",
               top = "20%",
               width="100%",
               height = "20%",
               itemStyle = list(borderColor = 'fff'),
               dayLabel = list(color = "#808080"),
               monthLabel = list(color =  "#808080",
                                 fontSize = 16)) %>%
    e_calendar(range = "2021", 
               top = "45%",
               height = "20%",
               width="100%",
               itemStyle = list(borderColor = 'fff'),
               dayLabel = list(color = "#808080"),
               monthLabel = list(color =  "#808080",
                                 fontSize = 16)) %>%
    e_calendar(range = "2022", 
               top = "70%",
               width="100%",
               height = "20%",
               itemStyle = list(borderColor = 'fff'),
               dayLabel = list(color = "#808080"),
               monthLabel = list(color =  "#808080",
                                 fontSize = 16)) %>%
    e_visual_map(min = min, 
                 max = max, 
                 itemWidth = 20,
                 itemHeight = 240,
                 left = "center",
                 bottom = "0%",
                 orient = "horizontal",
                 inRange = list(
                   color = c(hsv(hue, 0.1, 1), 
                             hsv(hue, 0.7, 0.95),
                             hsv(hue, 1, 0.5)))) %>%
    e_title(text = title,
            subtext = paste0(subtitle, " (", nrow(df), " days)"),
            textStyle = list(fontSize = 28, height = "50%"),
            subtextStyle = list(fontSize = 18)) %>%
    e_tooltip(trigger = "item",
              formatter = htmlwidgets::JS(
                "function(params){
                return(params.value[0] + ':' + '<br>' + '<strong>' + 
                params.value[1] + '</strong>')
                }")) %>%
    e_text_style(fontFamily = "Catamaran") %>%
    e_grid(width="80%")
  
  
}