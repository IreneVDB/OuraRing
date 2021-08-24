make.data <- function(df, columns, divide_by = 1){
  df <- df %>%
    select(Date, any_of(columns)) %>%
    mutate(across(any_of(columns), `/`, divide_by)) %>% 
    rename_with(., ~str_to_title(str_extract(.x, "(?<=\\.).*")), 
                .cols = any_of(columns)) %>%
    mutate(Total = rowSums(across(where(is.numeric))))
}
make.ebar.layout <- function(df, hue, title, sub_pre, sub_post, ylab,
                            font = "Catamaran"){
  bar_colors <- hsv(h=hue, s = c(0.8, 0.4, 1, 0.6), v = c(0.3, 1, 0.6, 0.85))
  
echart <- df %>%
  e_charts(Date) %>% 
  e_axis(axis = "x", name = "Date",
           axisLine = list(
             lineStyle = list(
               color = "grey60")),
           nameLocation = "center",
           nameGap = 25,
           nameTextStyle = list(
             fontWeight = "bold",
             fontSize = 16)) %>%
    e_axis(axis = "y", name = ylab,
           nameTextStyle = list(
             fontWeight = "bold",
             fontSize = 16)) %>%
    e_title(title, 
            subtext = paste0(sub_pre, " (", nrow(df), " ", sub_post, ")"),
            textStyle = list(fontSize = 28),
            subtextStyle = list(fontSize = 16)) %>%
    e_legend(top = 80, right = 20) %>%
    e_grid(top=140, right = 10, bottom = 180, left =40) %>%
    e_datazoom(x_index = c(0,1),
             height = 50,
             bottom = 50,
             dataBackground = list(
               lineStyle = list(
                 color = hsv(h=hue, s = 0.8, v = 0.3)),
               areaStyle = list(
                 color = hsv(h=hue, s = 0.6, v = 1))),
             borderColor = "grey90",
             fillerColor = adjustcolor(hsv(hue, 1, 0.95), alpha.f = 0.2),
             moveHandleStyle = list(color = hsv(h=hue, s = 0.8, v = 0.3)),
             moveHandleSize = 9,
             emphasis = list(
               moveHandleStyle = list(color = hsv(h=hue, s = 0.8, v = 0.3)))) %>%
  e_text_style(fontFamily = font) 
}

# Make echart bar for sleep data: ----
make.stacked.bar.sleep <- function(){
  
  Sleep <- make.data(oura[["Sleep"]], columns = c(
    "sleep.awake", "sleep.rem", "sleep.deep", "sleep.light"), divide_by = 3600) %>%
    mutate(Total_char = case_when(is.na(Total) ~ "",
                                  TRUE ~ paste0(floor(Total), "h:", str_pad(
                                    round(Total %% 1 * 60), 2, "left", "0"), "m")),
           across(where(is.numeric), round, digits = 2))
  
  Sleepbar <- make.ebar.layout(Sleep, hue = 0.55, title = "Total hours of sleep", 
                               sub_pre = "Measured with the Oura Ring", sub_post = "nights",
                               ylab = "Duration\n(hours)") %>%
    e_bar(Awake, stack = "grp", bind = Total_char,
          itemStyle = list(color = hsv(h=0.55, s = 0.8, v = 0.3))) %>% 
    e_bar(Rem, stack = "grp", bind = Total_char,
          itemStyle = list(color = hsv(h=0.55, s = 0.4, v = 1))) %>% 
    e_bar(Deep, stack = "grp", bind = Total_char,
          itemStyle = list(color = hsv(h=0.55, s = 1, v = 0.6))) %>%  
    e_bar(Light, stack ="grp", bind = Total_char,
          itemStyle = list(color = hsv(h=0.55, s = 0.6, v = 0.85))) %>%
    e_tooltip(trigger = 'axis',
              textStyle = list(fontFamily = "Catamaran"),
              formatter = htmlwidgets::JS(
                "function(params){
                var tp = [];
                params.forEach(function(x){
                tp.push([x.seriesName,': ', x.value[1], 'h'].join(''))
                }); 
                return('<strong>' + 'Date: ' + '</strong>' + params[0].value[0] + '<br>' + 
                '<strong>' + 'Total sleep: ' + '</strong>' + params[0].name + '<br>' + 
                '<br>' + tp.join('<br/>'))
}")) %>%
  saveWidget(selfcontained = TRUE, 
             file=file.path("output", "echarts_oura", "e_bar_stacked_sleep.html"),
             title = "Total hours of sleep")
}
make.stacked.bar.sleep()

# Same for Activity data ----
make.stacked.bar.activity <- function(){
  Activity <- make.data(oura[["Activity"]], columns = c(
    "activity.low", "activity.medium", "activity.high"), divide_by = 1)
  
  Activity_bar <- make.ebar.layout(Activity, hue = 0.65, title = "Total Activity", 
                                   sub_pre = "Measured with the Oura Ring", sub_post = "days",
                                   ylab = "Duration\n(min)") %>%
    e_bar(High, stack = "grp", bind = Total,
          itemStyle = list(color = hsv(h=0.65, s = 0.8, v = 0.3))) %>% 
    e_bar(Medium, stack = "grp", bind = Total,
          itemStyle = list(color = hsv(h=0.65, s = 0.4, v = 1))) %>% 
    e_bar(Low, stack = "grp", bind = Total,
          itemStyle = list(color = hsv(h=0.65, s = 1, v = 0.6)))  %>%
    e_tooltip(trigger = 'axis',
              textStyle = list(fontFamily = "Catamaran"),
              formatter = htmlwidgets::JS(
                "function(params){
              var total = params[0].name/60
              var tp = [];
              params.forEach(function(x){
              tp.push([x.seriesName,': ', x.value[1], ' min'].join(''))
              }); 
              return('<strong>' + 'Date: ' + '</strong>' + params[0].value[0] + '<br>' + 
              '<strong>' + 'Total Activity: ' + '</strong>' + total.toFixed(2) + ' h' + '<br>' + 
              '<br>' + tp.join('<br/>'))
              }")) %>%
    saveWidget(selfcontained = TRUE, 
               file=file.path("output", "echarts_oura", "e_bar_stacked_activity.html"),
               title = "Total Activity per Day")
}
make.stacked.bar.activity()