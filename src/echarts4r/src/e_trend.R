Make.trend <- function(df, column, hue, ymax, ymin, ylab,
                       title, subtitle,
                       plot_width = 500, plot_height = 300,
                       bar_gap = 20, left_mar = 50, top_mar = 50){
  col <- substitute(column)
  
  data <- df %>%
    select(Date, value = {{column}}) %>%
    mutate(value := imputeTS::na_ma(value, k = 4, maxgap = 2),
           label = str_extract(col, "(?<=\\.).*"),
      rollmean = zoo::rollmean(value, k = 7, fill = NA, align = "right"),
      norm_max = rollmean + zoo::rollapply(value, width = 7, sd, 
                                           fill = NA, align = "right"),
      norm_min = rollmean - zoo::rollapply(value, width = 7, sd, 
                                           fill = NA, align = "right"),
      color = case_when(value >= norm_max  ~ hsv(0,0,0.5),
                        value < norm_min ~ hsv(0,0,0.5),
                        TRUE~hsv(0,0,0.8)),
      mean = mean(value))
 
     yrange <- ymax - ymin
  
  data %>%
    e_charts(Date) %>%
    e_axis(axis = "y",
           max = ymax,
           min = ymin,
           name = ylab,
           splitLine = list(show = FALSE),
           nameTextStyle = list(
             fontWeight = "bold",
             fontSize = 16)) %>%
    e_axis(axis = "x",
           name = "Date",
           nameLocation = "center",
           nameGap = 25,
           nameTextStyle = list(
             fontWeight = "bold",
             fontSize = 16),
           axisLine = list(show = FALSE)) %>%
    e_bar(value, bind = label) %>%
    e_add("itemStyle", color) %>%
    e_line(rollmean, 
           symbol = "none",
           lineStyle = list(
             color = hsv(hue, 1, 0.95),
             width = 3
           )) %>%
    e_band2(norm_min, norm_max, 
            color = hsv(hue, 0.2, 1),
            itemStyle = list(borderWidth = 0)) %>%
    e_line(mean, symbol = "none",
           lineStyle = list(
             color = hsv(hue, 1, 0.5),
             type = "dashed",
             width = 1
           )) %>%
    e_mark_point(serie = "mean",
                 data = list(x = plot_width + left_mar + 30, 
                             y = (ymax - data$mean[1]) * plot_height/yrange + top_mar),
                 symbol = "rect",
                 symbolSize = c(20, 5),
                 itemStyle = list(color = hsv(hue, 1, 0.5)),
                 label = list(position = "right",
                              color = hsv(hue, 1, 0.5),
                              fontSize = 14),
                 title = paste0("All-time mean\n(", round(data$mean[1]), " ",
                                str_extract(ylab, "(?<=\\().*(?=\\))"), ")")) %>%
    e_mark_point(serie = "rollmean",
                 data = list(x = plot_width + left_mar + 30, 
                             y = (ymax - data$mean[1]) * plot_height/yrange + top_mar -30),
                 symbol = "rect",
                 symbolSize = c(20, 10),
                 itemStyle = list(color = hsv(hue, 1, 0.95)),
                 label = list(position = "right",
                              color = hsv(hue, 1, 0.95),
                              fontSize = 14),
                 title = "7-day rolling mean") %>%
    e_grid(left = left_mar, top = top_mar, right = 50,bottom = 150,
            width = plot_width, height = plot_height) %>%
    e_text_style(fontFamily = "Catamaran") %>%
    e_legend(show = FALSE) %>%
    e_datazoom(x_index = c(0,1),
               height = 40,
               top = plot_height + top_mar + 20,
               dataBackground = list(
                 lineStyle = list(
                   color = hsv(h=hue, s = 0.8, v = 0.5)),
                 areaStyle = list(
                   color = hsv(h=hue, s = 0.6, v = 1))),
               borderColor = hsv(0,0,0.9),
               fillerColor = adjustcolor(hsv(hue, 1, 0.95), alpha.f = 0.2),
               moveHandleStyle = list(color = hsv(h=hue, s = 0.8, v = 0.9)),
               moveHandleSize = 9,
               emphasis = list(
                 moveHandleStyle = list(color = hsv(h=hue, s = 0.8, v = 0.9)))) %>%
      e_title(text = title,
              subtext = paste0(subtitle, " (", nrow(data), " days)"),
              textStyle = list(fontSize = 36, height = "50%"),
              subtextStyle = list(fontSize = 24)) %>%
    e_tooltip(trigger = 'axis',
              formatter = htmlwidgets::JS(
                "function(params){
                return('<strong>' + 'Date: ' + '</strong>' + params[0].value[0] + '<br>' + 
                params[0].name + ': ' + Math.round(params[0].value[1]) + '<br>' + 
                '7-day rolling mean: ' + Math.round(params[1].value[1])) 
}"))
}

# Make.trend(oura[["Sleep"]], column = sleep.rmssd, ylab = "HRV (msec)",
#                 title = "Average Heart Rate Variability (rmssd) during sleep",
#                 subtitle = "Measured with the Oura Ring",
#                 hue = 0.55, ymax = 180, ymin = 60, top_mar = 150,
#            plot_width = 800, plot_height = 600) %>%
#   saveWidget(., selfcontained = TRUE,
#              file = file.path("output", "echarts_oura", "e_line_HRV_800.html"),
#              title = "HRVTrend")  

# Make.trend(oura[["Activity"]], column = activity.steps, ylab = "Steps (xx)",
#            title = "Daily Step Count",
#            subtitle = "Measured with the Oura Ring",
#            hue = 0.65, ymax = 40000, ymin = 0, top_mar = 150,
#            plot_width = 960, plot_height = 600) %>%
#   saveWidget(., selfcontained = TRUE,
#              file = file.path("output", "echarts_oura", "e_line_Steps.html"),
#              title = "StepsTrend")