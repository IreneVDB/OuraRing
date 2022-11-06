make.e_bar.scores <-function(category, hue, barwidth = 40){
  df <- oura[[category]][nrow(oura[[category]]), 
                        grep("score_", colnames(oura[[category]]))] %>%
    t() %>%
    data.frame() %>%
    magrittr::set_colnames("Value") %>%
    mutate(Score = gsub("_", " ", str_to_title(
      str_extract(rownames(.), pattern = "(?<=\\.score_).*$"))),
      Date = as.character(rev(oura[[category]]$Date)[1]))
  
  mar_t <- 50 + (8 - nrow(df)) * (barwidth + 10)
  mar_b <- 50

  heigth <- nrow(df) * (barwidth + 10) + mar_t + mar_b
  
  df %>%
    e_charts(Score) %>%
    e_bar(Value, 
          label =list(
            show = TRUE,
            color = "#fff",
            position = "insideRight",
            formatter = "{@0}%"),
          showBackground = TRUE,
          barWidth = barwidth,
          bind = Date) %>%
    e_visual_map(dimension = 0,
                 type = "piecewise",
                 show = FALSE,
                 pieces = list(list(gt = 60, color = hsv(hue, 0.5, 0.95)), 
                               list(lte = 60, color = hsv(hue, 1, 0.6)))) %>%
    e_flip_coords() %>%
    e_axis(axis = "y", 
           name = paste(category, "Score"),
           nameTextStyle = list(
             align = "center",
             fontWeight = "bold",
             fontSize = 16),
           nameGap = mar_t - 50,
           axisTick = list(show = FALSE), 
           axisLine = list(show = FALSE)) %>%
    e_axis(axis = "x", 
           name = "%",
           nameTextStyle = list(
             fontWeight = "bold",
             fontSize = 12,
             align = "left",
             verticalAlign = "top"),
           nameGap = -120,
           splitLine = list(show = FALSE)) %>%
    e_grid(left = 100, top = mar_t, bottom = mar_b) %>%
    e_legend(show= FALSE) %>%
    e_tooltip(trigger = 'item',
            formatter = htmlwidgets::JS(
              "function(params){
                var date = params.name.split(',')
                return('<strong>' + 'Date: ' + '</strong>' + date[0] + '<br>' +
                      '<strong>' + params.value[1] + ': ' + '</strong>' + params.value[0] + '%')
              }")) %>%
    e_text_style(fontFamily = "Catamaran")
  
}

