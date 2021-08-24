make.e_pie <- function(category, columns, hue, divide_by, title, radius = c(0, "75%")){
  pattern <- paste0("(?<=", tolower(category), "\\.).*$")
  
  df <- oura[[category]] %>%
    select(all_of(columns)) %>%
    pivot_longer(., cols = everything()) %>%
    group_by(name) %>%
    summarise(value = mean(value, na.rm = TRUE) / divide_by) %>%
    mutate(name = str_to_title(str_extract(name, pattern = pattern))) %>%
    arrange(value)
  
  df %>%
    e_charts(name) %>%
    e_pie(value, 
          legend = FALSE,
          radius = radius) %>%
    e_title(title,
            textStyle = list(fontSize = 28)) %>%
    e_color(color = hsv(h = hue, s = c(0.8, 0.4, 1, 0.6, 0.1), 
                        v = c(0.3, 1, 0.6, 0.85, 0.45))) %>%
    e_text_style(fontFamily = "Catamaran") 
}

make.pie.sleep <- function(){
  make.e_pie("Sleep", columns = c("sleep.deep", "sleep.rem", 
                                  "sleep.awake", "sleep.light"),
             hue = 0.55, divide_by = 3600, 
             radius = c(0, "60%"),
             title = "Sleep Distribution") %>%
    e_tooltip(trigger= "item",
              formatter = htmlwidgets::JS(
                "function(params){
                return('<strong>' + params.name + ': '  + '</strong>' + 
                Math.floor(params.value) + 'h:' +
                Math.round(params.value % 1 * 60) + 'm')
}"
              )) %>%
    saveWidget(., selfcontained = TRUE,
               file = file.path("output", "echarts_oura", "e_pie_sleep.html"),
               title = "SleepPie")
}
make.pie.sleep()

make.pie.activity <- function(){
  make.e_pie("Activity", columns = c("activity.rest", "activity.inactive", "activity.low", 
                                     "activity.medium", "activity.high"),
             hue = 0.65, divide_by = 1, title = "Activity Distribution",
             radius = c("30%", "60%")) %>%
    e_tooltip(trigger= "item",
              formatter = htmlwidgets::JS(
                "function(params){
                return('<strong>' + params.name + ': ' + '</strong>' + 
                params.value.toFixed(0) + ' minutes per day') 
}"
              )) %>%
    saveWidget(., selfcontained = TRUE,
               file = file.path("output", "echarts_oura", "e_pie_activity.html"),
               title = "ActivityDonut")
}
make.pie.activity()

