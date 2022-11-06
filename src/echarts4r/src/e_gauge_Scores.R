make.egauge.oura <- function(r){
  
  make.gauge <- function(e, category, hue, center = c("50%", "50%"), r = "80%", width = 30){
    colors <- hsv(h=hue, s = c(0.2, 0.6, 1 , 1), v= c(1, 0.85, 0.6, 0.3))
    value <- as.numeric(rev(oura[[category]][[paste0(tolower(category), ".score")]])[1])
    
    e %>%
      e_gauge(value, 
              name = category,
              radius = r,
              center = center,
              axisLine = list(
                lineStyle = list(
                  width  = width,
                  color=list(
                    c(0.6, colors[1]),
                    c(0.8, colors[2]),
                    c(1, colors[3])
                  )
                )),
              axisTick =list(
                distance = -width,
                length = 6,
                lineStyle= list(color= '#fff', width= 1)),
              splitLine = list(
                distance = -width,
                length = width,
                lineStyle = list(color= '#fff', width = 3)),
              axisLabel = list(distance = width + 8,
                               color = colors[2],
                               fontSize = 14,
                               fontFamily = "Catamaran"),
              anchor = list(
                show = TRUE,
                showAbove = TRUE,
                size = 24,
                itemStyle = list(
                  borderColor = colors[4],
                  borderWidth = 8)
              ),
              pointer = list(
                width = 8,
                itemStyle = list(color = "auto")),
              title = list(
                offsetCenter = c(0, "-135%"),
                color = colors[4],
                fontSize = 24,
                fontFamily = "Catamaran",
                fontWeight = "bold"),
              tooltip = list(
                borderColor = colors[3],
                borderWidth = 3,
                textStyle = list(color = colors[4])
              ),
              detail = list(formatter="{value}%",
                            offsetCenter = c(0, "100%"),
                            color = "auto",
                            fontSize = 42,
                            fontFamily = "Catamaran",
                            valueAnimation = TRUE))
  }
  
  e <- e_charts() %>%
    make.gauge("Sleep", hue = 0.55, center = c("16.7%", "60%"), r = r, width = 40) %>%
    make.gauge("Activity", 0.65, center = c("50%", "60%"), r = r, width = 40) %>%
    make.gauge("Readiness", 0.75, center = c("83.3%", "60%"), r = r, width = 40) %>%
    e_title("Sleep, Activity and Readiness Scores", 
            subtext= paste0("Measured with Oura Ring (",
                            strftime(rev(oura[[1]]$Date)[1], format = "%d %b %Y"), ")"),
            padding = c(20, 20, 0, 20),
            textStyle = list(fontSize = 36, height = "50%"),
            subtextStyle = list(fontSize = 24)) %>%
    e_grid(top = 200, width = "100%", height = 50) %>%
    e_text_style(fontFamily = "Catamaran") %>%
    e_tooltip(borderColor = hsv(c(0.55, 0.65, 0.75), 1, 1),
              formatter = htmlwidgets::JS(
                "function(params){
                return('<strong>' + params.name + ' Score: ' + '</strong>' + params.value + '%')
}")) 
}



