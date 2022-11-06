library(tidyverse)
library(echarts4r)
library(htmlwidgets)

source("src/Oura_api.R")

source("src/echarts4r/src/e_gauge_Scores.R")
source("src/echarts4r/src/e_bar_Scores.R")
source("src/echarts4r/src/e_pie.R")
source("src/echarts4r/src/e_stacked_bar.R")
source("src/echarts4r/src/e_calendar.R")
source("src/echarts4r/src/e_trend.R")

oura_plots <- pin_get("oura_plots")

oura_plots[["echarts4r"]][["e_gauge"]] <- make.egauge.oura()
oura_plots[["echarts4r"]][["e_bar"]][["sleep"]] <- make.e_bar.scores("Sleep", 0.55)  
oura_plots[["echarts4r"]][["e_bar"]][["activity"]] <- make.e_bar.scores("Activity", 0.65)
oura_plots[["echarts4r"]][["e_bar"]][["readiness"]] <- make.e_bar.scores("Readiness", 0.75)

oura_plots[["echarts4r"]][["e_pie"]][["sleep"]] <- make.pie.sleep()
oura_plots[["echarts4r"]][["e_pie"]][["activity"]] <-  make.pie.activity()
oura_plots[["echarts4r"]][["e_stacked_bar"]][["sleep"]] <- make.stacked.bar.sleep()
oura_plots[["echarts4r"]][["e_stacked_bar"]][["activity"]] <- make.stacked.bar.activity()
oura_plots[["echarts4r"]][["e_calendar"]][["sleep_rmssd"]] <- make.hm.cal(
  oura[["Sleep"]], "sleep.rmssd", hue = 0.55, min = 75, max = 125,
  title = "Mean HRV over the Years",
  subtitle = "Measured with Oura Ring") %>%
  e_heatmap(sleep.rmssd, coord_system = "calendar") 
oura_plots[["echarts4r"]][["e_calendar"]][["activity_steps"]] <- make.hm.cal(
  oura[["Activity"]], "activity.steps", hue = 0.65, max = 25000,
  title = "Daily Steps over the Years",
  subtitle = "Measured with Oura Ring") %>%
  e_heatmap(activity.steps, coord_system = "calendar")
  
oura_plots[["echarts4r"]][["e_trend"]][["sleep"]] <- Make.trend(
  oura[["Sleep"]], column = sleep.rmssd, ylab = "HRV (msec)",
                title = "Average Heart Rate Variability (rmssd) during sleep",
                subtitle = "Measured with the Oura Ring",
                hue = 0.55, ymax = 180, ymin = 60, top_mar = 150,
           plot_width = 800, plot_height = 600)

oura_plots[["echarts4r"]][["e_trend"]][["activity"]] <- Make.trend(
  oura[["Activity"]], column = activity.steps, ylab = "Steps (xx)",
  title = "Daily Step Count",
  subtitle = "Measured with the Oura Ring",
  hue = 0.65, ymax = 40000, ymin = 0, top_mar = 150,
  plot_width = 960, plot_height = 600)


pin(oura_plots, "oura_plots")
