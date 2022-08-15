# get latest Oura data:
source("src/Oura_api.R")

library(pins)
library(tidyverse)
library(lubridate)
library(ggtext)
library(grid)
library(gridExtra)
library(ggridges)
library(png)

#oura <- pin_get("oura")

# Organize the data: ----

oura_values <- map_dfr(oura, function(df){
  df %>%
    select(Date, contains("score"), 
           where(~ is.numeric(.x) && length(unique(.x)) > 10)) %>%
    pivot_longer(., cols = -Date) %>%
    select(Date, name, value) %>%
    mutate(category = str_to_sentence(str_extract(name, pattern = "^.*(?=\\.)")),
           name = str_to_title(gsub("_", " ", str_extract(name, pattern = "(?<=\\.).*$"))))
}) %>%
  filter(!name %in% c("Midpoint Time", "To Target Miles", "To Target Km", "Midpoint At Delta",
                      "Bedtime Start Delta", "Bedtime End Delta", "Temperature Delta", 
                      "Temperature Trend Deviation")) %>%
  mutate(var_add = case_when(name == "Score" ~ "Total Score, %",
                             grepl("Score", name) ~ "Score, %",
                             name %in% c("Total", "Rem", "Deep", "Light", "Duration") & category == "Sleep" ~ "Duration, h",
                             name %in% c("Total", "Non Wear", "Inactive", "Low", "Medium", "High", "Rest",
                                         "Awake", "Onset Latency") ~ "Duration, min",
                             name == "Rmssd" ~ "Heart Rate Variability, msec",
                             grepl("Hr", name) ~ "Heart Rate, BPM",
                             grepl("Temperature", name) ~ "Temperature, degC",
                             grepl("Cal", name) ~ "Energy, kcal",
                             grepl("Met", name) ~ "Metabolic equivalent minutes, MET mins",
                             name == "Daily Movement" ~ "Distance, km",
                             grepl("Breath", name) ~ "Breathing Rate, cpm",
                             name %in% c("Efficiency", "Restless") ~ "Percentage, %",
                             TRUE ~ c("Count, n"),
                             )) %>%
  separate(col = "var_add", into=c("label", "unit"), sep=", ") %>%
  mutate(value = case_when(unit == "h" & category == "Sleep" ~ value / 3600,
                           unit == "min" & category == "Sleep" ~ value / 60,
                           unit == "km" ~ value / 1000,
                           TRUE ~ value),
         name = case_when(name == "Score" ~ paste(category, name),
                          grepl("Score", name) ~ str_extract(name,"(?<=Score ).*$"),
                          TRUE ~ name))

# Function to make beeswarm plots and filter data: ----
make.beeswarm <- function(df, hue = 0.55, hue_range = 0.25, height_per_metric=2){
  
  summ <- df %>%
    group_by(name) %>%
    summarise(median = median(value, na.rm=TRUE),
              Q1 = quantile(value, 0.25, na.rm = TRUE),
              Q3 = quantile(value, 0.75, na.rm = TRUE)) %>%
    mutate(name = fct_reorder(name, median, .desc=TRUE),
           median = ifelse(median >= 100, round(median), signif(median, 2)))
  
  if(is.na(hue)){
    hues <- base_col[levels(df$category)]
  } else{
    hues <- seq(hue, hue + hue_range, length.out = nlevels(summ$name)) %% 1
  }
  
  poly_df <- rbind(summ, summ) %>%
    pivot_longer(cols = c("Q1", "Q3"), names_to = "Quantile", values_to = "coord_y") %>%
    arrange(name, Quantile) %>%
    mutate(coord_x = rep(1:nrow(summ), each = 4) + c(-0.35, 0.35, 0.35, -0.35))
  
  ggplot(df, aes(x = name, y = value)) +
    geom_violin(scale = "width", fill = "transparent", color = "transparent", size = 0.2, width = 0.6) +
    map(summ$name, function(i){ 
      geom_polygon(data = subset(poly_df, name == i), 
                   aes(x = coord_x, y = coord_y), fill = "grey80")
    }) +
    geom_segment(data = summ, aes(x = as.numeric(name) + 0.45, xend = as.numeric(name) - 0.45,
                                  y = median, yend = median), col = "grey40", lwd = 1) +
    geom_violin(scale = "width", aes(fill = name), color="grey50", size = 0.2, width = 0.6) +
    ggbeeswarm::geom_quasirandom(aes(color=name), shape= 16, size = 1.2, width = 0.25, alpha = 0.5) +
    coord_flip(clip="off") +
    labs(x = NULL, y = paste0(unique(df$label), " (", unique(df$unit), ")")) +
    theme_minimal(base_family = "Comfortaa") +
    theme(plot.margin = unit(c(2, 2, 2, 2), "mm"),
          aspect.ratio = nlevels(df$name) * 1.5 / 10,
          panel.grid.major.x = element_line(color = "grey90", size = 0.2),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          plot.title.position = "plot",
          plot.title = element_markdown(face = "bold", size = 15, hjust = 0, color="grey10"),
          plot.subtitle = element_text(size = 11, hjust = 0, color="grey20"),
          axis.text.y = element_text(size=11, face="bold", color = "grey40"),
          axis.title.x = element_text(face = "bold", margin = margin(t = 6), color = "grey40", size = 10),
          axis.text.x = element_text(size = 8, color = "grey50", margin = margin(t = 2))) +
    geom_text(data = summ, aes(x = name, y = -Inf, 
                               label = paste("\nmedian:", median, unique(df$unit))), 
              vjust = 1, hjust = 1.05, color = "grey40", size = 3, family = "Comfortaa") +
    scale_color_manual(values = hsv(hues, 1, 0.85)) +
    scale_fill_manual(values = hsv(hues, 0.2, 1)) 
}

filter_df <- function(Cat, Lab){
  df <- oura_values %>%
    filter(category %in% Cat, label == Lab) %>%
    mutate(name = fct_reorder(name, value, na.rm=TRUE, .desc=TRUE))
}

# set a base color_hue for the categories:
base_col <- setNames(c(0.55, 0.75, 0.95), c("Sleep", "Activity", "Readiness"))

# Make 6 different plots:----

oura_beeswarms <- list()

#1) Single Plot with Total Scores: ----
oura_beeswarms[["Total_Scores"]] <- filter_df(Cat = c("Sleep", "Activity", "Readiness"), Lab = "Total Score") %>%
  mutate(category = fct_reorder(category, name, unique)) %>%
  make.beeswarm(., hue = NA, hue_range = 0.4) +
  ggtitle(paste0("Daily <span style='color:", hsv(base_col[["Sleep"]], 1, 0.8), 
                 ";'>Sleep</span>, <span style='color:", hsv(base_col[["Readiness"]], 1, 0.8), 
                 ";'>Readiness </span>and <span style='color:", hsv(base_col[["Activity"]], 1, 0.8), 
                 ";'>Activity </span>Scores"),
          subtitle = paste0("Measured with the Oura Ring (", length(unique(oura_values$Date)), " days)"))

#2) 3 Separate plots for Sleep / Activity / Readiness Scores: ----
oura_beeswarms[["Scores"]] <- c("Sleep", "Readiness", "Activity") %>%
  set_names() %>%
  map(function(x) {
  df <- filter_df(Cat = x, Lab = "Score")
  
  make.beeswarm(df, hue=base_col[x], hue_range = 0.05) +
    ggtitle(paste0("Parameters for Daily <span style='color:", hsv(base_col[x], 1, 0.8), ";'>", x, "</span> Score"),
            subtitle = paste0("Measured with the Oura Ring (", length(unique(df$Date)), " days)"))
})


# 3) 3 Separate plots for all Sleep/Activity/Readiness metrics ----

oura_beeswarms[["metrics"]] <- c("Sleep", "Readiness", "Activity") %>%
  set_names() %>%
  map(function(x) {
    df <- oura_values %>%
      filter(category == x,
             grepl("Score", label) == FALSE)
    
    if(nrow(df) > 0){
      list_of_plots <- unique(df$unit) %>%
        set_names() %>%
        map2(., seq_along(.), function(Unit, i){
        df_per_unit <- df %>%
          filter(category == x, unit == Unit) %>%
          mutate(name = fct_reorder(name, value, na.rm=TRUE, .desc=TRUE))

        make.beeswarm(df_per_unit, hue=base_col[x] + 0.05 * (i - 1), hue_range = 0)
      })
      
      plot <- egg::ggarrange(plots=list_of_plots, ncol=1, draw=FALSE)
      title <- grobTree(textGrob(substitute(paste("Distribution of various ", phantom(bold(x)), " metrics"), list(x=x)),
                                 gp = gpar(fontface = "bold", fontfamily = "Comfortaa", fontsize = 15), hjust = 1, x = 0.98),
                        textGrob(substitute(paste(phantom("Distribution of various "), bold(x), phantom(" metrics")), list(x=x)),
                                 gp = gpar(fontface = "bold", fontfamily = "Comfortaa", fontsize = 15, 
                                           col = hsv(base_col[x], 1, 0.8)), hjust = 1, x = 0.98))
      subtitle <- textGrob(paste0("Measured with the Oura Ring (", length(unique(df$Date)), " days)"),
                           gp = gpar(fontfamily = "Comfortaa", fontsize = 11), hjust = 1, vjust = 0, x = 0.98)
      
      plot_w_title <- grid.arrange(title, subtitle, plot,
                              heights = unit.c(grobHeight(title) + unit(2, "line"),
                                               grobHeight(subtitle) + unit(0.5, "line"),
                                               unit(1,"null")))
      return(plot_w_title)
    }
  })

# Problems with other approaches:
# Pathwork ignores fixed aspect ratio or set panel width
# gridExtra::grid.arrange:  heights work in combi with fixed panel heigth but panel wdth not aligned

# Function to make density plot Timestamps: ----

oura_times <- oura[["Sleep"]] %>%
  mutate(sleep.midpoint = as_datetime(sleep.bedtime_end - seconds(sleep.midpoint_time), tz="CET")) %>%
  pivot_longer(., cols = where(is.POSIXct)) %>%
  select(Date,name, value) %>%
  mutate(time = value,
         value = as.numeric(data.table::as.ITime(time))/3600,
         value = ifelse(value < 12, value + 24, value),
         name = str_to_title(gsub("_", " ", str_extract(name, pattern = "(?<=\\.).*$"))),
         name = fct_reorder(name, value, na.rm = TRUE, .desc=FALSE))

plot.timestamps <- function(df, hue){
  summary <- df %>%
    group_by(name) %>%
    summarise(median = median(value, na.rm = TRUE)) %>%
    mutate(time_char = gsub("^0", "", substr(data.table::as.ITime(median * 3600), 1, 5)),
           name = fct_reorder(name, median, .desc=FALSE))
  
  hues <- (seq(hue, hue + 0.05 *nlevels(summary$name), length.out = nlevels(summary$name))) %% 1
  
  title <- "Distribution of Sleep Times"
  subtitle <-paste0("Measured with the Oura Ring (", length(unique(df$Date)), " nights)")
  
  plot <- ggplot(df, aes(y = name)) + 
    geom_density_ridges(aes(x = value, fill = name, color = name),
                        alpha = 1, size = 0.2, scale = 2,
                        bandwidth = 0.2, rel_min_height = 1e-15,
                        jittered_points = TRUE, point_shape = 8, point_size = 0.7, point_stroke = 0.2) +
    coord_cartesian(clip = "off") +
    labs(x = "Time", y = NULL,title = title,subtitle = subtitle) +
    theme_minimal(base_family = "Comfortaa") +
    theme(plot.margin = unit(rep(2, 4), "mm"),
          panel.grid.major.x = element_line(color = "grey90", size = 0.2),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          plot.title.position = "plot",
          plot.title = element_markdown(face = "bold", size = 15, hjust = 0, color = "grey20"),
          plot.subtitle = element_text(size = 11, hjust =  0, color="grey30",
                                       margin = margin(b=5,unit = "mm")),
          axis.title.x = element_text(face = "bold", margin = margin(t = 7), color = "grey30", size = 10),
          axis.text.x = element_text(size = 8, color = "grey50", margin = margin(t = 4)),
          axis.text.y = element_text(size=11, face="bold", color = "grey30", vjust=-1),
          axis.ticks.x = element_line(color = "grey80", size = 0.2)) +
    scale_x_continuous(expand=c(0.02, 0.05), breaks = seq(20, 34),
                       labels = c("20:00", "21:00", "22:00", "23:00", "0:00", "1:00", "2:00", "3:00",
                                  "4:00", "5:00", "6:00", "7:00", "8:00", "9:00", "10:00")) +
    scale_y_discrete(expand = c(0.05,0)) +
    scale_fill_manual(values = hsv(hues, 0.3, 0.95)) +
    scale_color_manual("point_color", values = hsv(hues, 1, 0.6)) +
    geom_text(data = summary, aes(x = -Inf, y = name, label = paste0("median: ", time_char)), 
              vjust = 1, hjust = 1.08, color = "grey50", size = 3, family = "Comfortaa")

}

oura_beeswarms[["timestamps"]] <- plot.timestamps(oura_times, 0.631)

oura_plots <- pin_get("oura_plots")
oura_plots[["beeswarm"]] <- oura_beeswarms
pin(oura_plots, "oura_plots")

# function to add the 1cm x 1cm logo in top right corner, away from outer margin ----
# does not work.

add.logo <- function(p, img, plot_w_cm = NA, plot_h_cm = NA){
  
  logo <- rasterGrob(readPNG(img), interpolate=TRUE)
  
  if(is.na(plot_w_cm)){
    plot_w_cm <- dev.size("cm")[1]
  }
  if(is.na(plot_h_cm)){
    plot_h_cm <- dev.size("cm")[2]
  }
  
  gt <- ggplotGrob(p)
  
  panel_pos <- gt$layout[which(gt$layout$name == "panel"), ]
  space_y <- sum(convertUnit(gt$heights[-panel_pos$t], "mm", valueOnly=TRUE))
  space_x <- sum(convertUnit(gt$widths[-panel_pos$l], "mm", valueOnly=TRUE))
  
  from_top <- sum(convertUnit(gt$heights[seq(panel_pos$t - 1)], "mm", valueOnly=TRUE))
  from_right <- sum(convertUnit(gt$widths[-seq(panel_pos$l)], "mm", valueOnly=TRUE))
  
  panel_w <- 10 * plot_w_cm - space_x
  
  gb <- ggplot_build(p)
  
  if(!is.null(gb$plot$theme$aspect.ratio)){
    panel_h <- gb$plot$theme$aspect.ratio * panel_w
  } else{
    panel_h <- 10 * plot_h_cm - space_y
  }
  
  top_mar <- convertUnit(gb$plot$theme$plot.margin[1], "mm", valueOnly=TRUE)
  right_mar <- convertUnit(gb$plot$theme$plot.margin[2], "mm", valueOnly=TRUE)
  
  xrange <- gb$layout$panel_params[[1]]$x.range
  yrange <- gb$layout$panel_params[[1]]$y.range
  
  x_conv <- diff(xrange) /  panel_w #("npc/mm")
  y_conv <- diff(yrange) / panel_h #("npc/mm")
  
  logo_ymax <- yrange[2] + (from_top - top_mar) * y_conv
  logo_ymin <- logo_ymax - 10 * y_conv
  logo_xmax <- xrange[2] + (from_right - top_mar) * x_conv
  logo_xmin <- logo_xmax - 10 * x_conv
  
  p + annotation_custom(logo, xmin = logo_xmin, xmax = logo_xmax,
                        ymin = logo_ymin, ymax = logo_ymax)
}

