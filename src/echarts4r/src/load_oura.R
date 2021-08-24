library(keyring)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)

get.oura.api <- function(startdate, token, username, password, timezone){
  connect.api <- function(startdate, token, username, password, data_type){
    url <- paste0("https://api.ouraring.com/v1/", data_type, "?start=", 
                  startdate, "&access_token=", token)
    get_data <- GET(url, authenticate(username, password, type = "basic"))
    content <- content(get_data, "text")
    json <- fromJSON(content, flatten = TRUE)
    df <- as.data.frame(json)
  }
  organize.df <- function(df, timezone){
    all_dates <- data.frame(Date = seq(as_date(df[1,1]), 
                                       as_date(df[nrow(df), 1]),
                                       by = "days"))
    
    df <- df %>%
      mutate(across(ends_with("summary_date"), as_date),
             across(ends_with(c("_start", "_end")), as_datetime, tz = timezone)) %>%
      left_join(all_dates, ., by = c("Date" = colnames(df)[1]))
  }
  
  data_types <- c("sleep", "activity", "readiness")
  oura <- map(data_types, connect.api, 
              startdate = startdate, 
              token = token, 
              username = username,
              password = password) %>%
    set_names(str_to_title(data_types))
  
  map(oura, organize.df, timezone = timezone)
}

oura <- get.oura.api(startdate = "2020-01-29", 
                     token = key_get("Token", keyring= "oura"), 
                     username = key_get("Username", keyring= "oura"),
                     password = key_get("Password", keyring= "oura"),
                     timezone = "Europe/Amsterdam")

save(oura, file = "data/oura.RData")
keyring_lock("oura")
