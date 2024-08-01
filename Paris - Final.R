library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(glue)
library(dplyr)
library(gtExtras)
library(gt)

#Individual Entries - 2024 Paris Games
{
  entries = read.csv("/Users/connorbradley/Desktop/swim/olympics2024entries.csv")
  entries$concat = paste0(entries$last,", ",entries$first," ",entries$ro, " ",entries$sex)
}

#Individual Entrants - 2024 Paris Games
{
  entrants = read.csv("/Users/connorbradley/Desktop/swim/entrants.csv")
  
  entrants = entrants %>%
    mutate(group = cumsum(Country == "VIEW PROFILE")) %>%
    filter(Country != "VIEW PROFILE")
  
  entrants = entrants %>%
    group_by(group) %>%
    mutate(row_num = row_number()) %>%
    pivot_wider(names_from = row_num, values_from = Country, names_prefix = "V") %>%
    ungroup() %>%
    select(-group) %>% 
    mutate(ro = substr(V1, nchar(V1) - 2, nchar(V1))) %>% 
    select(first = V3, last = V4, sex = V5, ro) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex))
}

#Mens Top Times - 2024 Paris Lead-up (2024 Times Only)
{
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=M&distance=100&stroke=BACKSTROKE&poolConfiguration=LCM&year=2024&startDate=&endDate=&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  m100back_24 = read.csv(url,stringsAsFactors = FALSE)
  m100back_24 = m100back_24 %>% 
    mutate(stroke="Back") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field,date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=M&distance=100&stroke=BREASTSTROKE&poolConfiguration=LCM&year=2024&startDate=&endDate=&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  m100breast_24 = read.csv(url,stringsAsFactors = FALSE)
  m100breast_24 = m100breast_24 %>% 
    mutate(stroke="Breast") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=M&distance=100&stroke=BUTTERFLY&poolConfiguration=LCM&year=2024&startDate=&endDate=&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  m100fly_24 = read.csv(url,stringsAsFactors = FALSE)
  m100fly_24 = m100fly_24 %>% 
    mutate(stroke="Fly") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=M&distance=100&stroke=FREESTYLE&poolConfiguration=LCM&year=2024&startDate=&endDate=&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  m100free_24 = read.csv(url,stringsAsFactors = FALSE)
  m100free_24 = m100free_24 %>% 
    mutate(stroke="Free") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  mentoptimes_24 = rbind(m100back_24,m100breast_24,m100fly_24,m100free_24)
  mentoptimes_24_filtered = mentoptimes_24 %>% filter(concat %in% entrants$concat)
  
}

#Womens Top Times - 2024 Paris Lead-up (2024 Times Only) DONE!!
{
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=F&distance=100&stroke=BACKSTROKE&poolConfiguration=LCM&year=2024&startDate=&endDate=&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  w100back_24 = read.csv(url,stringsAsFactors = FALSE)
  w100back_24 = w100back_24 %>% 
    mutate(stroke="Back") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field,date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=F&distance=100&stroke=BREASTSTROKE&poolConfiguration=LCM&year=2024&startDate=&endDate=&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  w100breast_24 = read.csv(url,stringsAsFactors = FALSE)
  w100breast_24 = w100breast_24 %>% 
    mutate(stroke="Breast") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=F&distance=100&stroke=BUTTERFLY&poolConfiguration=LCM&year=2024&startDate=&endDate=&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  w100fly_24 = read.csv(url,stringsAsFactors = FALSE)
  w100fly_24 = w100fly_24 %>% 
    mutate(stroke="Fly") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=F&distance=100&stroke=FREESTYLE&poolConfiguration=LCM&year=2024&startDate=&endDate=&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  w100free_24 = read.csv(url,stringsAsFactors = FALSE)
  w100free_24 = w100free_24 %>% 
    mutate(stroke="Free") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  womentoptimes_24 = rbind(w100back_24,w100breast_24,w100fly_24,w100free_24)
  womentoptimes_24_filtered = womentoptimes_24 %>% filter(concat %in% entrants$concat)
  
}

#Calculate Relays - 2024 Paris Games
{
  
  totaltop_24 = rbind(mentoptimes_24_filtered,womentoptimes_24_filtered)
  
  
  totaltop_24 = totaltop_24 %>% 
    mutate(
      time_m = ifelse(str_detect(time_field, ":"), 
                      as.numeric(str_extract(time_field, "^\\d{1,2}")),0),  
      time_s = ifelse(str_detect(time_field, ":"), 
                      as.numeric(str_extract(time_field, "(?<=:)\\d{2}")), 
                      as.numeric(str_extract(time_field, "^[0-9]{1,2}"))),
      time_f_raw = str_extract(time_field, "(?<=\\.)[0-9]{1,2}"),
      time_f = ifelse(is.na(time_f_raw) | time_f_raw == "00", "00", 
                      if_else(nchar(time_f_raw) == 1, paste0(time_f_raw, "0"), time_f_raw))) %>% 
    mutate(time_seconds = as.numeric(time_m * 60) + as.numeric(time_s) + as.numeric(time_f) / 100) %>% 
    select(first, last, ro, sex, stroke, time_seconds, date)
  
  
  mmr_countries_24 = c("AUS","CAN","CHN",
                    "FRA","GBR","ITA",
                    "JPN","USA","GER",
                    "BRA","NED","ISR",
                    "POL","SWE","KOR",
                    "GRE")
  
  convert_time = function(seconds) {
    minutes = floor(seconds / 60)
    remaining_seconds = seconds %% 60
    formatted_time = sprintf("%02d:%05.2f", minutes, remaining_seconds)
    return(formatted_time)
  }
  
  all_relays_24 = data.frame()
  for(r in mmr_countries_24){
    
    athletes = totaltop_24 %>% filter(ro == r)
    
    lineups = data.frame(
      Back = character(),
      Breast = character(),
      Fly = character(),
      Free = character(),
      total_time = numeric(),
      stringsAsFactors = FALSE
    )
    
    Back = athletes[athletes$stroke == "Back", ]
    Breast = athletes[athletes$stroke == "Breast", ]
    Fly = athletes[athletes$stroke == "Fly", ]
    Free = athletes[athletes$stroke == "Free", ]
    
    # Loop through all combinations
    for (i in 1:nrow(Back)) {
      for (j in 1:nrow(Breast)) {
        for (k in 1:nrow(Fly)) {
          for (l in 1:nrow(Free)) {
            
            team = rbind(Back[i, ], Breast[j, ], Fly[k, ], Free[l, ])
            if (any(is.na(team$sex))) {
              next
            }
            
            # Check for duplicate athletes in different strokes
            if (any(duplicated(team[, c("first", "last")]))) {
              next
            }
            
            # Check if the team has 2 males and 2 females
            if (sum(team$sex == "Male") == 2 && sum(team$sex == "Female") == 2) {
              total_time <- sum(team$time_seconds)
              
              Back_leg = paste0(team[1, "last"], ", ", team[1, "first"], " (", team[1, "sex"], ") ", formatC(team[1, "time_seconds"], format = "f", digits = 2))
              Breast_leg = paste0(team[2, "last"], ", ", team[2, "first"], " (", team[2, "sex"], ") ", formatC(team[2, "time_seconds"], format = "f", digits = 2))
              Fly_leg <- paste0(team[3, "last"], ", ", team[3, "first"], " (", team[3, "sex"], ") ", formatC(team[3, "time_seconds"],, format = "f", digits = 2))
              Free_leg <- paste0(team[4, "last"], ", ", team[4, "first"], " (", team[4, "sex"], ") ", formatC(team[4, "time_seconds"], format = "f", digits = 2))
              newtime = convert_time(total_time)
              country = mmr_countries_24[r]
              
              lineups = rbind(lineups, data.frame(
                Country = r,
                Time = newtime,
                Back = Back_leg,
                Breast = Breast_leg,
                Fly = Fly_leg,
                Free = Free_leg,
                stringsAsFactors = FALSE
              ))
            }
          }
        }
      }
    }
    all_relays_24 = rbind(all_relays_24, lineups)
  }
  
  all_relays_24 = all_relays_24 %>% mutate(back_s = if_else(grepl("Female",Back),"F","M"),
                                     breast_s = if_else(grepl("Female",Breast),"F","M"),
                                     fly_s = if_else(grepl("Female",Fly),"F","M"),
                                     free_s = if_else(grepl("Female",Free),"F","M")) %>% 
    mutate(Order = paste0(back_s,breast_s,fly_s,free_s)) %>% 
    select(-c(back_s,breast_s,fly_s,free_s))
  
  best_24 = all_relays_24 %>% arrange(Time) %>% group_by(Country) %>% slice_head(n=1)
  best_24_usa = all_relays_24 %>% arrange(Time) %>% filter(Country == "USA")
  
  ustop_24 = totaltop_24 %>%  filter(ro == "USA") %>% arrange(time_seconds) %>% group_by(stroke,sex) %>% mutate(rank = row_number()) %>% slice_head(n=1) %>% ungroup() %>% 
    select(First_Name = first, Last_Name = last, Country = ro,
           Sex = sex, Stroke = stroke, Swim_Time = time_seconds)
  
  
  
}


#Mens Top Times - 2024 Paris Lead-up (2024 Times Only)
{
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=M&distance=100&stroke=BACKSTROKE&poolConfiguration=LCM&year=&startDate=07%2F21%2F2024&endDate=&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  m100back_games = read.csv(url,stringsAsFactors = FALSE)
  m100back_games = m100back_games %>% 
    mutate(stroke="Back") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field,date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=M&distance=100&stroke=BREASTSTROKE&poolConfiguration=LCM&year=&startDate=07%2F21%2F2024&endDate=&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  m100breast_games = read.csv(url,stringsAsFactors = FALSE)
  m100breast_games = m100breast_games %>% 
    mutate(stroke="Breast") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=M&distance=100&stroke=BUTTERFLY&poolConfiguration=LCM&year=&startDate=07%2F21%2F2024&endDate=&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  m100fly_games = read.csv(url,stringsAsFactors = FALSE)
  m100fly_games = m100fly_games %>% 
    mutate(stroke="Fly") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=M&distance=100&stroke=FREESTYLE&poolConfiguration=LCM&year=&startDate=07%2F21%2F2024&endDate=&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  m100free_games = read.csv(url,stringsAsFactors = FALSE)
  m100free_games = m100free_games %>% 
    mutate(stroke="Free") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  mentoptimes_games = rbind(m100back_games,m100breast_games,m100fly_games,m100free_games)
  mentoptimes_games_filtered = mentoptimes_games %>% filter(concat %in% entrants$concat)
  
}

#Womens Top Times - 2024 Paris Lead-up (2024 Times Only)
{
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=F&distance=100&stroke=BACKSTROKE&poolConfiguration=LCM&year=&startDate=07%2F21%2F2024&endDate=&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  w100back_games = read.csv(url,stringsAsFactors = FALSE)
  w100back_games = w100back_games %>% 
    mutate(stroke="Back") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field,date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=F&distance=100&stroke=BREASTSTROKE&poolConfiguration=LCM&year=&startDate=07%2F21%2F2024&endDate=&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  w100breast_games = read.csv(url,stringsAsFactors = FALSE)
  w100breast_games = w100breast_games %>% 
    mutate(stroke="Breast") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=F&distance=100&stroke=BUTTERFLY&poolConfiguration=LCM&year=&startDate=07%2F21%2F2024&endDate=&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  w100fly_games = read.csv(url,stringsAsFactors = FALSE)
  w100fly_games = w100fly_games %>% 
    mutate(stroke="Fly") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=F&distance=100&stroke=FREESTYLE&poolConfiguration=LCM&year=&startDate=07%2F21%2F2024&endDate=&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  w100free_games = read.csv(url,stringsAsFactors = FALSE)
  w100free_games = w100free_games %>% 
    mutate(stroke="Free") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  womentoptimes_games = rbind(w100back_games,w100breast_games,w100fly_games,w100free_games)
  womentoptimes_games_filtered = womentoptimes_games %>% filter(concat %in% entrants$concat)
  
}

#Calculate Relays - 2024 Paris Games
{
  
  totaltop_games = rbind(mentoptimes_games_filtered,womentoptimes_games_filtered)
  
  
  totaltop_games = totaltop_games %>% 
    mutate(
      time_m = ifelse(str_detect(time_field, ":"), 
                      as.numeric(str_extract(time_field, "^\\d{1,2}")),0),  
      time_s = ifelse(str_detect(time_field, ":"), 
                      as.numeric(str_extract(time_field, "(?<=:)\\d{2}")), 
                      as.numeric(str_extract(time_field, "^[0-9]{1,2}"))),
      time_f_raw = str_extract(time_field, "(?<=\\.)[0-9]{1,2}"),
      time_f = ifelse(is.na(time_f_raw) | time_f_raw == "00", "00", 
                      if_else(nchar(time_f_raw) == 1, paste0(time_f_raw, "0"), time_f_raw))) %>% 
    mutate(time_seconds = as.numeric(time_m * 60) + as.numeric(time_s) + as.numeric(time_f) / 100) %>% 
    select(first, last, ro, sex, stroke, time_seconds, date)
  
  
  mmr_countries_24 = c("AUS","CAN","CHN",
                       "FRA","GBR","ITA",
                       "JPN","USA","GER",
                       "BRA","NED","ISR",
                       "POL","SWE","KOR",
                       "GRE")
  
  convert_time = function(seconds) {
    minutes = floor(seconds / 60)
    remaining_seconds = seconds %% 60
    formatted_time = sprintf("%02d:%05.2f", minutes, remaining_seconds)
    return(formatted_time)
  }
  
  all_relays_games = data.frame()
  for(r in mmr_countries_24){
    
    athletes = totaltop_games %>% filter(ro == r)
    
    lineups = data.frame(
      Back = character(),
      Breast = character(),
      Fly = character(),
      Free = character(),
      total_time = numeric(),
      stringsAsFactors = FALSE
    )
    
    Back = athletes[athletes$stroke == "Back", ]
    Breast = athletes[athletes$stroke == "Breast", ]
    Fly = athletes[athletes$stroke == "Fly", ]
    Free = athletes[athletes$stroke == "Free", ]
    
    # Loop through all combinations
    for (i in 1:nrow(Back)) {
      for (j in 1:nrow(Breast)) {
        for (k in 1:nrow(Fly)) {
          for (l in 1:nrow(Free)) {
            
            team = rbind(Back[i, ], Breast[j, ], Fly[k, ], Free[l, ])
            if (any(is.na(team$sex))) {
              next
            }
            
            # Check for duplicate athletes in different strokes
            if (any(duplicated(team[, c("first", "last")]))) {
              next
            }
            
            # Check if the team has 2 males and 2 females
            if (sum(team$sex == "Male") == 2 && sum(team$sex == "Female") == 2) {
              total_time <- sum(team$time_seconds)
              
              Back_leg = paste0(team[1, "last"], ", ", team[1, "first"], " (", team[1, "sex"], ") ", formatC(team[1, "time_seconds"], format = "f", digits = 2))
              Breast_leg = paste0(team[2, "last"], ", ", team[2, "first"], " (", team[2, "sex"], ") ", formatC(team[2, "time_seconds"], format = "f", digits = 2))
              Fly_leg <- paste0(team[3, "last"], ", ", team[3, "first"], " (", team[3, "sex"], ") ", formatC(team[3, "time_seconds"],, format = "f", digits = 2))
              Free_leg <- paste0(team[4, "last"], ", ", team[4, "first"], " (", team[4, "sex"], ") ", formatC(team[4, "time_seconds"], format = "f", digits = 2))
              newtime = convert_time(total_time)
              country = mmr_countries_24[r]
              
              lineups = rbind(lineups, data.frame(
                Country = r,
                Time = newtime,
                Back = Back_leg,
                Breast = Breast_leg,
                Fly = Fly_leg,
                Free = Free_leg,
                stringsAsFactors = FALSE
              ))
            }
          }
        }
      }
    }
    all_relays_games = rbind(all_relays_games, lineups)
  }
  
  all_relays_games = all_relays_games %>% mutate(back_s = if_else(grepl("Female",Back),"F","M"),
                                           breast_s = if_else(grepl("Female",Breast),"F","M"),
                                           fly_s = if_else(grepl("Female",Fly),"F","M"),
                                           free_s = if_else(grepl("Female",Free),"F","M")) %>% 
    mutate(Order = paste0(back_s,breast_s,fly_s,free_s)) %>% 
    select(-c(back_s,breast_s,fly_s,free_s))
  
  best_games = all_relays_games %>% arrange(Time) %>% group_by(Country) %>% slice_head(n=1)
  best_games_usa = all_relays_games %>% arrange(Time) %>% filter(Country == "USA")
  
  ustop_games = totaltop_games %>%  filter(ro == "USA") %>% arrange(time_seconds) %>% group_by(stroke,sex) %>% mutate(rank = row_number()) %>% slice_head(n=1) %>% ungroup() %>% 
    select(First_Name = first, Last_Name = last, Country = ro,
           Sex = sex, Stroke = stroke, Swim_Time = time_seconds)
  
}

#Calculate Time Difference Average
{
totaltop_24sum = totaltop_24 %>% group_by(sex,stroke) %>% summarise(avg = round(mean(time_seconds),2))
totaltop24sum = totaltop_24sum %>% pivot_wider(names_from = sex,values_from = avg) %>% 
  mutate(pct = round((Female-Male)/Male,4)) %>% mutate(diff = round(pct*Male,2))
}
                                                       
  
  
