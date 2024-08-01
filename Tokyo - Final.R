
#Individual Entrants - 2021 Tokyo Games
{
  entrants2020 = read.csv("/Users/connorbradley/Desktop/swim/entrants2020.csv")
  
  entrants2020 = entrants2020 %>%
    mutate(group = cumsum(Country == "VIEW PROFILE")) %>%
    filter(Country != "VIEW PROFILE")
  
  entrants2020 = entrants2020 %>%
    group_by(group) %>%
    mutate(row_num = row_number()) %>%
    pivot_wider(names_from = row_num, values_from = Country, names_prefix = "V") %>%
    ungroup() %>%
    select(-group) %>% 
    mutate(ro = substr(V1, nchar(V1) - 2, nchar(V1))) %>% 
    select(first = V3, last = V4, sex = V5, ro) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex))
}


#Mens Top Times - 2021 Tokyo Lead-up (2021 Times Only)
{
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=M&distance=100&stroke=BACKSTROKE&poolConfiguration=LCM&year=&startDate=01%2F01%2F2021&endDate=07%2F31%2F2021&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  m100back_20 = read.csv(url,stringsAsFactors = FALSE)
  m100back_20 = m100back_20 %>% 
    mutate(stroke="Back") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field,date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=M&distance=100&stroke=BREASTSTROKE&poolConfiguration=LCM&year=&startDate=01%2F01%2F2021&endDate=07%2F31%2F2021&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  m100breast_20 = read.csv(url,stringsAsFactors = FALSE)
  m100breast_20 = m100breast_20 %>% 
    mutate(stroke="Breast") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=M&distance=100&stroke=BUTTERFLY&poolConfiguration=LCM&year=&startDate=01%2F01%2F2021&endDate=07%2F31%2F2021&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  m100fly_20 = read.csv(url,stringsAsFactors = FALSE)
  m100fly_20 = m100fly_20 %>% 
    mutate(stroke="Fly") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=M&distance=100&stroke=FREESTYLE&poolConfiguration=LCM&year=&startDate=01%2F01%2F2021&endDate=07%2F31%2F2021&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  m100free_20 = read.csv(url,stringsAsFactors = FALSE)
  m100free_20 = m100free_20 %>% 
    mutate(stroke="Free") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  mentoptimes_20 = rbind(m100back_20,m100breast_20,m100fly_20,m100free_20)
  mentoptimes_20_filtered = mentoptimes_20 %>% filter(concat %in% entrants2020$concat)
  
}

#Womens Top Times - 2021 Tokyo Lead-up (2021 Times Only) DONE!!
{
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=F&distance=100&stroke=BACKSTROKE&poolConfiguration=LCM&year=&startDate=01%2F01%2F2021&endDate=07%2F31%2F2021&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  w100back_20 = read.csv(url,stringsAsFactors = FALSE)
  w100back_20 = w100back_20 %>% 
    mutate(stroke="Back") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field,date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=F&distance=100&stroke=BREASTSTROKE&poolConfiguration=LCM&year=&startDate=01%2F01%2F2021&endDate=07%2F31%2F2021&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  w100breast_20 = read.csv(url,stringsAsFactors = FALSE)
  w100breast_20 = w100breast_20 %>% 
    mutate(stroke="Breast") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=F&distance=100&stroke=BUTTERFLY&poolConfiguration=LCM&year=&startDate=01%2F01%2F2021&endDate=07%2F31%2F2021&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  w100fly_20 = read.csv(url,stringsAsFactors = FALSE)
  w100fly_20 = w100fly_20 %>% 
    mutate(stroke="Fly") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  url = "https://api.worldaquatics.com/fina/rankings/swimming/report/csv?gender=F&distance=100&stroke=FREESTYLE&poolConfiguration=LCM&year=&startDate=01%2F01%2F2021&endDate=07%2F31%2F2021&timesMode=BEST_TIMES&regionId=&countryId=&pageSize=200"
  w100free_20 = read.csv(url,stringsAsFactors = FALSE)
  w100free_20 = w100free_20 %>% 
    mutate(stroke="Free") %>% 
    select(name = full_name_computed, date = swim_date,ro = team_code,sex = gender,time_field = swim_time,stroke, date = swim_date) %>% 
    mutate(sex = if_else(sex=="M","Male","Female")) %>% 
    separate(name, into = c("last", "first"), sep = ", ", remove = FALSE) %>% 
    select(first,last,ro,sex,stroke,time_field, date) %>% 
    mutate(concat = paste0(last,", ", first," ", ro, " ", sex)) %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  womentoptimes_20 = rbind(w100back_20,w100breast_20,w100fly_20,w100free_20)
  womentoptimes_20_filtered = womentoptimes_20 %>% filter(concat %in% entrants2020$concat)
  
}

#Calculate Relays - 2021 Tokyo Games
{
  
  totaltop_20 = rbind(mentoptimes_20_filtered,womentoptimes_20_filtered)
  
  
  totaltop_20 = totaltop_20 %>% 
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
  
  
  mmr_countries_20 = c("GBR","USA","CHN",
                         "AUS","ITA","NED",
                         "ROC","ISR","JPN",
                         "GER","GRE","BLR",
                         "HUN","CAN","BRA",
                         "POL")
  
  convert_time = function(seconds) {
    minutes = floor(seconds / 60)
    remaining_seconds = seconds %% 60
    formatted_time = sprintf("%02d:%05.2f", minutes, remaining_seconds)
    return(formatted_time)
  }
  
  all_relays_20 = data.frame()
  for(r in mmr_countries_20){
    
    athletes = totaltop_20 %>% filter(ro == r)
    
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
              country = mmr_countries_20[r]
              
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
    all_relays_20 = rbind(all_relays_20, lineups)
  }
  
  all_relays_20 = all_relays_20 %>% mutate(back_s = if_else(grepl("Female",Back),"F","M"),
                                           breast_s = if_else(grepl("Female",Breast),"F","M"),
                                           fly_s = if_else(grepl("Female",Fly),"F","M"),
                                           free_s = if_else(grepl("Female",Free),"F","M")) %>% 
    mutate(Order = paste0(back_s,breast_s,fly_s,free_s)) %>% 
    select(-c(back_s,breast_s,fly_s,free_s))
  
  best_20 = all_relays_20 %>% arrange(Time) %>% group_by(Country) %>% slice_head(n=1)
  best_20_usa = all_relays_20 %>% arrange(Time) %>% filter(Country == "USA")
  
  ustop_20 = totaltop_20 %>%  filter(ro == "USA") %>% arrange(time_seconds) %>% group_by(stroke,sex) %>% mutate(rank = row_number()) %>% slice_head(n=1) %>% ungroup() %>% 
    select(First_Name = first, Last_Name = last, Country = ro,
           Sex = sex, Stroke = stroke, Swim_Time = time_seconds)
  
}