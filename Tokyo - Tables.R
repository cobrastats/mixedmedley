library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(glue)
library(dplyr)
library(gtExtras)
library(gt)

#TABLE PREP - 2021 - US Top Times by Sex
{
  ustop_20_pivot = ustop_20 %>% 
    mutate(combined = paste0("<div style='line-height: 1.4;'>",First_Name,"<br>",Last_Name,"<br>","<span style='font-weight: bold;'>", Swim_Time, "</span><br>")) %>% 
    select(combined,Stroke,Sex) %>% 
    pivot_wider(names_from = Stroke,values_from = combined)
  
  new_row <- data.frame(
    Sex = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>Time<br>Diff.",
    Back = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>5.67",
    Breast = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>6.58",
    Fly = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>6.21",
    Free = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>5.97")
  
  ustop_20_pivot = rbind(ustop_20_pivot,new_row)
}

#TABLE  - 2021 - US Top Times by Sex
{
  title_header <- glue(
    "<div style='display: flex; justify-content: space-between; align-items: center;'>
     <div style='flex-grow: 1;line-height: 1.5;'>
       <span style='font-family: Montserrat, sans-serif;font-weight: bold; font-size: 28px; line-height: 0.6;'>Top 100m Performances</span><br>
       <span style='font-family: Montserrat, sans-serif;font-size: 11px; font-weight: normal; line-height: 0.3;'>USA Swimmers (based on best flat-start times since 01/01/2021)</span>
     </div>
     <div>
       <img src='https://i.imgur.com/XYuCYcY.png' style='height: 70px; width: auto; vertical-align: right;'>
     </div>
   </div>"
  )
  
  gms = ustop_20_pivot %>% 
    mutate(Sex = gsub("Male","<div style='line-height: 1.2;font-size: 18px;'><span style='font-weight: bold;'>Top<br>Male<br>2021",Sex)) %>% 
    mutate(Sex = gsub("Female","<div style='line-height: 1.2;font-size: 18px;'><span style='font-weight: bold;'>Top<br>Female<br>2021",Sex)) %>%  
    mutate(Breast = gsub("64.72", "1:04.72", Breast)) %>%
    gt() |> gt::fmt_markdown() %>% 
    tab_header(title = html(title_header))%>% 
    gt_theme_538() %>% 
    cols_width(
      Back ~px(100),
      Breast ~ px(100),
      Fly ~ px(100),
      Free ~ px(100),
      Sex ~ px(100)) %>% 
    cols_label(
      Sex = "") %>% 
    cols_align(
      align = "center",
      columns = everything()) %>%
    gt_highlight_rows(
      columns = c(2,5),
      rows = 3,
      fill = "#cce6cc") %>% 
    gt_highlight_rows(
      columns = c(3,4),
      rows = 3,
      fill = "#ffb3b3") %>% 
    opt_table_font(
      font = list(
        google_font(name = "Montserrat"))) %>% 
    tab_style(
      style = cell_text(size = px(12),color = "black",weight = "bold",
                        font = google_font(name = "Montserrat")),
      locations = list(
        cells_column_labels(c(2:5)))) %>%
    tab_options(data_row.padding = px(12)) %>%
    gtsave("/Users/connorbradley/Desktop/swim/besttimes_usa_20.png", zoom = 6, expand = c(50,20,50,20))
}

#TABLE PREP -  2021 Tokyo Games TOP RELAYS (2021 Swims)
{
  
  all_relays_gt20 = data.frame()
  for(r in mmr_countries_20){
    
    athletes = totaltop_20 %>% filter(ro == r)
    athletes = athletes %>% mutate(FI = paste0(substr(first, 1, 1),"."))
    
    
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
              
              Back_leg = paste0('<div style="line-height: 1.5;">',
                                team[1, "FI"], " <span style='font-weight: normal;'>", team[1, "last"], "</span>",
                                "<span style='font-size: 0px; color: darkgrey;'>(", team[1, "sex"], ")</span><br>",
                                "<span style='font-weight: bold;'>", formatC(team[1, "time_seconds"], format = "f", digits = 2), "</span>",
                                "<span style='font-size: 12px; color: darkgrey;'> (","BK", ")</span>")              
              Breast_leg = paste0('<div style="line-height: 1.5;">',
                                  team[2, "FI"], " <span style='font-weight: normal;'>", team[2, "last"], "</span>",
                                  "<span style='font-size: 0px; color: darkgrey;'>(", team[2, "sex"], ")</span><br>",
                                  "<span style='font-weight: bold;'>", formatC(team[2, "time_seconds"], format = "f", digits = 2), "</span>",
                                  "<span style='font-size: 12px; color: darkgrey;'> (","BR", ")</span>")  
              Fly_leg = paste0( '<div style="line-height: 1.5;">',
                                team[3, "FI"], " <span style='font-weight: normal;'>", team[3, "last"], "</span>",
                                "<span style='font-size: 0px; color: darkgrey;'>(", team[3, "sex"], ")</span><br>",
                                "<span style='font-weight: bold;'>", formatC(team[3, "time_seconds"], format = "f", digits = 2), "</span>",
                                "<span style='font-size: 12px; color: darkgrey;'> (","FL", ")</span>")   
              Free_leg = paste0('<div style="line-height: 1.5;">',
                                team[4, "FI"], " <span style='font-weight: normal;'>", team[4, "last"], "</span>",
                                "<span style='font-size: 0px; color: darkgrey;'>(", team[4, "sex"], ")</span><br>",
                                "<span style='font-weight: bold;'>", formatC(team[4, "time_seconds"], format = "f", digits = 2), "</span>",
                                "<span style='font-size: 12px; color: darkgrey;'> (","FR", ")</span>")  
              newtime = convert_time(total_time)
              country = mmr_countries_20[r]
              
              lineups = rbind(lineups, data.frame(
                Country = r,
                Swim_Time = newtime,
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
    all_relays_gt20 = rbind(all_relays_gt20, lineups)
  }
  
  all_relays_gt20 = all_relays_gt20 %>% mutate(back_s = if_else(grepl("Female",Back),"F","M"),
                                               breast_s = if_else(grepl("Female",Breast),"F","M"),
                                               fly_s = if_else(grepl("Female",Fly),"F","M"),
                                               free_s = if_else(grepl("Female",Free),"F","M")) %>% 
    mutate(Order = paste0(back_s,breast_s,fly_s,free_s)) %>% 
    select(-c(back_s,breast_s,fly_s,free_s))
  
  best_gt20 = all_relays_gt20 %>% arrange(Swim_Time) %>% group_by(Country) %>% slice_head(n=1) %>% ungroup()
  best_usa_gt20 = all_relays_gt20 %>% arrange(Swim_Time) %>% filter(Country == "USA")
  
  ustop_gt20 = totaltop_20 %>%  filter(ro == "USA") %>% arrange(time_seconds) %>% group_by(stroke,sex) %>% mutate(rank = row_number()) %>% slice_head(n=1) %>% ungroup() %>% 
    select(First_Name = first, Last_Name = last, Country = ro,
           Sex = sex, Stroke = stroke, Swim_Time = time_seconds)
  
}

#TABLE  -  2021 Tokyo Games TOP RELAYS (2021 Swims)
{
  joiner = data.frame(Country = c("USA","GBR","AUS","CHN","NED","CAN","ITA","ROC"),
                      country_code_2 = c("US","GB","AU","CN","NL","CA","IT","RU"))
  
  flags = as.data.frame(info_flags())
  
  joiner = joiner %>% left_join(flags)
  
  title_header <- glue(
    "<div style='display: flex; justify-content: space-between; align-items: center;'>
     <div style='flex-grow: 1;line-height: 1.5;'>
       <span style='font-family: Montserrat, sans-serif;font-weight: bold; font-size: 48px; line-height: 0.6;'>Mixed 4x100 Medley Relay</span><br>
       <span style='font-family: Montserrat, sans-serif;font-size: 16px; font-weight: normal; line-height: 0.3;'>Top 8 Countries - Best Projected Lineup (based on best times since 01/01/2021)</span>
     </div>
     <div>
       <img src='https://i.imgur.com/06t8avk.png' style='height: 120px; width: auto; vertical-align: right;'>
     </div>
   </div>"
  )
  
  
  gms = best_gt20 %>% arrange(Swim_Time) %>% 
    slice_head(n=8) %>% 
    mutate(Swim_Time = paste0("<span style='font-weight: bold;font-size: larger;'>", Swim_Time, "</span><br>",
                              "<span style='font-size: 14px; color: darkgrey;'>(",Order, ")</span>")) %>% 
    left_join(joiner) %>% 
    mutate(flag = gsub("1em", "3.2em", flag)) %>% 
    mutate(flag = gsub("vertical-align:-0.125em","vertical-align:-0.5em",flag)) %>% 
    mutate(Breast = gsub("60.11","1:00.11",Breast)) %>% 
    mutate(Breast = gsub("65.90","1:05.90",Breast)) %>%  
    mutate(
      char1 = substr(Country, 1, 1),
      char2 = substr(Country, 2, 2),
      char3 = substr(Country, 3, 3),
      Country = paste0('<div style="line-height: 1.1;">',
                       "<span style='font-weight: bold;'>", char1, "</span><br>",
                       "<span style='font-weight: bold;'>", char2, "</span><br>",
                       "<span style='font-weight: bold;'>", char3, "</span>")) %>% 
    mutate(buffer = "<span style='font-size: 0px; color: white;'>",NA,"</span>") %>% 
    select(Country,flag,buffer,Back,Breast,Fly,Free,Swim_Time) %>% 
    gt() |> gt::fmt_markdown() %>% 
    tab_header(title = html(title_header))%>% 
    gt_theme_538() %>% 
    cols_align(
      align = "center",
      columns = everything()) %>% 
    cols_align(
      align = "left",
      columns = c("Back","Breast","Fly","Free","Swim_Time")) %>% 
    cols_label(
      Country = "",
      flag = "",
      Back = "",
      Breast = "",
      Fly = "",
      Free = "",
      buffer = "",
      Swim_Time = "") %>% 
    cols_width(
      Back ~px(180),
      Breast ~ px(180),
      Fly ~ px(180),
      Free ~ px(180),
      buffer ~px(25),
      Swim_Time ~px(120)) %>% 
    opt_table_font(
      font = list(
        google_font(name = "Montserrat"))) %>% 
    tab_options(data_row.padding = px(12)) %>%
    tab_source_note(md("Analysis by Connor Bradley (@cobrastats) | Data via World Aquatics ")) %>% 
    tab_style(
      style = cell_text(size = px(12)),
      locations = cells_source_notes()) %>% 
    gt_highlight_rows(
      columns = everything(),
      rows = c(2,3,4,7),
      fill = "#e0f0e0") %>% 
    gtsave("/Users/connorbradley/Desktop/swim/top_relays_20.png", zoom = 6, expand = c(50,50,50,50))
  
}


#TABLE PREP - 2020 Tokyo Games TOP US RELAYS (2020 Swims)
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
  
  
  all_relays_gt20usa = data.frame()
  for(r in mmr_countries_20){
    
    athletes = totaltop_20 %>% filter(ro == r)
    athletes = athletes %>% mutate(FI = paste0(substr(first, 1, 1),"."))
    
    
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
              
              Back_leg = paste0('<div style="line-height: 1.2;">',
                                "<span style='font-weight: normal;'>", team[1, "FI"]," ", team[1, "last"], "</span> ",
                                "<span style='font-weight: bold;'>", formatC(team[1, "time_seconds"], format = "f", digits = 2), "</span> ",
                                "<span style='font-size: 12px; color: black;'>(", team[1, "sex"], ")</span>")              
              Breast_leg = paste0('<div style="line-height: 1.2;">',
                                  "<span style='font-weight: normal;'>", team[2, "FI"]," ", team[2, "last"], "</span> ",
                                  "<span style='font-weight: bold;'>", formatC(team[2, "time_seconds"], format = "f", digits = 2), "</span> ",
                                  "<span style='font-size: 12px; color: black;'>(", team[2, "sex"], ")</span>")   
              Fly_leg = paste0('<div style="line-height: 1.2;">',
                               " <span style='font-weight: normal;'>", team[3, "FI"], " ",team[3, "last"], "</span> ",
                               "<span style='font-weight: bold;'>", formatC(team[3, "time_seconds"], format = "f", digits = 2), "</span> ",
                               "<span style='font-size: 12px; color: black;'>(", team[3, "sex"], ")</span>")    
              Free_leg = paste0('<div style="line-height: 1.2;">',
                                "<span style='font-weight: normal;'>", team[4, "FI"], " ",team[4, "last"], "</span> ",
                                "<span style='font-weight: bold;'>", formatC(team[4, "time_seconds"], format = "f", digits = 2), "</span> ",
                                "<span style='font-size: 12px; color: black;'>(", team[4, "sex"], ")</span>")    
              newtime = convert_time(total_time)
              country = mmr_countries_20[r]
              
              lineups = rbind(lineups, data.frame(
                Country = r,
                Swim_Time = newtime,
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
    all_relays_gt20usa = rbind(all_relays_gt20usa, lineups)
  }
  
  all_relays_gt20usa = all_relays_gt20usa %>% mutate(back_s = if_else(grepl("Female",Back),"F","M"),
                                                 breast_s = if_else(grepl("Female",Breast),"F","M"),
                                                 fly_s = if_else(grepl("Female",Fly),"F","M"),
                                                 free_s = if_else(grepl("Female",Free),"F","M")) %>% 
    mutate(Order = paste0(back_s,breast_s,fly_s,free_s)) %>% 
    select(-c(back_s,breast_s,fly_s,free_s))
  
  best_gt20usa = all_relays_gt20usa %>% arrange(Swim_Time) %>% group_by(Country) %>% slice_head(n=1) %>% ungroup()
  best_usa_gt20 = all_relays_gt20usa %>% arrange(Swim_Time) %>% filter(Country == "USA")
  
  ustop_gt20 = totaltop_20 %>%  filter(ro == "USA") %>% arrange(time_seconds) %>% group_by(stroke,sex) %>% mutate(rank = row_number()) %>% slice_head(n=1) %>% ungroup() %>% 
    select(First_Name = first, Last_Name = last, Country = ro,
           Sex = sex, Stroke = stroke, Swim_Time = time_seconds)
  
  
  ustopsum_gt20 = ustop_gt20 %>% 
    group_by(Stroke) %>% arrange(Swim_Time) %>% summarise(Top_Male = Swim_Time[1],
                                                          Top_Female = Swim_Time[2],
                                                          Difference = Top_Female-Top_Male)
}

#TABLE  - 2020 Tokyo Games TOP US RELAYS (2020 Swims)
{
  best_usa_gt20 = best_usa_gt20 %>% arrange(Swim_Time) %>% mutate(rank = row_number()) %>% 
    select(rank,Swim_Time,Back,Breast,Fly,Free, Order)
  
  gms1 = best_usa_gt20 %>% arrange(Swim_Time) %>% slice_head(n=10)
  gms2 = best_usa_gt20 %>% arrange(Swim_Time) %>% slice_head(n=83) %>% slice_tail(n=1)
  
  gmsrow = data.frame(
    rank = NA,
    Swim_Time=NA,
    Back = NA,
    Breast = NA,
    Fly = NA,
    Free = NA,
    Order = NA
  )
  gms = rbind(gms1,gmsrow,gms2)
  
  title_header <- glue(
    "<div style='display: flex; justify-content: space-between; align-items: center;'>
     <div style='flex-grow: 1;line-height: 1.5;'>
       <span style='font-family: Montserrat, sans-serif;font-weight: bold; font-size: 44px; line-height: 0.6;'>2020 USA - Mixed 4x100 Medley Relay</span><br>
       <span style='font-family: Montserrat, sans-serif;font-size: 16px; font-weight: normal; line-height: 0.3;'>Top Projected Lineups (based on best flat-start times since 01/01/2021)</span>
     </div>
     <div>
       <img src='https://i.imgur.com/06t8avk.png' style='height: 120px; width: auto; vertical-align: right;'>
     </div>
   </div>"
  )
  
  library(glue)
  library(dplyr)
  library(gtExtras)
  library(gt)
  gms = gms %>% 
    mutate(Swim_Time = paste0("<span style='font-weight: bold;font-size: larger;'>", Swim_Time, "</span>")) %>% 
    mutate(Back = gsub("Male", "M", Back)) %>%
    mutate(Breast = gsub("Male", "M", Breast)) %>%
    mutate(Fly = gsub("Male", "M", Fly)) %>%
    mutate(Free = gsub("Male", "M", Free)) %>%
    mutate(Back = gsub("Female", "F", Back)) %>%
    mutate(Breast = gsub("Female", "F", Breast)) %>%
    mutate(Fly = gsub("Female", "F", Fly)) %>%
    mutate(Free = gsub("Female", "F", Free)) %>%
    mutate(Breast = gsub("64.95", "1:04.95", Breast)) %>%
    select(rank,Swim_Time,Back,Breast,Fly,Free,Order) %>% 
    gt() |> gt::fmt_markdown() %>% 
    tab_header(title = html(title_header))%>% 
    gt_theme_538() %>%
    cols_align(
      align = "center",
      columns = c("rank")) %>% 
    cols_align(
      align = "left",
      columns = c("Back","Breast","Fly","Free","Swim_Time")) %>% 
    cols_label(
      Back = "BACK",
      Breast = "BREAST",
      Fly = "FLY",
      Free = "FREE",
      Swim_Time = "",
      rank = "") %>% 
    cols_width(
      Back ~px(220),
      Breast ~ px(220),
      Fly ~ px(220),
      Free ~ px(250),
      Swim_Time ~ px(150)) %>% 
    opt_table_font(
      font = list(
        google_font(name = "Montserrat"))) %>% 
    tab_options(data_row.padding = px(16)) %>%
    tab_style(
      style = cell_text(size = px(16),color = "black",weight = "bold",
                        font = google_font(name = "Montserrat")),
      locations = list(
        cells_column_labels(c(1:7)))) %>%
    tab_style(
      style = cell_text(size = px(5),color = "white",weight = "bold",
                        font = google_font(name = "Montserrat")),
      locations = list(
        cells_body(column = everything(),
                   row = 11))) %>%
    tab_style(
      style = cell_text(size = px(12)),
      locations = cells_source_notes()) %>% 
    gt_highlight_rows(
      columns = everything(),
      rows = c(12),
      fill = "#e0f0e0") %>% 
    tab_source_note(md("Analysis by Connor Bradley (@cobrastats) | Data via World Aquatics ")) %>% 
    tab_style(
      style = cell_text(size = px(12)),
      locations = cells_source_notes()) %>% 
    gtsave("/Users/connorbradley/Desktop/swim/bestrelays_usa_20.png", zoom = 6, expand = c(50,50,50,50),vwidth = 2000)
  
}