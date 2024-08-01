library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(glue)
library(dplyr)
library(gtExtras)
library(gt)

#USA TOP TIMES BY SEX - 2024 SEASON
{
#TABLE PREP - 2024 - US Top Times by Sex
{
  ustop_24_pivot = ustop_24 %>% 
    mutate(combined = paste0("<div style='line-height: 1.4;'>",First_Name,"<br>",Last_Name,"<br>","<span style='font-weight: bold;'>", Swim_Time, "</span><br>")) %>% 
    select(combined,Stroke,Sex) %>% 
    pivot_wider(names_from = Stroke,values_from = combined)
  
  new_row <- data.frame(
    Sex = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>Time<br>Diff.",
    Back = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>4.91",
    Breast = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>6.86",
    Fly = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>4.99",
    Free = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>5.21")
  
  ustop_24_pivot = rbind(ustop_24_pivot,new_row)
}

#TABLE  - 2024 - US Top Times by Sex
{
  title_header <- glue(
    "<div style='display: flex; justify-content: space-between; align-items: center;'>
     <div style='flex-grow: 1;line-height: 1.5;'>
       <span style='font-family: Montserrat, sans-serif;font-weight: bold; font-size: 28px; line-height: 0.6;'>Top 100m Performances</span><br>
       <span style='font-family: Montserrat, sans-serif;font-size: 11px; font-weight: normal; line-height: 0.3;'>USA Swimmers (based on best flat-start times since 01/01/2024)</span>
     </div>
     <div>
       <img src='https://i.imgur.com/XYuCYcY.png' style='height: 70px; width: auto; vertical-align: right;'>
     </div>
   </div>"
  )
  
  gms = ustop_24_pivot %>% 
    mutate(Sex = gsub("Male","<div style='line-height: 1.2;font-size: 18px;'><span style='font-weight: bold;'>Top<br>Male<br>2024",Sex)) %>% 
    mutate(Sex = gsub("Female","<div style='line-height: 1.2;font-size: 18px;'><span style='font-weight: bold;'>Top<br>Female<br>2024",Sex)) %>%  
    mutate(Breast = gsub("65.43","1:05.43",Breast)) %>% 
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
      columns = c(2,4),
      rows = 3,
      fill = "#cce6cc") %>% 
    gt_highlight_rows(
      columns = c(3,5),
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
    gtsave("/besttimes_usa_24.png", zoom = 6, expand = c(50,20,50,20))
}
}

#BEST LINEUPS - TOP 8 TEAMS - 2024 SEASON
{
#TABLE PREP -  2024 Paris Games TOP RELAYS (2024 Swims)
{

  all_relays_gt24 = data.frame()
  for(r in mmr_countries_24){
    
    athletes = totaltop_24 %>% filter(ro == r)
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
              country = mmr_countries_24[r]
              
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
    all_relays_gt24 = rbind(all_relays_gt24, lineups)
  }
  
  all_relays_gt24 = all_relays_gt24 %>% mutate(back_s = if_else(grepl("Female",Back),"F","M"),
                                           breast_s = if_else(grepl("Female",Breast),"F","M"),
                                           fly_s = if_else(grepl("Female",Fly),"F","M"),
                                           free_s = if_else(grepl("Female",Free),"F","M")) %>% 
    mutate(Order = paste0(back_s,breast_s,fly_s,free_s)) %>% 
    select(-c(back_s,breast_s,fly_s,free_s))
  
  best_gt24 = all_relays_gt24 %>% arrange(Swim_Time) %>% group_by(Country) %>% slice_head(n=1) %>% ungroup()
  best_usa_gt24 = all_relays_gt24 %>% arrange(Swim_Time) %>% filter(Country == "USA")
  
  ustop_gt24 = totaltop_24 %>%  filter(ro == "USA") %>% arrange(time_seconds) %>% group_by(stroke,sex) %>% mutate(rank = row_number()) %>% slice_head(n=1) %>% ungroup() %>% 
    select(First_Name = first, Last_Name = last, Country = ro,
           Sex = sex, Stroke = stroke, Swim_Time = time_seconds)
 
}

#TABLE  -  2024 Paris Games TOP RELAYS (2024 Swims)
{
  joiner = data.frame(Country = c("USA","AUS","CHN","GBR","NED","CAN","GER","ITA"),
                      country_code_2 = c("US","AU","CN","GB","NL","CA","DE","IT"))
  
  flags = as.data.frame(info_flags())
  
  joiner = joiner %>% left_join(flags)
  
  title_header <- glue(
    "<div style='display: flex; justify-content: space-between; align-items: center;'>
     <div style='flex-grow: 1;line-height: 1.5;'>
       <span style='font-family: Montserrat, sans-serif;font-weight: bold; font-size: 48px; line-height: 0.6;'>Mixed 4x100 Medley Relay</span><br>
       <span style='font-family: Montserrat, sans-serif;font-size: 16px; font-weight: normal; line-height: 0.3;'>Top 8 Countries - Best Projected Lineup (based on best times since 01/01/2024)</span>
     </div>
     <div>
       <img src='https://i.imgur.com/zVI04sB.png' style='height: 120px; width: auto; vertical-align: right;'>
     </div>
   </div>"
  )
  
  
  gms = best_gt24 %>% arrange(Swim_Time) %>% 
    slice_head(n=8) %>% 
    mutate(Swim_Time = paste0("<span style='font-weight: bold;font-size: larger;'>", Swim_Time, "</span><br>",
                              "<span style='font-size: 14px; color: darkgrey;'>(",Order, ")</span>")) %>% 
    left_join(joiner) %>% 
    mutate(flag = gsub("1em", "3.2em", flag)) %>% 
    mutate(flag = gsub("vertical-align:-0.125em","vertical-align:-0.5em",flag)) %>% 
    mutate(Back = gsub("60.00","1:00.00",Back)) %>% 
    mutate(Breast = gsub("60.66","1:00.66",Breast)) %>% 
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
    gtsave("/top_relays_24.png", zoom = 6, expand = c(50,50,50,50))
  
}
}

#USA TOP 10 RELAYS - 2024 SEASON
{
#TABLE PREP - 2024 Paris Games TOP US RELAYS (2024 Swims)
{
  all_relays_gt24_usa = data.frame()
  for(r in mmr_countries_24){
    
    athletes = totaltop_24 %>% filter(ro == r)
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
                                team[1, "FI"], " <span style='font-weight: normal;'>", team[1, "last"], "</span> ",
                                "<span style='font-weight: bold;'>", formatC(team[1, "time_seconds"], format = "f", digits = 2), "</span> ",
                                "<span style='font-size: 12px; color: black;'>(", team[1, "sex"], ")</span>")              
              Breast_leg = paste0('<div style="line-height: 1.2;">',
                                  team[2, "FI"], " <span style='font-weight: normal;'>", team[2, "last"], "</span> ",
                                  "<span style='font-weight: bold;'>", formatC(team[2, "time_seconds"], format = "f", digits = 2), "</span> ",
                                  "<span style='font-size: 12px; color: black;'>(", team[2, "sex"], ")</span>")   
              Fly_leg = paste0('<div style="line-height: 1.2;">',
                               team[3, "FI"], " <span style='font-weight: normal;'>", team[3, "last"], "</span> ",
                               "<span style='font-weight: bold;'>", formatC(team[3, "time_seconds"], format = "f", digits = 2), "</span> ",
                               "<span style='font-size: 12px; color: black;'>(", team[3, "sex"], ")</span>")    
              Free_leg = paste0('<div style="line-height: 1.2;">',
                                team[4, "FI"], " <span style='font-weight: normal;'>", team[4, "last"], "</span> ",
                                "<span style='font-weight: bold;'>", formatC(team[4, "time_seconds"], format = "f", digits = 2), "</span> ",
                                "<span style='font-size: 12px; color: black;'>(", team[4, "sex"], ")</span>")    
              newtime = convert_time(total_time)
              country = mmr_countries_24[r]
              
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
    all_relays_gt24_usa = rbind(all_relays_gt24_usa, lineups)
  }
  
  all_relays_gt24_usa = all_relays_gt24_usa %>% mutate(back_s = if_else(grepl("Female",Back),"F","M"),
                                                 breast_s = if_else(grepl("Female",Breast),"F","M"),
                                                 fly_s = if_else(grepl("Female",Fly),"F","M"),
                                                 free_s = if_else(grepl("Female",Free),"F","M")) %>% 
    mutate(Order = paste0(back_s,breast_s,fly_s,free_s)) %>% 
    select(-c(back_s,breast_s,fly_s,free_s))
  
  best_gt24_usa = all_relays_gt24_usa %>% arrange(Swim_Time) %>% group_by(Country) %>% slice_head(n=1) %>% ungroup()
  best_usa_gtusa = all_relays_gt24_usa %>% arrange(Swim_Time) %>% filter(Country == "USA")
  
  ustop_gt24_usa = totaltop_24 %>%  filter(ro == "USA") %>% arrange(time_seconds) %>% group_by(stroke,sex) %>% mutate(rank = row_number()) %>% slice_head(n=1) %>% ungroup() %>% 
    select(First_Name = first, Last_Name = last, Country = ro,
           Sex = sex, Stroke = stroke, Swim_Time = time_seconds)
}  

#TABLE  - 2024 Paris Games TOP US RELAYS (2024 Swims)
{
  
  title_header <- glue(
    "<div style='display: flex; justify-content: space-between; align-items: center;'>
     <div style='flex-grow: 1;line-height: 1.5;'>
       <span style='font-family: Montserrat, sans-serif;font-weight: bold; font-size: 48px; line-height: 0.6;'>USA - Mixed 4x100 Medley Relay</span><br>
       <span style='font-family: Montserrat, sans-serif;font-size: 16px; font-weight: normal; line-height: 0.3;'>Top 10 Best Projected Lineups (based on best flat-start times since 01/01/2024)</span>
     </div>
     <div>
       <img src='https://i.imgur.com/XYuCYcY.png' style='height: 120px; width: auto; vertical-align: right;'>
     </div>
   </div>"
  )
  
  library(glue)
  library(dplyr)
  library(gtExtras)
  library(gt)
  gms = best_usa_gtusa %>% arrange(Swim_Time) %>% 
    slice_head(n=10) %>% 
    mutate(Swim_Time = paste0("<span style='font-weight: bold;font-size: larger;'>", Swim_Time, "</span>")) %>% 
    left_join(joiner) %>% 
    mutate(Back = gsub("Male", "M", Back)) %>%
    mutate(Breast = gsub("Male", "M", Breast)) %>%
    mutate(Fly = gsub("Male", "M", Fly)) %>%
    mutate(Free = gsub("Male", "M", Free)) %>%
    mutate(Back = gsub("Female", "F", Back)) %>%
    mutate(Breast = gsub("Female", "F", Breast)) %>%
    mutate(Fly = gsub("Female", "F", Fly)) %>%
    mutate(Free = gsub("Female", "F", Free)) %>%
    mutate(rank = row_number()) %>% 
    select(rank,Swim_Time,Back,Breast,Fly,Free,Swim_Time,Order) %>% 
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
    tab_source_note(md("Analysis by Connor Bradley (@cobrastats) | Data via World Aquatics ")) %>% 
    tab_style(
      style = cell_text(size = px(12)),
      locations = cells_source_notes()) %>% 
    gtsave("/bestrelays_usa_24.png", zoom = 6, expand = c(50,50,50,50),vwidth = 2000)
  
    }
}

#CHINA TOP TIMES BY SEX - 2024 SEASON
{
#TABLE PREP - 2024 - China Top Times by Sex
{
  
  chinatop_24 = totaltop_24 %>%  filter(ro == "CHN") %>% arrange(time_seconds) %>% group_by(stroke,sex) %>% mutate(rank = row_number()) %>% slice_head(n=1) %>% ungroup() %>% 
    select(First_Name = first, Last_Name = last, Country = ro,
           Sex = sex, Stroke = stroke, Swim_Time = time_seconds)
  
  chinatop_24_pivot = chinatop_24 %>% 
    mutate(combined = paste0("<div style='line-height: 1.4;'>",First_Name,"<br>",Last_Name,"<br>","<span style='font-weight: bold;'>", Swim_Time, "</span><br>")) %>% 
    select(combined,Stroke,Sex) %>% 
    pivot_wider(names_from = Stroke,values_from = combined)
  
  new_row <- data.frame(
    Sex = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>Time<br>Diff.",
    Back = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>7.00",
    Breast = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>6.15",
    Fly = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>4.95",
    Free = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>6.08")
  
  chinatop_24_pivot = rbind(chinatop_24_pivot,new_row)
}

#TABLE  - 2024 - China Top Times by Sex
{
  title_header <- glue(
    "<div style='display: flex; justify-content: space-between; align-items: center;'>
     <div style='flex-grow: 1;line-height: 1.5;'>
       <span style='font-family: Montserrat, sans-serif;font-weight: bold; font-size: 28px; line-height: 0.6;'>Top 100m Performances</span><br>
       <span style='font-family: Montserrat, sans-serif;font-size: 11px; font-weight: normal; line-height: 0.3;'>China (based on best flat-start times since 01/01/2024)</span>
     </div>
     <div>
       <img src='https://i.imgur.com/QEmW3Fw.png' style='height: 70px; width: auto; vertical-align: right;'>
     </div>
   </div>"
  )
  
  gms = chinatop_24_pivot %>% 
    mutate(Sex = gsub("Male","<div style='line-height: 1.2;font-size: 18px;'><span style='font-weight: bold;'>Top<br>Male<br>2024",Sex)) %>% 
    mutate(Sex = gsub("Female","<div style='line-height: 1.2;font-size: 18px;'><span style='font-weight: bold;'>Top<br>Female<br>2024",Sex)) %>%  
    mutate(Breast = gsub("64.39","1:04.39",Breast)) %>% 
    mutate(Fly = gsub("51.2","51.20",Fly)) %>% 
    mutate(Free = gsub("46.4","46.40",Free)) %>% 
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
      columns = c(4,5),
      rows = 3,
      fill = "#cce6cc") %>% 
    gt_highlight_rows(
      columns = c(2,3),
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
    gtsave("/besttimes_china_24.png", zoom = 6, expand = c(50,20,50,20))
}
}

#Australia TOP 10 RELAYS - 2024 SEASON
{
#TABLE PREP - 2024 Paris Games TOP AUS RELAYS (2024 Swims)
{

  best_aus_gt = all_relays_gt24_usa %>% arrange(Swim_Time) %>% filter(Country == "AUS")
  
}  

#TABLE  - 2024 Paris Games TOP US RELAYS (2024 Swims)
{
  
  title_header <- glue(
    "<div style='display: flex; justify-content: space-between; align-items: center;'>
     <div style='flex-grow: 1;line-height: 1.5;'>
       <span style='font-family: Montserrat, sans-serif;font-weight: bold; font-size: 48px; line-height: 0.6;'>Australia - Mixed 4x100 Medley Relay</span><br>
       <span style='font-family: Montserrat, sans-serif;font-size: 16px; font-weight: normal; line-height: 0.3;'>Top 10 Best Projected Lineups (based on best flat-start times since 01/01/2024)</span>
     </div>
     <div>
       <img src='https://i.imgur.com/bVca19g.png' style='height: 120px; width: auto; vertical-align: right;'>
     </div>
   </div>"
  )
  
  library(glue)
  library(dplyr)
  library(gtExtras)
  library(gt)
  gms = best_aus_gt %>% arrange(Swim_Time) %>% 
    slice_head(n=10) %>% 
    mutate(Swim_Time = paste0("<span style='font-weight: bold;font-size: larger;'>", Swim_Time, "</span>")) %>% 
    left_join(joiner) %>% 
    mutate(Back = gsub("Male", "M", Back)) %>%
    mutate(Breast = gsub("Male", "M", Breast)) %>%
    mutate(Fly = gsub("Male", "M", Fly)) %>%
    mutate(Free = gsub("Male", "M", Free)) %>%
    mutate(Back = gsub("Female", "F", Back)) %>%
    mutate(Breast = gsub("Female", "F", Breast)) %>%
    mutate(Fly = gsub("Female", "F", Fly)) %>%
    mutate(Free = gsub("Female", "F", Free)) %>%
    mutate(rank = row_number()) %>% 
    select(rank,Swim_Time,Back,Breast,Fly,Free,Swim_Time,Order) %>% 
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
      Back ~px(250),
      Breast ~ px(250),
      Fly ~ px(250),
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
    tab_source_note(md("Analysis by Connor Bradley (@cobrastats) | Data via World Aquatics ")) %>% 
    tab_style(
      style = cell_text(size = px(12)),
      locations = cells_source_notes()) %>% 
    gtsave("/bestrelays_AUS_24.png", zoom = 6, expand = c(50,50,50,50),vwidth = 2000)
  
}
}

#CHINA TOP TIMES BY SEX - PARIS OLYMPIC GAMES
{
#TABLE PREP - 2024 - China Top Times by Sex
{
  
  chinatop_games = totaltop_games %>%  filter(ro == "CHN") %>% arrange(time_seconds) %>% group_by(stroke,sex) %>% mutate(rank = row_number()) %>% slice_head(n=1) %>% ungroup() %>% 
    select(First_Name = first, Last_Name = last, Country = ro,
           Sex = sex, Stroke = stroke, Swim_Time = time_seconds)
  
  chinatop_games_pivot = chinatop_games %>% 
    mutate(combined = paste0("<div style='line-height: 1.4;'>",First_Name,"<br>",Last_Name,"<br>","<span style='font-weight: bold;'>", Swim_Time, "</span><br>")) %>% 
    select(combined,Stroke,Sex) %>% 
    pivot_wider(names_from = Stroke,values_from = combined)
  
  new_row <- data.frame(
    Sex = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>Time<br>Diff.",
    Back = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>7.85",
    Breast = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>6.61",
    Fly = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'> ",
    Free = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>6.08")
  
  chinatop_games_pivot = rbind(chinatop_games_pivot,new_row)
}

#TABLE  - 2024 - China Top Times by Sex
{
  title_header <- glue(
    "<div style='display: flex; justify-content: space-between; align-items: center;'>
     <div style='flex-grow: 1;line-height: 1.5;'>
       <span style='font-family: Montserrat, sans-serif;font-weight: bold; font-size: 28px; line-height: 0.6;'>Top 100m Performances</span><br>
       <span style='font-family: Montserrat, sans-serif;font-size: 14px; font-weight: normal; line-height: 0.3;'>China at the Paris Olympic Games</span>
     </div>
     <div>
       <img src='https://i.imgur.com/QEmW3Fw.png' style='height: 70px; width: auto; vertical-align: right;'>
     </div>
   </div>"
  )
  
  gms = chinatop_games_pivot %>% 
    mutate(Sex = gsub("Male","<div style='line-height: 1.2;font-size: 18px;'><span style='font-weight: bold;'>Top<br>Male<br>2024",Sex)) %>% 
    mutate(Sex = gsub("Female","<div style='line-height: 1.2;font-size: 18px;'><span style='font-weight: bold;'>Top<br>Female<br>2024",Sex)) %>%  
    mutate(Breast = gsub("65.54","1:05.54",Breast)) %>% 
    mutate(Free = gsub("46.4","46.40",Free)) %>%
    mutate(Fly = gsub("<div style='line-height: 1.4;'>Zhanshuo<br>ZHANG<br><span style='font-weight: bold;'>57.35</span><br>","",Fly)) %>% 
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
      columns = c(5),
      rows = 3,
      fill = "#cce6cc") %>% 
    gt_highlight_rows(
      columns = c(2,3),
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
    tab_source_note(md("Analysis by Connor Bradley (@cobrastats) | Data via World Aquatics<br>Men's 100m Fly had not been completed at the time of this article")) %>% 
    tab_style(
      style = cell_text(size = px(9)),
      locations = cells_source_notes()) %>% 
    gtsave("/besttimes_china_games.png", zoom = 6, expand = c(50,20,50,20))
}
}

#CHINA TOP TIMES BY SEX - PARIS OLYMPIC GAMES
{
#TABLE PREP - 2024 - usa Top Times by Sex
{
  
  usatop_games = totaltop_games %>%  filter(ro == "USA") %>% arrange(time_seconds) %>% group_by(stroke,sex) %>% mutate(rank = row_number()) %>% slice_head(n=1) %>% ungroup() %>% 
    select(First_Name = first, Last_Name = last, Country = ro,
           Sex = sex, Stroke = stroke, Swim_Time = time_seconds)
  
  usatop_games_pivot = usatop_games %>% 
    mutate(combined = paste0("<div style='line-height: 1.4;'>",First_Name,"<br>",Last_Name,"<br>","<span style='font-weight: bold;'>", Swim_Time, "</span><br>")) %>% 
    select(combined,Stroke,Sex) %>% 
    pivot_wider(names_from = Stroke,values_from = combined)
  
  new_row <- data.frame(
    Sex = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>Time<br>Diff.",
    Back = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>5.27",
    Breast = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>6.55",
    Fly = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'> ",
    Free = "<div style='line-height: 1.2;font-size: 20px;'><span style='font-weight: bold;'>4.72")
  
  usatop_games_pivot = rbind(usatop_games_pivot,new_row)
}

#TABLE  - 2024 - USA Top Times by Sex
{
  title_header <- glue(
    "<div style='display: flex; justify-content: space-between; align-items: center;'>
     <div style='flex-grow: 1;line-height: 1.5;'>
       <span style='font-family: Montserrat, sans-serif;font-weight: bold; font-size: 28px; line-height: 0.6;'>Top 100m Performances</span><br>
       <span style='font-family: Montserrat, sans-serif;font-size: 14px; font-weight: normal; line-height: 0.3;'>USA at the Paris Olympic Games</span>
     </div>
     <div>
       <img src='https://i.imgur.com/XYuCYcY.png' style='height: 50px; width: auto; vertical-align: right;'>
     </div>
   </div>"
  )
  
  gms = usatop_games_pivot %>% 
    mutate(Sex = gsub("Male","<div style='line-height: 1.2;font-size: 18px;'><span style='font-weight: bold;'>Top<br>Male<br>2024",Sex)) %>% 
    mutate(Sex = gsub("Female","<div style='line-height: 1.2;font-size: 18px;'><span style='font-weight: bold;'>Top<br>Female<br>2024",Sex)) %>%  
    mutate(Breast = gsub("65.6","1:05.60",Breast)) %>% 
    mutate(Fly = gsub("51.2","51.20",Fly)) %>% 
    mutate(Free = gsub("46.4","46.40",Free)) %>%
    mutate(Fly = gsub("<div style='line-height: 1.4;'>Luca<br>URLANDO<br><span style='font-weight: bold;'>53.93</span><br>","",Fly)) %>% 
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
      columns = c(5),
      rows = 3,
      fill = "#cce6cc") %>% 
    gt_highlight_rows(
      columns = c(2,3),
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
    tab_source_note(md("Analysis by Connor Bradley (@cobrastats) | Data via World Aquatics<br>Men's 100m Fly had not been completed at the time of this article")) %>% 
    tab_style(
      style = cell_text(size = px(9)),
      locations = cells_source_notes()) %>% 
    gtsave("/besttimes_usa_games.png", zoom = 6, expand = c(50,20,50,20))
}
}

#TIME DIFFERENCE ON AVERAGE
{
title_header <- glue(
  "<div style='display: flex; justify-content: space-between; align-items: center;'>
     <div style='flex-grow: 1;line-height: 1.5;'>
       <span style='font-family: Montserrat, sans-serif;font-weight: bold; font-size: 28px; line-height: 0.6;'>Male vs. Female 100m Times</span><br>
       <span style='font-family: Montserrat, sans-serif;font-size: 12px; font-weight: normal; line-height: 0.3;'>Based on Top-200 Swimmers Worldwide in 2024</span>
     </div>
     <div>
     </div>
   </div>"
)


gms = totaltop24sum %>% 
  mutate(Female = gsub("60.24","1:00.24",Female)) %>% 
  mutate(Female = gsub("66.87","1:06.87",Female)) %>% 
  mutate(Female = gsub("54.3","54.30",Female)) %>% 
  
  gt() |> gt::fmt_markdown() %>% 
  tab_header(title = html(title_header))%>% 
  gt_theme_538() %>% 
  cols_align(
    align = "right",
    columns = everything()) %>% 
  cols_align(
    align = "left",
    columns = c("stroke")) %>% 
  cols_label(
    stroke = "Stroke",
    Female = md("Avg.<br>Female<br>Time"),
    Male = md("Avg.<br>Male<br>Time"),
    pct = md("% Time<br>Diff."),
    diff = md("Diff.<br>in Time")) %>% 
  cols_width(
    stroke ~px(90),
    Female ~ px(90),
    Male ~ px(90),
    pct ~ px(90),
    diff ~px(90)) %>% 
  fmt_percent(
    columns = pct,
    decimals = 2
  ) %>% 
  opt_table_font(
    font = list(
      google_font(name = "Montserrat"))) %>% 
  tab_style(
    style = cell_text(size = px(12),color = "black",weight = "bold",
                      font = google_font(name = "Montserrat")),
    locations = list(
      cells_column_labels(c(1:5)))) %>%
  tab_style(
    style = cell_text(size = px(16),color = "black",weight = "bold",
                      font = google_font(name = "Montserrat")),
    locations = cells_body(
          columns = 5,
          rows = everything())) %>%
  tab_options(data_row.padding = px(12)) %>%
  tab_source_note(md("Analysis by Connor Bradley (@cobrastats) | Data via World Aquatics ")) %>% 
  tab_style(
    style = cell_text(size = px(8)),
    locations = cells_source_notes()) %>% 
  gtsave("/time_diff.png", zoom = 6, expand = c(50,50,50,50))
}

