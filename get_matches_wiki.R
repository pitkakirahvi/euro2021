library(rvest)
library(tidyverse)

get_group_matches <- function(group_name, path){
  
  # get dates, home and away teams
  nos <- path %>%
    read_html(encoding = "UTF-8") %>%
    html_elements(".fscore") %>%
    html_text() %>%
    str_trim()
    
  dates <- path %>%
    read_html(encoding = "UTF-8") %>%
    html_elements(".fdate") %>%
    html_text() %>%
    str_replace("\\([^()]*\\)", "") %>%
    str_trim()
  
  home_teams <- path %>%
    read_html(encoding = "UTF-8") %>%
    html_elements(".fhome") %>%
    html_text() %>%
    str_trim()
  
  away_teams <- path %>%
    read_html(encoding = "UTF-8") %>%
    html_elements(".faway") %>%
    html_text() %>%
    str_trim() 
  
  matches  <- tibble(
    no = nos,
    group = group_name,
    date = dates,
    home = home_teams,
    away = away_teams
  )
  
}

group_a <- get_group_matches(
  "Group A",
  "https://en.wikipedia.org/wiki/UEFA_Euro_2020_Group_A" 
  )

group_b <- get_group_matches(
  "Group B",
  "https://en.wikipedia.org/wiki/UEFA_Euro_2020_Group_B" 
)

group_c <- get_group_matches(
  "Group C",
  "https://en.wikipedia.org/wiki/UEFA_Euro_2020_Group_C" 
)

group_d <- get_group_matches(
  "Group D",
  "https://en.wikipedia.org/wiki/UEFA_Euro_2020_Group_D" 
)

group_e <- get_group_matches(
  "Group E",
  "https://en.wikipedia.org/wiki/UEFA_Euro_2020_Group_E" 
)

group_f <- get_group_matches(
  "Group F",
  "https://en.wikipedia.org/wiki/UEFA_Euro_2020_Group_F" 
)

knockout <- get_group_matches(
  "Knockout",
  "https://en.wikipedia.org/wiki/UEFA_Euro_2020_knockout_phase"
)

all_matches <- bind_rows(
  group_a,
  group_b,
  group_c,
  group_d,
  group_e,
  group_f,
  knockout
  ) 

group_matches <- bind_rows(
  group_a,
  group_b,
  group_c,
  group_d,
  group_e,
  group_f
) 

all_matches %>%
  write_delim("matches.txt", delim = "\t")

group_matches %>%
  write_delim("group_matches.txt", delim = "\t")

teams <- c(group_matches$home, group_matches$away)
teams <- unique(teams) 
teams <- teams[order(teams)]

teams %>%
  write("teams.txt")
