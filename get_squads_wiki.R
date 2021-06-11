library(rvest)
library(tidyverse)
library(here)

path <- "https://en.wikipedia.org/wiki/UEFA_Euro_2020_squads"

squads <- path %>%
  read_html(encoding = "UTF-8") %>%
  html_elements(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  html_table(fill = TRUE)

squads <- squads[1:24] %>%
  bind_rows() %>%
  mutate(
    Player = str_replace(Player, "\\(captain\\)", ""),
    Player = str_trim(Player)) %>%
  select(Player, Caps, Goals, Club) %>%
  arrange(Player)

squads <- squads %>%
  write_delim(here("results", "squads.txt"), delim = "\t")
