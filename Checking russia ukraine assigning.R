# Load packages:
pacman::p_load(dplyr)


# Load file w users
load("D:/Data/df_users.R")

# Make varable that changes names around
df_users[, c("Id", "display_name", "country")] %>% 
  filter(country %in% c("Ukraine", "Russia")) %>%
  mutate(country_2  = case_when(country == "Ukraine" ~ "Russia",
                             country == "Russia" ~ "Ukraine")) %>% write.csv("D:/names.csv")
