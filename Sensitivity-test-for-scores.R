# sensitivity test of score

load("D:/Data/Matched_a.R")
load("D:/Data/Matched_q.R")


pacman::p_load(dplyr, fixest, ggplot2)

StanFun <- function(x) {
  return((x-mean(x, na.rm = T))/sd(x, na.rm = T))
}


### Take score as a control variables


matched_data_a <- matched_data_a %>%  mutate(scores_std = StanFun(Score))
  
matched_data_q <- matched_data_q %>%  mutate(scores_std = StanFun(Score))




# Hypothesis 1 & 2 test ---------------------------------------------------


h1_full_a <- feols(edited ~ Feminine + first_post + first_post * Feminine +
                     scores_std + scores_std*Feminine, 
                   data = matched_data_a,
                   cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_a <- feols(edited ~ Feminine + scores_std + scores_std + scores_std*Feminine,
                       data = matched_data_a, 
                       cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_q <- feols(edited ~ Feminine + scores_std + scores_std + scores_std*Feminine,
                       data = matched_data_q,
                       cluster = c("OwnerUserId", "share_of_femininity"))

etable(list( h1_feminine_q, h1_feminine_a, h1_full_a), tex = T)





h1_full_a <- feols(more_than_20 ~ Feminine + first_post + first_post * Feminine + scores_std, 
                   data = matched_data_a,
                   cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_a <- feols(more_than_20 ~ Feminine  + scores_std ,
                       data = matched_data_a, 
                       cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_q <- feols(more_than_20 ~ Feminine + scores_std ,
                       data = matched_data_q,
                       cluster = c("OwnerUserId", "share_of_femininity"))

etable(list( h1_feminine_q, h1_feminine_a, h1_full_a), tex = T)
