load("D:/Data/Matched_a.R")
load("D:/Data/Matched_q.R")
load("D:/Data/analysis_1.R")
load("D:/Data/df_users_binary.R")

pacman::p_load(dplyr, fixest)



# Make variable for users' location being provided by themselves ----------

users_country <- df_users[, c("AccountId", "country")] %>%
  
  #Rename accountID to fit w df_analysis_1
  rename(OwnerUserId = AccountId) %>% 
  
  #Mutate provided location
  mutate(not_provided_location = as.numeric(is.na(country)))

# Add variable to df
df_analysis_1 <- df_analysis_1 %>% left_join(users_country)

# remove objects
rm(users_country, df_users)






StanFun <- function(x) {
  return((x-mean(x, na.rm = T))/sd(x, na.rm = T))
}


# Then apply function to all contineous variables
df_analysis_1_ <- df_analysis_1 %>% 
  mutate_at(vars(ViewCount_n, AnswerCount_n, FavoriteCount_n,
                 length_body, CommentCount_n, Number_of_posts_in_tag, creation_year,
                 weighted_female_share_post, post_creation_year, not_provided_location, post_number,
                 rollback_edits),
            ~StanFun(.)) %>% 
  rename(number_in_tag = Number_of_posts_in_tag,
         share_of_femininity = weighted_female_share_post)



df_analysis = df_analysis_1_ %>% mutate(edits_type = case_when(edited == 1 & Question == 1 ~ "Edited Question",
                                                edited == 0 & Question == 1 ~ "Undited Question",
                                                edited == 1 & Question == 0 ~ "Edited Answer",
                                                edited == 0 & Question == 0 ~ "Unedited Answer"),
                                       more_than_20 = ifelse(change_to_size > 0.2, 1, 0)) %>% 
  filter(OwnerUserId %in% matched_data_a$OwnerUserId | OwnerUserId %in% matched_data_q$OwnerUserId)



df_analysis_posts = df_analysis_1_ %>% mutate(edits_type = case_when(edited == 1 & Question == 1 ~ "Edited Question",
                                                              edited == 0 & Question == 1 ~ "Undited Question",
                                                              edited == 1 & Question == 0 ~ "Edited Answer",
                                                              edited == 0 & Question == 0 ~ "Unedited Answer"),
                                             more_than_20 = ifelse(change_to_size > 0.2, 1, 0)
) %>% 
  filter(PostId %in% matched_data_a$PostId | PostId %in% matched_data_q$PostId)


# posts only in matched data ----------------------------------------------


days_diff_no_FE_no_interact <- feols(date_diff ~  edited ,
                         df_analysis_posts, cluster =  c("OwnerUserId", "share_of_femininity") )
last_post_no_FE_no_interact <- feols(last_post ~  edited , 
                         df_analysis_posts, cluster =  c("OwnerUserId", "share_of_femininity"))



days_diff_no_interact <- feols(date_diff ~  edited   | OwnerUserId,
                               df_analysis_posts, cluster =  c("OwnerUserId", "share_of_femininity") )
last_post_no_interact <- feols(last_post ~  edited  | OwnerUserId,
                               df_analysis_posts, cluster =  c("OwnerUserId", "share_of_femininity"))


days_diff_no_interact_w_control <- feols(date_diff ~  edited +  Question +
                                        CommentCount_n + ViewCount_n + post_creation_year + self_edited + 
                                        answer_to_own_question + answer_own_question  | OwnerUserId,
                                      df_analysis_posts, cluster =  c("OwnerUserId", "share_of_femininity") )

last_post_no_interact_w_control <- feols(last_post ~ edited + Question +
                                        
                                        CommentCount_n + ViewCount_n + post_creation_year + self_edited + 
                                         answer_own_question  | OwnerUserId,
                                      df_analysis_posts, cluster =  c("OwnerUserId", "share_of_femininity"))


## Models for analysis:


etable(list(days_diff_no_FE_no_interact,   days_diff_no_interact,  days_diff_no_interact_w_control ), tex = T)

etable(list(last_post_no_FE_no_interact,   last_post_no_interact,  last_post_no_interact_w_control ), tex = T)
 


## Models for robustness-tests





last_post_questioninteract <- feols(last_post ~  edited + Question + Question * edited  +
                              CommentCount_n + ViewCount_n + post_creation_year + self_edited + 
                              answer_own_question | OwnerUserId,
                            df_analysis_posts, cluster =  c("OwnerUserId", "share_of_femininity"))





last_post_feminine_interact_question <- feols(last_post ~  edited + Question + Question  + Feminine * edited +
                                        
                                        CommentCount_n + ViewCount_n + post_creation_year + self_edited + 
                                        answer_own_question  | OwnerUserId,
                                      df_analysis_posts, cluster =  c("OwnerUserId", "share_of_femininity"))






etable(list( last_post_questioninteract, last_post_feminine_interact_question  ), tex = T)
last_post_no_control

etable(list(), tex = T)

days_diff_no_control

