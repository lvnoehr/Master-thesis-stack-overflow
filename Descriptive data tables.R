# Descriptive tables of data


## Get packages and data
pacman::p_load(psych, dplyr)

load("D:/Data/df_users_binary.R")
load("D:/Data/analysis_1.R")

# Get data from users
names_and_location <- df_users[, c("display_name", "AccountId", "firstname", "country", "country_OR_US")] %>% 
  mutate(OwnerUserId = as.integer(AccountId),
         originally_from_us = as.numeric(country == "USA"),
         provided_own_location = as.numeric(!is.na(country))  )

# Post-based variables
df_analysis_1 %>%  mutate(more_than_20 = ifelse(change_to_size > 0.2, 1, 0),
                          change_to_size = ifelse(change_to_size == Inf, 0, change_to_size),
                          answer_own_question = ifelse(Question == 1, NA_integer_, answer_own_question),
                          answer_to_own_question = ifelse(Question ==0, NA_integer_, answer_to_own_question),
                          last_posts = ifelse(last_post == T, 1, 0)) %>%
  left_join(names_and_location) %>% 
  .[, c("Feminine", "edited", "change_to_size", "date_diff", "last_posts",  "first_post", "post_number",
        "code", "indentation", "link", "ViewCount_n", "AnswerCount_n", "FavoriteCount_n",
        "CommentCount_n", "self_edited", "answer_own_question", "answer_to_own_question",
        "weighted_female_share_post", "Number_of_posts_in_tag", "creation_year", "provided_own_location"
        )] %>% describe(quant = c(0.25, 0.5, 0.75), skew = F) %>% 
  write.csv2("D:/Tabeller/final_descriptive_table.csv")



# Gender-based tables
table <- df_analysis_1 %>%  mutate(more_than_20 = ifelse(change_to_size > 0.2, 1, 0),
                          change_to_size = ifelse(change_to_size == Inf, 0, change_to_size),
                          answer_own_question = ifelse(Question == 1, NA_integer_, answer_own_question),
                          answer_to_own_question = ifelse(Question ==0, NA_integer_, answer_to_own_question),
                          last_posts = ifelse(last_post == T, 1, 0)) %>%
  left_join(names_and_location) %>% 
  .[, c("Feminine", "edited", "change_to_size", "date_diff", "last_posts",  "first_post", "post_number",
        "code", "indentation", "link", "ViewCount_n", "AnswerCount_n", "FavoriteCount_n",
        "CommentCount_n", "self_edited", "answer_own_question", "answer_to_own_question",
        "weighted_female_share_post", "Number_of_posts_in_tag", "creation_year", "provided_own_location"
  )] %>%
  describeBy(x = ., group = "Feminine", quant = c(0.25, 0.5, 0.75), skew = F, mat = T) 
  

table %>%  write.csv2("D:/Tabeller/final_descriptive_table_gender.csv")
