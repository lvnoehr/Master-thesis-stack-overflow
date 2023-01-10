## Show propensity score models:


## Load packages:
pacman::p_load(MatchIt, 
               lmtest,
               dplyr,
               lme4,
               stargazer,
               sjPlot,
               sandwich,
               psych,
               fixest,
               ggplot2
)


# Load Data
load("D:/Data/analysis_1.R")
load("D:/Data/df_users_binary.R")



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


# mean centralise and standardise continious variables -------------------------------

# First - make standardising function:
StanFun <- function(x) {
  return((x-mean(x, na.rm = T))/sd(x, na.rm = T))
}


# Then apply function to all contineous variables
df_analysis_1_ <- df_analysis_1 %>% 
  mutate_at(vars(ViewCount_n, AnswerCount_n, FavoriteCount_n, post_creation_year,
                 length_body, CommentCount_n, Number_of_posts_in_tag,
                 weighted_female_share_post, creation_year, not_provided_location, post_number,
                 rollback_edits),
            ~StanFun(.)) %>% 
  rename(number_in_tag = Number_of_posts_in_tag,
         share_of_femininity = weighted_female_share_post)

# And then apply to answers and questions seperately, as these variables are custommade for them
df_1q <- df_analysis_1_ %>% filter(Question == 1) %>% mutate(answer_to_own_question = StanFun(answer_to_own_question))
df_1a <- df_analysis_1_ %>% filter(Question == 0) %>% mutate(answer_own_question = StanFun(answer_own_question))


# Probit-model - ----------------------------------------------------------

questions <- feglm(Feminine ~ post_number + ViewCount_n +  CommentCount_n + post_creation_year +
      code + indentation + link + self_edited  + answer_to_own_question +
      length_body  + share_of_femininity + number_in_tag + not_provided_location +
      creation_year, data = df_1q, cluster = c("OwnerUserId", "share_of_femininity")) 



answers <- feglm(Feminine ~ post_number + ViewCount_n +  CommentCount_n + post_creation_year +
      code + indentation + link + self_edited  + answer_own_question +
      length_body  + share_of_femininity + number_in_tag + not_provided_location +
      creation_year, data = df_1a, cluster = c("OwnerUserId", "share_of_femininity")) 


etable(list(questions, answers),tex = T)
