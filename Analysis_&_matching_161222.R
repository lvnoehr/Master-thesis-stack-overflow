# Loading data and packages -----------------------------------------------

# Analysis the 6th december script
## Matching w attention criteria + with location variable

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
  mutate_at(vars(ViewCount_n, AnswerCount_n, FavoriteCount_n,
                 length_body, CommentCount_n, Number_of_posts_in_tag, creation_year,
                 weighted_female_share_post, post_creation_year, not_provided_location, post_number,
                 rollback_edits),
            ~StanFun(.)) %>% 
  rename(number_in_tag = Number_of_posts_in_tag,
         share_of_femininity = weighted_female_share_post)

# And then apply to answers and questions seperately, as these variables are custommade for them
df_1q <- df_analysis_1_ %>% filter(Question == 1) %>% mutate(answer_to_own_question = StanFun(answer_to_own_question))
df_1a <- df_analysis_1_ %>% filter(Question == 0) %>% mutate(answer_own_question = StanFun(answer_own_question))




# Then - match the objects ------------------------------------------------
#Here, we initially match femininity on all of the 

match_obj_q <- matchit(Feminine ~ post_number + ViewCount_n +  CommentCount_n + creation_year +
                                      code + indentation + link + self_edited  + answer_to_own_question +
                                      length_body  + share_of_femininity + number_in_tag + 
                                    not_provided_location + post_creation_year,
                                    data = df_1q, method = "nearest", distance ="glm",
                                    ratio = 1,
                                    replace = FALSE)

matched_obj_numbers <- summary(match_obj_q)
matched_obj_numbers
#plotting the balance between smokers and non-smokers
plot(match_obj_q, type = "histogram", interactive = FALSE)

plot(summary(match_obj_q), abs = FALSE)



matched_summary_q <- as.data.frame(matched_obj_numbers$sum.matched) %>%  mutate(before_mat_std_dif = as.data.frame(matched_obj_numbers$sum.all)[,c( 3)]  )

rownames(matched_summary_q) <- c("Distance", "Post number", "Views", "Comments", "Post Creation year", 
                                 "Code", "Indentation", "Link", "Self-edited",
                                 "Answer to own question", "Length body", "Share of feminine authors",
                                 "Posts in tag", "User not provided location", "Creation year")  

matched_summary_q$row_names <- row.names(matched_summary_q)

matched_summary_q %>% 
  mutate(row_names = factor(row_names, 
                            ordered = T,
                            levels = c("Distance", "Post number", "Views", "Comments", "Post Creation year", 
                                        "Code", "Indentation", "Link", "Self-edited",
                                        "Answer to own question", "Length body", "Share of feminine authors",
                                        "Posts in tag", "User not provided location",
                                        "Creation year"))) %>% 
  ggplot(aes(x = before_mat_std_dif, y = row_names)) +
  geom_point(color = "darkgrey", size = 2.4 ) +
  geom_point(aes(`Std. Mean Diff.`), color = "black", size = 2.4) +
  theme_minimal() + 
  labs(title = "Questions \n Std. Differences in Data Before and After Matching",
       y = " ",
       x = "Standardized difference between questions authored by masculine and feminine users ",
       caption = "Lines at 0.05 difference and dashed line at 0.1 difference.
       Data from the Internet Archieve on use of Stack Overflow.
       29,734 matched questions") +
  geom_vline(xintercept = c(0.05, -0.05)) +
  geom_vline(xintercept = c(0.1, -0.1), linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5))


#Extract the matched data and save the data into the variable matched_data
matched_data_q <- match.data(match_obj_q)

### Save the matched data - it can be used for all analysis
save(matched_data_q, file = "D:/Data/Matched_q.R")
#load("D:/Data/Matched_q.R")


match_obj_a <- matchit(Feminine ~ ViewCount_n +  CommentCount_n + post_creation_year +
                                      code + indentation + link + self_edited  + answer_own_question + post_number +
                                      length_body  + share_of_femininity + number_in_tag + not_provided_location +
                                      creation_year,
                                    data = df_1a, method = "nearest", distance ="glm",
                                    ratio = 1,
                                    replace = FALSE)
summary(match_obj_a)

## Plot the balances

plot(match_obj_a, type = "histogram", interactive = FALSE)



matched_obj_numbers <- summary(match_obj_a)


matched_summary_a <- as.data.frame(matched_obj_numbers$sum.matched) %>%  mutate(before_mat_std_dif = as.data.frame(matched_obj_numbers$sum.all)[,c( 3)]  )

rownames(matched_summary_a) <- c("Distance",  "Views", "Comments", "Post Creation year", 
                                 "Code", "Indentation", "Link", "Self-edited",
                                 "Answer to own question", "Post number", "Length body", "Share of feminine authors",
                                 "Posts in tag", "User not provided location", "Creation year")  

matched_summary_a$row_names <- row.names(matched_summary_a)

matched_summary_a %>% 
  mutate(row_names = factor(row_names, 
                            ordered = T,
                            levels =c("Distance",  "Views", "Comments", "Post Creation year", 
                                       "Code", "Indentation", "Link", "Self-edited",
                                       "Answer to own question", "Post number", "Length body", "Share of feminine authors",
                                       "Posts in tag", "User not provided location", "Creation year")  )) %>% 
  ggplot(aes(x = before_mat_std_dif, y = row_names)) +
  geom_point(color = "darkgrey", size = 2.4 ) +
  geom_point(aes(`Std. Mean Diff.`), color = "black", size = 2.4) +
  theme_minimal() + 
  labs(title = "Answers \n Std. Differences in Data Before and After Matching",
       y = " ",
       x = "Standardized difference between questions authored by masculine and feminine users ",
       caption = "Lines at 0.05 difference and dashed line at 0.1 difference.
       Data from the Internet Archieve on use of Stack Overflow.
       35,524 matched answers") +
  geom_vline(xintercept = c(0.05, -0.05)) +
  geom_vline(xintercept = c(0.1, -0.1), linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5))




#Extract the matched data and save the data into the variable matched_data
matched_data_a <- match.data(match_obj_a)

### Save the matched data - it can be used for all analysis
save(matched_data_a, file = "D:/Data/Matched_a.R")


# Hypothesis 1a & 2a: Users are more often moderated ----------------------

h1_full_a <- feols(edited ~ Feminine + first_post + first_post * Feminine, 
                   data = matched_data_a, 
                   cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_a <- feols(edited ~ Feminine,
                       data = matched_data_a, 
                       cluster = c("OwnerUserId", "share_of_femininity"))


h1_feminine_q <- feols(edited ~ Feminine,
                       data = matched_data_q,
                       cluster = c("OwnerUserId", "share_of_femininity"))

etable(list( h1_feminine_q, h1_feminine_a, h1_full_a), tex = T)


## Make model for estimates - First for full model w interaction

# Get data to dataframe
h1_full_a_df <- as.data.frame(coef(h1_full_a))

# Add confidenceintervals
h1_full_a_df$`min` <- confint(h1_full_a)[,1]
h1_full_a_df$`max` <- confint(h1_full_a)[,2]

# Make model - first mutate names and get the coefficients to be called the right things
h1_full_a_g <- h1_full_a_df %>%  mutate(rnames = row.names(.),
                         coefficients = case_when(rnames == "first_post" ~ "First post",
                                                 rnames == "Feminine:first_post" ~ "Feminine x First post",
                                                 rnames == "Feminine" ~ "Feminine",
                                                 rnames == "(Intercept)" ~ "Intercept"),
                         coefficient = factor(coefficients, ordered = T, levels = c("Intercept",
                                                                       "Feminine",
                                                                       "First post",
                                                                       "Feminine x First post"))) %>% 
  # Then add the points to the plot
  ggplot(aes(x = `coef(h1_full_a)`, y = coefficient )) +
  geom_point(size = 3) +
  # Add line showing the null-hypothesis
  geom_vline(xintercept = 0, 
             color = "#901A1E", lty = "dashed") +
  
  # add confidence interval
  geom_pointrange(aes(xmin = min, xmax = max )) + 
  
  # Add text to data
  labs(title = "OLS-Answers-Interaction",
       y = "",
       x = expression(beta),
       caption = "Data from the Internet Archieve on use of Stack Overflow.
       35,524 matched answers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  +
  scale_x_continuous(limits = c(-0.02, 0.3))
 
  
  
# Make the next model


# Get data to dataframe
h1_feminine_a_df <- as.data.frame(coef(h1_feminine_a))

# Add confidenceintervals
h1_feminine_a_df$`min` <- confint(h1_feminine_a)[,1]
h1_feminine_a_df$`max` <- confint(h1_feminine_a)[,2]


h1_feminine_a_g <- h1_feminine_a_df %>%  mutate(rnames = row.names(.),
                                        coefficients = case_when(rnames == "first_post" ~ "First post",
                                                                 rnames == "Feminine:first_post" ~ "Feminine x First post",
                                                                 rnames == "Feminine" ~ "Feminine",
                                                                 rnames == "(Intercept)" ~ "Intercept"),
                                        coefficient = factor(coefficients, ordered = T, levels = c("Intercept",
                                                                                                   "Feminine",
                                                                                                   "First post",
                                                                                                   "Feminine x First post"))) %>% 
  ggplot(aes(x = `coef(h1_feminine_a)`, y = coefficient )) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, # Null-hypothesis line.
             color = "#901A1E", lty = "dashed") +
  geom_pointrange(aes(xmin = min, xmax = max )) + 
  labs(title = "OLS-Answers",
       y = "",
       x = expression(beta),
       caption = "Data from the Internet Archieve on use of Stack Overflow.
       35,524 matched answers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(-0.02, 0.3))



# Make the next model


# Get data to dataframe
h1_feminine_q_df <- as.data.frame(coef(h1_feminine_q))

# Add confidenceintervals
h1_feminine_q_df$`min` <- confint(h1_feminine_q)[,1]
h1_feminine_q_df$`max` <- confint(h1_feminine_q)[,2]


h1_feminine_q_g <- h1_feminine_q_df %>%  mutate(rnames = row.names(.),
                                                coefficients = case_when(rnames == "Feminine" ~ "Feminine",
                                                                         rnames == "(Intercept)" ~ "Intercept"),
                                                coefficient = factor(coefficients, ordered = T, levels = c("Intercept",
                                                                                                           "Feminine" ))) %>% 
  ggplot(aes(x = `coef(h1_feminine_q)`, y = coefficient )) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, # Null-hypothesis line.
             color = "#901A1E", lty = "dashed") +
  geom_pointrange(aes(xmin = min, xmax = max )) + 
  labs(title = "OLS-Questions",
       y = "",
       x = expression(beta),
       caption = "Data from the Internet Archieve on use of Stack Overflow.
       29,734 matched questions") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(-0.02, 0.3))



  


ggpubr::ggarrange(h1_feminine_q_g, h1_feminine_a_g, h1_full_a_g, ncol = 3)

ggsave("D:/Graphs/final graphs/h1_coefficients.png", height = 5, width = 12)



### Hypothesis 1b: Feminine users are moderated more than masculine users
matched_data_a <- matched_data_a %>%  mutate(more_than_20 = ifelse(change_to_size > 0.2, 1, 0))

matched_data_q <- matched_data_q %>%  mutate(more_than_20 = ifelse(change_to_size > 0.2, 1, 0))

h1b_full_a <- feols(more_than_20 ~ Feminine + first_post + first_post * Feminine, data = matched_data_a, cluster = c("OwnerUserId", "share_of_femininity"))
h1b_feminine_a <- feols(more_than_20 ~ Feminine, data = matched_data_a, cluster = c("OwnerUserId", "share_of_femininity"))


h1b_full_q <- feols(more_than_20 ~ Feminine + first_post + first_post * Feminine, data = matched_data_q, cluster = c("OwnerUserId", "share_of_femininity"))
h1b_feminine_q <- feols(more_than_20 ~ Feminine, data = matched_data_q, cluster = c("OwnerUserId", "share_of_femininity"))

etable(list( h1b_feminine_q, h1b_feminine_a, h1b_full_a), tex = T)



# Get data to dataframe
h1b_full_a_df <- as.data.frame(coef(h1b_full_a))

# Add confidenceintervals
h1b_full_a_df$`min` <- confint(h1b_full_a)[,1]
h1b_full_a_df$`max` <- confint(h1b_full_a)[,2]

# Make model - first mutate names and get the coefficients to be called the right things
h1b_full_a_g <- h1b_full_a_df %>%  mutate(rnames = row.names(.),
                                        coefficients = case_when(rnames == "first_post" ~ "First post",
                                                                 rnames == "Feminine:first_post" ~ "Feminine x First post",
                                                                 rnames == "Feminine" ~ "Feminine",
                                                                 rnames == "(Intercept)" ~ "Intercept"),
                                        coefficient = factor(coefficients, ordered = T, levels = c("Intercept",
                                                                                                   "Feminine",
                                                                                                   "First post",
                                                                                                   "Feminine x First post"))) %>% 
  # Then add the points to the plot
  ggplot(aes(x = `coef(h1b_full_a)`, y = coefficient )) +
  geom_point(size = 3) +
  # Add line showing the null-hypothesis
  geom_vline(xintercept = 0, 
             color = "#901A1E", lty = "dashed") +
  
  # add confidence interval
  geom_pointrange(aes(xmin = min, xmax = max )) + 
  
  # Add text to data
  labs(title = "OLS-Answers-Interaction",
       y = "",
       x = expression(beta),
       caption = "Data from the Internet Archieve on use of Stack Overflow.
       35,524 matched answers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(-0.02, 0.05))



# Make the next model


# Get data to dataframe
h1b_feminine_a_df <- as.data.frame(coef(h1b_feminine_a))

# Add confidenceintervals
h1b_feminine_a_df$`min` <- confint(h1b_feminine_a)[,1]
h1b_feminine_a_df$`max` <- confint(h1b_feminine_a)[,2]


h1b_feminine_a_g <- h1b_feminine_a_df %>%  mutate(rnames = row.names(.),
                                                coefficients = case_when(rnames == "first_post" ~ "First post",
                                                                         rnames == "Feminine:first_post" ~ "Feminine x First post",
                                                                         rnames == "Feminine" ~ "Feminine",
                                                                         rnames == "(Intercept)" ~ "Intercept"),
                                                coefficient = factor(coefficients, ordered = T, levels = c("Intercept",
                                                                                                           "Feminine",
                                                                                                           "First post",
                                                                                                           "Feminine x First post"))) %>% 
  ggplot(aes(x = `coef(h1b_feminine_a)`, y = coefficient )) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, # Null-hypothesis line.
             color = "#901A1E", lty = "dashed") +
  geom_pointrange(aes(xmin = min, xmax = max )) + 
  labs(title = "OLS-Answers",
       y = "",
       x = expression(beta),
       caption = "Data from the Internet Archieve on use of Stack Overflow.
       35,524 matched answers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(-0.02, 0.05)) 


# Make the next model


# Get data to dataframe
h1b_feminine_q_df <- as.data.frame(coef(h1b_feminine_q))

# Add confidenceintervals
h1b_feminine_q_df$`min` <- confint(h1b_feminine_q)[,1]
h1b_feminine_q_df$`max` <- confint(h1b_feminine_q)[,2]


h1b_feminine_q_g <- h1b_feminine_q_df %>%  mutate(rnames = row.names(.),
                                                coefficients = case_when(rnames == "first_post" ~ "First post",
                                                                         rnames == "Feminine:first_post" ~ "Feminine x First post",
                                                                         rnames == "Feminine" ~ "Feminine",
                                                                         rnames == "(Intercept)" ~ "Intercept"),
                                                coefficient = factor(coefficients, ordered = T, levels = c("Intercept",
                                                                                                           "Feminine",
                                                                                                           "First post",
                                                                                                           "Feminine x First post"))) %>% 
  ggplot(aes(x = `coef(h1b_feminine_q)`, y = coefficient )) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, # Null-hypothesis line.
             color = "#901A1E", lty = "dashed") +
  geom_pointrange(aes(xmin = min, xmax = max )) + 
  labs(title = "OLS-Questions",
       y = "",
       x = expression(beta),
       caption = "Data from the Internet Archieve on use of Stack Overflow.
       29,734 matched questions") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(-0.02, 0.05))






ggpubr::ggarrange(h1b_feminine_q_g, h1b_feminine_a_g, h1b_full_a_g, ncol = 3)

ggsave("D:/Graphs/final graphs/h1b_coefficients.png", height = 5, width = 12)

