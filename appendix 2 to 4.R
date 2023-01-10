# Appendix 2: Feminine share of users


load("D:/Data/Matched_a.R")
load("D:/Data/Matched_q.R")
load("D:/Data/posts_data_analysis_binary.R")
df <- read.csv("D:/Data/df_pid.csv") ## All actions done to the posts of the random selection of users

pacman::p_load(fixest, dplyr, ggplot2)


h1_full_a <- feols(edited ~ Feminine + first_post + first_post * Feminine + share_of_femininity, 
                   data = matched_data_a,
                   cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_a <- feols(edited ~ Feminine +  share_of_femininity,
                       data = matched_data_a, 
                       cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_q <- feols(edited ~ Feminine +  share_of_femininity,
                       data = matched_data_q,
                       cluster = c("OwnerUserId", "share_of_femininity"))

etable(list( h1_feminine_q, h1_feminine_a, h1_full_a), tex = T)

### Appendix 3: moderation of both posts, tags and edits


# First, make an object with the owner user (the author id) and the post id
postid_and_userid <- posts[,c("OwnerUserId", "X0") ] %>% rename(PostId = `X0`)

# Then make an object with edited posts (posthistorytypeid 4, 5 and 6).
edited_posts_all <- df %>%  filter( PostHistoryTypeId %in% c(4, 5, 6)) %>% 
  
  # To avoid getting people's self edits, then left join the owneruserid and remove
  # observations w same userid (the one making the edit) and owneruserid
  left_join(postid_and_userid) %>% 
  filter(OwnerUserId != UserId) %>% 
  
  #Then group by the post id to enforce a conjoined list and make a new variable
  # called edited all
  group_by(PostId) %>% 
  summarise(edited_all = 1)

# Then add the edited all list (and object) to the psots. For the posts, which were
# not edited, they will not have anything assigned in the edited_all - variable. Add 
# a zero to them

posts_w_new_edit <- posts %>% rename(PostId = `X0`) %>%  left_join(edited_posts_all) %>% 
  mutate(edited_all = ifelse(is.na(edited_all), 0, 1)) %>% subset(select = c("PostId", "edited_all"))

# Then, add the variables to the matched data

matched_data_a <- matched_data_a %>% left_join(posts_w_new_edit)

matched_data_q <- matched_data_q %>% left_join(posts_w_new_edit)




# Estimate the models: w the full edit variable----------------------------------------------------



h1_full_a <- feols(edited_all ~ Feminine + first_post + first_post * Feminine, 
                   data = matched_data_a,
                   cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_a <- feols(edited_all ~ Feminine,
                       data = matched_data_a, 
                       cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_q <- feols(edited_all ~ Feminine,
                       data = matched_data_q,
                       cluster = c("OwnerUserId", "share_of_femininity"))

etable(list( h1_feminine_q, h1_feminine_a, h1_full_a), tex = T)


matched_data_a <- matched_data_a %>%  subset(select= -edited_all)
matched_data_q <- matched_data_q %>%  subset(select= -edited_all)



# Appendix 4: A qualitative cutoff at different levels --------------------
matched_data_a <- matched_data_a %>%  mutate(more_than_5 = ifelse(change_to_size > 0.05, 1, 0),
                                             more_than_10 = ifelse(change_to_size > 0.1, 1, 0),
                                             more_than_20 = ifelse(change_to_size > 0.2, 1, 0),
                                             more_than_30 = ifelse(change_to_size > 0.3, 1, 0),
                                             more_than_40 = ifelse(change_to_size > 0.4, 1, 0),
                                             more_than_50 = ifelse(change_to_size > 0.5, 1, 0)
                                             )

matched_data_q <- matched_data_q %>%  mutate(more_than_5 = ifelse(change_to_size > 0.05, 1, 0),
                                             more_than_10 = ifelse(change_to_size > 0.1, 1, 0),
                                             more_than_20 = ifelse(change_to_size > 0.2, 1, 0),
                                             more_than_30 = ifelse(change_to_size > 0.3, 1, 0),
                                             more_than_40 = ifelse(change_to_size > 0.4, 1, 0),
                                             more_than_50 = ifelse(change_to_size > 0.5, 1, 0)
)




h1_full_a5 <- feols(more_than_5 ~ Feminine + first_post + first_post * Feminine, 
                   data = matched_data_a,
                   cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_a5 <- feols(more_than_5 ~ Feminine,
                       data = matched_data_a, 
                       cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_q5 <- feols(more_than_5 ~ Feminine,
                       data = matched_data_q,
                       cluster = c("OwnerUserId", "share_of_femininity"))
h1_full_a1 <- feols(more_than_10 ~ Feminine + first_post + first_post * Feminine, 
                   data = matched_data_a,
                   cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_a1 <- feols(more_than_10 ~ Feminine,
                       data = matched_data_a, 
                       cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_q1 <- feols(more_than_10 ~ Feminine,
                       data = matched_data_q,
                       cluster = c("OwnerUserId", "share_of_femininity"))
h1_full_a2 <- feols(more_than_20 ~ Feminine + first_post + first_post * Feminine, 
                   data = matched_data_a,
                   cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_a2 <- feols(more_than_20 ~ Feminine,
                       data = matched_data_a, 
                       cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_q2 <- feols(more_than_20 ~ Feminine,
                       data = matched_data_q,
                       cluster = c("OwnerUserId", "share_of_femininity"))
h1_full_a3 <- feols(more_than_30 ~ Feminine + first_post + first_post * Feminine, 
                    data = matched_data_a,
                    cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_a3 <- feols(more_than_30 ~ Feminine,
                        data = matched_data_a, 
                        cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_q3 <- feols(more_than_30 ~ Feminine,
                        data = matched_data_q,
                        cluster = c("OwnerUserId", "share_of_femininity"))
h1_full_a4 <- feols(more_than_40 ~ Feminine + first_post + first_post * Feminine, 
                   data = matched_data_a,
                   cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_a4 <- feols(more_than_40 ~ Feminine,
                       data = matched_data_a, 
                       cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_q4 <- feols(more_than_40 ~ Feminine,
                       data = matched_data_q,
                       cluster = c("OwnerUserId", "share_of_femininity"))


h1_full_a50 <- feols(more_than_50 ~ Feminine + first_post + first_post * Feminine, 
                   data = matched_data_a,
                   cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_a50 <- feols(more_than_50 ~ Feminine,
                       data = matched_data_a, 
                       cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_q50 <- feols(more_than_50 ~ Feminine,
                       data = matched_data_q,
                       cluster = c("OwnerUserId", "share_of_femininity"))



etable(list( h1_feminine_q5, h1_feminine_q1, h1_feminine_q2, h1_feminine_q3, h1_feminine_q4, h1_feminine_q50), tex = T)
etable(list( h1_feminine_a5, h1_feminine_a1,  h1_feminine_a2,h1_feminine_a3, h1_feminine_a4, h1_feminine_a50), tex = T)
etable(list( h1_full_a5, h1_full_a1, h1_full_a2, h1_full_a3, h1_full_a4, h1_full_a50), tex = T)


## Make dataframe for graph
coef_cutoffs <- tibble(Cutoff = factor(c("5", "10", "20", "30", "40", "50"),
                       ordered = T,
                       levels = c("5", "10", "20", "30", "40", "50")
                       ),
       Coefficient = c(0.0239, 0.0248, 0.0185, 0.0226, 0.0181,0.0174 ),
       min  = c(-0.001735062 , 0.002896271 , 0.0006424041 , 0.007018863 , 0.004126543 , 0.005023037  ),
       max = c(.049541053, 0.046685554, 0.036426733, 0.038215947,0.032006895, 0.02975195 ))

coef_cutoffs %>% 
  ggplot(aes(y = Coefficient, x = Cutoff )) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, # Null-hypothesis line.
             color = "#901A1E", lty = "dashed") +
  geom_pointrange(aes(ymin = min, ymax = max )) + 
  labs(title = "Estimated Interaction Effect at Different Cutoffs",
       x = "Different Cutoffs for Thorough Edit",
       y = expression(beta),
       caption = "Estimated interaction effect between an answer as first post and the author's gender
       Data from the Internet Archieve on use of Stack Overflow.
       35,524 matched answers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("D:/Graphs/final graphs/cutoff_coefficients.png", height = 3.5, width = 12)
