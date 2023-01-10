# sensitivity test of feminine perception

load("D:/Data/analysis_1.R")
load("D:/Data/Matched_a.R")
load("D:/Data/Matched_q.R")


pacman::p_load(dplyr, fixest, ggplot2)

# Make variable that shows whether the post prior have been edited

former_edited <- df_analysis_1[, c("OwnerUserId", "CreationDate", "edited", "PostId")] %>% 
  arrange(OwnerUserId, CreationDate ) %>% 
  group_by(OwnerUserId) %>% 
  mutate(former_post_edited = lag(edited)) %>% 
  ungroup()

matched_data_a <- matched_data_a %>%  left_join(former_edited)
matched_data_q <- matched_data_q %>%  left_join(former_edited)


rm(former_edited)



df_analysis_1 %>% 
  filter(PostId %in% matched_data_a$PostId | PostId %in% matched_data_q$PostId,
         post_number < 51) %>% 
  mutate(Question_ = ifelse(Question == 1, "Question", "Answer"),
         Questions = factor(Question_, ordered = T, levels = c("Question", "Answer") )) %>% 
  group_by(Questions, Gender_, post_number) %>% 
  summarise(Edited = mean(edited)) %>% 
  rename(Gender = Gender_) %>% 
  ggplot(aes(y = Edited, x = post_number, linetype = Gender, shape = Gender)) +
  geom_point(aes(color = Gender )) + 
  geom_line() +
  scale_colour_manual(values=c("black", "orange")) +
  facet_wrap(~ Questions) +
  theme_minimal() + 
  labs(title = "Share of Posts Edited over Number of Posts Written",
       y = "Share of posts edited",
       x = "Post number by user",
       caption = "Data from the Internet Archieve on use of Stack Overflow.
       25,095 matched questions and 22,858  matched answers.") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("D:/Graphs/final graphs/feminine_perception.png", height = 3.5, width = 10)



model_a1 <- feols(edited ~ Feminine + former_post_edited  + post_number + Feminine * former_post_edited | OwnerUserId, 
                 data = matched_data_a, cluster = c("OwnerUserId")) 
model_b1 <- feols(edited ~ Feminine + former_post_edited +  post_number + Feminine * former_post_edited| OwnerUserId , 
                 data = matched_data_q, cluster = c("OwnerUserId"))



model_a <- feols(edited ~ Feminine + former_post_edited   +  post_number | OwnerUserId , 
     data = matched_data_a, cluster = c("OwnerUserId", "share_of_femininity")) 
model_b <- feols(edited ~ Feminine + former_post_edited  +  post_number | OwnerUserId, 
     data = matched_data_q, cluster = c("OwnerUserId", "share_of_femininity"))

etable(list( model_b1, model_a1), tex = T)




# Share edited ------------------------------------------------------------
matched_data_a <- matched_data_a %>%  mutate(more_than_20 = ifelse(change_to_size > 0.2, 1, 0))

matched_data_q <- matched_data_q %>%  mutate(more_than_20 = ifelse(change_to_size > 0.2, 1, 0))



model_a <- feols(more_than_20 ~ Feminine + former_post_edited   +  post_number + Feminine * former_post_edited, 
                 data = matched_data_a, cluster = c("OwnerUserId", "share_of_femininity")) 
model_b <- feols(more_than_20 ~ Feminine + former_post_edited  +  post_number + Feminine * former_post_edited , 
                 data = matched_data_q, cluster = c("OwnerUserId", "share_of_femininity"))

etable(list( model_b, model_a), tex = T)


