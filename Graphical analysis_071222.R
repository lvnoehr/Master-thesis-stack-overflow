## New graphical analysis of iv & dv

load("D:/Data/analysis_1.R")
load("D:/Data/Matched_a.R")
load("D:/Data/Matched_q.R")

pacman::p_load(ggplot2, dplyr)



# Feminine users ----------------------------------------------------------


df_analysis_1 %>% 
  mutate(Question_num = ifelse(Question == 0, "Answers", "Questions"),
         Questions = factor(Question_num, levels = c("Questions", "Answers")),
         Feminine = Gender_) %>%
  group_by(Questions, Feminine) %>% 
  summarise(Share = n()) %>% 
  ggplot(aes(y = Feminine, x = Share, label = Share)) +
  geom_bar(stat = "identity", fill = "grey", colour = "black" ) +
  geom_text(hjust = 1.0) +
  facet_wrap(~Questions, nrow = 2) +
  theme_minimal() + 
  scale_x_continuous(breaks = (nrow(df_analysis_1) * c(0:6)*0.1), 
                     labels = c("0 pct", "10 pct",  "20 pct", "30 pct", "40 pct", "50pct", "60 pct"),
                     limits = c(0, 160000)) +

    labs(title = "Posts by Gender",
       x = "Posts in dataset",
       y = " ",
       caption = "Data from the Internet Archieve on use of Stack Overflow.
       105,528 questions and 158,893 answers.") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggsave("D:/Graphs/final graphs/distribution_of_posts.png", height = 3.5, width = 10)

# H1 ----------------------------------------------------------------------

df_analysis_1 %>% 
  mutate(Question_num = ifelse(Question == 0, "Answers", "Questions"),
         Questions = factor(Question_num, levels = c("Questions", "Answers")),
         Feminine = Gender_) %>%
  group_by(Questions, Feminine) %>% 
  summarise(Edited = mean(edited)) %>% 
  ggplot(aes(y = Feminine, x = Edited, label = round(Edited, digits = 3))) +
  geom_bar(stat = "identity", orientation = "y") +
  geom_text(hjust = 1.5, color = "white") +
  facet_wrap(~Questions, nrow = 2) +
  theme_minimal() + 
  labs(title = "Share of Posts Edited Conditioned on Gender",
       x = "Share of posts edited",
       y = " ",
       caption = "Data from the Internet Archieve on use of Stack Overflow.
       105,528 questions and 158,893 answers.") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("D:/Graphs/final graphs/share_of_posts_edited.png", height = 3.5, width = 10)


# H2 ----------------------------------------------------------------------

(h2_2 <- df_analysis_1 %>% 
  filter(Question == 0,
         first_post == 1) %>% 
  mutate(Feminine = Gender_) %>%
  group_by(Feminine) %>% 
  summarise(Edited = mean(edited)) %>% 
  ggplot(aes(y = Feminine, x = Edited, label = round(Edited, digits = 3))) +
  geom_bar(orientation = "y", stat = "identity", width = 0.5) +
  geom_text(hjust = 2.5, color = "white") +
  theme_minimal() + 
  labs(title = "Share of First Posts as Answers Edited \n Conditioned on Gender",
       x = "Share of first posts as answers edited",
       y = " ",
       caption = "
       Data from the Internet Archieve on use of Stack Overflow.
       7,572 answers.") +
  theme(plot.title = element_text(hjust = 0.5)))


(h2_1 <- df_analysis_1 %>% 
  filter(Question == 0,
         first_post == 1) %>% 
  mutate(Feminine = Gender_) %>%

  ggplot(aes(x = Feminine, y = log(change_to_size))) +
  geom_violin() +
  theme_minimal() + 
  labs(title = "Thoroughness of Edits on First Posts as Answers \n Conditioned on Gender",
       y = "Thoroughness of edits in log on \n first posts as answers",
       x = " ",
       caption = "Orange line signalling changes of more than 20 percent of the words.
       Data from the Internet Archieve on use of Stack Overflow.
       1,923 answers.") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = log(0.2), color = "darkorange")
)

ggpubr::ggarrange(h2_2, h2_1)

ggsave("D:/Graphs/final graphs/h2_graphical.png", height = 3.5, width = 10)


# H1 throughoutness -------------------------------------------------------


df_analysis_1 %>% 
  filter(edited == 1) %>% 
  mutate(Question_num = ifelse(Question == 0, "Answers", "Questions"),
         Questions = factor(Question_num, levels = c("Questions", "Answers")),
         Feminine = Gender_) %>%
  group_by(Questions, Feminine) %>% 
  ggplot(aes( y =log(change_to_size), x = Feminine)) +
  geom_violin(fill = "white") +
  facet_wrap(~Questions) +
  theme_minimal() + 
  labs(title = "Thoroughness of Edits on First Posts as Answers \n Conditioned on Gender",
       y = "Thoroughness of Edits in log",
       x = " ",
       caption = " Orange line signalling 20 percent of the words being changed
       Data from the Internet Archieve on use of Stack Overflow.
       From 28,715 edited questions and 14,251 edited answers.") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = log(0.2), color = "darkorange")
ggsave("D:/Graphs/final graphs/thoroughness_h1_graphical.png", height = 3.5, width = 10)



# h2 days between posts ---------------------------------------------------

(df_analysis_1 %>% 
  mutate(Edit = ifelse ( edited == 1, "Edited", "Not edited")) %>%
  group_by(edited) %>% 
  ggplot(aes( y = log(date_diff), x = Edit, group = Edit)) +
  geom_violin(fill = "white") +
  theme_minimal() + 
  labs(title = "Time between Posts Conditioned on Gender & Edits",
       y = "Days between posts in log", 
       x = " ",
       caption = "Data from the Internet Archieve on use of Stack Overflow.
       87,816 questions and 148,893 answers, excluding users' last answer.") +
  theme(plot.title = element_text(hjust = 0.5)))

ggsave("D:/Graphs/final graphs/days_between_edits.png", height = 3.5, width = 10)


# H2 edited post ----------------------------------------------------------

largest_post_number <- df_analysis_1 %>% 
  group_by(OwnerUserId) %>% 
  summarise(largest_post = max(post_number))

df_analysis_1 %>% left_join(largest_post_number) %>% 
  mutate(post_before_last = post_number - largest_post,
         last_post = ifelse(last_post == 1, "Last Post", "Not Last Post")) %>% 
  filter(post_before_last > -20) %>% 
  ungroup %>% 
  # Filter by 100, to not get it too long!
  group_by(last_post) %>% 
  summarize(Edited = mean(edited)) %>% 
  ungroup() %>% 
  mutate(Last = ifelse(last_post == 0, 1, 0)) %>% 
  ggplot(aes(y = last_post, x = Edited, label = round(Edited, digits = 3))) +
  geom_bar(stat = "identity", alpha = 0.8, color = "black", orientation = "y", width = 0.5) +
  theme_minimal() + 
  scale_fill_manual(values=c("lightgrey", "darkgrey"))  +
  labs(title = "Last Post Edited by Gender",
     x = "Share of Posts Edited",
     y = " ",
     caption = "Data from the Internet Archieve on use of Stack Overflow.
       105,528 questions and 158,893 answers.") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(hjust = 2.3, position = position_dodge(width = 0.9), color = "white")

ggsave("D:/Graphs/final graphs/last post.png", height = 3.5, width = 10)


