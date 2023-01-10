#######################
##  THESIS           ##
##                   ##
##  GENDER COMPUTER  ##
##  TESTS            ##
##                   ##
#######################


# Look through the assignment of gendercomputer. This implies checking how often names 
# are assigned feminine or masculine, checking what the most common names are, checking 
# how often they are assigned.

# Load packages:
pacman::p_load(ggplot2,
               stringr,
               ggpubr)


# Load file w users
load("D:/Data/df_users.R")


# See initial table over assigned gender
df_users %>% tabyl(assumed_gender)


# set seed
set.seed()#number


# Check if the names and assumed gender makes sense - draw a random sample of females, males and unisex
male_users <- df_users %>% filter(assumed_gender == "male") %>% select(c(display_name, location ))

male_users[sample(nrow(male_users), size=100), ] %>%  View()

# Most common masculine names:

(graph_m <- male_users %>% 
  mutate(firstname = str_split_fixed(display_name, " ", 1)) %>% 
  count(firstname, sort = TRUE) %>%
  mutate(firstname = reorder(firstname, n)) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>% 
  ggplot(aes(n, firstname)) +
  geom_col() +
  labs(y = NULL,
       x = "Frequency of Male Identified Users",
       caption = "Top 10 most frequent first names in the 23334 identified male users"))



## With the newly identified male users:

male_users <- df_users %>% filter(assumed_gender_checked == "male")


(graph_m_checked <- male_users %>% 
    mutate(firstname = str_split_fixed(tolower(display_name), " ", 1)) %>% 
    count(firstname, sort = TRUE) %>%
    mutate(firstname = reorder(firstname, n)) %>%
    arrange(desc(n)) %>%
    slice(1:10) %>% 
    ggplot(aes(n, firstname)) +
    geom_col() +
    labs(y = NULL,
         x = "Frequency of Male Identified Users",
         caption = "Top 10 most frequent first names in the 23433 identified male users"))


# Female users
female_users <- df_users %>% filter(assumed_gender == "female") %>% select(c(display_name, location, aboutme, WebsiteUrl ))

female_users[sample(nrow(female_users), size=100), ] %>%  View()
 # Weird w 2 codes: EnaGames, NiceC0der

# When making this graph, it becomes evident, that there is some issues w my code and particularly female
# users. Therefore, I will go through  the identified female users, and recode names which are clearly 
# masculine or not-identified
(graph_f <- female_users %>% 
    mutate(firstname = str_split_fixed(tolower(display_name), " ", 1)) %>% 
    count(firstname, sort = TRUE) %>%
    mutate(firstname = reorder(firstname, n)) %>%
    arrange(desc(n)) %>%
    slice(1:10) %>% 
    ggplot(aes(n, firstname)) +
    geom_col() +
    labs(y = NULL,
         x = "Frequency of Female Identified Names",
         caption = "Top 10 most frequent first names in the 5165 identified female users"))

# List of display names of female assigned users


female_users %>% 
  count(display_name, sort = TRUE) %>%
  mutate(display_name = reorder(tolower(display_name), n)) %>%
  arrange(desc(n)) %>%  View()

# List of names identified as female by gendercomputer, but appearing male
malenames <- c("names")



anonymous <- c("examples"
               )

female_users <- df_users %>% filter(assumed_gender_checked == "female") %>% select(c(display_name, location ))


# THEN - our checked list:
(graph_f_checked <- female_users %>% 
    mutate(firstname_low = tolower(display_name),
           firstname_spl = str_split_fixed(firstname_low, " ", 1)) %>% 
    count(firstname_spl, sort = TRUE) %>%
    mutate(firstname_spl = reorder(firstname_spl, n)) %>%
    arrange(desc(n)) %>%
    slice(1:10) %>% 
    ggplot(aes(n, firstname_spl)) +
    geom_col() +
    labs(y = NULL,
         x = "Frequency of Female Identified Names",
         caption = "Top 10 most frequent first names in the 5096 identified female users"))

# List of display names of female assigned users

female_users %>% 
  mutate(firstname = str_split_fixed(display_name, " ", 1)) %>% 
  mutate(display_name = tolower(display_name)) %>% 
  count(display_name, sort = TRUE) %>% 
  arrange(desc(n)) %>%  View()




## list of names to check

masc_names <- c("examples (100 lines of names)")


df_users %>% 
  mutate(firstname = str_split_fixed(display_name, " ", 1)) %>% 
  mutate(display_name_low = tolower(display_name)) %>% 
  filter(!display_name_low %in% masc_names) 


# Unisex users
unisex_users <- df_users %>% filter(assumed_gender == "unisex") %>% select(c(display_name, location )) %>%  View()

unisex_users[sample(nrow(unisex_users), size=100), ] %>%  View()

malenames <- append(malenames, values = c("masculine example"))
anonymous <- append(anonymous, values = c("examples as 'working account'"))

unisex_users <- df_users %>% filter(assumed_gender_checked == "unisex") %>% select(c(display_name, location ))


(graph_unisex_checked <- unisex_users %>% 
    mutate(firstname = str_split_fixed(tolower(display_name), " ", 1)) %>% 
    count(firstname, sort = TRUE) %>%
    mutate(firstname = reorder(firstname, n)) %>%
    arrange(desc(n)) %>%
    slice(1:10) %>% 
    ggplot(aes(n, firstname)) +
    geom_col() +
    labs(y = NULL,
         x = "Frequency of Unisex Identified Names",
         caption = "Top 10 most frequent first names in the 1095 identified unisex users"))



# not identified users
notidentified_users <- df_users %>% filter(is.na(assumed_gender) & 
                                             !grepl(pattern = "user", x = df_users$display_name)) %>%
  select(c(display_name, location ))

notidentified_users[sample(nrow(notidentified_users), size=500), ] %>%  View()
# Pretty anonymous - might want to add a few gendered words, as "Daddy"

# First - check out if there is 

## Add gendered attributes - as `Daddy´, "dude
female_attributes = regex("mrs |mrs. |miss |princess|girl|female|woman|queen")


female_attributes = c("mrs.", "miss" )


notidentified_users <- df_users %>% filter(is.na(assumed_gender))


(graph_notidentified <- notidentified_users %>% 
    mutate(firstname = str_split_fixed(tolower(display_name), " ", 1)) %>% 
    count(firstname, sort = TRUE) %>%
    mutate(firstname = reorder(firstname, n)) %>%
    arrange(desc(n)) %>%
    slice(1:10) %>% 
    ggplot(aes(n, firstname)) +
    geom_col() +
    labs(y = NULL,
         x = "Frequency of Notidentified Names",
         caption = "Top 10 most frequent first names in the 25593 identified users"))


## Make a combined figure
ggarrange(graph_m, graph_f_checked, graph_unisex_checked, graph_notidentified) %>% 
  annotate_figure(top = text_grob("Most Common First Name (or Word)", 
                                        color = "black", face = "bold", size = 14))
ggsave("D:/Graphs/most_used_names.png")






# New most common names: --------------------------------------------------
female_users_2 <- df_users %>%  filter(assumed_gender_checked == "female")

# THEN - our checked list:
(graph_f_checked <- female_users_2 %>% 
   mutate(firstname_low = tolower(display_name),
          firstname_spl = str_split_fixed(firstname_low, " ", 1)) %>% 
   count(firstname_spl, sort = TRUE) %>%
   mutate(firstname_spl = reorder(firstname_spl, n)) %>%
   arrange(desc(n)) %>%
   slice(1:10) %>% 
   ggplot(aes(n, firstname_spl)) +
   geom_col() +
   labs(y = NULL,
        x = "Frequency of Female Identified Names",
        caption = "Top 10 most frequent first names in the 5096 identified female users"))

