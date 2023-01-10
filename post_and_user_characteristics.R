# Create variables from posts and users.

###  Import packages
pacman::p_load(dplyr,
               lubridate,
               ggplot2,
               janitor,
               psych
)

# read in csv w users and posts
posts <- read.csv("D:/Data/posts.csv") ## All answers and questions by the random subset


# Due to a filtering of posts (not wanting to include users which has existed less than
# a day), I have to remove posts which are not in the users anymore + users not feminine
# or masculine gendered
# First - load the users
load("D:/Data/df_users_binary.R")


# Then, make in innerjoin with the AccountId and the OwnerUserId, which is the link between the two
# files. Innerjoin ensures, that we only get observations from posts which has an assigned user:
df_posts <-  inner_join((df_users['AccountId'] %>% rename(OwnerUserId = AccountId)) , posts, by = "OwnerUserId") 


# And then - exclude everything which is not questions or answers
df_posts <- df_posts %>% filter(PostTypeId %in% c(1,2))



# Dates -------------------------------------------------------------------

df_posts <- df_posts %>%  mutate(CreationDate = as.Date(CreationDate))


# Due to the "missing" observations in lasteditdate, communityowneddate and closeddate, then
# there is an issue with how we they are mutated. Therefore, they will here be assigned once at a time

df_posts <- left_join(df_posts, (df_posts %>%  filter(LastEditDate != "missing") %>% mutate(LastEditDateF = as.Date(LastEditDate))))

df_posts <- left_join(df_posts, (df_posts %>%  filter(ClosedDate != "missing") %>% mutate(ClosedDateF = as.Date(ClosedDate))))

df_posts <- left_join(df_posts, (df_posts %>%  filter(CommunityOwnedDate != "missing") %>% mutate(CommunityOwnedDateF = as.Date(CommunityOwnedDate))))



# User-specs --------------------------------------------------------------
# Make variable showing how long they have been on platform

df_users <- df_users %>% mutate(time_on_platform = as.Date(lastaccessdate) - as.Date(creation))

# left-join characteristics of users to posts
posts <- df_posts %>%  left_join((df_users  %>%
                                    # When created
                                    transmute(CreationOfUser = as.Date(creation),
                                              # When last accessed
                                              LastAccessUser = as.Date(lastaccessdate),
                                              # Their id - to be able to join them
                                              OwnerUserId = AccountId,
                                              # The time on the platform
                                              time_on_platform = time_on_platform,
                                              # Their gendering
                                              Gender = ifelse(is.na(assumed_gender_checked), "Not identified", assumed_gender_checked),
                                              # Whether they have connected their account to a link
                                              connected_link = ifelse(WebsiteUrl != "missing", 1, 0),
                                              # Their creation year of user
                                              creation_year = year(creation),
                                              # The location of users (whether there is an assigned location)
                                              location = as.numeric(!grepl(pattern = "missing", df_users$location)),
                                              # Whether they assigned their own username
                                              own_username = as.numeric(!grepl(pattern = "user[0-9]", df_users$display_name))
                                    ))) 



# Posts specs -------------------------------------------------------------
# Posts specs made
                          # Length of text-body
posts <- posts %>% mutate(PostLength = nchar(Body),
                          # Moderation proxy
                          ModerationProxy = ifelse(LastEditorUserId == OwnerUserId, 0, 1),
                          # Whether the post is a question
                          Question = ifelse(PostTypeId == 1, 1, 0))


# Days_since_measures ----------------------------------------------------

posts <- posts %>%  mutate(DaysSinceCreationUser = CreationDate - CreationOfUser,
                           YearsSincePlatformStart = year(CreationDate) - 2008,
                           YearsSincePlatformStartFactor = factor(year(CreationDate)),
                           Closed = ifelse(ClosedDate == "missing", 0,1))


save(posts, file = "D:/Data/posts_data_analysis_binary.R")
load("D:/Data/posts_data_analysis_binary.R")