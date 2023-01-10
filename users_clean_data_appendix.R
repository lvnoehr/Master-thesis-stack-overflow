#######################
##  THESIS           ##
##                   ##
##  DATA GENERATION  ##
##  USERS            ##
##                   ##
##  REMOVE USERS  W  ##
##  NO POSTS OR      ##
##  PROFILES EXISTED ##
##  LESS THAN ONE    ##
##  DAY.             ##
##                   ##
#######################




# Load packages and data --------------------------------------------------

pacman::p_load(dplyr, # For general management
               janitor, # For making tables
               lubridate, # For handling dates
               readr,
               stringr
)

# Load the data in

post <- read.csv("C:/PATH/posts.csv")
users <- read.csv("C:/PATH/users.csv")


### Aggregate posts to make a variable for users showing how many posts they
## have ever made on SO.

(ave_stat <- post %>% 
    
    # Group by user
    group_by(OwnerUserId) %>% 
    
    # Summarise the number of posts
    summarize(number = n()) %>% 
    arrange(desc(number)) %>% 
    
    # Change the variable-name to correspond w the name in the dataset
    rename( AccountId = OwnerUserId))



# Add the time spend on the platform aswell
# Take the last access date and withdraw the creation date
users_w_posts <- users %>% mutate(timonplatform =as.numeric(ymd_hms(lastaccessdate) - ymd_hms(creation) ))


# Remove users w less than 1 post or answer, as these cannot be modified
users_w_posts <- users_w_posts %>% left_join(ave_stat) %>% filter(!is.na(number)) 


# only select people who are more than a day-fly
users_w_posts <- filter(users_w_posts, timonplatform > 24*60*60)
# loses another 10.000! users

# Edit were people are from -----------------------------------------------
## Identified by country. This part has been anonymised due to ethical concerns
# of protecting the users. I have chosen to keep one example, to show how the
# code works generally.


users_w_posts <- users_w_posts %>%  mutate(country = ifelse(grepl("Denmark", users_w_posts$location), "Denmark", country))


## See how many who have not written anything about their location
NumberCountriesMissing <- users_w_posts %>% filter(location != "missing" & nchar(location) >0 ) %>% count(is.na(country))

                                           
# Same code as the initial, but at other levels than countries
users_w_posts <- users_w_posts %>%  mutate(country = ifelse(grepl("Copenhagen", users_w_posts$location), "Denmark", country))


# Count number w/o cities
NumberCountriesMissing_W_Cities <- users_w_posts %>% filter(location != "missing" & nchar(location) >0 ) %>% count(is.na(country))

                                            

# Change object `name`
df_users <- users_w_posts 

# Assign US if users have not named their location themselves
df_users <- df_users %>%  mutate(country_OR_US = ifelse(is.na(country), "USA", country))


# Make file for genderComputer
df_users %>%  subset(select= c(AccountId, country_OR_US, display_name)) %>% write.csv("D:/Data/genderc_users.csv")



# After having read the file to jupyter, and run genderComputer on the usernames and locations,
# we are now taking them back into R

genderc_users_coded <- read_csv("D:/Data/genderc_users_coded.csv") %>% select(c( "AccountId", "country_OR_US",
                                                                                 "display_name", "assumed_gender" ))

df_users <- left_join(df_users, genderc_users_coded, by = c("AccountId", "country_OR_US", "display_name"))


# After having combined the datasets, I did tests / checks of the coding of the four different types.
# It seemed evidens, that there was some issues in the code, particularly of females and unisex.
# I therefore looked through these, and controlled anything that looked odd. 


# List of names identified as female by gendercomputer, but appearing male or anonymous
malenames <- c("examples")

anonymous <- c("examples")

male_attributes = regex("daddy|dude|boi|boy")
female_attributes = regex("mrs |mrs. |miss |princess|girl|female|woman|queen")


df_users <- df_users %>% mutate(assumed_gender_checked = case_when(tolower(df_users$display_name) %in% malenames ~ "male",
                                                                   tolower(df_users$display_name) %in% anonymous ~ NA_character_,
                                                                   grepl(x = tolower(df_users$display_name), pattern = male_attributes) ~ "male",
                                                                   grepl(x = tolower(df_users$display_name), pattern = female_attributes) ~ "female",
                                                                   TRUE ~ assumed_gender
                                                       ))




# Write file to use in general
save(df_users, file = "D:/Data/df_users.R")                                          

write.csv(df_users['AccountId'], "D:/Data/Accounts.csv")

### Make a subset which only includes the feminine and masculine uesrs
df_users <- df_users %>% filter(assumed_gender_checked %in% c("male", "female"))

## Save the file
save(df_users, file = "D:/Data/df_users_binary.R")                                          
