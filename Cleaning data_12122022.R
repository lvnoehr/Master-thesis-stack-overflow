####################################################################
##                                                                ##
##     DATA CLEANING OF POST-HISTORIC                             ##
##                                                                ##
##     FIRST LOOK-THROUGH                                         ##
##                                                                ##
##     STARTED ON 051022                                          ##
##                                                                ##
##                                                                ##
##                                                                ##
####################################################################




# Load data and packages---------------------------------------------------------------

pacman::p_load(dplyr,
               janitor,
               tm,
               tidytext,
               psych)

# Load data w. historic over how all posts have been edited over time:
df <- read.csv("D:/Data/df_pid.csv") ## All actions done to the posts of the random selection of users

# Also - load the users 
load("D:/Data/df_users_binary.R")

# And - load the posts
load("D:/Data/posts_data_analysis_binary.R")

## Only keep the posts which are in user-file
df_posts <-  inner_join((df_users['AccountId'] %>% rename(OwnerUserId = AccountId)) , posts, by = "OwnerUserId") 


# And then - exclude everything which is not questions or answers
# ALSO - make variable for a question, the length of the post, and make creation date into a date-object
df_posts <- df_posts %>% filter(PostTypeId %in% c(1,2)) %>% mutate(PostLength = nchar(Body),
                                                              Question = ifelse(PostTypeId == 1, 1, 0),
                                                              CreationDate = as.Date(CreationDate))



# Get the answers' tags and favorite and answer count --------------------

# As the tags, favorite and answer counts are stored with the "parent" question, I have
# loaded the posts which the answers are answering. From these, I will obtain the tags,
# favorite- and answercount

# Load the questions which are answerered
post_answers_columns <- read.csv("D:/Data/posts_answers.csv") %>% 


  # Remove the empty parentId column
  subset(select = -ParentID) %>%
  
  # Transmute the variables, to both rename the columns, subset the dataset, and change the
  # X0 variable which is the post id number, to be the parent ID, for later "pairing" with
  # the original dataset. 
  transmute(ParentID = as.character(`X0`),
            QAnswerCount = AnswerCount,
            QFavoriteCount = FavoriteCount,
            QViewCount = ViewCount,
            
            QTags = Tags)

# Then, left_join the post answers columns, and mutate the original columns for answers (Question ==0) 
# to be the new variables (from post_answer_columns). Then, subset the dataset, to remove the columns 
# from the questions-which-are-answered dataset
posts <- posts %>%  left_join(post_answers_columns) %>% 
  mutate(AnswerCount = ifelse(Question == 0, QAnswerCount, AnswerCount),
         FavoriteCount = ifelse(Question == 0, QFavoriteCount, FavoriteCount),
         ViewCount = ifelse(Question == 0, QViewCount, ViewCount),
         Tags = ifelse(Question == 0, QTags, Tags)) %>% 
  subset(select= -c(QAnswerCount, QFavoriteCount, QViewCount, QTags))


# Finally, remove the object post answers columns

rm(post_answers_columns)

# Location - US or NOT variable -------------------------------------------

posts <- posts %>%  left_join((df_users %>%  transmute(US = ifelse(country_OR_US == "USA", 1, 0),
                        OwnerUserId = AccountId)))




# Good question/answer------------------------------------------------------
# A variable which shows whether a text has been formatted (i.e., any indentation, any code and any references)

original_posts <- df %>%  filter(PostHistoryTypeId == 2) 

df_t <- original_posts%>% 
  mutate(indentation = as.numeric(grepl(pattern = "&#xA;|&#xD;", original_posts$Text)),
               code = as.numeric(grepl(pattern = '(code&gt|code&amp|;code|/code|&lt;code&gt|```&)', original_posts$Text)),
               link = as.numeric(grepl(pattern = "www.|href|http:", original_posts$Text)),
               PostId == PostId)

## Add to posts
posts <- posts %>% rename("PostId" = "X0") %>%  left_join(df_t[,c("indentation", "code", "link", "PostId")])




## Remove object
rm(df_t, original_posts)




# Make a variable wih all of the tags in it, and add a rownumber. Then, unnest words, left join gender,
tags <- posts %>%  mutate(tags = gsub(Tags, pattern = "&lt;|&gt;", replacement = " "),
                          rown = row_number()) %>% 
  unnest_tokens(word, tags) %>%  
  mutate(Feminine = as.numeric(Gender == "female"))


## Group by word, and calculate the share of feminine users
tags_list <- tags %>% 
  group_by(word) %>%  summarise(female_share = mean(Feminine, na.rm = T)) 

# Count number of times a tag occurs
antal_ord <- tags %>%  group_by(word) %>%  count()

## Then add the count of words to the female share of words

tags_list <- tags_list %>%  left_join(antal_ord)



list_of_femininity <- tags %>%  left_join(tags_list) %>% 
  
  # First - get the weighted share (weighted by total n)
  mutate(weighted_female_share = female_share * 1/n) %>% 
  
  # Then group by the posts and get overall measure
  group_by(PostId) %>% 
  
  summarise(weighted_female_share_post = sum(weighted_female_share)/n(),
         Number_of_posts_in_tag = sum(n))


# And - then I will add the feminine users to the post-sample
posts <- posts %>% left_join(list_of_femininity[,c("weighted_female_share_post", "PostId", "Number_of_posts_in_tag")])


# Remove old parts
rm(tags)
rm(tags_list, list_of_femininity)


# Clean text --------------------------------------------------------------


### Clean text for HTML patterns: - function from: https://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
## Question asked by Ryan Warnick, Solution by Scott Richie

cleanFun <- function(htmlString) {
  return(gsub("&#xD;|&#xA;|&quot;|&gt;", "", htmlString))
}


# Show difference between edits and original text

# Make a dataset which only consists of the original posts. Get the userId from here, to create a 
# column which shows whether it is the owner of the post who edits their own text:

originals = df %>% filter(PostHistoryTypeId == 2) %>% mutate(OwnerOfPostId = UserId,
                                                             Text_clean = Text,
                                                             Text = cleanFun(Text))


# Then make a dataframe which only contains changes done to the text-body. 
# As I am only interested in changes made by others to the text, I'll also remove all changes done by
# the user themselves.
# Finally, we need to make a rownumber, for the later unnesting of words

edits = df %>% filter(PostHistoryTypeId == 5) %>% 
  left_join(originals[,c("PostId", "OwnerOfPostId")]) %>% 
  filter(UserId != OwnerOfPostId) %>% 
  mutate(r_number = seq.int(nrow(.)),
         Text_clean = Text,
         Text =   cleanFun(Text))
  

# Next, I'll unnest the tokens and count how often a specific word is occuring in the text.

edits_word = edits %>% unnest_tokens(word, Text) %>%  count(r_number, word) %>% 
  left_join(edits[, c("PostId", "r_number")])

# Afterwards, I do the same to the original posts

originals_words <- originals %>%  filter(PostId %in% edits_word$PostId) %>%  unnest_tokens(word, Text) %>% 
  count(`PostId`, word) %>% rename(original_n = n)



# I'll then join the original posts to the edited posts, to be able to compare them. Notice however,
# that the comparison will be between the first and the last post
edits_word <- edits_word %>% left_join(originals_words[,c("PostId", "word", "original_n")]) %>% 
  mutate(original_n = ifelse(is.na(original_n), 0, original_n))


# Finally, I will calculate the difference (on word-level) between the original and the new

subset_edits_final <- edits_word %>% mutate(difference = ifelse(n >  original_n, n - original_n, original_n - n)) %>% 
  group_by(r_number, PostId) %>% summarise(change = sum(difference, na.rm = T),
                                           original_words = sum(original_n),
                                           edit_words = sum(n)) %>% 
  mutate(edited = 1)


# Then, read in the posts-data
subset_edits_final_1 <- subset_edits_final %>% 
    group_by(PostId) %>% 
    summarise(change = mean(change),
              original_words = sum(original_words),
              change_to_size = change/original_words
              )

posts <- posts %>%  
  left_join(subset_edits_final_1)


# Remove old objects:
rm(edits, originals,
   edits_word, subset_edits_final, 
   originals_words, edited, subset_edits_final_1)




# Control variable for user having edited their own text ------------------

# Make a dataset which only consists of the original posts. Get the userId from here, to create a 
# column which shows whether it is the owner of the post who edits their own text:

originals = df %>% filter(PostHistoryTypeId == 2) %>% mutate(OwnerOfPostId = UserId)


# Then make a dataframe which only contains changes done to the text body by the users themselves

edits_own = df %>% filter(PostHistoryTypeId == 5) %>% 
  left_join(originals[,c("PostId", "OwnerOfPostId")]) %>% 
  filter(UserId == OwnerOfPostId)

# Then make a new variable 
posts <- posts %>% mutate(self_edited = as.numeric(posts$PostId %in% edits_own$PostId)) 



# Continious variable for edits -------------------------------------------

n_edits <- df %>% filter(PostHistoryTypeId == 5) %>% 
  
  # Filter away people who are editing themselves (the majority)
  left_join(originals[, c("PostId", "OwnerOfPostId")]) %>% 
  filter(OwnerOfPostId != UserId) %>% 
  
  # Then group by postid and count the number of edits
  group_by(PostId) %>%  
  count() %>%  rename(n_edits = n)

posts <- posts %>%  left_join(n_edits) %>% 
  mutate(n_edits = ifelse(is.na(n_edits), 0, as.numeric(n_edits)),
         edited = as.numeric(n_edits >0)) 


# Remove old objects:
rm(edits_own, originals, n_edits)

# Make control for posts which are migrated or merged (offtopic o --------

posts <- posts %>% mutate(merged_or_migrated = as.numeric(posts$PostId %in% filter(df, PostHistoryTypeId %in% c(35, 36, 37, 38))$PostId))


# Remove posts which are migrated or merged

posts <- posts %>%  filter(merged_or_migrated != 1)


# Control for users rollback of their edits -------------------------------

roll_back_edits_posts <- filter(df, PostHistoryTypeId == 8)$PostId

posts <- posts %>% mutate(rollback_edits = as.numeric(PostId %in% roll_back_edits_posts))

rm(roll_back_edits_posts)



# Control - users answer own post -----------------------------------------
## As ~a third of all posts on SO is answered by the one who asks the question,
# and these might face less edits of others, it might be interesting to have this 
# as a control variable

#First - find all answers
answers <- posts %>%  filter(Question == 0)

# Then - make a list of postIds
postsid <-  posts$PostId

# Then find parentids in postids - indicating that the answers are in the posts. As
# the dataset has all posts by users in here, I expect answers by the same users to be in here
answers_to_q_in_df <- answers %>%  mutate(ParentID = as.integer(ParentID)) %>% 
  filter(ParentID %in% postsid)


# Then, filter the users in question, get their post id and question id
users_in_questions <- posts %>%  filter(PostId %in% answers_to_q_in_df$ParentID) %>% 
  subset(select = c(PostId, OwnerUserId)) %>% rename(ParentID = PostId,
                                                     QuestionId = OwnerUserId)

# And add these to the object
answers_to_q_in_df <- answers_to_q_in_df %>% left_join(users_in_questions) %>% filter(OwnerUserId == QuestionId)


# And make variables from this
posts <- posts %>% mutate(answer_own_question = as.numeric(PostId %in% answers_to_q_in_df$PostId),
                 answer_to_own_question = as.numeric(PostId %in% answers_to_q_in_df$ParentID))

rm(users_in_questions, answers_to_q_in_df, answers)

# Make posts counts and quantiles of change to post -----------------------


posts <- posts %>% 
  mutate(Change_words = change/original_words) %>% 
  arrange("change_to_size") %>% 
  mutate(Change_words_n = cut(row_number(change_to_size), 5, labels = c("Q1-20", "Q20-40",
                                                                      "Q40-60", "Q60-80",
                                                                      "Q80-100")),
         Gender_ = ifelse(Gender == "female", "Feminine", "Masculine"),
         Feminine = ifelse(Gender_ == "Feminine", 1, 0),
         more_than_50 = ifelse(change_to_size > 0.50, 1, 0),
         more_than_60 = ifelse(change_to_size > 0.60, 1, 0)) %>%
  group_by(OwnerUserId) %>% 
  arrange("time_on_platform") %>% 
  mutate(posts = row_number()) %>% 
  ungroup() %>% 
  arrange(OwnerUserId, CreationDate ) %>% 
  group_by(OwnerUserId) %>% 
  mutate(post_number = row_number(),
         date_diff_2 = as.numeric(lead(CreationDate) - CreationDate),
         Post_prior_change_to_size = lead(change_to_size), 
         Post_prior_edited = lead(edited), 
         Posts_created_ever = n()) %>% 
  ungroup() %>%
  
  ## Also make change to size = 0 if never changed, and make length numerical, a variable
  # for first post and make answers cound and viewcound numerical as well
  mutate(change_to_size = ifelse(is.na(change_to_size), 0, change_to_size),
         length_body = nchar(Body),
         first_post = ifelse(posts == 1, 1, 0),
         AnswerCount_n = ifelse(AnswerCount == "missing", 0,
                                   as.integer(AnswerCount)),
         ViewCount_n = ifelse(ViewCount == "missing", 0,
                                 as.numeric(ViewCount)),
         CommentCount_n = ifelse(CommentCount == "missing", 0,
                                    as.numeric(CommentCount)),
         FavoriteCount_n = ifelse(FavoriteCount == "missing", 0,
                                     as.numeric(FavoriteCount))) %>% 
  group_by(OwnerUserId) %>%  arrange(posts) %>% 
    mutate(last_post = posts == max(posts)) %>%  ungroup()



# Creation year + mutation of question

# Make a year of when the post was initially created
year_creation <- df %>%  filter(PostHistoryTypeId == 2) %>% mutate(post_creation_year = lubridate::year(CreationDate)) %>% 
  group_by(UserId) %>%
  arrange(CreationDate) %>% 
  mutate(number_of_posts = n(),
         CreationDate = as.Date(CreationDate),
    date_diff =                        as.numeric(lead(CreationDate) - CreationDate)) %>% 
  ungroup() 

# Join variable
posts <- posts %>% left_join(year_creation[, c("PostId", "post_creation_year", "date_diff")])


# Make object for the analysis --------------------------------------------


# And combine them in an aggregated format
df_analysis_1 <- posts 



save(df_analysis_1, file = "D:/Data/analysis_1.R")

