

###
#Get the parent id from the answers, to be able to get the tags


pacman::p_load(dplyr
               )


# And - load the posts
load("D:/Data/posts_data_analysis_binary.R")


## Test for overlaps of the data

# 

answers <- posts %>%  filter(Question == 0)
answers[, "ParentID"] %>% write.csv("D:/Data/PostId_Answers.csv")




