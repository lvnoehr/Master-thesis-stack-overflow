## Sensitivitytest for location

# 

h1_full_a <- feols(edited ~ Feminine + first_post + first_post * Feminine + not_provided_location, 
                   data = matched_data_a, 
                   cluster = c("OwnerUserId", "share_of_femininity"))

h1_feminine_a <- feols(edited ~ Feminine  + not_provided_location,
                       data = matched_data_a, 
                       cluster = c("OwnerUserId", "share_of_femininity"))

etable(list( h1_feminine_q, h1_feminine_a), tex = T)
