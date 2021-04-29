univ_df %>% glimpse()

univ_info %>% glimpse()

univ_info %>% count(control)

univ_info %>% filter(control != "Public") %>% count(classification)

univ_info %>% filter(control != "Public", classification == 'private_libarts') %>% arrange(univ_name) %>% select(univ_name)

univ_info %>% filter(control != "Public", classification == 'private_national') %>% arrange(univ_name) %>% select(univ_name)



univ_info %>% filter(control == "Public") %>% count(classification)


univ_info %>% filter(control == "Public", classification == 'public_research') %>% arrange(univ_name) %>% select(univ_name)

#####


univ_info %>% left_join(y=univ_df, by = c('univ_id' = 'school_id')) %>% glimpse()

# Investigate characteristics of public research; region
univ_df %>% 
  select(school_id,region,religion,ranking,ranking_numeric) %>% 
  right_join(y=univ_info, by = c('school_id' = 'univ_id')) %>% 
  filter(classification != 'private_libarts') %>% 
  filter(control == 'Public') %>% count(region)

# investigate number of universities by control and region; don't have good coverage of "west" for private research universities
univ_df %>% 
  select(school_id,region,religion,ranking,ranking_numeric) %>% 
  right_join(y=univ_info, by = c('school_id' = 'univ_id')) %>% 
  filter(classification != 'private_libarts') %>% 
  group_by(region) %>% count(control)
  
  
# religion: pretty good mix of religion for private research universities

univ_df %>% 
  select(school_id,region,religion,ranking,ranking_numeric) %>% 
  right_join(y=univ_info, by = c('school_id' = 'univ_id')) %>% 
  filter(classification != 'private_libarts') %>% 
  group_by(control) %>% count(religion)


##############
############## PRIVATE HIGH SCHOOL CHARACTERISTICS THAT MAY BE OF INTEREST
##############

privhs_data %>% glimpse()

privhs_data %>% select(school_type,community_type,locale_code) %>% val_labels()