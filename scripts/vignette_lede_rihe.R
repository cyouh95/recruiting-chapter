library(igraph)
library(tidyverse)

options(max.print = 2000)


# CODE TO RUN FIRST

  # create_igraph_objects up through line 539, which creates the object g_2mode
  # network_recruiting_analysis
    # you might need to run lines 1 through 81 to create objects for categorical variables
    # lines 891 through 935; creates two functions, including the function create_2mode_table

  twomode_both <- create_2mode_table(g_2mode)
  twomode_both[[1]] %>% str() # data frame

  pick_schools_df <- twomode_both[[1]]
  pick_schools_df %>% glimpse()
  
  pick_schools_df %>% filter(city == "DALLAS")
  
  pick_schools_df %>% filter(state_code == "TN")  %>% arrange(city,-degree)
  
  pick_schools_df %>% filter(state_code == "MO")
  
  
  pick_schools_df %>% filter(state_code == "TX") %>% arrange(city,-degree)
  
#             name                                  school_name           city state_code region     religion pct_blacklatinxnative  ranking ranking_numeric degree closeness strength  
#01296378 01296378                          HARPETH HALL SCHOOL      NASHVILLE         TN  south nonsectarian              5.660377       A+             337     21 0.4192007       24
#A9300845 A9300845                      MONTGOMERY BELL ACADEMY      NASHVILLE         TN  south nonsectarian              8.984375       A+             260     20 0.3968744       25
#01296764 01296764               UNIVERSITY SCHOOL OF NASHVILLE      NASHVILLE         TN  south nonsectarian             15.023474       A+             127     18 0.3845388       19
#01294971 01294971                      FATHER RYAN HIGH SCHOOL      NASHVILLE         TN  south     catholic              7.768053        A              NA     15 0.4061448       18
#01932509 01932509                           ST CECILIA ACADEMY      NASHVILLE         TN  south     catholic             20.077220        A              NA     15 0.4148753       16
#02163165 02163165                        FRANKLIN ROAD ACADEMY      NASHVILLE         TN  south    christian             10.405405        A              NA     13 0.4007475       15
#01296141 01296141                          THE ENSWORTH SCHOOL      NASHVILLE         TN  south nonsectarian             21.043324       A+             468     12 0.3983488       14
#A9106269 A9106269                  CHRIST PRESBYTERIAN ACADEMY      NASHVILLE         TN  south    christian              3.839590        A              NA     12 0.3928353       13
 
# NEXT, CHOOSE DF WITH EVENTS AND SELECT PARTICULAR HIGH SCHOOLS
  
  events_data %>% glimpse()
  events_data %>% count(event_type)
  
  events_data %>% filter(event_type == 'priv_hs') %>% glimpse()
  
  events_data %>% filter(event_type == 'priv_hs', school_id == '01296378')
  school_id 
  
  #01296378 01296378                          HARPETH HALL SCHOOL
  events_data %>% left_join(y=univ_df %>% rename(univ_name = school_name), by = c('univ_id'= 'school_id')) %>%
    filter(event_type == 'priv_hs', school_id == '01296378') %>% arrange(event_date) %>% 
    select(univ_name, school_id, event_location_name, event_date) %>% print(n=30)
  
#01296378 01296378                          HARPETH HALL SCHOOL  
#A9300845 A9300845                      MONTGOMERY BELL ACADEMY      NASHVILLE         TN  south nonsectarian              8.984375       A+             260     20 0.3968744       25
#01296764 01296764               UNIVERSITY SCHOOL OF NASHVILLE      NASHVILLE         TN  south nonsectarian             15.023474       A+             127     18 0.3845388       19
#01294971 01294971                      FATHER RYAN HIGH SCHOOL      NASHVILLE         TN  south     catholic              7.768053        A              NA     15 0.4061448       18

  
print_visits <- function(pss_id) {
  
  events_data %>%
    left_join(y=univ_df %>% rename(univ_name = school_name) %>% select(school_id,univ_name), by = c('univ_id'= 'school_id')) %>%
    filter(event_type == 'priv_hs', school_id == pss_id) %>% 
    left_join(y = privhs_df %>% select(school_id,school_name,religion,pct_white,pct_black,pct_hispanic,pct_asian,pct_amerindian,pct_nativehawaii,pct_tworaces,enroll,ranking,ranking_numeric),
              by = c('school_id')) %>% 
    arrange(event_date) %>% 
    select(univ_name, school_id, school_name, event_date,religion,enroll,ranking,ranking_numeric,pct_white,pct_black,pct_hispanic,pct_asian) %>% print(n=30)
  
}

#01296378 01296378                          HARPETH HALL SCHOOL  
  print_visits(pss_id = '01296378')
  
#A9300845 A9300845                      MONTGOMERY BELL ACADEMY      NASHVILLE         TN  south nonsectarian              8.984375       A+             260     20 0.3968744       25
  print_visits(pss_id = 'A9300845')
  
#01296764 01296764               UNIVERSITY SCHOOL OF NASHVILLE      NASHVILLE         TN  south nonsectarian             15.023474       A+             127     18 0.3845388       19
  print_visits(pss_id = '01296764')
  
#01294971 01294971                      FATHER RYAN HIGH SCHOOL      NASHVILLE         TN  south     catholic              7.768053        A              NA     15 0.4061448       18
  print_visits(pss_id = '01294971')
  

# A tibble: 18 × 12
   univ_name         school_id school_name             event_date religion enroll ranking ranking_numeric pct_white pct_black pct_hispanic pct_asian
   <chr>             <chr>     <chr>                   <chr>      <chr>     <int> <chr>             <int>     <dbl>     <dbl>        <dbl>     <dbl>
 
 
 