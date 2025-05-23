
library(igraph)
library(tidyverse)
library(labelled)

#rm(list = ls())

## ----------
## LOAD DATA
## ----------

# Recruiting events data
# universities to exclude:
  # Wellesley college; data seem suspect
  # NC State; univ_id ==199193
    # rationale: data seem suspect
  # UC Irvine: univ_id == 110653 [5/23/2025 -- keeping this now]
    # rationale: we have four UCs: berkeley; san diego; irvine; riverside; don't want so many; Irvine and San Diego have same ranking but San Diego has more out-of-state visits and more private hs visits so keep that one; and keep riverside because it has very different rank

#events_data <- read.csv('./data/events_data_2020-07-27.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character', 'event_type' = 'character')) %>% as_tibble()
events_data_temp1 <- read.csv('./data/events_data_2020-10-20.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character', 'event_type' = 'character')) %>% as_tibble()
events_data_marquette <- read.csv('./data/events_data_marquette.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character', 'event_type' = 'character')) %>% as_tibble()
events_data <- events_data_temp1 %>% bind_rows(events_data_marquette) %>% 
  #filter(!(univ_id %in% c('168218','199193','110653')))  # Wellesley, NCSU, UCI
  filter(!(univ_id %in% c('168218','199193')))  # Wellesley, NCSU, UCI
rm(events_data_marquette,events_data_temp1)

# University data from IPEDS
univ_data <- readRDS('./data/ipeds_1718.RDS')
univ_ipeds <- read.csv('./data/ipeds_data.csv', header = TRUE, na.strings = c('', 'NULL', 'NA'), stringsAsFactors = FALSE, colClasses = c('unitid' = 'character')) %>% as_tibble() %>%
  filter(endyear == 2017)
univ_info <- read.csv('./data/univ_data.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character', 'zip_code' = 'character')) %>% as_tibble() %>% 
  filter(!(univ_id %in% c('168218','199193','149222')))  # Wellesley, NCSU, UCI, SIU-Carbondale

# Public HS data from CCD
pubhs_data_1718 <- readRDS('./data/ccd_1718.RDS')
pubhs_data_1415 <- read.csv('./data/meta_high_school_public.csv', header = TRUE, na.strings = c('', 'NA', 'NULL'), stringsAsFactors = FALSE, colClasses = c('ncessch' = 'character')) %>%  # original set used for merging
  mutate(  # total_students = wh + am + as + hi + bl + hp + tr (each is sum of 9-12 grades)
    pct_amerindian = am / total_students * 100,
    pct_asian = as / total_students * 100,
    pct_black = bl / total_students * 100,
    pct_hispanic = hi / total_students * 100,
    pct_nativehawaii = hp / total_students * 100,
    pct_tworaces = tr / total_students * 100,
    pct_white = wh / total_students * 100
  )

# Private HS data from PSS
privhs_data_1718 <- readRDS('./data/pss_1718.RDS')
privhs_data_1516 <- readRDS('./data/pss_1516.RDS')
privhs_data_1314 <- readRDS('./data/pss_1314.RDS')
privhs_overrides <- read.csv('./data/pss_updated_id.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE) %>% as_tibble()

# Rankings data from Niche
niche_data <- read.csv('./data/niche_private.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE) %>% as_tibble()
niche_extra_data <- read.csv('./data/niche_private_middlehigh_2021.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE) %>% as_tibble()
niche_overrides <- read.csv('./data/niche_updated_id.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE) %>% as_tibble()

niche_data <- niche_data %>% bind_rows(niche_extra_data %>% filter(ncessch == 'A9700305'))
rm(niche_extra_data)

# Rankings data from US News & World Report
usnews_data <- read.csv('./data/usnews_rankings.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character')) %>% as_tibble()


## ----------
## PREP DATA
## ----------

# Add overrides for miscategorized events
# View(events_data %>% filter(!(pid %in% c(55696, 44420, 43964, 30678, 25278, 27707, 52235, 32037, 34938, 82251, 43944, 46788, 84552, 64238, 25637, 63281, 68942, 44224, 24830)) | !(school_id %in% c('00299041', 'A0109336', 'A0701311', 'A0702146', 'A0901459', 'A1192055', 'A1303450', 'A9101558', 'A9101599', 'A9105352', 'A9300600', 'BB060710', 'BB161076'))))
table(events_data$event_type, useNA = 'always')

events_data$event_type[events_data$pid %in% c(55696, 44420, 43964, 30678, 25278, 27707, 32037, 82251, 46788, 63281, 68942, 24830, 33944) | events_data$school_id %in% c('00299041', 'A0109336', 'A0701311', 'A0901459', 'A1303450', 'A9101599', 'A9300600')] <- 'pub_hs'
events_data$school_id[events_data$pid == 55696] <- '340126002704'  # Bayonne High School
events_data$school_id[events_data$pid == 44420] <- '170993006467'  # Acero Chtr Netrk - Major Garcia
events_data$school_id[events_data$pid == 43964] <- '170993006454'  # Noble St Chtr-hansberry Prep Slvr
events_data$school_id[events_data$pid == 30678] <- '220117000939'  # Mcdonogh #35 College Preparatory School
events_data$school_id[events_data$pid == 25278] <- '340840003152'  # Lawrence High School
events_data$school_id[events_data$pid == 27707] <- '080002006437'  # Thomas Maclaren State Charter School
events_data$school_id[events_data$pid == 32037] <- '069100514059'  # Contra Costa School Of Performing Arts
events_data$school_id[events_data$pid == 82251] <- '080687001171'  # Telluride High School
events_data$school_id[events_data$pid == 46788] <- '420006100503'  # Philadelphia Academy Cs
events_data$school_id[events_data$pid == 63281] <- '481134000612'  # Bridgeport H S
events_data$school_id[events_data$pid == 68942] <- '450390101620'  # Green Charter School
events_data$school_id[events_data$pid == 24830] <- '422109006413'  # Scranton HS
events_data$school_id[events_data$pid == 33944] <- '401059000474'  # Memorial HS
events_data$school_id[events_data$school_id == '00299041'] <- '130012004145'  # Atlanta Classical Academy
events_data$school_id[events_data$school_id == 'A0109336'] <- '481527013128'  # Veterans Memorial H S
events_data$school_id[events_data$school_id == 'A0701311'] <- '251143001887'  # Swampscott High
events_data$school_id[events_data$school_id == 'A0901459'] <- '170993006499'  # Noble St Chtr Rauner College Prep
events_data$school_id[events_data$school_id == 'A1303450'] <- '482364002483'  # Debakey H S For Health Prof
events_data$school_id[events_data$school_id == 'A9101599'] <- '080336006441'  # VENTURE PREP HIGH SCHOOL
events_data$school_id[events_data$school_id == 'A9300600'] <- '340075103180'  # Great Oaks Legacy Charter School

events_data$event_type[events_data$school_id == 'A9101558'] <- 'pub_4yr'
events_data$ipeds_id[events_data$school_id == 'A9101558'] <- '126775'  # Colorado School of Mines

events_data$event_type[events_data$pid %in% c(52235, 43944, 84552, 64238) | events_data$school_id == 'BB161076'] <- 'other'  # non-profit org, outreach program, etc.
events_data$school_id[events_data$pid %in% c(52235, 43944, 84552, 64238) | events_data$school_id %in% c('BB161076', 'A9101558')] <- NA

events_data <- events_data %>% filter(!(pid %in% c(34938, 44224, 33941, 25637)), !(school_id %in% c('A0702146', 'A1192055', 'BB060710')))  # private HS w/ no NCES ID
table(events_data$event_type, useNA = 'always')


# Clean up events data (drop visits to HS not meeting criteria - 45533 to 44660 obs)
events_df <- events_data %>% 
  select(univ_id, univ_state, event_type, school_id, event_date, event_state, event_loc)

# Override school_id with updated 2017-18 PSS ID
get_pss_override <- function(x) {
  new_id <- privhs_overrides[privhs_overrides$old_id == x, ]$new_id
  ifelse(length(new_id) == 0, x, new_id)
}
v_get_pss_override <- Vectorize(get_pss_override, USE.NAMES = FALSE)

events_df <- events_df %>% mutate(school_id = v_get_pss_override(school_id))


# Combine 2017-18 PSS data w/ past years as needed
pss_missing_ncessch <- setdiff(events_df$school_id[events_df$event_type == 'priv_hs'], privhs_data_1718$ncessch)
pss_1516_ncessch <- privhs_data_1516[privhs_data_1516$ncessch %in% pss_missing_ncessch, ]$ncessch
pss_1314_ncessch <- setdiff(pss_missing_ncessch, pss_1516_ncessch)

privhs_data <- privhs_data_1718 %>%
  dplyr::union(privhs_data_1516 %>% filter(ncessch %in% pss_1516_ncessch)) %>%
  dplyr::union(privhs_data_1314 %>% filter(ncessch %in% pss_1314_ncessch))


# Verify no unmerged pss id
setdiff(events_df$school_id[events_df$event_type == 'priv_hs'], privhs_data$ncessch)

# Criteria for private HS to be included in our analysis is 12th grade enrollment of 10 or more (we're no longer restricting by school type)
# This is the universe of private HS in our analysis (23184 to 4439 obs) - all 2017-18 schools + any previous year's schools that were visited
privhs_data <- privhs_data %>% filter(total_12 >= 10)

privhs_data %>% group_by(year) %>% count()  # 4168 from 2017-18, 116 from 2015-16, 155 from 2013-14


# Filter private HS events to only ones that meet criteria (13880 to 13459 obs)
events_df <- events_df %>% filter(event_type != 'priv_hs' | school_id %in% privhs_data$ncessch)
table(events_df$event_type, useNA = 'always')


# Determine public HS that meet criteria (use 2017-18 if available, otherwise original 2014-15 dataset)
ccd_missing_ncessch <- setdiff(events_df$school_id[events_df$event_type == 'pub_hs'], pubhs_data_1718$ncessch)
ccd_meet_criteria_1718 <- pubhs_data_1718 %>%
  filter(g_12_offered == 'Yes', g12 >= 10, virtual %in% c('NOTVIRTUAL', 'SUPPVIRTUAL'), fipst < 60, updated_status %in% c('1', '3', '8')) %>% 
  mutate(year = '1718') %>% 
  select(year, ncessch, state_code, g12, pct_amerindian, pct_asian, pct_black, pct_hispanic, pct_nativehawaii, pct_tworaces, pct_white,latitude,longitude,sch_name)

# Verify no unmerged ccd id
setdiff(ccd_missing_ncessch, pubhs_data_1415$ncessch)
ccd_meet_criteria_1415 <- pubhs_data_1415 %>%
  filter(g12offered == 1, g12 >= 10, virtual == 0, state_fips_code < 60, updated_status %in% c(1, 3, 8)) %>% 
  mutate(year = '1415') %>% 
  select(year, ncessch, state_code, g12, pct_amerindian, pct_asian, pct_black, pct_hispanic, pct_nativehawaii, pct_tworaces, pct_white,latitude,longitude,name) %>% 
  rename(sch_name = name)

# Universe of public HS meeting criteria (20809 obs)
pubhs_data <- ccd_meet_criteria_1718 %>%
  dplyr::union(ccd_meet_criteria_1415 %>% filter(ncessch %in% ccd_missing_ncessch))
rm(ccd_meet_criteria_1718, ccd_meet_criteria_1415)

pubhs_data %>% group_by(year) %>% count()  # 20756 from 2017-18, 53 from 2014-15

# Filter public HS events to only ones that meet criteria (24397 to 23945 obs)
events_df <- events_df %>% filter(event_type != 'pub_hs' | school_id %in% pubhs_data$ncessch)
table(events_df$event_type, useNA = 'always')

# Add race & enroll categorical variables to public HS data
pubhs_data <- pubhs_data %>% mutate(
  pct_white_cat = case_when(
    pct_white < 50 ~ 'c1_lt50',
    pct_white < 75 ~ 'c2_50to75',
    pct_white < 85 ~ 'c3_75to85',
    pct_white >= 85 ~ 'c4_85+'
  ),
  pct_blacklatinxnative = pct_black + pct_hispanic + pct_amerindian + pct_nativehawaii,
  pct_blacklatinxnative_cat = case_when(
    pct_blacklatinxnative < 10 ~ 'c1_lt10',
    pct_blacklatinxnative < 20 ~ 'c2_10to20',
    pct_blacklatinxnative < 50 ~ 'c3_20to50',
    pct_blacklatinxnative >= 50 ~ 'c4_50+'
  ),
  enroll_cat1 = case_when(
    g12<50  ~ 'c1_lt50',
    g12>=50 & g12<100  ~ 'c2_50to100',
    g12>=100 & g12<150  ~ 'c3_100to150',
    g12>=150  ~ 'c4_gt150'
  ),
  enroll_cat2 = case_when(
    g12<100  ~ 'c1_lt100',
    g12>=100 & g12<150  ~ 'c2_100to150',
    g12>=150 & g12<200  ~ 'c3_150to200',
    g12>=200  ~ 'c4_gt200'
  )
)

# Save dataframes
saveRDS(pubhs_data, file = str_c('./data/pubhs_universe.RDS'))
saveRDS(events_df %>% left_join(univ_info %>% select(univ_id, univ_abbrev, classification), by ='univ_id'), file = str_c('./data/events_data.RDS'))



# Focus on private HS visits by the univs
# Final univ sample: 15 public research, 14 private nationals, 12 private liberal arts
privhs_events <- events_df %>%
  filter(event_type == 'priv_hs')



# Add ranking from Niche data
niche_df <- niche_data %>% mutate(overall_niche_letter_grade = case_when(
  overall_niche_grade == 4.33 ~ 'A+',
  overall_niche_grade == 4 ~ 'A',
  overall_niche_grade == 3.66 ~ 'A-',
  overall_niche_grade == 3.33 ~ 'B+',
  overall_niche_grade == 3 ~ 'B',
  overall_niche_grade == 2.66 ~ 'B-',
  overall_niche_grade == 2.33 ~ 'C+',
  overall_niche_grade == 2 ~ 'C',
  overall_niche_grade == 1.66 ~ 'C-',
  TRUE ~ 'Unranked'
))

# Universe of private HS - NCES data + Niche data
privhs_df <- privhs_data %>% left_join(dplyr::union(
    niche_df %>% select(ncessch, overall_niche_letter_grade, rank_within_category),
    niche_overrides %>% left_join(niche_df %>% select(guid, overall_niche_letter_grade, rank_within_category), by = 'guid') %>% select(-guid)
  ), by = 'ncessch'
  ) %>%
  mutate(type = 'priv hs', control = 'private')

# Save dataframes
saveRDS(privhs_df, file = str_c('./data/privhs_universe.RDS'))

# All NA/unmerged Niche data were checked and manually added if available. There are 33 known true NA's
table((privhs_df %>% filter(ncessch %in% privhs_events$school_id))$overall_niche_letter_grade, useNA = 'always')


# Add ranking from US News & World Report data
usnews_df <- usnews_data %>%
  mutate(type = recode(source,
                       'national-liberal-arts-colleges' = 'lib arts',
                       'national-universities' = 'univ'
                       )) %>%
  select(univ_id, type, control, score_text, rank)

# Create variables from IPEDS data
univ_ipeds <- univ_ipeds %>% 
  mutate(
    pgrnt_p = pgrnt_n / cohortsfaef * 100,
    pctfreshwh = ugftptfreshwhmf / ugftptfreshtot * 100,
    pctfreshbl = ugftptfreshblmf / ugftptfreshtot * 100,
    pctfreshap = ugftptfreshapmf / ugftptfreshtot * 100,  # ap = as + nh
    pctfreshhi = ugftptfreshhimf / ugftptfreshtot * 100,
    pctfreshal = ugftptfreshalmf / ugftptfreshtot * 100,
    pctfreshmr = ugftptfreshmrmf / ugftptfreshtot * 100,
    pctfreshna = ugftptfreshnamf / ugftptfreshtot * 100,
    pctfreshun = ugftptfreshunmf / ugftptfreshtot * 100,
    pctfreshas = ugftptfreshasmf / ugftptfreshtot * 100,
    pctfreshnh = ugftptfreshnhmf / ugftptfreshtot * 100
  )

# Sample of universities - IPEDS data + USNWR data
univ_df <- univ_info %>% select(univ_id, univ_abbrev, classification, state_code) %>%
  left_join(univ_data %>% select(-control, -state_code), by = 'univ_id') %>% 
  left_join(univ_ipeds %>% select(-locale, -sector), by = c('univ_id' = 'unitid')) %>% 
  left_join(usnews_df, by = 'univ_id')

saveRDS(univ_df, file = str_c('./data/univ_sample.RDS'))


# Select variables of interest from private HS universe data to build attributes_df
privhs_df <- privhs_df %>%
  select(ncessch, name, city, state_code, region, religion, pct_white, pct_black, pct_hispanic, pct_asian, pct_amerindian, pct_nativehawaii, pct_tworaces, total_12, type, control, overall_niche_letter_grade, rank_within_category)

# Select variables of interest from univ sample data to build attributes_df
univ_df <- univ_df %>%
  select(univ_id, univ_abbrev, city, state_code, region, religion, pctfreshwh, pctfreshbl, pctfreshhi, pctfreshap, pctfreshna, pctfreshnh, pctfreshmr, ugftptfreshtot, type, control, score_text, rank)

# Create attributes_df
var_names <- c('school_id', 'school_name', 'city', 'state_code', 'region', 'religion', 'pct_white', 'pct_black', 'pct_hispanic', 'pct_asian', 'pct_amerindian', 'pct_nativehawaii', 'pct_tworaces', 'enroll', 'school_type', 'control', 'ranking', 'ranking_numeric')
names(privhs_df) <- var_names
names(univ_df) <- var_names
attributes_df <- dplyr::union(privhs_df, univ_df)


# naming convention:
  # all  
    # g_2mode
    # psi_vec
  # private colleges and universities
    # g_2mode_priv
    # priv_vec
  # public universities
    # g_2mode_pubu
    # pubu_vec
  # public and private universities
    #g_2mode_u
    # univ_vec
  # private universities
    # g_2mode_privu
    # privu_vec
  # private colleges
    # g_2mode_privc
    # privc_vec

##### Vectors of PSIs and of private hs

# vector of all psis (all public and private, colleges and universities)
psi_vec <- unique(privhs_events$univ_id) %>% str_sort(numeric = TRUE)
  #psi_vec
  #psi_vec %>% length()

# vector of private universities and private colleges
priv_vec <- unique((univ_info %>% filter(search_sector == 'PRIVATE', univ_id %in% psi_vec))$univ_id)
  #priv_vec
  #priv_vec %>% length()

# vector of public universities    
pubu_vec <- unique((univ_info %>% filter(search_sector == 'PUBLIC', univ_id %in% psi_vec))$univ_id)
  #pubu_vec
  #pubu_vec %>% length()

# vector of public universities and private universities
univ_vec <- unique((univ_info %>% filter(classification %in% c('public_research','private_national'), univ_id %in% psi_vec))$univ_id)
  #univ_vec
  #univ_vec %>% length()
  
# vector of private universities
privu_vec <- unique((univ_info %>% filter(classification %in% c('private_national'), univ_id %in% psi_vec))$univ_id)
  #privu_vec
  #privu_vec %>% length()    

# vector of private colleges
privc_vec <- unique((univ_info %>% filter(classification %in% c('private_libarts'), univ_id %in% psi_vec))$univ_id)
  #privc_vec
  #privc_vec %>% length()    
  

##### vectors for private high schools visited by particular subsets of postsecondary institutions
#####

# private high schools visited by any psi in df privhs_events  
privhs_vec <- unique(privhs_events$school_id) %>% str_sort(numeric = TRUE)
  privhs_vec %>% length()

# vector of private high schools visited by private universities and private colleges; priv_vec
privhs_visited_by_priv_vec <- unique((privhs_events %>% filter(univ_id %in% priv_vec))$school_id)
  privhs_visited_by_priv_vec %>% length()

# vector of private high schools visited by public universities; pubu_vec
privhs_visited_by_pubu_vec <- unique((privhs_events %>% filter(univ_id %in% pubu_vec))$school_id)
  privhs_visited_by_pubu_vec %>% length()

# vector of private high schools visited by public universities and private universities; univ_vec
privhs_visited_by_univ_vec <- unique((privhs_events %>% filter(univ_id %in% univ_vec))$school_id)  
  privhs_visited_by_univ_vec %>% length()
  
# vector of private high schools visited by private universities; privu_vec
privhs_visited_by_privu_vec <- unique((privhs_events %>% filter(univ_id %in% privu_vec))$school_id)
  privhs_visited_by_privu_vec %>% length()
  
# vector of private high schools visited by private colleges; privc_vec
privhs_visited_by_privc_vec <- unique((privhs_events %>% filter(univ_id %in% privc_vec))$school_id)
  privhs_visited_by_privc_vec %>% length()

  
  
# Check distribution of religion variable
attributes_df %>% filter(school_id %in% psi_vec) %>% select(religion) %>% table(useNA = 'ifany')
attributes_df %>% filter(school_id %in% privhs_vec) %>% select(religion) %>% table(useNA = 'ifany')


## ----------------------------
## CREATE ADDITIONAL VARIABLES
## ----------------------------


# 12th grade enrollment variables
attributes_df <- attributes_df %>% mutate(
  enroll_cat1 = case_when(
    enroll<50  ~ 'c1_lt50',
    enroll>=50 & enroll<100  ~ 'c2_50to100',
    enroll>=100 & enroll<150  ~ 'c3_100to150',
    enroll>=150  ~ 'c4_gt150'
  ),
  enroll_cat2 = case_when(
    enroll<100  ~ 'c1_lt100',
    enroll>=100 & enroll<150  ~ 'c2_100to150',
    enroll>=150 & enroll<200  ~ 'c3_150to200',
    enroll>=200  ~ 'c4_gt200'
  )
)

attributes_df %>% filter(school_type == 'priv hs') %>% count(enroll_cat1)
attributes_df %>% filter(school_type == 'priv hs') %>% count(enroll_cat2)

# Race categorical variables
attributes_df <- attributes_df %>% mutate(
  pct_white_cat = case_when(
    pct_white < 50 ~ 'c1_lt50',
    pct_white < 75 ~ 'c2_50to75',
    pct_white < 85 ~ 'c3_75to85',
    pct_white >= 85 ~ 'c4_85+'
  ),
  pct_blacklatinxnative = pct_black + pct_hispanic + pct_amerindian + pct_nativehawaii,
  pct_blacklatinxnative_cat = case_when(
    pct_blacklatinxnative < 10 ~ 'c1_lt10',
    pct_blacklatinxnative < 20 ~ 'c2_10to20',
    pct_blacklatinxnative < 50 ~ 'c3_20to50',
    pct_blacklatinxnative >= 50 ~ 'c4_50+'
  )
)

# Ranking categorical variables
attributes_df <- attributes_df %>% mutate(
  rank_cat1 = case_when(
    ranking_numeric <= 100 & ranking == 'A+' ~ 'c1_top100',
    ranking_numeric <= 200 & ranking == 'A+' ~ 'c2_top200',
    ranking_numeric > 200 & ranking == 'A+' ~ 'c3_A+',
    ranking != 'A+' & !is.na(ranking) & nchar(school_id) != 6 ~ 'c4_ltA+'
  ),
  rank_cat2 = case_when(
    ranking_numeric <= 200 & ranking == 'A+' ~ 'c1_top200',
    ranking_numeric > 200 & ranking == 'A+' ~ 'c2_A+',
    ranking == 'A' ~ 'c3_A',
    !is.na(ranking) & nchar(school_id) != 6 ~ 'c4_ltA'
  )
)

# Private HS visit counts
privhs_count <- privhs_events %>% group_by(univ_id) %>% summarise(
  privhs_visits_in = sum(univ_state == event_state),
  privhs_visits_out = sum(univ_state != event_state),
  privhs_visits_tot = n()
)
attributes_df <- attributes_df %>% left_join(privhs_count, by = c('school_id' = 'univ_id'))

# Recode region variable
table(attributes_df$region, useNA = 'always')
val_labels(attributes_df$region)  # https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf

v_get_val <- Vectorize(function(x) str_to_lower(val_label(attributes_df$region, x)))
attributes_df <- attributes_df %>% mutate(
  region = v_get_val(region)
)

table(attributes_df$region, useNA = 'always')

# Preview vertex attributes that will be merged
  #View(attributes_df %>% filter(school_id %in% psi_vec))
  #View(attributes_df %>% filter(school_id %in% privhs_vec))

# Export attributes data on the private HS visited by at least 1 univ in our sample
saveRDS(attributes_df %>% filter(school_id %in% privhs_vec), file = str_c('./data/privhs_visited.RDS'))

# Export attributes data on universe of private HS
saveRDS(attributes_df %>% filter(nchar(school_id) == 8), file = str_c('./data/privhs_universe.RDS'))  # all 2017-18 + used schools from past years


## -------------------
## AFFILIATION MATRIX
## -------------------

# Create empty matrix: # of rows = # of HS, # of cols = # of univs
affiliation_matrix <- matrix(0, length(privhs_vec), length(psi_vec))
dimnames(affiliation_matrix) <- list(privhs_vec, psi_vec)

# Populate matrix
for (i in privhs_vec) {
  for (j in psi_vec) {
    affiliation_matrix[i, j] <- privhs_events %>% filter(school_id == i, univ_id == j) %>% nrow()
  }
}


## -------------------------
## ATTACH VERTEX ATTRIBUTES
## -------------------------

# Get vertex and edge info from 2-mode graph: https://stackoverflow.com/a/49738671/6373540
g_2mode <- graph_from_incidence_matrix(affiliation_matrix, weighted = TRUE)
df <- igraph::as_data_frame(g_2mode, 'both')

# Edge list
e_2mode <- df$edges %>% rename(ncessch = 'from', univ_id = 'to') %>% as_tibble()

v_get_loc <- Vectorize(function(x, y) ifelse(privhs_data[privhs_data$ncessch == x, ]$state_code == univ_info[univ_info$univ_id == y, ]$state_code, 'instate', 'outofstate'), USE.NAMES = FALSE)
v_get_states <- Vectorize(function(x, y) paste0(privhs_data[privhs_data$ncessch == x, ]$state_code, '|', univ_info[univ_info$univ_id == y, ]$state_code), USE.NAMES = FALSE)
e_2mode <- e_2mode %>% mutate(
  visiting_univ = case_when(
    univ_id %in% priv_vec ~ 'private',
    univ_id %in% pubu_vec ~ 'public'
  ),
  visit_loc = v_get_loc(ncessch, univ_id),
  state_codes = v_get_states(ncessch, univ_id)
)

# Vertex list (name must come first)
v_2mode <- tibble(name = df$vertices$name, type = df$vertices$type)
v_2mode <- left_join(v_2mode, attributes_df, c('name' = 'school_id'))

# View vertex and edge attributes
#View(v_2mode)
#View(e_2mode)

# Check unmerged
nrow(v_2mode %>% filter(is.na(school_name)))  # unmerged PSS
nrow(v_2mode %>% filter(is.na(ranking), !(name %in% niche_data$ncessch)))  # unmerged Niche (33 known true NA)


## -----------------------------
## CREATE 2-MODE IGRAPH OBJECTS
## -----------------------------

# naming convention:
  # all  
    # g_2mode
    # psi_vec
  # private colleges and universities
    # g_2mode_priv
    # priv_vec
  # public universities
    # g_2mode_pubu
    # pubu_vec
  # public and private universities
    #g_2mode_u
    # univ_vec
  # private universities
    # g_2mode_privu
    # privu_vec
  # private colleges
    # g_2mode_privc
    # privc_vec

# Recreate 2-mode graph with attributes, 
g_2mode <- graph_from_data_frame(d = e_2mode,
                                 directed = FALSE,
                                 vertices = v_2mode)

  #vertex_attr(graph = g_2mode, name = "type") %>% str()
  #sum(vertex_attr(graph = g_2mode, name = "type")) # counts number of vertices where type==TRUE, which refers to universities
  #sum(!(vertex_attr(graph = g_2mode, name = "type"))) # counts number of vertices where type==FALSE, which refers to high schools


# create 2-mode subgraph for private colleges and universities and their private HS visits only
  # g_2mode_priv
  # priv_vec
  # privhs_visited_by_priv_vec
g_2mode_priv <- induced_subgraph(
  graph = g_2mode,
  vids = V(g_2mode)[V(g_2mode)$name %in% c(privhs_visited_by_priv_vec, priv_vec)]
)

  #g_2mode_priv
  E(graph = g_2mode_priv) %>% length() # number of edges
  sum(vertex_attr(graph = g_2mode_priv, name = "type")) # counts number of vertices where type==TRUE, which refers to universities
  sum(!(vertex_attr(graph = g_2mode_priv, name = "type"))) # counts number of vertices where type==FALSE, which refers to high schools
  priv_vec %>% length()
  privhs_visited_by_priv_vec %>% length()


# create 2-mode subgraph for public universities and their private HS visits only
  # g_2mode_pubu
  # pubu_vec
  # privhs_visited_by_pubu_vec
  
g_2mode_pubu <- induced_subgraph(
  graph = g_2mode,
  vids = V(g_2mode)[V(g_2mode)$name %in% c(privhs_visited_by_pubu_vec, pubu_vec)]
)
  
  #E(graph = g_2mode_pubu) %>% length() # number of edges
  #sum(vertex_attr(graph = g_2mode_pubu, name = "type")) # counts number of vertices where type==TRUE, which refers to universities
  #sum(!(vertex_attr(graph = g_2mode_pubu, name = "type"))) # counts number of vertices where type==FALSE, which refers to high schools
  #pubu_vec %>% length()
  #privhs_visited_by_pubu_vec %>% length()


# create 2-mode subgraph for public and private universities and their private HS visits only
  #g_2mode_u
  # univ_vec
  # privhs_visited_by_univ_vec

g_2mode_u <- induced_subgraph(
  graph = g_2mode,
  vids = V(g_2mode)[V(g_2mode)$name %in% c(privhs_visited_by_univ_vec, univ_vec)]
)

  #E(graph = g_2mode_u) %>% length() # number of edges
  #sum(vertex_attr(graph = g_2mode_u, name = "type")) # counts number of vertices where type==TRUE, which refers to universities
  #sum(!(vertex_attr(graph = g_2mode_u, name = "type"))) # counts number of vertices where type==FALSE, which refers to high schools
  #univ_vec %>% length()
  #privhs_visited_by_univ_vec %>% length()


# create 2-mode subgraph for private universities and their private HS visits only
  # g_2mode_privu
  # privu_vec
  # privhs_visited_by_privu_vec

g_2mode_privu <- induced_subgraph(
  graph = g_2mode,
  vids = V(g_2mode)[V(g_2mode)$name %in% c(privhs_visited_by_privu_vec, privu_vec)]
)

  #sum(vertex_attr(graph = g_2mode_privu, name = "type")) # counts number of vertices where type==TRUE, which refers to universities
  #sum(!(vertex_attr(graph = g_2mode_privu, name = "type"))) # counts number of vertices where type==FALSE, which refers to high schools
  #privu_vec %>% length()
  #privhs_visited_by_privu_vec %>% length()
  #E(graph = g_2mode_privu) %>% length() # number of edges


# create 2-mode subgraph for private colleges and their private HS visits only
  # g_2mode_privc
  # privc_vec    
  # privhs_visited_by_privc_vec

g_2mode_privc <- induced_subgraph(
  graph = g_2mode,
  vids = V(g_2mode)[V(g_2mode)$name %in% c(privhs_visited_by_privc_vec, privc_vec)]
)

  sum(vertex_attr(graph = g_2mode_privc, name = "type")) # counts number of vertices where type==TRUE, which refers to universities
  sum(!(vertex_attr(graph = g_2mode_privc, name = "type"))) # counts number of vertices where type==FALSE, which refers to high schools
  privc_vec %>% length()
  privhs_visited_by_privc_vec %>% length()
  E(graph = g_2mode_privc) %>% length() # number of edges




## -----------------------------
## CREATE 1-MODE IGRAPH OBJECTS
## -----------------------------

# [private colleges and universities] Create 1-mode graphs from g_2mode_priv
# [public universities] Create 1-mode graphs from g_2mode_pubu
# [public and private universities] Create 1-mode graphs from g_2mode_u
# [private universities] Create 1-mode graphs from g_2mode_privu
# [private colleges] Create 1-mode graphs from g_2mode_privc  
  
  
#vec_2mode <- c('g_2mode','g_2mode_priv','g_2mode_pubu','g_2mode_u','g_2mode_privu','g_2mode_privc')
#vec_2mode <- c('mode','mode_priv','mode_pubu','mode_u','mode_privu','mode_privc')
vec_2mode <- c('','_priv','_pubu','_u','_privu','_privc')
vec_2mode
    
for(v in vec_2mode) {
  
  writeLines(str_c(''))
  writeLines(str_c('object v=', v))
  
  v2 <- str_c('g_2mode',v)
  writeLines(str_c('object v2=',v2))
  
  v1_hs <- str_c('g_1mode',v,'_hs')
  writeLines(str_c('object v1_hs=',v1_hs))  
  
  v1_psi <- str_c('g_1mode',v,'_psi')
  writeLines(str_c('object v1_hs=',v1_psi))  
  

  projection <- bipartite_projection(graph = get(v2),
                                     types = NULL,
                                     multiplicity = TRUE,
                                     which = c('both'),
                                     probe1 = NULL,
                                     remove.type = TRUE)
  
  hs <- projection[['proj1']]  # all private HS visited
  psi <- projection[['proj2']]  # all univs in sample

  assign(x= v1_hs, value = hs)
  assign(x= v1_psi, value = psi)
  
}


# print out 1 mode objects to see if they look ok

  # all
  g_1mode_hs
  g_1mode_psi

  # [private colleges and universities] Create 1-mode graphs from g_2mode_priv
  g_1mode_priv_hs
  g_1mode_priv_psi
  
  # [public universities] Create 1-mode graphs from g_2mode_pubu
  g_1mode_pubu_hs
  g_1mode_pubu_psi
  
  # [public and private universities] Create 1-mode graphs from g_2mode_u
  g_1mode_u_hs
  g_1mode_u_psi
  
  # [private universities] Create 1-mode graphs from g_2mode_privu
  g_1mode_privu_hs
  g_1mode_privu_psi
  
  # [private colleges] Create 1-mode graphs from g_2mode_privc  
  g_1mode_privc_hs
  g_1mode_privc_psi
  

## --------------------------
## CREATE EGO IGRAPH OBJECTS
## --------------------------
  

list_temp <- list(c('','psi_vec','privhs_vec'),c('_priv','priv_vec','privhs_visited_by_priv_vec'),c('_pubu','pubu_vec','privhs_visited_by_pubu_vec'),c('_u','univ_vec','privhs_visited_by_univ_vec'),c('_privu','privu_vec','privhs_visited_by_privu_vec'),c('_privc','privc_vec','privhs_visited_by_privc_vec'))
#list_temp <- list(c('','psi_vec','privhs_vec'),c('_priv','priv_vec','privhs_visited_by_priv_vec'))  

for(v in list_temp) {

  print('')  
  #print(v)
  
  two_mode_name <- str_c('g_2mode',v[1])
  writeLines(str_c('object two_mode_name=',two_mode_name))
  
  e_hs_name <- str_c('egos_hs',v[1])
  writeLines(str_c('object e_hs_name=',e_hs_name))
  
  e_psi_name <- str_c('egos_psi',v[1])
  writeLines(str_c('object e_psi_name=',e_psi_name))  
  
  psi_vec_name <- v[2]
  hs_vec_name <- v[3]
  
  #print(two_mode)
  print(psi_vec_name)
  print(hs_vec_name)
  
  two_mode_obj <- get(two_mode_name)

  # Create ego objects for each high school  
  
    e_hs_obj <- make_ego_graph(graph = two_mode_obj,order = 2,nodes = V(two_mode_obj)[V(two_mode_obj)$type == FALSE],mindist = 0)  
    #print(get(hs_vec_name))
    
    # name the list of ego networks
    names(e_hs_obj) <- get(hs_vec_name) %>% str_sort()  # name the list of ego networks
    
    # assign order as edge attribute
    for (i in 1:length(e_hs_obj)) {  # assign order as edge attribute
      E(e_hs_obj[[i]])$order <- if_else(str_detect(string = attr(E(e_hs_obj[[i]]), 'vnames'), pattern = names(e_hs_obj)[[i]]), 1, 2)
    }

  # create ego objects for each psi
    e_psi_obj <- make_ego_graph(graph = two_mode_obj,
                               order = 2,
                               nodes = V(two_mode_obj)[V(two_mode_obj)$type == TRUE],  # only include univs
                               mindist = 0)    
  
      
    # name the list of ego networks
    names(e_psi_obj) <- get(psi_vec_name) %>% str_sort()

    # assign order as edge attribute    
    for (i in 1:length(e_psi_obj)) {
      E(e_psi_obj[[i]])$order <- if_else(str_detect(string = attr(E(e_psi_obj[[i]]), 'vnames'), pattern = str_c('\\|', names(e_psi_obj)[[i]])), 1, 2)
    }
    
    
  # assign ego objects desired object name  
    
    assign(x= e_hs_name, value = e_hs_obj)
    assign(x= e_psi_name, value = e_psi_obj)
}  




#egos_hs
# egos_hs[[1]] %>% class()
# E(egos_hs[[1]])$order
# privhs_vec
# 
# egos_psi_priv %>% length()
# egos_psi_priv[[1]]
# egos_psi_priv[1]


