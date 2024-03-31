library(tidyverse)
library(labelled)


pss_vars <- c(
  # directory info
  'ppin', 'pinst', 'paddrs', 'pl_add', 'pcity', 'pstabb', 'pzip', 'p415',
  # offered/enrolled grades 09-12
  'p265', 'p270', 'p275', 'p280', 'p285', 'p290', 'p295', 'p300',
  # total students and by gender and race
  'numstuds', 'p330', 'p325', 'p320', 'p316', 'p310', 'p318', 'p332',
  # percent students by race
  'p_white', 'p_black', 'p_hisp', 'p_asian', 'p_indian', 'p_pacific', 'p_tr',
  # religious affiliation and associations
  'p430', 'p440', 'p455', 'p460', 'p465', 'p520',
  # additional info
  'males', 'p360', 'region', 'ucommtyp'
)

pss_vars_root <- c('ulocale', 'latitude', 'longitude')  # variables w/ suffixes based on year

pss_years <- c('1718', '1516', '1314')


for (pss_year in pss_years) {
  end_year <- str_sub(pss_year, start = 3)
  
  # Read in data and dictionary
  pss <- read.csv(str_c('./data/pss_', pss_year,'.csv'), header = TRUE, na.strings = c('', 'NA'), stringsAsFactors = F,  colClasses = c('PZIP' = 'character', 'pzip' = 'character'))
  pss_dictionary <- read.csv(str_c('./data/pss_dictionary_', pss_year, '.csv'), header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)
  
  # Turn variables to lowercase
  names(pss) <- tolower(names(pss))
  names(pss_dictionary) <- tolower(names(pss_dictionary))
  pss_dictionary <- pss_dictionary %>% mutate(names_lc = tolower(name))
  
  # Select variables of interest
  pss <- pss %>% select(c(all_of(pss_vars), str_c(pss_vars_root, end_year)))
  
  # Use 0=No/1=Yes instead of 1=Yes/2=No (ie. change the 2 to 0)
  pss <- pss %>% mutate(p265 = ifelse(p265 == 1, 1, 0),
                        p275 = ifelse(p275 == 1, 1, 0),
                        p285 = ifelse(p285 == 1, 1, 0),
                        p295 = ifelse(p295 == 1, 1, 0),
                        p430 = ifelse(p430 == 1, 1, 0))
  
  # Label variables
  for (i in names(pss)) { 
    description <- (pss_dictionary %>% filter(names_lc == i))$label
    var_label(pss[[i]]) <- description
  }
  
  # Rename variables
  pss <- pss %>% rename(
    ncessch = 'ppin',
    name = 'pinst',
    address = 'paddrs',
    address_secondary = 'pl_add',
    city = 'pcity',
    state_code = 'pstabb',
    zip_code = 'pzip',
    school_type = 'p415',
    offered_09 = 'p265',
    total_09 = 'p270',
    offered_10 = 'p275',
    total_10 = 'p280',
    offered_11 = 'p285',
    total_11 = 'p290',
    offered_12 = 'p295',
    total_12 = 'p300',
    total_students = 'numstuds',
    total_white = 'p330',
    total_black = 'p325',
    total_hispanic = 'p320',
    total_asian = 'p316',
    total_amerindian = 'p310',
    total_nativehawaii = 'p318',
    total_tworaces = 'p332',
    pct_white = 'p_white',
    pct_black = 'p_black',
    pct_hispanic = 'p_hisp',
    pct_asian = 'p_asian',
    pct_amerindian = 'p_indian',
    pct_nativehawaii = 'p_pacific',
    pct_tworaces = 'p_tr',
    is_religious = 'p430',
    religious_orientation = 'p440',
    accelerated_christian_edu = 'p455',
    amer_assoc_christian_sch = 'p460',
    assoc_christian_sch_intl = 'p465',
    oral_roberts = 'p520',
    total_male = 'males',
    pct_to_4yr = 'p360',
    community_type = 'ucommtyp',
    locale_code = str_c('ulocale', end_year),
    latitude = str_c('latitude', end_year),
    longitude = str_c('longitude', end_year)
  )
  
  # Label values
  pss <- pss %>% set_value_labels(
    school_type = c('Regular elementary or secondary' = 1, 
                    'Montessori' = 2,
                    'Special program emphasis' = 3,
                    'Special education' = 4,
                    'Career/technical/vocational' = 5,
                    'Alternative/other' = 6,
                    'Early childhood program/child care center' = 7),
    religious_orientation = c('Valid Skip' = -1,
                              'Roman Catholic' = 1,
                              'African Methodist Episcopal' = 2,
                              'Amish' = 3,
                              'Assembly of God' = 4,
                              'Baptist' = 5,
                              'Brethren' = 6,
                              'Calvinist' = 7,
                              'Christian (no specific denomination)' = 8,
                              'Church of Christ' = 9,
                              'Church of God' = 10,
                              'Church of God in Christ' = 11,
                              'Church of the Nazarene' = 12,
                              'Disciples of Christ' = 13,
                              'Episcopal' = 14,
                              'Friends' = 15, 
                              'Greek Orthodox' = 16,
                              'Islamic' = 17,
                              'Jewish' = 18,
                              'Latter Day Saints' = 19,
                              'Lutheran Church - Missouri Synod' = 20, 
                              'Evangelical Lutheran Church in America' = 21,
                              'Wisconsin Evangelical Lutheran Synod' = 22,
                              'Other Lutheran' = 23,
                              'Mennonite' = 24, 
                              'Methodist' = 25, 
                              'Pentecostal' = 26,
                              'Presbyterian' = 27, 
                              'Seventh-Day Adventist' = 28,
                              'Other' = 29),
    region = c('Northeast' = 1,
               'Midwest' = 2,
               'South' = 3,
               'West' = 4),
    community_type = c('City' = 1,
                       'Suburb' = 2, 
                       'Town' = 3,
                       'Rural' = 4),
    locale_code = c('City, large' = 11,
                'City, midsize' = 12, 
                'City, small' = 13, 
                'Suburb, large' = 21,
                'Suburb, midsize' = 22,
                'Suburb, small' = 23,
                'Town, fringe' = 31,
                'Town, distant' = 32, 
                'Town, remote' = 33, 
                'Rural, fringe' = 41,
                'Rural, distant' = 42,
                'Rural, remote' = 43)
  )
  
  # val_labels(pss$religious_orientation)
  christian <- c(1:16, 19:28)  # everything except Valid Skip, Islamic, Jewish, and Other
  
  # Add religious affiliation
  pss <- pss %>% mutate(is_christian = ifelse(religious_orientation %in% christian, 1, 0),
                        is_conservative = ifelse(accelerated_christian_edu == 1 | amer_assoc_christian_sch == 1 | assoc_christian_sch_intl == 1 | oral_roberts == 1, 1, 0))
  
  pss <- pss %>% mutate(
    religion_5 = case_when(
      is_religious == 0 ~ 'nonsectarian',
      religious_orientation == 1 ~ 'catholic',
      is_conservative == 1 ~ 'conservative_christian',
      is_christian == 1 ~ 'other_christian',
      TRUE ~ 'other_religion'
    ),
    religion_4 = ifelse(religion_5 %in% c('other_christian', 'other_religion'), 'other_religion', religion_5),
    religion = case_when(
      religion_5 %in% c('conservative_christian', 'other_christian') ~ 'christian',
      religion_5 == 'other_religion' ~ 'other',
      TRUE ~ religion_5
    )
  )
  
  # Add data year
  pss$year <- pss_year
  
  # Export data
  pss <- as_tibble(x = pss)
  saveRDS(pss, file = str_c('./data/pss_', pss_year, '.RDS'))
}
