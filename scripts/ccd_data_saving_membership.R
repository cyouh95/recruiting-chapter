library(tidyverse)
library(readxl)


data_dir <- file.path('.', 'data')


# Read in directory data and dictionary (102337 obs. of 65 variables)
ccd_directory <- read.csv(file.path(data_dir, 'ccd_directory_1718.csv'), header = TRUE, na.strings = c('', 'NA'), stringsAsFactors = F,  colClasses = c('FIPST' = 'character', 'LEAID' = 'character', 'NCESSCH' = 'character', 'SCHID' = 'character', 'MZIP' = 'character', 'MZIP4' = 'character', 'LZIP' = 'character', 'LZIP4' = 'character', 'SY_STATUS' = 'character', 'UPDATED_STATUS' = 'character', 'SCH_TYPE' = 'character', 'STATE_AGENCY_NO' = 'character', 'UNION' = 'character'))

# Turn variables to lowercase
names(ccd_directory) <- tolower(names(ccd_directory))


# Read in characteristics data and dictionary (99899 obs. of 20 variables)
ccd_characteristics <- read.csv(file.path(data_dir, 'ccd_characteristics_1718.csv'), header = TRUE, na.strings = c('', 'NA'), stringsAsFactors = F,  colClasses = c('FIPST' = 'character', 'LEAID' = 'character', 'NCESSCH' = 'character', 'SCHID' = 'character', 'STATE_AGENCY_NO' = 'character', 'UNION' = 'character'))

# Turn variables to lowercase
names(ccd_characteristics) <- tolower(names(ccd_characteristics))

# Add virtual variable
table(ccd_characteristics$virtual)
table(ccd_characteristics$virtual_text)

ccd_characteristics <- ccd_characteristics %>%
  mutate(is_virtual = case_when(
    virtual == 'NOTVIRTUAL' ~ 0,
    virtual == 'Not reported' ~ NA_real_,
    TRUE ~ 1  # FULLVIRTUAL, FACEVIRTUAL, SUPPVIRTUAL
  ))

table(ccd_characteristics$is_virtual, useNA = 'always')

# Filter duplicate variables from directory (10 remaining variables)
ccd_characteristics <- ccd_characteristics %>% 
  select(ncessch, setdiff(names(ccd_characteristics), names(ccd_directory)))


# Read in membership data and dictionary (12112911 obs. of 18 variables)
ccd_membership <- read.csv(file.path(data_dir, 'ccd_membership_1718.csv'), header = TRUE, na.strings = c('', 'NA'), stringsAsFactors = F,  colClasses = c('FIPST' = 'character', 'LEAID' = 'character', 'NCESSCH' = 'character', 'SCHID' = 'character', 'STATE_AGENCY_NO' = 'character', 'UNION' = 'character'))

# Turn variables to lowercase
names(ccd_membership) <- tolower(names(ccd_membership))

# Pivot table (33369 obs.)
ccd_membership_by_grade_race_gender <- ccd_membership %>%
  filter(grade %in% c('Grade 9', 'Grade 10', 'Grade 11', 'Grade 12', 'Grade 13'), total_indicator == 'Category Set A - By Race/Ethnicity; Sex; Grade') %>%
  mutate(
    grade = recode(grade,
                   'Grade 9' = 'g09',
                   'Grade 10' = 'g10',
                   'Grade 11' = 'g11',
                   'Grade 12' = 'g12',
                   'Grade 13' = 'g13'),
    race_ethnicity = recode(race_ethnicity,
                            'American Indian or Alaska Native' = 'amerindian',
                            'Asian' = 'asian',
                            'Black or African American' = 'black',
                            'Hispanic/Latino' = 'hispanic',
                            'Native Hawaiian or Other Pacific Islander' = 'nativehawaii',
                            'Not Specified' = 'unknown',
                            'Two or more races' = 'tworaces',
                            'White' = 'white'),
    sex = recode(sex,
                 'Female' = 'f',
                 'Male' = 'm',
                 'Not Specified' = 'x'),
    grade_race_sex = str_c(grade, race_ethnicity, sex, sep = '_')
  ) %>% 
  select(ncessch, grade_race_sex, student_count) %>% 
  pivot_wider(id = ncessch, names_from = grade_race_sex, values_from = student_count) %>% 
  rowwise() %>%
  mutate(
    g09_f = sum(g09_white_f, g09_asian_f, g09_black_f, g09_hispanic_f, g09_amerindian_f, g09_nativehawaii_f, g09_tworaces_f, na.rm = T),
    g09_m = sum(g09_white_m, g09_asian_m, g09_black_m, g09_hispanic_m, g09_amerindian_m, g09_nativehawaii_m, g09_tworaces_m, na.rm = T),
    g09 = sum(g09_f, g09_m, g09_unknown_x, na.rm = T),
    g10_f = sum(g10_white_f, g10_asian_f, g10_black_f, g10_hispanic_f, g10_amerindian_f, g10_nativehawaii_f, g10_tworaces_f, na.rm = T),
    g10_m = sum(g10_white_m, g10_asian_m, g10_black_m, g10_hispanic_m, g10_amerindian_m, g10_nativehawaii_m, g10_tworaces_m, na.rm = T),
    g10 = sum(g10_f, g10_m, g10_unknown_x, na.rm = T),
    g11_f = sum(g11_white_f, g11_asian_f, g11_black_f, g11_hispanic_f, g11_amerindian_f, g11_nativehawaii_f, g11_tworaces_f, na.rm = T),
    g11_m = sum(g11_white_m, g11_asian_m, g11_black_m, g11_hispanic_m, g11_amerindian_m, g11_nativehawaii_m, g11_tworaces_m, na.rm = T),
    g11 = sum(g11_f, g11_m, g11_unknown_x, na.rm = T),
    g12_f = sum(g12_white_f, g12_asian_f, g12_black_f, g12_hispanic_f, g12_amerindian_f, g12_nativehawaii_f, g12_tworaces_f, na.rm = T),
    g12_m = sum(g12_white_m, g12_asian_m, g12_black_m, g12_hispanic_m, g12_amerindian_m, g12_nativehawaii_m, g12_tworaces_m, na.rm = T),
    g12 = sum(g12_f, g12_m, g12_unknown_x, na.rm = T),
    g13_f = sum(g13_white_f, g13_asian_f, g13_black_f, g13_hispanic_f, g13_amerindian_f, g13_nativehawaii_f, g13_tworaces_f, na.rm = T),
    g13_m = sum(g13_white_m, g13_asian_m, g13_black_m, g13_hispanic_m, g13_amerindian_m, g13_nativehawaii_m, g13_tworaces_m, na.rm = T),
    g13 = sum(g13_f, g13_m, g13_unknown_x, na.rm = T),
    total_students = sum(g09, g10, g11, g12, g13, na.rm = T)
  )


# Read in lat/lng data (102337 obs. of 24 variables)
geo_data <- read_excel(file.path(data_dir, 'EDGE_GEOCODE_PUBLICSCH_1718.xlsx'))

geo_data <- geo_data %>% 
  select(NCESSCH, LAT, LON) %>% 
  dplyr::rename(
    'ncessch' = 'NCESSCH',
    'latitude' = 'LAT',
    'longitude' = 'LON'
  )


# Join tables
ccd <- left_join(ccd_directory, geo_data, by = 'ncessch') %>% 
  left_join(ccd_characteristics, by = 'ncessch') %>% 
  left_join(ccd_membership_by_grade_race_gender, by = 'ncessch')

# Rename variables
ccd <- ccd %>% rename(state_code = 'st')

# Check variables
unique(ccd$g_12_offered)
table(ccd$is_virtual, useNA = 'always')
all(nchar(ccd$mzip) == 5)
all(nchar(ccd$lzip) == 5)
all(nchar(ccd$state_code) == 2)
all(nchar(ccd$ncessch) == 12)


# Export data
saveRDS(ccd, file = file.path(data_dir, 'ccd_membership_1718.RDS'))
