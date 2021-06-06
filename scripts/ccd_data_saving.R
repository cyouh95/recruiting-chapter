library(tidyverse)
library(labelled)


data_dir <- file.path('.', 'data')


# Read in directory data and dictionary (102337 obs. of 65 variables)
ccd_directory <- read.csv(file.path(data_dir, 'ccd_directory_1718.csv'), header = TRUE, na.strings = c('', 'NA'), stringsAsFactors = F,  colClasses = c('FIPST' = 'character', 'LEAID' = 'character', 'NCESSCH' = 'character', 'SCHID' = 'character', 'MZIP' = 'character', 'MZIP4' = 'character', 'LZIP' = 'character', 'LZIP4' = 'character', 'SY_STATUS' = 'character', 'UPDATED_STATUS' = 'character', 'SCH_TYPE' = 'character', 'STATE_AGENCY_NO' = 'character', 'UNION' = 'character'))
ccd_directory_dictionary <- read.csv(file.path(data_dir, 'ccd_directory_dictionary_1718.csv'), header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)

# Turn variables to lowercase
names(ccd_directory) <- tolower(names(ccd_directory))
names(ccd_directory_dictionary) <- tolower(names(ccd_directory_dictionary))
ccd_directory_dictionary <- ccd_directory_dictionary %>% mutate(names_lc = tolower(variable.name))

# Label variables
for (i in names(ccd_directory)) { 
  description <- (ccd_directory_dictionary %>% filter(names_lc == i))$description
  var_label(ccd_directory[[i]]) <- description
}

# Label values
ccd_directory <- ccd_directory %>% set_value_labels( 
  sy_status = c('open' = '1', 
                'closed' = '2',
                'new' = '3',
                'added' = '4',
                'changed boundary/agency' = '5',
                'inactive' = '6',
                'future' = '7',
                'reopened' = '8'),
  updated_status = c('open' = '1',
                     'closed' = '2',
                     'new' = '3',
                     'added' = '4',
                     'changed boundary/agency' = '5',
                     'inactive' = '6',
                     'future' = '7',
                     'reopened' = '8'),
  sch_type = c('regular school' = '1',
               'special education school' = '2',
               'career and technical school' = '3',
               'alternative education school' = '4'),
  gslo = c('1st grade students' = '1',
           '2nd grade students' = '2',
           '3rd grade students' = '3',
           '4th grade students' = '4',
           '5th grade students' = '5',
           '6th grade students' = '6',
           '7th grade students' = '7',
           '8th grade students' = '8',
           '9th grade students' = '9',
           '10th grade students' = '10',
           '11th grade students' = '11',
           '12th grade students' = '12',
           '13th grade students' = '13',
           'adult education students' = 'AE',
           'kindergarten students' = 'KG',
           'not applicable' = 'N',
           'prekindergarten students' = 'PK',
           'students in ungraded classes' = 'UG',
           'missing' = 'M'),
  gshi = c('1st grade students' = '1',
           '2nd grade students' = '2',
           '3rd grade students' = '3',
           '4th grade students' = '4',
           '5th grade students' = '5',
           '6th grade students' = '6',
           '7th grade students' = '7',
           '8th grade students' = '8',
           '9th grade students' = '9',
           '10th grade students' = '10',
           '11th grade students' = '11',
           '12th grade students' = '12',
           '13th grade students' = '13',
           'adult education students' ='AE',
           'kindergarten students' = 'KG',
           'not applicable' = 'N',
           'prekindergarten students' = 'PK',
           'students in ungraded classes' = 'UG',
           'missing' = 'M')
)


# Read in characteristics data and dictionary (99899 obs. of 20 variables)
ccd_characteristics <- read.csv(file.path(data_dir, 'ccd_characteristics_1718.csv'), header = TRUE, na.strings = c('', 'NA'), stringsAsFactors = F,  colClasses = c('FIPST' = 'character', 'LEAID' = 'character', 'NCESSCH' = 'character', 'SCHID' = 'character', 'STATE_AGENCY_NO' = 'character', 'UNION' = 'character'))
ccd_characteristics_dictionary <- read.csv(file.path(data_dir, 'ccd_characteristics_dictionary_1718.csv'), header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)

# Turn variables to lowercase
names(ccd_characteristics) <- tolower(names(ccd_characteristics))
names(ccd_characteristics_dictionary) <- tolower(names(ccd_characteristics_dictionary))
ccd_characteristics_dictionary <- ccd_characteristics_dictionary %>% mutate(names_lc = tolower(variable.name))

# Label variables
for (i in names(ccd_characteristics)) { 
  description <- (ccd_characteristics_dictionary %>% filter(names_lc == i))$description
  var_label(ccd_characteristics[[i]]) <- description
}

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


# Read in characteristics data and dictionary (12112911 obs. of 18 variables)
ccd_membership <- read.csv(file.path(data_dir, 'ccd_membership_1718.csv'), header = TRUE, na.strings = c('', 'NA'), stringsAsFactors = F,  colClasses = c('FIPST' = 'character', 'LEAID' = 'character', 'NCESSCH' = 'character', 'SCHID' = 'character', 'STATE_AGENCY_NO' = 'character', 'UNION' = 'character'))
ccd_membership_dictionary <- read.csv(file.path(data_dir, 'ccd_membership_dictionary_1718.csv'), header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)

# Turn variables to lowercase
names(ccd_membership) <- tolower(names(ccd_membership))
names(ccd_membership_dictionary) <- tolower(names(ccd_membership_dictionary))
ccd_membership_dictionary <- ccd_membership_dictionary %>% mutate(names_lc = tolower(variable.name))

# Label variables
for (i in names(ccd_membership)) { 
  description <- (ccd_membership_dictionary %>% filter(names_lc == i))$description
  var_label(ccd_membership[[i]]) <- description
}

# Pivot table (33369 obs.)
ccd_membership_by_grade <- ccd_membership %>%
  filter(grade %in% c('Grade 9', 'Grade 10', 'Grade 11', 'Grade 12', 'Grade 13'), total_indicator == 'Subtotal 4 - By Grade') %>%  # filter by 'Category Set A - By Race/Ethnicity; Sex; Grade' if want to further break down by race
  select(ncessch, grade, student_count) %>%
  mutate(grade = recode(grade,
                        'Grade 9' = 'g09',
                        'Grade 10' = 'g10',
                        'Grade 11' = 'g11',
                        'Grade 12' = 'g12',
                        'Grade 13' = 'g13')
         ) %>% 
  pivot_wider(names_from = grade, values_from = student_count) %>%
  rowwise() %>% 
  mutate(total_students = sum(g09, g10, g11, g12, g13, na.rm = T))


# Join tables
ccd <- left_join(ccd_directory, ccd_characteristics, by = 'ncessch') %>% 
  left_join(ccd_membership_by_grade, by = 'ncessch')

# Rename variables
ccd <- ccd %>% rename(state_code = 'st')

# Check variables
unique(ccd$g_12_offered)
summary(ccd$g12)
table(ccd$is_virtual, useNA = 'always')
val_labels(ccd$sch_type)
val_labels(ccd$updated_status)
all(nchar(ccd$mzip) == 5)
all(nchar(ccd$lzip) == 5)
all(nchar(ccd$state_code) == 2)
all(nchar(ccd$ncessch) == 12)

# Export data
ccd <- as_tibble(x = ccd)
saveRDS(ccd, file = file.path(data_dir, 'ccd_1718.RDS'))
