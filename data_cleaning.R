#Following Data Cleaning Guidelines

# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("readr")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("kableExtra")
# install.packages("labelled")
# install.packages("haven")
# install.packages("stargazer")
# install.packages("Hmisc")
# install.packages("sjlabelled")
# install.packages("assertthat")
# install.packages("dplyr")


library(tidyverse)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)
library(kableExtra)
library(labelled)
library(haven) 
library(stargazer)
library(Hmisc)
library(sjlabelled)
library(assertthat)
library(dplyr)

###CCD Data
##Loading & investigating data: CCD School Directory Data for 2017-2018
#Check working directory
getwd()
setwd("/Users/irmacastaneda/Documents/UCLA/GSR/recruiting-chapter")

#Loading Data; Starting with public high school data
hs_directory <- read.csv('./data/ccd_school_directory_1718.csv', header = TRUE, na.strings=c("","NA"), colClasses = c('SCHOOL_YEAR' = "character", 'FIPST' = 'character', 'STATENAME' = 'character', 'ST' = 'character', 'SCH_NAME' = 'character', 'LEA_NAME' = 'character', 'STATE_AGENCY_NO' = 'character', 'UNION' = 'character', 'ST_LEAID' = 'character', 'LEAID' = 'character', 'ST_SCHID' = 'character', 'NCESSCH' = 'character', 'SCHID' = 'character', 
                                                                                                                      'MSTREET1' = 'character', 'MSTREET2' = 'character', 'MSTREET3' = 'character', 'MCITY' = 'character', 'MSTATE' = 'character', 'MZIP' = 'character', 'MZIP4' = 'character', 'LSTREET1' = 'character', 'LSTREET2' = 'character', 'LSTREET3'= 'character', 'LCITY' = 'character', 'LSTATE' = 'character', 'LZIP' = 'character', 'LZIP4' = 'character',
                                                                                                                      'PHONE' = 'character','WEBSITE' = 'character','SY_STATUS' = 'character', 'SY_STATUS_TEXT' = 'character', 'UPDATED_STATUS' = 'character', 'UPDATED_STATUS_TEXT' = 'character', 'EFFECTIVE_DATE' = 'character', 'SCH_TYPE' = 'character', 'SCH_TYPE_TEXT' = 'character', 'RECON_STATUS' = 'character', 'OUT_OF_STATE_FLAG' = 'character',  
                                                                                                                      'CHARTER_TEXT' = 'character','CHARTAUTH1' = 'character', 'CHARTAUTHN1' = 'character', 'CHARTAUTH2' = 'character', 'CHARTAUTHN2' = 'character', 'G_9_OFFERED' = 'character', 'G_10_OFFERED' = 'character', 'G_11_OFFERED' = 'character', 'G_12_OFFERED' = 'character', 'G_13_OFFERED'= 'character', 'LEVEL' = 'character', 'GSLO' = 'character', 'GSHI' = 'character'))

#Investigate the data & data frame
str(hs_directory) # structure of the data frame

nrow(hs_directory) #num of rows (obs): 102,337 observations
ncol(hs_directory) #num of columns: 65 variables

names(hs_directory) # variable names 
names(hs_directory) <- tolower(names(hs_directory)) #convert to lowercase
glimpse(hs_directory)
head(hs_directory) # prints first 6 obs

hs_directory %>% arrange(ncessch, sch_name, school_year, sy_status, sch_type, g_8_offered, g_9_offered,
                         g_10_offered, g_11_offered, g_12_offered, g_13_offered, g_ug_offered, g_ae_offered) %>% head()



##Labeling variables 
#Importing data dictionary
hs_dictionary <- read.csv('./data/ccd_school_directory_dictionary_1718.csv', header = TRUE, na.strings=c("","NA")) #na.strings converts empty strings to NA value    
hs_direct_dict <- hs_dictionary %>% select(Variable.Name, Description)

#Lowercase variable names in dictionary
names(hs_direct_dict) <- tolower(names(hs_direct_dict))
hs_direct_dict <- hs_direct_dict %>% mutate(names_lc = tolower(variable.name))


#Loop to label data with dictionary
for (i in names(hs_directory)) { #loop executed 65 times, first iteration of loop = school_year -> last variable igoffered
  description <- (hs_direct_dict %>% filter(names_lc == i))$description 
  var_label(hs_directory[[i]]) <- description #label variables
}

#Check variable labels
hs_directory %>% var_label() # variable labels


#Adding value labels
hs_directory %>% select(recon_status, out_of_state_flag, charter_text, nogrades, g_pk_offered, g_kg_offered, 
                        g_1_offered, g_2_offered, g_3_offered, g_4_offered, g_5_offered, g_6_offered, 
                        g_7_offered, g_8_offered, g_9_offered, g_10_offered, g_11_offered, g_12_offered, 
                        g_13_offered, g_ug_offered, g_ae_offered, level,igoffered) %>% glimpse() #already text

hs_directory <- hs_directory %>% set_value_labels( 
  sy_status = c("open" = "1", 
                "closed" = "2",
                "new" = "3",
                "added" = "4",
                "changed boundary/agency" = "5",
                "inactive" = "6",
                "future" = "7",
                "reopened" = "8"),
  updated_status = c("open" = "1",
                     "closed" = "2",
                     "new" = "3",
                     "added" = "4",
                     "changed boundary/agency" = "5",
                     "inactive" = "6",
                     "future" = "7",
                     "reopened" = "8"),
  sch_type = c("regular school" = "1",
               "special education school" = "2",
               "career and technical school" = "3",
               "alternative education school" = "4"),
  gslo = c("1st grade students" = "1",
           "2nd grade students" = "2",
           "3rd grade students" = "3",
           "4th grade students" = "4",
           "5th grade students" = "5",
           "6th grade students" = "6",
           "7th grade students" = "7",
           "8th grade students" = "8",
           "9th grade students" = "9",
           "10th grade students" = "10",
           "11th grade students" = "11",
           "12th grade students" = "12",
           "13th grade students" = "13",
           "adult education students" = "AE",
           "kindergarten students" = "KG",
           "not applicable" = "N",
           "prekindergarten students" = "PK",
           "students in ungraded classes" = "UG",
           "missing" = "M"),
  gshi = c("1st grade students" = "1",
           "2nd grade students" = "2",
           "3rd grade students" = "3",
           "4th grade students" = "4",
           "5th grade students" = "5",
           "6th grade students" = "6",
           "7th grade students" = "7",
           "8th grade students" = "8",
           "9th grade students" = "9",
           "10th grade students" = "10",
           "11th grade students" = "11",
           "12th grade students" = "12",
           "13th grade students" = "13",
           "adult education students" ="AE",
           "kindergarten students" = "KG",
           "not applicable" = "N",
           "prekindergarten students" = "PK",
           "students in ungraded classes" = "UG",
           "missing" = "M"))


#check value labels
val_labels(hs_directory$sy_status)
val_labels(hs_directory$updated_status)
val_labels(hs_directory$sch_type)
val_labels(hs_directory$gslo)
val_labels(hs_directory$gshi)


##Checking data validity

#Check that school year is 2017-2018
hs_directory$school_year
hs_directory %>% filter((school_year == "2017-2018")) %>% count() #102,337 observations

#Check state code: digits & foreign key constraints
hs_directory$fipst
nrow(hs_directory %>% filter(nchar(fipst) == 2)) #102,337 (all length 2)

#Check state code: digits & foreign key constraints
hs_directory$st
nrow(hs_directory %>% filter(nchar(st) == 2)) #all 2 observations

#Check identifier of the reporting state agency
hs_directory$state_agency_no
nrow(hs_directory %>% filter(nchar(fipst) == 2)) #102,337 (all length 2)

#Check union
hs_directory$union #a lot of NAs
hs_directory %>% filter(is.na(union)) %>% count() #99,857 missing
nrow(hs_directory %>% filter(nchar(union) >= 3)) #2,480, length greater than or equal to 3, dictionary says variable is a length of 3

#Check state local education number
hs_directory$st_leaid
nrow(hs_directory %>% filter(nchar(st_leaid) != 6)) #87,985, from quick look at data frame, many appear to be a length of 6
nrow(hs_directory %>% filter(nchar(st_leaid) == 10)) #14,556, a large portion of observations are of length 10, but they vary in length and the dictionary says they can be up to 35 characters long
nrow(hs_directory %>% filter(nchar(st_leaid) <= 10)) #84,879, most observations are less than or equal to 10 characters long


#Check that state local education number is a unique identifier
hs_directory %>% group_by(st_leaid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #st_leaid is not a unique identifier

#Check that NCES agency identification number
hs_directory$leaid
nrow(hs_directory %>% filter(nchar(leaid) == 7)) #102,337 (all length 7), dictionary says that length of 7

#Check that NCES agency is unique identifier
hs_directory %>% group_by(leaid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #leaid is not a unique identifier

#Check state school identifier
hs_directory$st_schid
nrow(hs_directory %>% filter(nchar(st_schid) == 11)) #10,139,from a quick look at the first thousand observations, they are a length of 11
nrow(hs_directory %>% filter(nchar(st_schid) >= 38)) #looking for outliers, dictionary says they can be up to 45 characters long
st_schid_long <- hs_directory %>% select(st_schid) %>% filter(nchar(st_schid) >= 38) #1,253, OR has really long identifiers, mostly zeros

#Check that state school identifier is a unique identifier
hs_directory %>% group_by(st_schid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #st_schid is a unique identifier

#Check that NCES School ID has 12 digits
hs_directory$ncessch
nrow(hs_directory %>% filter(nchar(ncessch) == 12)) #all ncessch variables are 12 characters long
see_if(are_equal(nrow(hs_directory %>% filter(nchar(ncessch) != 12)),0)) #true, double checking
see_if(assert_that(nrow(hs_directory %>% filter(nchar(ncessch) != 12)) == 1)) #false, double checking

#Check that NCES is unique identifier
hs_directory %>% group_by(ncessch) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #Yes, unique identifier, 102,337 observations

#Check that unique school ID has 7 digits
hs_directory$schid
nrow(hs_directory %>% filter(nchar(schid) == 7)) #all schid variables are 12 characters long 

#Check that unique school ID is unique identifier
hs_directory %>% group_by(schid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #Yes, unique identifier, 102,337 observations

#Check mailing state & zipcode
hs_directory$mstate
nrow(hs_directory %>% filter(nchar(mstate) == 2)) #all mstate are 2 characters long

hs_directory$mzip
nrow(hs_directory %>% filter(nchar(mzip) != 5)) #all have 5 digits

hs_directory$mzip4
hs_directory %>% filter(is.na(mzip4)) %>% count() #40,487 are missing
nrow(hs_directory %>% filter(nchar(mzip4) == 4)) #61,850 have 4 digits


#Check location state & zipcode
hs_directory$lstate
nrow(hs_directory %>% filter(nchar(lstate) != 2))

hs_directory$lzip
nrow(hs_directory %>% filter(nchar(lzip) != 5)) #all have 5 digits

hs_directory$lzip4
hs_directory %>% filter(is.na(lzip4)) %>% count() #42,414 are missing
nrow(hs_directory %>% filter(nchar(lzip4) == 4)) #59,923 have 4 digits

#Check effective date
hs_directory$effective_date
nrow(hs_directory %>% filter(nchar(effective_date) == 10)) #all are 10 character long, which the accurate length for a date mm/dd/yyyy

#Check charter authorizer state ID (1)
hs_directory$chartauth1 #a lot of NAs
hs_directory %>% filter(is.na(chartauth1)) %>% count()#95,584, most missing

#Check charter authorizer state ID (2)
hs_directory$chartauth2 #a lot of NAs
hs_directory %>% filter(is.na(chartauth2)) %>% count()#102,212, most missing

#Checking data completeness (i.e. missing data)
#loop through the variables and filter for is
for(i in names(hs_directory)) {
  print(paste(i, sum(is.na(hs_directory[[i]])))) #anything put inside paste, prints out both items separated by a space
}

#union: 99,857 missing
#mstreet2: 100,551 missing
#mstreet3: 102,309 missing
#mzip4: 40,487 missing
#lstreet2: 101,724 missing
#lstreet3: 102,326 missing
#lzip4: 42,414 missing
#website: 45,311 missing
#chartauth1:95,584 missing
#chartauthn1:95,584 missing
#chartauth2: 102,212 missing
#chartauthn2: 102,212 missing

#Renaming variables
hs_directory <- hs_directory %>% rename(year = "school_year",
                                        state_code = "st",
                                        state_fips_code = "fipst",
                                        name = "sch_name",
                                        address = "mstreet1", 
                                        city = "mcity",
                                        zip_code = "mzip", 
                                        address_2 = "lstreet1") 






names(hs_directory)                                  

##Loading & investigating data: CCD School Characteristics Data for 2017-2018
hs_character<- read.csv('./data/ccd_school_characteristics_1718.csv', header = TRUE, na.strings=c("","NA"), colClasses = c('SCHOOL_YEAR' = 'character', 'FIPST' = 'character', 'STATENAME' = 'character', 'ST' = 'character', 'SCH_NAME' = 'character', 'STATE_AGENCY_NO' = 'character', 'UNION' = 'character', 'ST_LEAID' = 'character', 'LEAID' = 'character', 'ST_SCHID' = 'character', 'NCESSCH' = 'character', 'SCHID' = 'character',
                                                                                                                           'SHARED_TIME' = 'character', 'TITLEI_STATUS' = 'character', 'TITLEI_STATUS_TEXT' = 'character', 'MAGNET_TEXT' = 'character', 'NSLP_STATUS' = 'character', 'NSLP_STATUS_TEXT' = 'character', 'VIRTUAL' = 'character', 'VIRTUAL_TEXT' = 'character'))
str(hs_character) # structure of the data frame

nrow(hs_character) #num of rows (obs): 99,899 observations
ncol(hs_character) #num of columns: 20 variables

names(hs_character) # variable names 
names(hs_character) <- tolower(names(hs_character)) #convert to lowercase
glimpse(hs_character)
head(hs_character) # prints first 6 obs

hs_character %>% arrange(ncessch, sch_name, school_year, titlei_status, nslp_status) %>% head()

##Labeling variables 
#Importing data dictionary
hs_character_dictionary <- read.csv('./data/ccd_school_characteristics_dictionary_1718.csv', header = TRUE, na.strings=c("","NA")) #na.strings converts empty strings to NA value    
hs_character_dict_subset <- hs_character_dictionary %>% select(Variable.Name, Description)

#Lowercase variable names in dictionary
names(hs_character_dict_subset) <- tolower(names(hs_character_dict_subset))
hs_character_dict_subset <- hs_character_dict_subset %>% mutate(names_lc = tolower(variable.name))

#Loop to label data with dictionary
for (i in names(hs_character)) {
  description2 <- (hs_character_dict_subset %>% filter(names_lc == i))$description 
  var_label(hs_character[[i]]) <- description2
}

#Check variable labels
hs_character %>% var_label() # variable labels

#Adding value labels
hs_character %>% select(shared_time, titlei_status, titlei_status_text, magnet_text, nslp_status, nslp_status_text, virtual, virtual_text) %>% glimpse() #already text & labeled

##Checking data validity

#Check that school year is 2017-2018
hs_character$school_year
hs_character %>% filter((school_year == "2017-2018")) %>% count() #99,899 observations

#Check state code: digits & foreign key constraints
hs_character$fipst
nrow(hs_character %>% filter(nchar(fipst) == 2)) #81,244 (2 characters)
nrow(hs_character %>% filter(nchar(fipst) == 1)) #18,655 (1 character)

#Check postal state code
hs_character$st
nrow(hs_character %>% filter(nchar(st) == 2)) #99,899 

#Check union
hs_character$union #a lot of NAs
hs_character %>% filter(is.na(union)) %>% count() #97,438
nrow(hs_character %>% filter(nchar(union) >= 3)) #1,598, length greater than or equal to 3, dictionary says variable is a length of 3

#Check state local education number
hs_character$st_leaid
nrow(hs_character %>% filter(nchar(st_leaid) != 6)) #85,970, from quick look at data frame, many appear to be a length of 6
nrow(hs_character %>% filter(nchar(st_leaid) <= 17)) #looking for outliers, most under length of 17
st_leaid_long <- hs_character %>% select(st_leaid) %>% filter(nchar(st_leaid) >= 17) #5,490, IL has really long identifiers

#Check that state local education number is a unique identifier
hs_character %>% group_by(st_leaid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #st_leaid is not a unique identifier

#Check that NCES agency identification number
hs_character$leaid
nrow(hs_character %>% filter(nchar(leaid) == 7)) #81,244, dictionary says that length of 7
nrow(hs_character %>% filter(nchar(leaid) < 7)) #18,655 less than 7


#Check that NCES agency is unique identifier
hs_character %>% group_by(leaid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #leaid is not a unique identifier

#Check state school identifier
hs_character$st_schid
nrow(hs_character %>% filter(nchar(st_schid) != 11)) #90,072, from quick look at data frame, many appear to be a length of 11, dictionary says that variable is a length of 45
nrow(hs_character %>% filter(nchar(st_schid) >= 38)) #1,249, looking for outliers, most under length of 38
st_schid2_long <- hs_character %>% select(st_schid) %>% filter(nchar(st_schid) >= 38) #1,249, OR has really long identifiers

#Check that state school identifier is a unique identifier
hs_character %>% group_by(st_schid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #st_schid is a unique identifier

#Check that school identifier (NCES) ID has 12 digits
hs_character$ncessch
nrow(hs_character %>% filter(nchar(ncessch) != 12)) #99,899, dictionary says ncessch is length of 12
nrow(hs_character %>% filter(nchar(ncessch) == 11)) #91,253 most are 11 characters long

#Check that school identifier (NCES) ID is unique identifier
hs_character %>% group_by(ncessch) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #ncessch is not a unique identifier

#Check that unique school ID 
hs_character$schid
nrow(hs_character %>% filter(nchar(schid) == 7)) #81,244, dictionary says schid is length of 7
nrow(hs_character %>% filter(nchar(schid) == 6)) #18,655

#Check that unique school ID is unique identifier
hs_character %>% group_by(schid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #schid is a unique identifier

#Checking data completeness (i.e. missing data)
#loop through the variables and filter for is
for(i in names(hs_character)) {
  print(paste(i, sum(is.na(hs_character[[i]])))) #anything put inside paste, prints out both items separated by a space
}

#union: 97,438 missing

##Loading & investigating data: CCD School Lunch Program Eligibility Data for 2017-2018
hs_lunch <- read.csv('./data/ccd_school_lunch_1718.csv', header = TRUE, na.strings=c("","NA"), colClasses = c('SCHOOL_YEAR' = 'character', 'FIPST' = 'character', 'STATENAME' = 'character', 'ST' = 'character', 'SCH_NAME' = 'character', 'STATE_AGENCY_NO' = 'character', 'UNION' = 'character', 'ST_LEAID' = 'character', 'LEAID' = 'character',  
                                                                                                              'ST_SCHID' = 'character', 'NCESSCH' = 'character', 'SCHID' = 'character','DATA_GROUP' = 'character', 'LUNCH_PROGRAM' = 'character', 'STUDENT_COUNT' = 'integer', 'TOTAL_INDICATOR' = 'character', 'DMS_FLAG' = 'character'))

#Investigate the data & data frame
str(hs_lunch) # structure of the data frame

nrow(hs_lunch) #num of rows (obs): 494,745 observations
ncol(hs_lunch) #num of columns: 17 variables

names(hs_lunch) # variable names 
names(hs_lunch) <- tolower(names(hs_lunch)) #convert to lowercase
glimpse(hs_lunch)
head(hs_lunch) # prints first 6 obs

hs_lunch %>% arrange(ncessch, sch_name, school_year, data_group, lunch_program, student_count) %>% head()

##Labeling variables 
#Importing data dictionary
hs_lunch_dictionary <- read.csv('./data/ccd_school_lunch_dictionary_1718.csv', header = TRUE, na.strings=c("","NA")) #na.strings converts empty strings to NA value    
hs_lunch_dict_subset <- hs_lunch_dictionary %>% select(Variable.Name, Description)

#Lowercase variable names in dictionary
names(hs_lunch_dict_subset) <- tolower(names(hs_lunch_dict_subset))
hs_lunch_dict_subset <- hs_lunch_dict_subset %>% mutate(names_lc = tolower(variable.name))

#Loop to label data with dictionary
for (i in names(hs_lunch)) { 
  description3 <- (hs_lunch_dict_subset %>% filter(names_lc == i))$description 
  var_label(hs_lunch[[i]]) <- description3 
}

#Check variable labels
hs_lunch %>% var_label() # variable labels

#Adding value labels
hs_lunch %>% select(data_group,lunch_program,total_indicator,dms_flag) %>% glimpse() #already text & labeled

##Checking data validity

#Check that school year is 2017-2018
hs_lunch$school_year
hs_lunch %>% filter((school_year == "2017-2018")) %>% count() #494,745 observations

#Check state code: digits & foreign key constraints
hs_lunch$fipst
nrow(hs_lunch %>% filter(nchar(fipst) == 2)) #494,745 (2 characters)

#Check postal state code
hs_lunch$st
nrow(hs_lunch %>% filter(nchar(st) == 2)) #494,745 (2 characters)

#Check identifier of the reporting state agency
hs_lunch$state_agency_no
nrow(hs_lunch %>% filter(nchar(fipst) == 2)) #494,745 (all length 2)

#Check union
hs_lunch$union #a lot of NAs
hs_lunch %>% filter(is.na(union)) %>% count() #484,440
nrow(hs_lunch %>% filter(nchar(union) >= 3)) #12,305, length greater than or equal to 3, dictionary says variable is a length of 3

#Check state local education number
hs_lunch$st_leaid
nrow(hs_lunch %>% filter(nchar(st_leaid) != 6)) #426,490, from quick look at data frame, many are a length of 6
nrow(hs_lunch %>% filter(nchar(st_leaid) <= 15)) #467,300 most observations are under 15 characters, dictionary says they st_leaid can be up to 35 characters
st_leaid2_long <- hs_lunch %>% select(st_leaid) %>% filter(nchar(st_leaid) >= 17) #27,445, IL & OR have really long identifiers

#Check that state local education number is a unique identifier
hs_lunch %>% group_by(st_leaid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #st_leaid is not a unique identifier

#Check that NCES agency identification number
hs_lunch$leaid
nrow(hs_lunch %>% filter(nchar(leaid) == 7)) #494,745 (all are length 7 as dictionary indicates)

#Check that NCES agency is unique identifier
hs_lunch %>% group_by(leaid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #leaid is not a unique identifier

#Check state school identifier
hs_lunch$st_schid
nrow(hs_lunch %>% filter(nchar(st_schid) <= 20)) #412,355 from quick look at data frame, most appear under 20 characters long, dictionary says that variable is a max length of 45
nrow(hs_lunch %>% filter(nchar(st_schid) == 38)) #6,245, looking for outliers
st_schid3_long <- hs_lunch %>% select(st_schid) %>% filter(nchar(st_schid) == 38) #6,245, OR has really long identifiers but mostly zeros

#Check that state school identifier is a unique identifier
hs_lunch %>% group_by(st_schid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #st_schid is not a unique identifier

#Check that school identifier (NCES) ID has 12 digits
hs_lunch$ncessch
nrow(hs_lunch %>% filter(nchar(ncessch) == 12)) #494,745 (all length 12 as indicated by dictionary)

#Check that school identifier (NCES) ID is unique identifier
hs_lunch %>% group_by(ncessch) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #ncessch is not a unique identifier

#Check that unique school ID 
hs_lunch$schid
nrow(hs_lunch %>% filter(nchar(schid) == 7)) #494,745 (all length 7 as indicated by dictionary)

#Check that unique school ID is unique identifier
hs_lunch %>% group_by(schid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #schid is not a unique identifier

#Checking data completeness (i.e. missing data)
#loop through the variables and filter for is
for(i in names(hs_lunch)) {
  print(paste(i, sum(is.na(hs_lunch[[i]])))) #anything put inside paste, prints out both items separated by a space
}

#union: 482,440 missing
#student count: 199,399 missing

##Loading & investigating data: CCD School Membership Data for 2017-2018
hs_membership <- read.csv('./data/ccd_school_membership_1718.csv', header = TRUE, na.strings=c("","NA"), colClasses = c('SCHOOL_YEAR' = 'character', 'FIPST' = 'character', 'STATENAME' = 'character', 'ST' = 'character', 'SCH_NAME' = 'character', 'STATE_AGENCY_NO' = 'character', 'UNION' = 'character', 'ST_LEAID' = 'character', 'LEAID' = 'character', 'ST_SCHID' = 'character', 'NCESSCH' = 'character', 'SCHID' = 'character', 'GRADE' = 'character', 'RACE_ETHNICITY' = 'character', 'SEX' = 'character', 'TOTAL_INDICATOR' = 'character', 'DMS_FLAG' = 'character'))

#Investigate the data & data frame
str(hs_membership) # structure of the data frame

nrow(hs_membership) #num of rows (obs): 1,937,490 observations
ncol(hs_membership) #num of columns: 19 variables

names(hs_membership) # variable names 
names(hs_membership) <- tolower(names(hs_membership)) #convert to lowercase
glimpse(hs_membership)
head(hs_membership) 

hs_membership %>% arrange(school_year,sch_name, ncessch, race_ethnicity, sex) %>% head()


##Labeling variables 
#Importing data dictionary
hs_membership_dict <- read.csv('./data/ccd_school_membership_dictionary_1718.csv', header = TRUE, na.strings=c("","NA")) #na.strings converts empty strings to NA value    
hs_membership_dict_subset <- hs_membership_dict %>% select(Variable.Name, Description)

#Lowercase variable names in dictionary
names(hs_membership_dict_subset) <- tolower(names(hs_membership_dict_subset))
hs_membership_dict_subset <- hs_membership_dict_subset %>% mutate(names_lc = tolower(variable.name))


#Loop to label data with dictionary
for (i in names(hs_membership)) {
  description5 <- (hs_membership_dict_subset %>% filter(names_lc == i))$description 
  var_label(hs_membership[[i]]) <- description5
} 

#Check variable labels
hs_membership %>% var_label() # variable labels

#recode race & ethnicity
hs_membership %>% group_by(race_ethnicity) %>% count()

#create race,grade,gender & then pivot wider
hs_membership <-hs_membership %>% mutate(race_grade_gender = case_when(
  race_ethnicity == 'American Indian or Alaska Native' & grade == 'Grade 9' & sex == 'Female' ~ 'am09f', 
  race_ethnicity == 'American Indian or Alaska Native' & grade == 'Grade 9' & sex == 'Male' ~ 'am09m', 
  race_ethnicity == 'American Indian or Alaska Native' & grade == 'Grade 9' & sex == 'Not Specified' ~ 'am09ns', 
  race_ethnicity == 'American Indian or Alaska Native' & grade == 'Grade 10' & sex == 'Female' ~ 'am10f', 
  race_ethnicity == 'American Indian or Alaska Native' & grade == 'Grade 10' & sex == 'Male' ~ 'am10m', 
  race_ethnicity == 'American Indian or Alaska Native' & grade == 'Grade 10' & sex == 'Not Specified' ~ 'am10ns', 
  race_ethnicity == 'American Indian or Alaska Native' & grade == 'Grade 11' & sex == 'Female' ~ 'am11f', 
  race_ethnicity == 'American Indian or Alaska Native' & grade == 'Grade 11' & sex == 'Male' ~ 'am11m', 
  race_ethnicity == 'American Indian or Alaska Native' & grade == 'Grade 11' & sex == 'Not Specified' ~ 'am11ns', 
  race_ethnicity == 'American Indian or Alaska Native' & grade == 'Grade 12' & sex == 'Female' ~ 'am12f', 
  race_ethnicity == 'American Indian or Alaska Native' & grade == 'Grade 12' & sex == 'Male' ~ 'am12m', 
  race_ethnicity == 'American Indian or Alaska Native' & grade == 'Grade 12' & sex == 'Not Specified' ~ 'am12ns', 
  race_ethnicity == 'American Indian or Alaska Native' & grade == 'Grade 13' & sex == 'Female' ~ 'am13f', 
  race_ethnicity == 'American Indian or Alaska Native' & grade == 'Grade 13' & sex == 'Male' ~ 'am13m', 
  race_ethnicity == 'American Indian or Alaska Native' & grade == 'Grade 13' & sex == 'Not Specified' ~ 'am13ns', 
  race_ethnicity == 'Asian' & grade == 'Grade 9' & sex == 'Female' ~ 'as09f',
  race_ethnicity == 'Asian' & grade == 'Grade 9' & sex == 'Male' ~ 'as09m',
  race_ethnicity == 'Asian' & grade == 'Grade 9' & sex == 'Not Specified' ~ 'as09ns',
  race_ethnicity == 'Asian' & grade == 'Grade 10' & sex == 'Female' ~ 'as10f',
  race_ethnicity == 'Asian' & grade == 'Grade 10' & sex == 'Male' ~ 'as10m',
  race_ethnicity == 'Asian' & grade == 'Grade 10' & sex == 'Not Specified' ~ 'as10ns',
  race_ethnicity == 'Asian' & grade == 'Grade 11' & sex == 'Female' ~ 'as11f',
  race_ethnicity == 'Asian' & grade == 'Grade 11' & sex == 'Male' ~ 'as11m',
  race_ethnicity == 'Asian' & grade == 'Grade 11' & sex == 'Not Specified' ~ 'as11ns',
  race_ethnicity == 'Asian' & grade == 'Grade 12' & sex == 'Female' ~ 'as12f',
  race_ethnicity == 'Asian' & grade == 'Grade 12' & sex == 'Male' ~ 'as12m',
  race_ethnicity == 'Asian' & grade == 'Grade 12' & sex == 'Not Specified' ~ 'as12ns',
  race_ethnicity == 'Asian' & grade == 'Grade 13' & sex == 'Female' ~ 'as13f',
  race_ethnicity == 'Asian' & grade == 'Grade 13' & sex == 'Male' ~ 'as13m',
  race_ethnicity == 'Asian' & grade == 'Grade 13' & sex == 'Not Specified' ~ 'as13ns',
  race_ethnicity == 'Black or African American' & grade == 'Grade 9' & sex == 'Female' ~ 'bl09f',
  race_ethnicity == 'Black or African American' & grade == 'Grade 9' & sex == 'Male' ~ 'bl09m',
  race_ethnicity == 'Black or African American' & grade == 'Grade 9' & sex == 'Not Specified' ~ 'bl09ns', 
  race_ethnicity == 'Black or African American' & grade == 'Grade 10' & sex == 'Female' ~ 'bl10f',
  race_ethnicity == 'Black or African American' & grade == 'Grade 10' & sex == 'Male' ~ 'bl10m',
  race_ethnicity == 'Black or African American' & grade == 'Grade 10' & sex == 'Not Specified' ~ 'bl10ns', 
  race_ethnicity == 'Black or African American' & grade == 'Grade 11' & sex == 'Female' ~ 'bl11f',
  race_ethnicity == 'Black or African American' & grade == 'Grade 11' & sex == 'Male' ~ 'bl11m',
  race_ethnicity == 'Black or African American' & grade == 'Grade 11' & sex == 'Not Specified' ~ 'bl11ns',
  race_ethnicity == 'Black or African American' & grade == 'Grade 12' & sex == 'Female' ~ 'bl12f',
  race_ethnicity == 'Black or African American' & grade == 'Grade 12' & sex == 'Male' ~ 'bl12m',
  race_ethnicity == 'Black or African American' & grade == 'Grade 12' & sex == 'Not Specified' ~ 'bl12ns', 
  race_ethnicity == 'Black or African American' & grade == 'Grade 13' & sex == 'Female' ~ 'bl13f',
  race_ethnicity == 'Black or African American' & grade == 'Grade 13' & sex == 'Male' ~ 'bl13m',
  race_ethnicity == 'Black or African American' & grade == 'Grade 13' & sex == 'Not Specified' ~ 'bl13ns', 
  race_ethnicity == 'Hispanic/Latino' & grade == 'Grade 9' & sex == 'Female' ~ 'hi09f',
  race_ethnicity == 'Hispanic/Latino' & grade == 'Grade 9' & sex == 'Male' ~ 'hi09m',
  race_ethnicity == 'Hispanic/Latino' & grade == 'Grade 9' & sex == 'Not Specified' ~ 'hi09ns',
  race_ethnicity == 'Hispanic/Latino' & grade == 'Grade 10' & sex == 'Female' ~ 'hi10f',
  race_ethnicity == 'Hispanic/Latino' & grade == 'Grade 10' & sex == 'Male' ~ 'hi10m',
  race_ethnicity == 'Hispanic/Latino' & grade == 'Grade 10' & sex == 'Not Specified' ~ 'hi10ns',
  race_ethnicity == 'Hispanic/Latino' & grade == 'Grade 11' & sex == 'Female' ~ 'hi11f',
  race_ethnicity == 'Hispanic/Latino' & grade == 'Grade 11' & sex == 'Male' ~ 'hi11m',
  race_ethnicity == 'Hispanic/Latino' & grade == 'Grade 11' & sex == 'Not Specified' ~ 'hi11ns',
  race_ethnicity == 'Hispanic/Latino' & grade == 'Grade 12' & sex == 'Female' ~ 'hi12f',
  race_ethnicity == 'Hispanic/Latino' & grade == 'Grade 12' & sex == 'Male' ~ 'hi12m',
  race_ethnicity == 'Hispanic/Latino' & grade == 'Grade 12' & sex == 'Not Specified' ~ 'hi12ns',
  race_ethnicity == 'Hispanic/Latino' & grade == 'Grade 13' & sex == 'Female' ~ 'hi13f',
  race_ethnicity == 'Hispanic/Latino' & grade == 'Grade 13' & sex == 'Male' ~ 'hi13m',
  race_ethnicity == 'Hispanic/Latino' & grade == 'Grade 13' & sex == 'Not Specified' ~ 'hi13ns',
  race_ethnicity == 'Native Hawaiian or Other Pacific Islander' & grade == 'Grade 9' & sex == 'Female' ~ 'hp09f',
  race_ethnicity == 'Native Hawaiian or Other Pacific Islander' & grade == 'Grade 9' & sex == 'Male' ~ 'hp09m',
  race_ethnicity == 'Native Hawaiian or Other Pacific Islander' & grade == 'Grade 9' & sex == 'Not Specified' ~ 'hp09ns',
  race_ethnicity == 'Native Hawaiian or Other Pacific Islander' & grade == 'Grade 10' & sex == 'Female' ~ 'hp10f',
  race_ethnicity == 'Native Hawaiian or Other Pacific Islander' & grade == 'Grade 10' & sex == 'Male' ~ 'hp10m',
  race_ethnicity == 'Native Hawaiian or Other Pacific Islander' & grade == 'Grade 10' & sex == 'Not Specified' ~ 'hp10ns',
  race_ethnicity == 'Native Hawaiian or Other Pacific Islander' & grade == 'Grade 11' & sex == 'Female' ~ 'hp11f',
  race_ethnicity == 'Native Hawaiian or Other Pacific Islander' & grade == 'Grade 11' & sex == 'Male' ~ 'hp11m',
  race_ethnicity == 'Native Hawaiian or Other Pacific Islander' & grade == 'Grade 11' & sex == 'Not Specified' ~ 'hp11ns',
  race_ethnicity == 'Native Hawaiian or Other Pacific Islander' & grade == 'Grade 12' & sex == 'Female' ~ 'hp12f',
  race_ethnicity == 'Native Hawaiian or Other Pacific Islander' & grade == 'Grade 12' & sex == 'Male' ~ 'hp12m',
  race_ethnicity == 'Native Hawaiian or Other Pacific Islander' & grade == 'Grade 12' & sex == 'Not Specified' ~ 'hp12ns',
  race_ethnicity == 'Native Hawaiian or Other Pacific Islander' & grade == 'Grade 13' & sex == 'Female' ~ 'hp13f',
  race_ethnicity == 'Native Hawaiian or Other Pacific Islander' & grade == 'Grade 13' & sex == 'Male' ~ 'hp13m',
  race_ethnicity == 'Native Hawaiian or Other Pacific Islander' & grade == 'Grade 13' & sex == 'Not Specified' ~ 'hp13ns',
  race_ethnicity == 'Not Specified' & grade == 'Grade 9' & sex == 'Female' ~ 'ns09f',
  race_ethnicity == 'Not Specified' & grade == 'Grade 9' & sex == 'Male' ~ 'ns09m',
  race_ethnicity == 'Not Specified' & grade == 'Grade 9' & sex == 'Not Specified' ~ 'ns09ns',
  race_ethnicity == 'Not Specified' & grade == 'Grade 10' & sex == 'Female' ~ 'ns10f',
  race_ethnicity == 'Not Specified' & grade == 'Grade 10' & sex == 'Male' ~ 'ns10m',
  race_ethnicity == 'Not Specified' & grade == 'Grade 10' & sex == 'Not Specified' ~ 'ns10ns',
  race_ethnicity == 'Not Specified' & grade == 'Grade 11' & sex == 'Female' ~ 'ns11f',
  race_ethnicity == 'Not Specified' & grade == 'Grade 11' & sex == 'Male' ~ 'ns11m',
  race_ethnicity == 'Not Specified' & grade == 'Grade 11' & sex == 'Not Specified' ~ 'ns11ns',
  race_ethnicity == 'Not Specified' & grade == 'Grade 12' & sex == 'Female' ~ 'ns12f',
  race_ethnicity == 'Not Specified' & grade == 'Grade 12' & sex == 'Male' ~ 'ns12m',
  race_ethnicity == 'Not Specified' & grade == 'Grade 12' & sex == 'Not Specified' ~ 'ns12ns',
  race_ethnicity == 'Not Specified' & grade == 'Grade 13' & sex == 'Female' ~ 'ns13f',
  race_ethnicity == 'Not Specified' & grade == 'Grade 13' & sex == 'Male' ~ 'ns13m',
  race_ethnicity == 'Not Specified' & grade == 'Grade 13' & sex == 'Not Specified' ~ 'ns13ns',
  race_ethnicity == 'Two or more races' & grade == 'Grade 9' & sex == 'Female' ~ 'tr09f',
  race_ethnicity == 'Two or more races' & grade == 'Grade 9' & sex == 'Male' ~ 'tr09m',
  race_ethnicity == 'Two or more races' & grade == 'Grade 9' & sex == 'Not Specified' ~ 'tr09ns',
  race_ethnicity == 'Two or more races' & grade == 'Grade 10' & sex == 'Female' ~ 'tr10f',
  race_ethnicity == 'Two or more races' & grade == 'Grade 10' & sex == 'Male' ~ 'tr10m',
  race_ethnicity == 'Two or more races' & grade == 'Grade 10' & sex == 'Not Specified' ~ 'tr10ns',
  race_ethnicity == 'Two or more races' & grade == 'Grade 11' & sex == 'Female' ~ 'tr11f',
  race_ethnicity == 'Two or more races' & grade == 'Grade 11' & sex == 'Male' ~ 'tr11m',
  race_ethnicity == 'Two or more races' & grade == 'Grade 11' & sex == 'Not Specified' ~ 'tr11ns',
  race_ethnicity == 'Two or more races' & grade == 'Grade 12' & sex == 'Female' ~ 'tr12f',
  race_ethnicity == 'Two or more races' & grade == 'Grade 12' & sex == 'Male' ~ 'tr12m',
  race_ethnicity == 'Two or more races' & grade == 'Grade 12' & sex == 'Not Specified' ~ 'tr12ns',
  race_ethnicity == 'Two or more races' & grade == 'Grade 13' & sex == 'Female' ~ 'tr13f',
  race_ethnicity == 'Two or more races' & grade == 'Grade 13' & sex == 'Male' ~ 'tr13m',
  race_ethnicity == 'Two or more races' & grade == 'Grade 13' & sex == 'Not Specified' ~ 'tr13ns',
  race_ethnicity == 'White' & grade == 'Grade 9' & sex == 'Female' ~ 'wh09f',
  race_ethnicity == 'White' & grade == 'Grade 9' & sex == 'Male' ~ 'wh09m',
  race_ethnicity == 'White' & grade == 'Grade 9' & sex == 'Not Specified' ~ 'wh09ns',
  race_ethnicity == 'White' & grade == 'Grade 10' & sex == 'Female' ~ 'wh10f',
  race_ethnicity == 'White' & grade == 'Grade 10' & sex == 'Male' ~ 'wh10m',
  race_ethnicity == 'White' & grade == 'Grade 10' & sex == 'Not Specified' ~ 'wh10ns',
  race_ethnicity == 'White' & grade == 'Grade 11' & sex == 'Female' ~ 'wh11f',
  race_ethnicity == 'White' & grade == 'Grade 11' & sex == 'Male' ~ 'wh11m',
  race_ethnicity == 'White' & grade == 'Grade 11' & sex == 'Not Specified' ~ 'wh11ns',
  race_ethnicity == 'White' & grade == 'Grade 12' & sex == 'Female' ~ 'wh12f',
  race_ethnicity == 'White' & grade == 'Grade 12' & sex == 'Male' ~ 'wh12m',
  race_ethnicity == 'White' & grade == 'Grade 12' & sex == 'Not Specified' ~ 'wh12ns',
  race_ethnicity == 'White' & grade == 'Grade 13' & sex == 'Female' ~ 'wh13f',
  race_ethnicity == 'White' & grade == 'Grade 13' & sex == 'Male' ~ 'wh13m',
  race_ethnicity == 'White' & grade == 'Grade 13' & sex == 'Not Specified' ~ 'wh13ns',
))

#other code option: hs_membership <- hs_membership %>% mutate(race_grade_gender = str_c(race_ethnicity, grade, sex, sep = '_'))

hs_membership %>% group_by(race_grade_gender) %>% count()

hs_membership<- reshape(data = hs_membership, idvar = 'ncessch', v.names = 'student_count', timevar = 'race_grade_gender', direction = 'wide') 

#rename race_grade_gender variables
hs_membership <- hs_membership %>% rename(
  'am09f' = 'student_count.am09f',
  'am09m' = 'student_count.am09m',
  #'am09ns' = 'student_count.am09ns',
  'am10f' = 'student_count.am10f',
  'am10m' = 'student_count.am10m',
  #'am10ns' = 'student_count.am10ns',
  'am11f' = 'student_count.am11f',
  'am11m' = 'student_count.am11m',
  #'am11ns' = 'student_count.am11ns',
  'am12f' = 'student_count.am12f',
  'am12m' = 'student_count.am12m',
  #'am12ns' = 'student_count.am12ns',
  'am13f' = 'student_count.am13f',
  'am13m' = 'student_count.am13m',
  #'am13ns' = 'student_count.am13ns',
  'as09f' = 'student_count.as09f',
  'as09m' = 'student_count.as09m',
  #'as09ns' = 'student_count.as09ns',
  'as10f' = 'student_count.as10f',
  'as10m' = 'student_count.as10m',
  #'as10ns' = 'student_count.as10ns',
  'as11f' = 'student_count.as11f',
  'as11m' = 'student_count.as11m',
  #'as11ns' = 'student_count.as11ns',
  'as12f' = 'student_count.as12f',
  'as12m' = 'student_count.as12m',
  #'as12ns' = 'student_count.as12ns',
  'as13f' = 'student_count.as13f',
  'as13m' = 'student_count.as13m',
  #'as13ns' = 'student_count.as13ns',
  'bl09f' = 'student_count.bl09f',
  'bl09m' = 'student_count.bl09m',
  #'bl09ns' = 'student_count.bl09ns',
  'bl10f' = 'student_count.bl10f',
  'bl10m' = 'student_count.bl10m',
  #'bl10ns' = 'student_count.bl10ns',
  'bl11f' = 'student_count.bl11f',
  'bl11m' = 'student_count.bl11m',
  #'bl11ns' = 'student_count.bl11ns',
  'bl12f' = 'student_count.bl12f',
  'bl12m' = 'student_count.bl12m',
  #'bl12ns' = 'student_count.bl12ns',
  'bl13f' = 'student_count.bl13f',
  'bl13m' = 'student_count.bl13m',
  #'bl13ns' = 'student_count.bl13ns',
  'wh09f' = 'student_count.wh09f',
  'wh09m' = 'student_count.wh09m',
  #'wh09ns' = 'student_count.wh09ns',
  'wh10f' = 'student_count.wh10f',
  'wh10m' = 'student_count.wh10m',
  #'wh10ns' = 'student_count.wh10ns',
  'wh11f' = 'student_count.wh11f',
  'wh11m' = 'student_count.wh11m',
  #'wh11ns' = 'student_count.wh11ns',
  'wh12f' = 'student_count.wh12f',
  'wh12m' = 'student_count.wh12m',
  #'wh12ns' = 'student_count.wh12ns',
  'wh13f' = 'student_count.wh13f',
  'wh13m' = 'student_count.wh13m',
  #'wh13ns' = 'student_count.wh13ns',
  'hp09f' = 'student_count.hp09f',
  'hp09m' = 'student_count.hp09m',
  #'hp09ns' = 'student_count.hp09ns',
  'hp10f' = 'student_count.hp10f',
  'hp10m' = 'student_count.hp10m',
  #'hp10ns' = 'student_count.hp10ns',
  'hp11f' = 'student_count.hp11f',
  'hp11m' = 'student_count.hp11m',
  #'hp11ns' = 'student_count.hp11ns',
  'hp12f' = 'student_count.hp12f',
  'hp12m' = 'student_count.hp12m',
  #'hp12ns' = 'student_count.hp12ns',
  'hp13f' = 'student_count.hp13f',
  'hp13m' = 'student_count.hp13m',
  #'hp13ns' = 'student_count.hp13ns',
  'hi09f' = 'student_count.hi09f',
  'hi09m' = 'student_count.hi09m',
  #'hi09ns' = 'student_count.hi09ns',
  'hi10f' = 'student_count.hi10f',
  'hi10m' = 'student_count.hi10m',
  #'hi10ns' = 'student_count.hi10ns',
  'hi11f' = 'student_count.hi11f',
  'hi11m' = 'student_count.hi11m',
  #'hi11ns' = 'student_count.hi11ns',
  'hi12f' = 'student_count.hi12f',
  'hi12m' = 'student_count.hi12m',
  #'hi12ns' = 'student_count.hi12ns',
  'hi13f' = 'student_count.hi13f',
  'hi13m' = 'student_count.hi13m',
  #'hi13ns' = 'student_count.hi13ns',
  'tr09f' = 'student_count.tr09f',
  'tr09m' = 'student_count.tr09m',
  #'tr09ns' = 'student_count.tr09ns',
  'tr10f' = 'student_count.tr10f',
  'tr10m' = 'student_count.tr10m',
  #'tr10ns' = 'student_count.tr10ns',
  'tr11f' = 'student_count.tr11f',
  'tr11m' = 'student_count.tr11m',
  #'tr11ns' = 'student_count.tr11ns',
  'tr12f' = 'student_count.tr12f',
  'tr12m' = 'student_count.tr12m',
  #'tr12ns' = 'student_count.tr12ns',
  'tr13f' = 'student_count.tr13f',
  'tr13m' = 'student_count.tr13m',
  #'tr13ns' = 'student_count.tr13ns',
  #'ns09f' = 'student_count.ns09f',
  #'ns09m' = 'student_count.ns09m',
  'ns09ns' = 'student_count.ns09ns',
  #'ns10f' = 'student_count.ns10f',
  #'ns10m' = 'student_count.ns10m',
  'ns10ns' = 'student_count.ns10ns',
  #'ns11f' = 'student_count.ns11f',
  #'ns11m' = 'student_count.ns11m',
  'ns11ns' = 'student_count.ns11ns',
  #'ns12f' = 'student_count.ns12f',
  #'ns12m' = 'student_count.ns12m',
  'ns12ns' = 'student_count.ns12ns',
  #'ns13f' = 'student_count.ns13f',
  #'ns13m' = 'student_count.ns13m',
  'ns13ns' = 'student_count.ns13ns')

#Sum race & gender
hs_membership$amalf <- rowSums(hs_membership[,c('am09f','am10f','am11f','am12f','am13f')], na.rm = TRUE)
hs_membership$amalm <- rowSums(hs_membership[,c('am09m','am10m','am11m','am12m','am13m')], na.rm = TRUE)
hs_membership$am <- rowSums(hs_membership[,c('amalf','amalm')], na.rm = TRUE)

hs_membership$asalf <- rowSums(hs_membership[,c('as09f','as10f','as11f','as12f','as13f')], na.rm = TRUE)
hs_membership$asalm <- rowSums(hs_membership[,c('as09m','as10m','as11m','as12m','as13m')], na.rm = TRUE)
hs_membership$as <- rowSums(hs_membership[,c('asalf','asalm')], na.rm = TRUE)

hs_membership$blalf <- rowSums(hs_membership[,c('bl09f','bl10f','bl11f','bl12f','bl13f')], na.rm = TRUE)
hs_membership$blalm <- rowSums(hs_membership[,c('bl09m','bl10m','bl11m','bl12m','bl13m')], na.rm = TRUE)
hs_membership$bl <- rowSums(hs_membership[,c('blalf','blalm')], na.rm = TRUE)

hs_membership$whalf <- rowSums(hs_membership[,c('wh09f','wh10f','wh11f','wh12f','wh13f')], na.rm = TRUE)
hs_membership$whalm <- rowSums(hs_membership[,c('wh09m','wh10m','wh11m','wh12m','wh13m')], na.rm = TRUE)
hs_membership$wh <- rowSums(hs_membership[,c('whalf','whalm')], na.rm = TRUE)

hs_membership$hpalf <- rowSums(hs_membership[,c('hp09f','hp10f','hp11f','hp12f','hp13f')], na.rm = TRUE)
hs_membership$hpalm <- rowSums(hs_membership[,c('hp09m','hp10m','hp11m','hp12m','hp13m')], na.rm = TRUE)
hs_membership$hp <- rowSums(hs_membership[,c('hpalf','hpalm')], na.rm = TRUE)

hs_membership$hialf <- rowSums(hs_membership[,c('hi09f','hi10f','hi11f','hi12f','hi13f')], na.rm = TRUE)
hs_membership$hialm <- rowSums(hs_membership[,c('hi09m','hi10m','hi11m','hi12m','hi13m')], na.rm = TRUE)
hs_membership$hi <- rowSums(hs_membership[,c('hialf','hialm')], na.rm = TRUE)

hs_membership$tralf <- rowSums(hs_membership[,c('tr09f','tr10f','tr11f','tr12f','tr13f')], na.rm = TRUE)
hs_membership$tralm <- rowSums(hs_membership[,c('tr09m','tr10m','tr11m','tr12m','tr13m')], na.rm = TRUE)
hs_membership$tr <- rowSums(hs_membership[,c('tralf','tralm')], na.rm = TRUE)

hs_membership$ns <- rowSums(hs_membership[,c('ns09ns', 'ns10ns', 'ns11ns', 'ns12ns', 'ns13ns')], na.rm = TRUE)


#to do: merge (merge all ccd), rename, anything else?

