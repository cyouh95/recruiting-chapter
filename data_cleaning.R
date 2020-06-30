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
                                        address = "mstreet1", #what do I do about mstreet2 & mstreet3
                                        city = "mcity",
                                        zip_code = "mzip", #what do I do about mzip4
                                        address_2 = "lstreet1") #what do I do about lstreet2 & lstreet3
                                        
                                        
                                        
                                        
                                        
                                  
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

###PSS Data
##Loading & investigating data: PSS (private school data)
privatehs <- read.csv('./data/pss_1718.csv', header = TRUE, na.strings=c("","NA"), stringsAsFactors=F, colClasses = c('pzip'='character', 'pzip4' = 'character', 'pl_zip' = 'character', 'pl_zip4' = 'character', 'ppin' = 'character')) 

#Investigate the data & data frame
str(privatehs)

nrow(privatehs) #num of rows: 22,895 observations
ncol(privatehs) #num of columns: 349 variables

names(privatehs) # variable names 
names(privatehs) <- tolower(names(privatehs)) #convert to lowercase
glimpse(privatehs)
head(privatehs) # prints first 6 obs

privatehs %>% arrange(p270,p280,p290,p300, p305,p320,p330,p325,p316,p318,p310,p332) %>% head()


##Labeling variables 
#Importing data dictionary
pss_dictionary <- read.csv('./data/pss_dictionary_1718.csv', header = TRUE, na.strings=c("","NA"))  
pss_dict_subset <- pss_dictionary %>% select(Name, Label)

#Lowercase variable names in dictionary
names(pss_dict_subset) <- tolower(names(pss_dict_subset))
pss_dict_subset<- pss_dict_subset %>% mutate(names_lc = tolower(name))

#Loop to label data with dictionary
for (i in names(privatehs)) { 
  description4 <- (pss_dict_subset %>% filter(names_lc == i))$label 
  var_label(privatehs[[i]]) <- description4 #label variables
}

#Check variable labels
privatehs %>% var_label() # variable labels

#Adding value labels
privatehs %>% select(csource,p265,p275,p285,p295,p335,p345,p415,p430,p440,p445,p450,p455,p460,p465,p467,p468,p470,p475,
                     p480,p485,p490,p492,p495,p500,p505,p510,p515,p520,p522,p525,p530,p535,p540,p542,p545,p550,p555,p575,
                     p580,p585,p590,p600,p602,p605,p610,p620,p622,p630,p635,p640,region,pstansi,ulocale18,logr2018,higr2018,
                     typology,relig,orient,diocese,level,size,ucommtyp) %>% glimpse() #not labeled

privatehs <- privatehs %>% set_value_labels( 
  csource = c("Mail" = 1,
              "Internet" = 2,
              "Regional office follow-up" = 3,
              "Computer-assisted telephone interview" = 4),
  p135 = c("Yes" = 1,
           "No" = 2),
  p265 = c("Yes" = 1,
           "No" = 2),
  p275 = c("Yes" = 1,
          "No" = 2),
  p285 = c("Yes" = 1,
          "No" = 2),
  p295 = c("Yes" = 1,
           "No" = 2),
  p420 = c("Yes" = 1, 
           "No" = 2),
  p425 = c("Yes" = 1, 
         "No" = 2),
  p430 = c("Yes" = 1,
           "No" = 2),
  p435 = c("Yes" = 1,
           "No" = 2),
  p440 = c("Roman Catholic" = 1,
           "African Methodist Episcopal" = 2,
           "Amish" = 3,
           "Assembly of God" = 4,
           "Baptist" = 5,
           "Brethren" = 6,
           "Calvinist" = 7,
           "Christian (no specific denomination)" = 8,
           "Church of Christ" = 9,
           "Church of God" = 10,
           "Church of God in Christ" = 11,
           "Church of the Nazarene" = 12,
           "Disciples of Christ" = 13,
           "Episcopal" = 14,
           "Friends" = 15, 
           "Greek Orthodox" = 16,
           "Islamic" = 17,
           "Jewish" = 18,
           "Latter Day Saints" = 19,
           "Lutheran Church - Missouri Synod" = 20, 
           "Evangelical Lutheran Church in America" = 21,
           "Wisconsin Evangelical Lutheran Synod" = 22,
           "Other Lutheran" = 23,
           "Mennonite" = 24, 
           "Methodist" = 25, 
           "Pentecostal" = 26,
           "Presbyterian" = 27, 
           "Seventh-Day Adventist" = 28,
           "Other" = 29),
  p445 = c("Parochial" = 1,
           "Diocesan" = 2,
           "Private" = 3),
  p450 = c("Does belong to an organization/association" = 0,
           "Does not belong to an organization/association" = 1), 
  p455 = c("Not member" = 0,
           "Member" = 1), 
  p460 = c("Not member" = 0,
           "Member" = 1),
  p465 = c("Not member" = 0,
           "Member" = 1),
  p467 = c("Not member" = 0,
           "Member" = 1),
  p468 = c("Not member" = 0,
           "Member" = 1),
  p470 = c("Not member" = 0,
           "Member" = 1),
  p475 = c("Not member" = 0,
           "Member" = 1),
  p480 = c("Not member" = 0,
           "Member" = 1),
  p485 = c("Not member" = 0,
           "Member" = 1),
  p490 = c("Not member" = 0,
           "Member" = 1),
  p492 = c("Not member" = 0,
           "Member" = 1),
  p495 = c("Not member" = 0,
           "Member" = 1),
  p500 = c("Not member" = 0,
           "Member" = 1),
  p505 = c("Not member" = 0,
           "Member" = 1),
  p510 = c("Not member" = 0,
           "Member" = 1),
  p515 = c("Not member" = 0,
           "Member" = 1),
  p520 = c("Not member" = 0,
           "Member" = 1),
  p522 = c("Not member" = 0,
           "Member" = 1),
  p525 = c("Not member" = 0,
           "Member" = 1),
  p530 = c("Not member" = 0,
           "Member" = 1),
  p535 = c("Not member" = 0,
           "Member" = 1),
  p540 = c("Not member" = 0,
           "Member" = 1),
  p542 = c("Not member" = 0,
           "Member" = 1),
  p545 = c("Not member" = 0,
           "Member" = 1),
  p550 = c("Not member" = 0,
           "Member" = 1),
  p555 = c("Not member" = 0,
           "Member" = 1),
  p575 = c("Not member" = 0,
           "Member" = 1),
  p580 = c("Not member" = 0,
           "Member" = 1),
  p585 = c("Not member" = 0,
           "Member" = 1),
  p590 = c("Not member" = 0,
           "Member" = 1),
  p600 = c("Not member" = 0,
           "Member" = 1),
  p602 = c("Not member" = 0,
           "Member" = 1),
  p605 = c("Not member" = 0,
           "Member" = 1),
  p610 = c("Not member" = 0,
           "Member" = 1),
  p620 = c("Not member" = 0,
           "Member" = 1),
  p622 = c("Not member" = 0,
           "Member" = 1),
  p630 = c("Not member" = 0,
         "Member" = 1),
  p635 = c("Not member" = 0,
           "Member" = 1),
  p640 = c("Not member" = 0,
           "Member" = 1),
  region = c("Northeast" = 1,
             "Midwest" = 2,
             "South" = 3,
             "West" = 4),
  pstansi = c("Alabama" = 01,
              "Alaska" = 02,
              "Arizona" = 04,
              "Arkansas" = 05,
              "California" = 06,
              "Colorado" = 08,
              "Connecticut" = 09,
              "Delaware" = 10,
              "District of Columbia" = 11,
              "Florida" = 12, 
              "Georgia" = 13,
              "Hawaii" = 15,
              "Idaho" = 16,
              "Illinois" = 17,
              "Indiana" = 18, 
              "Iowa" = 19, 
              "Kansas" = 20, 
              "Kentucky" = 21,
              "Louisiana" = 22,
              "Maine" = 23,
              "Maryland" = 24,
              "Massachusetts" = 25,
              "Michigan" = 26, 
              "Minnesota" = 27, 
              "Mississippi" = 28, 
              "Missouri" = 29,
              "Montana" = 30, 
              "Nebraska" = 31, 
              "Nevada" = 32, 
              "New Hampshire" = 33, 
              "New Jersey" = 34, 
              "New Mexico" = 35, 
              "New York" = 36, 
              "North Carolina" = 37, 
              "North Dakota" = 38, 
              "Ohio" = 39, 
              "Oklahoma" = 40, 
              "Oregon" = 41, 
              "Pennsylvania" = 42, 
              "Rhode Island" = 44,
              "South Carolina" = 45, 
              "South Dakota" = 46, 
              "Tennessee" = 47, 
              "Texas" = 48, 
              "Utah" = 49,
              "Vermont" = 50, 
              "Virginia" = 51,
              "Washington" = 53,
              "West Virginia" = 54,
              "Wisconsin" = 55,
              "Wyoming" = 56), 
  ulocale18 = c("City, large" = 11,
                "City, midsize" = 12, 
                "City, small" = 13, 
                "Suburb, large" = 21,
                "Suburb, midsize" = 22,
                "Suburb, small" = 23,
                "Town, fringe" = 31,
                "Town, distant" = 32, 
                "Town, remote" = 33, 
                "Rural, fringe" = 41,
                "Rural, distant" = 42,
                "Rural, remote" = 43),
  logr2018 = c("All ungraded" = 1, 
               "Lowest grade in school is prekindergarten" = 2,
               "Lowest grade in school is kindergarten" = 3, 
               "Lowest grade in school is transitional kindergarten" = 4,
               "Lowest grade in school is transitional first grade" = 5,
               "Lowest grade in school is 1st grade" = 6, 
               "Lowest grade in school is 2nd grade" = 7,
               "Lowest grade in school is 3rd grade" = 8, 
               "Lowest grade in school is 4th grade" = 9, 
               "Lowest grade in school is 5th grade" = 10, 
               "Lowest grade in school is 6th grade" = 11,
               "Lowest grade in school is 7th grade" = 12, 
               "Lowest grade in school is 8th grade" = 13, 
               "Lowest grade in school is 9th grade" = 14, 
               "Lowest grade in school is 10th grade" = 15, 
               "Lowest grade in school is 11th grade" = 16, 
               "Lowest grade in school is 12th grade" = 17),
  higr2018 = c("All ungraded" = 1, 
               "Highest grade in school is prekindergarten" = 2,
               "Highest grade in school is kindergarten" = 3, 
               "Highest grade in school is transitional kindergarten" = 4,
               "Highest grade in school is transitional first grade" = 5,
               "Highest grade in school is 1st grade" = 6, 
               "Highest grade in school is 2nd grade" = 7,
               "Highest grade in school is 3rd grade" = 8, 
               "Highest grade in school is 4th grade" = 9, 
               "Highest grade in school is 5th grade" = 10, 
               "Highest grade in school is 6th grade" = 11,
               "Highest grade in school is 7th grade" = 12, 
               "Highest grade in school is 8th grade" = 13, 
               "Highest grade in school is 9th grade" = 14, 
               "Highest grade in school is 10th grade" = 15, 
               "Highest grade in school is 11th grade" = 16, 
               "Highest grade in school is 12th grade" = 17),
  tabflag = c("Schools offering ungraded or grade 1 or above" = 1,
              "Schools offering no grade higher than KG" = 2), 
  typology = c("Catholic, parochial" = 1,
               "Catholic, diocesan" = 2,
               "Catholic, private" = 3, 
               "Other religious, conservative Christian" = 4,
               "Other religious, affiliated with established denominaiton" = 5,
               "Other religious, not affiliated with any denomination" = 6,
               "Nonsectarian, regular school" = 7,
               "Nonsectarian, special program" = 8, 
               "Nonsectarian, special education" = 9),
  relig = c("Catholic (P430=1 and P440=1" = 1,
            "Other religious (P430=1 and P440 ne 1" = 2,
            "Nonsectarian (P430=2)" = 3),
  orient = c("Roman Catholic" = 1,
             "African Methodist Episcopal" = 2, 
             "Amish" = 3, 
             "Assembly of God" = 4, 
             "Baptist" = 5,
             "Brethren" = 6, 
             "Calvinist" = 7, 
             "Christian (no specific denomination" = 8, 
             "Church of Christ" = 9,
             "Church of God" = 10,
             "Church of God in Christ" = 11,
             "Church of the Nazarene" = 12,
             "Disciples of Christ" = 13,
             "Episcopal" = 14, 
             "Friends" = 15, 
             "Greek Orthodox" = 16, 
             "Islamic" = 17, 
             "Jewish" = 18, 
             "Latter Day Saints" = 19, 
             "Lutheran Church - Missouri Synod" = 20, 
             "Evangelical Lutheran Church in America" = 21, 
             "Wisconsin Evangelical Lutheran Synod" = 22,
             "Other Lutheran" = 23, 
             "Mennonite" = 24, 
             "Methodist" = 25, 
             "Pentecostal" = 26, 
             "Presbyterian" = 27, 
             "Seventh-Day Adventist" = 28, 
             "Other" = 29, 
             "Nonsectarian" = 30), 
  diocese = c( "Diocese of Birmingham, AL" = 0101,
              "Diocese of Mobile, AL" = 0102, 
              "Archdiocese of Anchorage, AK" = 0201,
              "Diocese of Fairbanks, AK" = 0202,
              "Diocese of Juneau, AK" = 0203,
              "Diocese of Phoenix, AZ" = 0401,
              "Diocese of Tucson, AZ" = 0402,
              "Diocese of Little Rock, AR" = 0501,
              "Archdiocese of Los Angeles, CA" = 0601,
              "Archdiocese of San Francisco, CA" = 0602,
              "Diocese of Fresno, CA" = 0603,
              "Diocese of Monterey, CA" = 0604,
              "Diocese of Oakland, CA" = 0605,
              "Diocese of Orange, CA" = 0606,
              "Diocese of Sacramento, CA" = 0607,
              "Diocese of San Bernardino, CA" = 0608,
              "Diocese of San Diego, CA" = 0609, 
              "Diocese of San Jose, CA" = 0610,
              "Diocese of Santa Rosa, CA" = 0611,
              "Diocese of Stockton, CA" = 0612,
              "Archdiocese of Denver, CO" = 0801,
              "Diocese of Colorado Springs, CO" = 0802,
              "Diocese of Pueblo, CO" = 0803,
              "Archdiocese of Hartford, CT" = 0901,
              "Diocese of Bridgeport, CT" = 0902,
              "Diocese of Norwich, CT" = 0903,
              "Diocese of Wilmington, DE" = 1001,
              "Archdiocese of Washington, DC" = 1101,
              "Archdiocese of Miami, FL" = 1201,
              "Diocese of Pensacola-Tallahassee, FL" = 1202,
              "Diocese of Orlando, FL" = 1203,
              "Diocese of Palm Beach, FL" = 1204,
              "Diocese of St. Augustine, FL" = 1205,
              "Diocese of St. Petersburg, FL" = 1206, 
              "Diocese of Venice, FL" = 1207,
              "Archdiocese of Atlanta, GA" = 1301,
              "Diocese of Savannah, GA" = 1302,
              "Diocese of Honolulu, HI" = 1501,
              "Diocese of Boise, ID" = 1601,
              "Archdiocese of Chicago, IL" = 1701,
              "Diocese of Belleville, IL" = 1702,
              "Diocese of Joliet, IL" = 1703,
              "Diocese of Peoria, IL" = 1704,
              "Diocese of Rockford, IL" = 1705,
              "Diocese of Springfield, IL" = 1706, 
              "Archdiocese of Indianapolis, IN" = 1801,
              "Diocese of Evansville, IN" = 1802, 
              "Diocese of Ft. Wayne-South Bend, IN" = 1803, 
              "Diocese of Gary, IN" = 1804, 
              "Diocese of Lafayette, IN" = 1805, 
              "Archdiocese of Dubuque, IA" = 1901,
              "Diocese of Davenport, IA" = 1902, 
              "Diocese of Des Moines, IA" = 1903,
              "Diocese of Sioux City, IA" = 1904,
              "Archdiocese of Kansas City, KS" = 2001,
              "Diocese of Dodge City, KS" = 2002, 
              "Diocese of Salina, KS" = 2003, 
              "Diocese of Wichita, KS" = 2004,
              "Archdiocese of Louisville, KY" = 2101,
              "Diocese of Covington, KY" = 2102, 
              "Diocese of Lexington, KY" = 2103, 
              "Diocese of Owensboro, KY" = 2104,
              "Archdiocese of New Orleans, LA" = 2201,
              "Diocese of Alexandria, LA" = 2202, 
              "Diocese of Baton Rouge, LA" = 2203, 
              "Diocese of Houma-Thibodaux, LA" = 2204,
              "Diocese of Lafayette, LA" = 2205, 
              "Diocese of Lake Charles, LA" = 2206, 
              "Diocese of Shrevenport, LA" = 2207, 
              "Diocese of Portland, ME" = 2301, 
              "Archdiocese of Baltimore, MD" = 2401, 
              "Archdiocese of Boston, MA" = 2501, 
              "Diocese of Fall River, MA" = 2502, 
              "Diocese of Springfield, MA" = 2503, 
              "Diocese of Worcester, MA" = 2504,
              "Archdiocese of Detroit, MI" = 2601, 
              "Diocese of Grand Rapids, MI" = 2602, 
              "Diocese of Gaylord, MI" = 2603, 
              "Diocese of Kalamazoo, MI" = 2604,
              "Diocese of Lansing, MI" = 2605,
              "Diocese of Marquette, MI" = 2606, 
              "Diocese of Saginaw, MI" = 2607, 
              "Archdiocese of St. Paul-Minneapolis, MN" = 2701, 
              "Diocese of Crookston, MN" = 2702, 
              "Diocese of Duluth, MN" = 2703, 
              "Diocese of New Ulm, MN" = 2704, 
              "Diocese of St. Cloud, MN" = 2705,
              "Diocese of Winona, MN" = 2706, 
              "Diocese of Biloxi, MS" = 2801, 
              "Diocese of Jackson, MS" = 2802,
              "Archdiocese of St. Louis, MO" = 2901,
              "Diocese of Jefferson City, MO" = 2902,
              "Diocese of Kansas City-St. Joseph, MO" = 2903, 
              "Diocese of Springfield-Cape Girardeau, MO" = 2904,
              "Diocese of Great Falls-Billings, MT" = 3001, 
              "Diocese of Helena, MT" = 3002, 
              "Archdiocese of Omaha, NE" = 3101, 
              "Diocese of Grand Island, NE" = 3102, 
              "Diocese of Lincoln, NE" = 3103, 
              "Diocese of Las Vegas, NV" = 3201, 
              "Diocese of Reno, NV" = 3202, 
              "Diocese of Manchester, NH" = 3301, 
              "Archdiocese of Newark, NJ" = 3401, 
              "Diocese of Camden, NJ" = 3402, 
              "Diocese of Metuchen, NJ" = 3403,
              "Diocese of Paterson, NJ" = 3404, 
              "Diocese of Trenton, NJ" = 3405, 
              "Archdiocese of Santa Fe, NM" = 3501,
              "Diocese of Gallup, NM" = 3502, 
              "Diocese of Las Cruces, NM" = 3503,
              "Archdiocese of New York, NY" = 3601, 
              "Diocese of Albany, NY" = 3602, 
              "Diocese of Brooklyn, NY" = 3603, 
              "Diocese of Buffalo, NY" = 3604, 
              "Diocese of Ogdensburg, NY" = 3605, 
              "Diocese of Rochester, NY" = 3606, 
              "Diocese of Rockville Centre, NY" = 3607, 
              "Diocese of Syracuse, NY" = 3608, 
              "Diocese of Charlotte, NC" = 3701, 
              "Diocese of Raleigh, NC" = 3702, 
              "Diocese of Bismarck, ND" = 3801, 
              "Diocese of Fargo, ND" = 3802, 
              "Archdiocese of Cincinnati, OH" = 3901, 
              "Diocese of Cleveland, OH" = 3902, 
              "Diocese of Columbus, OH" = 3903, 
              "Diocese of Steubenville, OH" = 3904, 
              "Diocese of Toledo, OH" = 3905, 
              "Diocese of Youngstown, OH" = 3906, 
              "Archdiocese of Oklahoma City, OK" = 4001, 
              "Diocese of Tulsa, OK" = 4002, 
              "Archdiocese of Portland, OR" = 4101, 
              "Diocese of Baker, OR" = 4102, 
              "Archdiocese of Philadelphia, PA" = 4201,
              "Diocese of Allentown, PA" = 4202, 
              "Diocese of Altoona-Johnstown, PA" = 4203, 
              "Diocese of Erie, PA" = 4204, 
              "Diocese of Greensburg, PA" = 4205, 
              "Diocese of Harrisburg, PA" = 4206, 
              "Diocese of Pittsburg, PA" = 4207, 
              "Diocese of Scranton, PA" = 4208, 
              "Diocese of Providence, RI" = 4401, 
              "Diocese of Charleston, SC" = 4501, 
              "Diocese of Rapid City, SD" = 4601,
              "Diocese of Sioux Falls, SD" = 4602,
              "Diocese of Knoxville, TN" = 4701, 
              "Diocese of Memphis, TN" = 4702, 
              "Diocese of Nashville, TN" = 4703, 
              "Archdiocese of San Antonio, TX" = 4801,
              "Diocese of Amarillo, TX" = 4802, 
              "Diocese of Austin, TX" = 4803,
              "Diocese of Beaumont, TX" = 4804, 
              "Diocese of Brownsville, TX" = 4805, 
              "Diocese of Corpus Christi, TX" = 4806, 
              "Diocese of Dallas, TX" = 4807, 
              "Diocese of El Paso, TX" = 4808, 
              "Diocese of Ft. Worth, TX" = 4809, 
              "Diocese of Galveston-Houston, TX" = 4810, 
              "Diocese of Lubbock, TX" = 4811, 
              "Diocese of San Angelo, TX" = 4812, 
              "Diocese of Tyler, TX" = 4813, 
              "Diocese of Victoria, TX" = 4814, 
              "Diocese of Laredo, TX" = 4815, 
              "Diocese of Salt Lake, UT" = 4901,
              "Diocese of Burlington, VT" = 5001, 
              "Diocese of Arlington, VA" = 5101, 
              "Diocese of Richmond, VA" = 5102, 
              "Archdiocese of Seattle, WA" = 5301,
              "Diocese of Spokane, WA" = 5302, 
              "Diocese of Yakima, WA" = 5303, 
              "Diocese of Wheeling-Charleston, WV" = 5401, 
              "Archdiocese of Milwaukee, WI" = 5501, 
              "Diocese of Green Bay, WI" = 5502, 
              "Diocese of La Crosse, WI" = 5503,
              "Diocese of Madison, WI" = 5504, 
              "Diocese of Superior, WI" = 5505, 
              "Diocese of Cheyenne, WY" = 5601), 
  level = c("Elementary" = 1, 
            "Secondary" = 2, 
            "Combined elementary and secondary" = 3),
  size = c("Less than 50 students" = 1,
           "50-149 students" = 2,
           "150-299 students" = 3, 
           "300-499 students" = 4,
           "500-749 students" = 5,
           "750 students or more" = 6), 
  ucommtyp = c("City (ulocale18 = 11,12,13)" = 1,
               "Suburb (ulocale18 = 21,22,23)" = 2, 
               "Town (ulocale18 = 31,32,33)" = 3,
               "Rural (ulocale18 = 41,42,43)" = 4),
  p415 = c("Regular elementary or secondary" = 1, 
           "Montessori" = 2,
           "Special program emphasis" = 3,
           "Special education" = 4,
           "Career/technical/vocational" = 5,
           "Alternative/other" = 6,
           "Early childhood program/child care center" = 7),
  p335 = c("Yes" = 1,
           "No, it is an all-female school" = 2,
           "No, it is an all-male school" = 3),
  p345 = c("Yes" = 1,
            "No" = 2))

#check value labels
val_labels(privatehs$p295)
val_labels(privatehs$diocese)
val_labels(privatehs$pstansi)
val_labels(privatehs$orient)
val_labels(privatehs$higr2018)
val_labels(privatehs$logr2018)


##Checking data validity

#Check for negative enrollment numbers
enrollment_phs <- privatehs %>% select(p240,p250,p260,p270,p280,p290,p300,p305,p320,p330,p325,p316,p318,p310,p332,p340,p350,numstuds)

for(i in names(enrollment_phs)) {  
  negative <- (enrollment_phs %>% filter(i < 0) %>% count())
  assert_that(negative == 0)
}

#Sixth grade enrollment, Seventh grade enrollment, Eighth grade enrollment, Ninth grade enrollment, Tenth grade enrollment, Eleventh grade enrollment, Twelfth grade enrollment
#Total student enrollment, Hispanic or Latino students, White students, Black students, Asian students, Native Hawaiian/Pacific Islander students, American Indian/Alaska Native students, Students of Two or More Races
#Number of male students, Number of 12th graders last year, #Total number of students

#Check for negative percents or over 100
privatehs %>% filter(p360 < 0 | p360 >100) %>% count() #4-year college
privatehs %>% filter(p_indian < 0 | p_indian > 100) %>% count() #American Indian or Alaska Native students
privatehs %>% filter(p_asian < 0 | p_asian > 100) %>% count() #Asian students
privatehs %>% filter(p_pacific < 0 | p_pacific > 100) %>% count() #Native Hawaiian or Pacific Islander students
privatehs %>% filter(p_hisp < 0 | p_hisp > 100) %>% count() #Hispanic students
privatehs %>% filter(p_white < 0 | p_white > 100) %>% count() #White students
privatehs %>% filter(p_black < 0 | p_black > 100) %>% count() #Black students
privatehs %>% filter(p_tr < 0 | p_tr > 100) %>% count() #Two or More Races

#Valid skips turned to negatives
privatehs %>% select(p435) %>% as.factor() #labeled as -1,  but on codebook its "."
privatehs %>% select(p440) %>% as.factor() #labeled as -1, but on codebook its "."
privatehs %>% select(p445) %>% as.factor() #labeled as -1,  but on codebook its "."
privatehs %>% select(diocese) %>% as.factor() #labeled as -1,  but on codebook its "."

#State postal code abbreviation
privatehs$pstabb
nrow(privatehs %>% filter(nchar(pstabb) == 2)) #22,895 (2 characters)

#Check zip codes
privatehs$pzip
nrow(privatehs %>% filter(nchar(pzip) != 5)) #2020 not 5 characters
nrow(privatehs %>% filter(nchar(pzip) == 4)) #the rest are 4 characters
privatehs %>% filter(is.na(pzip)) %>% count() #zero missing

p <- privatehs %>% select(pzip,pl_stabb) %>% filter(nchar(pzip) == 4) #CT,FL,ME,MA,MT,NH,NJ,RI,VT

privatehs$pzip4
nrow(privatehs %>% filter(nchar(pzip4) != 5)) #19,109 are not 5 characters
nrow(privatehs %>% filter(nchar(pzip4) == 4)) #17,973 are 4 characters
nrow(privatehs %>% filter(nchar(pzip4) == 3)) #932 are 3 characters
privatehs %>% filter(is.na(pzip4)) %>% count() #3786 missing

p4 <- privatehs %>% select(pzip4,pl_stabb) %>% filter(nchar(pzip4) == 3)

#Location state postal code abbreviation
privatehs$pl_stabb
nrow(privatehs %>% filter(nchar(pl_stabb) == 2)) #3,776 (2 characters), a lot missing

#Check location zip codes
privatehs$pl_zip
nrow(privatehs %>% filter(nchar(pl_zip) == 5)) #3,414 are 5 characters
nrow(privatehs %>% filter(nchar(pl_zip) == 4)) #338 are 4 characters
privatehs %>% filter(is.na(pl_zip)) %>% count() #19,143 are missing

pl <- privatehs %>% select(pl_zip,pl_stabb) %>% filter(nchar(pl_zip) == 4) 

privatehs$pl_zip4
nrow(privatehs %>% filter(nchar(pl_zip4) == 4)) #1,518 are 4 characters
nrow(privatehs %>% filter(nchar(pl_zip4) == 3)) #23 are 3 characters
privatehs %>% filter(is.na(pl_zip4)) %>% count() #21,346 are missing

pl4 <- privatehs %>% select(pl_zip4,pl_stabb) %>% filter(nchar(pl_zip4) == 3)

#Check that permanent identification number is a unique identifier
privatehs %>% group_by(ppin) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #permanent identification number is a unique identifier

#Check that permanent identification number is 8 characters
nrow(privatehs %>% filter(nchar(ppin) == 8)) #all are 8 characters long

#Checking data completeness (i.e. missing data)
for(i in names(privatehs)) {
  print(paste(i, sum(is.na(privatehs[[i]])))) 
}

#p140: 21,598 missing
#p150: 9,558 missing
#p160: 6,271 missing
#p170: 21,134 missing
#p180: 22,426 missing
#p190: 7,150 missing
#p200: 7,169 missing
#p210: 7,185 missing
#p220: 7,303 missing  
#p230: 7,371 missing  
#p240: 7,717 missing
#p250: 8,448 missing 
#p260: 8,510 missing 
#p270: 15,226 missing 
#p280: 15,472 missing
#p290: 15,764 missing
#p300: 15,846 missing
#p340: 1,039 missing                                                                                                
#p350: 15,941 missing  
#p360: 16,262 missing 
#p370: 6,074 missing 
#pzip4: 3,786 missing 
#pphone: 561 missing 
#pl_zip: 19,143 missing 
#pl_zip4: 21,346 missing  

#Renaming variables
privatehs <- privatehs %>% rename(total_09 = "p270",
                                  total_10 = "p280",
                                  total_11 = "p290",
                                  total_12 = "p300",
                                  total_teachers = "numteach",
                                  total_students = "numstuds",
                                  total_enrolled = "p305",
                                  total_hispanic = "p320",
                                  total_white = "p330",
                                  total_black = "p325",
                                  total_asian = "p316",
                                  total_nativehawaii = "p318",
                                  total_amerindian = "p310",
                                  total_tworaces = "p332",
                                  pct_amerindian = "p_indian",
                                  pct_asian = "p_asian",
                                  pct_nativehawaii = "p_pacific",
                                  pct_hispanic = "p_hisp",
                                  pct_white = "p_white",
                                  pct_black = "p_black",
                                  pct_tworaces = "p_tr",
                                  community_type = "ucommtyp",
                                  latitude = "latitude18",
                                  longitude = "longitude18",
                                  locale_code = "ulocale18",
                                  state_fips_code = "pstansi",
                                  name = "pinst",
                                  address = "paddrs",
                                  city = "pcity",
                                  state_code = "pstabb",
                                  zip_code = "pzip",
                                  county_fips_code = "pcnty",
                                  county_name = "pcntnm",
                                  address_2 = "pl_add",
                                  pct_to_4yr = "p360",
                                  total_males = "males",
                                  school_type = "p415",
                                  ncessch = "ppin")
names(privatehs)                                  
                                  
                                  
                                  
                                  
        
        
        
