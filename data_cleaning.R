#Following Data Cleaning Guidelines

install.packages("tidyverse")
install.packages("tidyr")
install.packages("readr")
install.packages("lubridate")
install.packages("stringr")
install.packages("kableExtra")
install.packages("labelled")
install.packages("haven")
install.packages("stargazer")
install.packages("Hmisc")
install.packages("sjlabelled")
install.packages("assertthat")


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

##Loading & investigating data: CCD School Directory Data for 2017-2018
#Check working directory
getwd()
setwd("/Users/irmacastaneda/Documents/UCLA/GSR/recruiting-chapter")

#Loading Data; Starting with public high school data
hs_directory <- read.csv('./data/ccd_school_directory_1718.csv', header = TRUE, na.strings=c("","NA"), colClasses = c('SCHOOL_YEAR' = "character", 'FIPST' = 'character', 'STATENAME' = 'character', 'ST' = 'character', 'SCH_NAME' = 'character', 'LEA_NAME' = 'character', 'STATE_AGENCY_NO' = 'character', 'UNION' = 'character', 'ST_LEAID' = 'character', 'LEAID' = 'character', 'ST_SCHID' = 'character', 'NCESSCH' = 'character', 'SCHID' = 'character', 
                                                                                                                      'MSTREET1' = 'character', 'MSTREET2' = 'character', 'MSTREET3' = 'character', 'MCITY' = 'character', 'MSTATE' = 'character', 'MZIP' = 'integer', 'MZIP4' = 'integer', 'LSTREET1' = 'character', 'LSTREET2' = 'character', 'LSTREET3'= 'character', 'LCITY' = 'character', 'LSTATE' = 'character', 'LZIP' = 'integer', 'PHONE' = 'character',
                                                                                                                      'WEBSITE' = 'character','SY_STATUS' = 'character', 'SY_STATUS_TEXT' = 'character', 'UPDATED_STATUS' = 'character', 'UPDATED_STATUS_TEXT' = 'character', 'EFFECTIVE_DATE' = 'character', 'SCH_TYPE' = 'character', 'SCH_TYPE_TEXT' = 'character', 'RECON_STATUS' = 'character', 'OUT_OF_STATE_FLAG' = 'character', 'CHARTER_TEXT' = 'character', 
                                                                                                                      'CHARTAUTH1' = 'character', 'CHARTAUTHN1' = 'character', 'CHARTAUTH2' = 'character', 'CHARTAUTHN2' = 'character', 'G_9_OFFERED' = 'character', 'G_10_OFFERED' = 'character', 'G_11_OFFERED' = 'character', 'G_12_OFFERED' = 'character', 'G_13_OFFERED'= 'character', 'LEVEL' = 'character', 'GSLO' = 'character', 'GSHI' = 'character'))

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
hs_dictionary <- read.csv('./data/2017-18_School_Directory_Dictionary.csv', header = TRUE, na.strings=c("","NA")) #na.strings converts empty strings to NA value    
hs_dictionary_subset <- hs_dictionary %>% select(Variable.Name, Description)

#Lowercase variable names in dictionary
names(hs_dictionary_subset) <- tolower(names(hs_dictionary_subset))
hs_dictionary_subset <- hs_dictionary_subset %>% mutate(names_lc = tolower(variable.name))

#Loop to label data with dictionary
for (i in names(hs_directory)) { #loop executed 65 times, first iteration of loop = school_year -> last variable igoffered
  description <- (hs_dictionary_subset %>% filter(names_lc == i))$description 
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
hs_directory$st
nrow(hs_directory %>% filter(nchar(st) != 2))

#Check state school identifier
hs_directory$st_schid
nrow(hs_directory %>% filter(nchar(st_schid) != 11)) #92,198
st_schid_long <- hs_directory %>% select(st_schid) %>% filter(nchar(st_schid) >= 38) #1253, OR has really long identifiers

#Check that state school identifier is a unique identifier
hs_directory %>% group_by(st_schid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #st_schid is a unique identifier

#Check that NCES School ID has 12 digits
hs_directory$ncessch
nrow(hs_directory %>% filter(nchar(ncessch) != 12)) #all ncessch variables are 12 characters long
see_if(are_equal(nrow(hs_directory %>% filter(nchar(ncessch) != 12)),0)) #true
see_if(assert_that(nrow(hs_directory %>% filter(nchar(ncessch) != 12)) == 1)) #false

#Check that NCES is unique identifier
hs_directory %>% group_by(ncessch) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #Yes, unique identifier, 102,337 observations

#Check that unique school ID has 7 digits
hs_directory$schid
nrow(hs_directory %>% filter(nchar(schid) != 7)) 

#Check that unique school ID is unique identifier
hs_directory %>% group_by(schid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #Yes, unique identifier, 102,337 observations

#Check mailing state & zipcode
hs_directory$mstate
nrow(hs_directory %>% filter(nchar(mstate) != 2))

hs_directory$mzip
nrow(hs_directory %>% filter(nchar(mzip) != 5)) #8787 don't have 5 digits

#Check location state & zipcode
hs_directory$lstate
nrow(hs_directory %>% filter(nchar(lstate) != 2))

hs_directory$lzip
nrow(hs_directory %>% filter(nchar(lzip) != 5)) #8797 don't have 5 digits

#Checking data completeness (i.e. missing data)
names(hs_directory)
hs_directory %>% filter(is.na(school_year)) %>% count()
hs_directory %>% filter(is.na(fipst)) %>% count()
hs_directory %>% filter(is.na(statename)) %>% count()
hs_directory %>% filter(is.na(st)) %>% count()
hs_directory %>% filter(is.na(sch_name)) %>% count()
hs_directory %>% filter(is.na(lea_name)) %>% count()
hs_directory %>% filter(is.na(state_agency_no)) %>% count()
hs_directory %>% filter(is.na(union)) %>% count() #99,857 missing
hs_directory %>% filter(is.na(st_leaid)) %>% count()
hs_directory %>% filter(is.na(leaid)) %>% count()
hs_directory %>% filter(is.na(st_schid)) %>% count() 
hs_directory %>% filter(is.na(ncessch)) %>% count()
hs_directory %>% filter(is.na(schid)) %>% count()
hs_directory %>% filter(is.na(mstreet1)) %>% count()
hs_directory %>% filter(is.na(mstreet2)) %>% count() #100,551 missing
hs_directory %>% filter(is.na(mstreet3)) %>% count() #102,309 missing
hs_directory %>% filter(is.na(mcity)) %>% count()
hs_directory %>% filter(is.na(mstate)) %>% count()
hs_directory %>% filter(is.na(mzip)) %>% count()
hs_directory %>% filter(is.na(mzip4)) %>% count() #40,487
hs_directory %>% filter(is.na(lstreet1)) %>% count()
hs_directory %>% filter(is.na(lstreet2)) %>% count() #101,724 missing
hs_directory %>% filter(is.na(lstreet3)) %>% count() #102,326 missing
hs_directory %>% filter(is.na(lcity)) %>% count() 
hs_directory %>% filter(is.na(lstate)) %>% count() 
hs_directory %>% filter(is.na(lzip)) %>% count()
hs_directory %>% filter(is.na(lzip4)) %>% count() #42,414 missing
hs_directory %>% filter(is.na(phone)) %>% count()
hs_directory %>% filter(is.na(website)) %>% count() #45,311 missing
hs_directory %>% filter(is.na(sy_status)) %>% count() 
hs_directory %>% filter(is.na(sy_status_text)) %>% count()
hs_directory %>% filter(is.na(updated_status)) %>% count()
hs_directory %>% filter(is.na(updated_status_text)) %>% count()
hs_directory %>% filter(is.na(effective_date)) %>% count() 
hs_directory %>% filter(is.na(sch_type_text)) %>% count()
hs_directory %>% filter(is.na(sch_type)) %>% count() 
hs_directory %>% filter(is.na(recon_status)) %>% count()
hs_directory %>% filter(is.na(out_of_state_flag)) %>% count()
hs_directory %>% filter(is.na(charter_text)) %>% count()
hs_directory %>% filter(is.na(chartauth1)) %>% count() #95,584 missing
hs_directory %>% filter(is.na(chartauthn1)) %>% count() #95,584 missing
hs_directory %>% filter(is.na(chartauth2)) %>% count() #102,212 missing
hs_directory %>% filter(is.na(chartauthn2)) %>% count() #102,212 missing
hs_directory %>% filter(is.na(nogrades)) %>% count() 
hs_directory %>% filter(is.na(g_pk_offered)) %>% count() 
hs_directory %>% filter(is.na(g_kg_offered)) %>% count() 
hs_directory %>% filter(is.na(g_1_offered)) %>% count() 
hs_directory %>% filter(is.na(g_2_offered)) %>% count() 
hs_directory %>% filter(is.na(g_3_offered)) %>% count()
hs_directory %>% filter(is.na(g_4_offered)) %>% count() 
hs_directory %>% filter(is.na(g_5_offered)) %>% count() 
hs_directory %>% filter(is.na(g_6_offered)) %>% count() 
hs_directory %>% filter(is.na(g_7_offered)) %>% count() 
hs_directory %>% filter(is.na(g_8_offered)) %>% count() 
hs_directory %>% filter(is.na(g_9_offered)) %>% count() 
hs_directory %>% filter(is.na(g_10_offered)) %>% count() 
hs_directory %>% filter(is.na(g_11_offered)) %>% count() 
hs_directory %>% filter(is.na(g_12_offered)) %>% count()
hs_directory %>% filter(is.na(g_13_offered)) %>% count()
hs_directory %>% filter(is.na(gslo)) %>% count()
hs_directory %>% filter(is.na(gshi)) %>% count()
hs_directory %>% filter(is.na(level)) %>% count()

##Loading & investigating data: CCD School Companion Data for 2017-2018
hs_companion <- read.csv('./data/ccd_school_companion_1718.csv', header = TRUE, na.strings=c("","NA"), colClasses = c('SCHOOL_YEAR' = 'character', 'FIPST' = 'character', 'STATENAME' = 'character', 'ST' = 'character', 'SCH_NAME' = 'character', 'STATE_AGENCY_NO' = 'character', 'UNION' = 'character', 'ST_LEAID' = 'character', 'LEAID' = 'character', 'ST_SCHID' = 'character', 'NCESSCH' = 'character', 'SCHID' = 'character',
                                                                                                                      'SHARED_TIME' = 'character', 'TITLEI_STATUS' = 'character', 'TITLEI_STATUS_TEXT' = 'character', 'MAGNET_TEXT' = 'character', 'NSLP_STATUS' = 'character', 'NSLP_STATUS_TEXT' = 'character', 'VIRTUAL' = 'character', 'VIRTUAL_TEXT' = 'character'))
str(hs_companion) # structure of the data frame

nrow(hs_companion) #num of rows (obs): 99,899 observations
ncol(hs_companion) #num of columns: 20 variables

names(hs_companion) # variable names 
names(hs_companion) <- tolower(names(hs_companion)) #convert to lowercase
glimpse(hs_companion)
head(hs_companion) # prints first 6 obs

hs_companion %>% arrange(ncessch, sch_name, school_year, titlei_status, nslp_status) %>% head()

names(hs_companion) <- tolower(names(hs_companion))

##Labeling variables 
#Importing data dictionary
hs_companion_dict <- read.csv('./data/2017-18_ccd_school_companion_dictionary.csv', header = TRUE, na.strings=c("","NA")) #na.strings converts empty strings to NA value    
hs_companion_dict_subset <- hs_companion_dict %>% select(Variable.Name, Description)

#Lowercase variable names in dictionary
names(hs_companion_dict_subset) <- tolower(names(hs_companion_dict_subset))
hs_companion_dict_subset <- hs_companion_dict_subset %>% mutate(names_lc = tolower(variable.name))

#Loop to label data with dictionary
for (i in names(hs_companion)) {
  description2 <- (hs_companion_dict_subset %>% filter(names_lc == i))$description 
  var_label(hs_companion[[i]]) <- description2 
}

#Check variable labels
hs_companion %>% var_label() # variable labels
  
#Adding value labels
hs_companion %>% select(shared_time, titlei_status, titlei_status_text, magnet_text, nslp_status, nslp_status_text, virtual, virtual_text) %>% glimpse() #already text & labeled

##Checking data validity

#Check that school year is 2017-2018
hs_companion$school_year
hs_companion %>% filter((school_year == "2017-2018")) %>% count() #99,899 observations

#Check state code: digits & foreign key constraints
hs_companion$fipst
nrow(hs_companion %>% filter(nchar(fipst) != 2)) #18,655 (1 character)
nrow(hs_companion %>% filter(nchar(fipst) != 1)) #81,244 (2 characters)

#Check postal state code
hs_companion$st
nrow(hs_companion %>% filter(nchar(st) != 2))

#Check union
hs_companion$union
nrow(hs_companion %>% filter(nchar(union) >= 3)) #1,598

#Check state local education number
hs_companion$st_leaid
nrow(hs_companion %>% filter(nchar(st_leaid) != 6)) #85,970
nrow(hs_companion %>% filter(nchar(st_leaid) != 17))
st_leaid_long <- hs_companion %>% select(st_leaid) %>% filter(nchar(st_leaid) >= 17) #5,490, IL has really long identifiers

#Check that state local education number is a unique identifier
hs_companion %>% group_by(st_leaid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #st_leaid is not a unique identifier

#Check that NCES agency identification number
hs_companion$leaid
nrow(hs_companion %>% filter(nchar(leaid) != 6)) #81,244
nrow(hs_companion %>% filter(nchar(leaid) >= 7)) #81,244


#Check that NCES agency is unique identifier
hs_companion %>% group_by(leaid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #leaid is not a unique identifier

#Check state school identifier
hs_companion$st_schid
nrow(hs_companion %>% filter(nchar(st_schid) != 11)) #90,072
nrow(hs_companion %>% filter(nchar(st_schid) >= 38)) #1,249
st_schid_long <- hs_companion %>% select(st_schid) %>% filter(nchar(st_schid) >= 38) #1,249, OR has really long identifiers

#Check that state school identifier is a unique identifier
hs_companion %>% group_by(st_schid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #st_schid is a unique identifier

#Check that school identifier (NCES) ID has 12 digits
hs_companion$ncessch
nrow(hs_companion %>% filter(nchar(ncessch) != 12)) #99,899
nrow(hs_companion %>% filter(nchar(ncessch) == 11)) #91,253 are 11 characters long

#Check that school identifier (NCES) ID is unique identifier
hs_companion %>% group_by(ncessch) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #ncessch is not a unique identifier

#Check that unique school ID has 7 digits
hs_companion$schid
nrow(hs_companion %>% filter(nchar(schid) != 7)) #18,655
nrow(hs_companion %>% filter(nchar(schid) == 6)) #18,655

#Check that unique school ID is unique identifier
hs_companion %>% group_by(schid) %>% 
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group) #schid is a unique identifier

##start here!

#check for missing variables
names(hs_companion)
hs_companion %>% filter(is.na(school_year)) %>% count()
hs_companion %>% filter(is.na(fipst)) %>% count()
hs_companion %>% filter(is.na(statename)) %>% count()
hs_companion %>% filter(is.na(st)) %>% count()
hs_companion %>% filter(is.na(sch_name)) %>% count()
hs_companion %>% filter(is.na(state_agency_no)) %>% count()
hs_companion %>% filter(is.na(union)) %>% count() #97,438
hs_companion %>% filter(is.na(st_leaid)) %>% count() 
hs_companion %>% filter(is.na(leaid)) %>% count()
hs_companion %>% filter(is.na(st_schid)) %>% count()
hs_companion %>% filter(is.na(ncessch)) %>% count()
hs_companion %>% filter(is.na(schid)) %>% count()
hs_companion %>% filter(is.na(shared_time)) %>% count()
hs_companion %>% filter(is.na(titlei_status)) %>% count()
hs_companion %>% filter(is.na(titlei_status_text)) %>% count()
hs_companion %>% filter(is.na(magnet_text)) %>% count()
hs_companion %>% filter(is.na(nslp_status)) %>% count()
hs_companion %>% filter(is.na(nslp_status_text)) %>% count()
hs_companion %>% filter(is.na(virtual)) %>% count()
hs_companion %>% filter(is.na(virtual_text)) %>% count()











