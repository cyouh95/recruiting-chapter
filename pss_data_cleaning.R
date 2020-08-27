library(tidyverse)
#library(tidyr)
#library(readr)
#library(lubridate)
#library(stringr)
#library(kableExtra)
library(labelled)
library(haven) 
#library(stargazer)
library(Hmisc)
#library(sjlabelled)
library(assertthat)
#library(dplyr)



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
  #str(pss_dict_subset) # dataframe w/ 349 obs and 3 vars; 
    #vars = name (var name); label (variable label); names_lc (lower case variable name)

#Loop to label data with dictionary
for (i in names(privatehs)) { 
  #description4 <- (pss_dict_subset %>% filter(names_lc == i))$label 
  description4 <- (pss_dict_subset %>% filter(names_lc == i))$label %>% as.character()
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

class(privatehs)

# save data
  privatehs <- as_tibble(x = privatehs) # change from data frame to tibble

save(privatehs, file = "./data/pss_1718.RData")
rm(privatehs)

# load data
load(file = "./data/pss_1718.RData")