library(tidyverse)
library(labelled)


# Function for labeling variables and values
label_df <- function(df, df_dict, df_vals) {

  # Turn variables to lowercase
  names(df) <- tolower(names(df))
  df_dict <- df_dict %>% mutate(varname_lc = tolower(varname))
  df_vals <- df_vals %>% mutate(varname_lc = tolower(varname))
  
  # Label values
  for (i in 1:nrow(df_vals)) {
    varname <- df_vals[[i, 'varname_lc']]
    
    if (class(df[[varname]]) != 'haven_labelled') {
      df[[varname]] <- as.character(df[[varname]])
    }
    
    val_label(df[[varname]], df_vals[[i, 'codevalue']]) <- df_vals[[i, 'valuelabel']]
  }
  
  # Label variables
  for (i in names(df)) { 
    description <- (df_dict %>% filter(varname_lc == i))$varTitle
    var_label(df[[i]]) <- description
  }

  return(df)
}


# Read in HD data and dictionary (7153 obs. of 72 variables)
hd <- read.csv('./data/ipeds_hd2017.csv', header = TRUE, na.strings = c('', ' ', 'NA'), stringsAsFactors = F,  colClasses = c('UNITID' = 'character', 'OPEID' = 'character', 'FIPS' = 'character'))
hd_dictionary <- read.csv('./data/ipeds_hd2017_dictionary.csv', header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)
hd_values <- read.csv('./data/ipeds_hd2017_values.csv', header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)

hd <- label_df(hd, hd_dictionary, hd_values)

# Take first 5 digits of zip code
hd <- hd %>% mutate(zip = substr(zip, 1, 5))

# Add region: https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
hd <- hd %>% mutate(region = case_when(
  stabbr %in% c('CT', 'ME', 'MA', 'NH', 'RI', 'VT', 'NJ', 'NY', 'PA') ~ 1L,  # Northeast
  stabbr %in% c('IN', 'IL', 'MI', 'OH', 'WI', 'IA', 'KS', 'MN', 'MO', 'NE', 'ND', 'SD') ~ 2L,  # Midwest
  stabbr %in% c('AZ', 'CO', 'ID', 'NM', 'MT', 'UT', 'NV', 'WY', 'AK', 'CA', 'HI', 'OR', 'WA') ~ 4L,  # West
  TRUE ~ 3L  # South
))

var_label(hd$region) <- 'Census Region'
val_label(hd$region, 1) <- 'Northeast'
val_label(hd$region, 2) <- 'Midwest'
val_label(hd$region, 3) <- 'South'
val_label(hd$region, 4) <- 'West'


# Read in IC data and dictionary (6882 obs. of 121 variables)
ic <- read.csv('./data/ipeds_ic2017_rv.csv', header = TRUE, na.strings = c('', ' ', 'NA'), stringsAsFactors = F,  colClasses = c('UNITID' = 'character'))
ic_dictionary <- read.csv('./data/ipeds_ic2017_dictionary.csv', header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)
ic_values <- read.csv('./data/ipeds_ic2017_values.csv', header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F,  colClasses = c('codevalue' = 'character'))

ic <- label_df(ic, ic_dictionary, ic_values)

# Add religion variable
get_val <- function(x) {
  val_label(ic$relaffil, x) %>% tolower() %>% str_replace_all(' ', '_')
}
v_get_val <- Vectorize(get_val)

ic <- ic %>% mutate(religion = case_when(
  cntlaffi != '4' ~ 'nonsectarian',
  TRUE ~ v_get_val(relaffil)
))


# Read in EF data and dictionary (127770 obs. of 65 variables)
ef <- read.csv('./data/ipeds_ef2017a_rv.csv', header = TRUE, na.strings = c('', ' ', 'NA'), stringsAsFactors = F,  colClasses = c('UNITID' = 'character'))
ef_dictionary <- read.csv('./data/ipeds_ef2017a_dictionary.csv', header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)
ef_values <- read.csv('./data/ipeds_ef2017a_values.csv', header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F,  colClasses = c('codevalue' = 'character'))

ef <- label_df(ef, ef_dictionary, ef_values)

# Filter variables (35 remaining variables)
ef <- ef %>% select(-starts_with('xef'))

# Filter for "Full-time, first-time, first-year, degree-seeking undergraduates" (6009 obs.)
ef <- ef %>% filter(line == '1')

# Add race percentage variables
ef <- ef %>% mutate(
  pct_freshman_white = efwhitt / eftotlt * 100,
  pct_freshman_black = efbkaat / eftotlt * 100,
  pct_freshman_hispanic = efhispt / eftotlt * 100,
  pct_freshman_asian = efasiat / eftotlt * 100,
  pct_freshman_amerindian = efaiant / eftotlt * 100,
  pct_freshman_nativehawaii = efnhpit / eftotlt * 100,
  pct_freshman_tworaces = ef2mort / eftotlt * 100,
  pct_freshman_unknownrace = efunknt / eftotlt * 100,
  pct_freshman_non_res_alien = efnralt / eftotlt * 100,
  pct_white = efwhitt / (efwhitt + efbkaat + efhispt + efasiat + efaiant + efnhpit + ef2mort) * 100,
  pct_black = efbkaat / (efwhitt + efbkaat + efhispt + efasiat + efaiant + efnhpit + ef2mort) * 100,
  pct_hispanic = efhispt / (efwhitt + efbkaat + efhispt + efasiat + efaiant + efnhpit + ef2mort) * 100,
  pct_asian = efasiat / (efwhitt + efbkaat + efhispt + efasiat + efaiant + efnhpit + ef2mort) * 100,
  pct_amerindian = efaiant / (efwhitt + efbkaat + efhispt + efasiat + efaiant + efnhpit + ef2mort) * 100,
  pct_nativehawaii = efnhpit / (efwhitt + efbkaat + efhispt + efasiat + efaiant + efnhpit + ef2mort) * 100,
  pct_tworaces = ef2mort / (efwhitt + efbkaat + efhispt + efasiat + efaiant + efnhpit + ef2mort) * 100
)


# Join tables
ipeds <- left_join(hd, ic, by = 'unitid')
ipeds <- left_join(ipeds, ef, by = 'unitid')

# Rename variables
ipeds <- ipeds %>% rename(univ_id = 'unitid',
                          univ_name = 'instnm',
                          state_code = 'stabbr',
                          fips_state_code = 'fips',
                          longitude = 'longitud',
                          zip_code = 'zip')

# Check variables
val_labels(ipeds$region)
val_labels(ipeds$control)
val_labels(ipeds$cntlaffi)
val_labels(ipeds$relaffil)
all(nchar(ipeds$unitid) == 6)
all(nchar(ipeds$state_code) == 2)
all(nchar(ipeds$zip_code) == 5)

# Export data
ipeds <- as_tibble(x = ipeds)
saveRDS(ipeds, file = './data/ipeds_1718.RDS')
