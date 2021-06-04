library(tidyverse)
library(labelled)


data_dir <- file.path('.', 'data')


# Function for labeling variables and values
label_df <- function(df, df_dict, df_vals = NULL) {

  # Turn variables to lowercase
  names(df) <- tolower(names(df))
  df_dict <- df_dict %>% mutate(varname_lc = tolower(varname))
  
  if (!is.null(df_vals)) {
    df_vals <- df_vals %>% mutate(varname_lc = tolower(varname))
    
    # Label values
    for (i in 1:nrow(df_vals)) {
      varname <- df_vals[[i, 'varname_lc']]
      
      if (class(df[[varname]]) != 'haven_labelled') {
        df[[varname]] <- as.character(df[[varname]])
      }
      
      val_label(df[[varname]], df_vals[[i, 'codevalue']]) <- df_vals[[i, 'valuelabel']]
    }
  }
  
  # Label variables
  for (i in names(df)) { 
    description <- (df_dict %>% filter(varname_lc == i))$varTitle
    var_label(df[[i]]) <- description
  }

  return(df)
}


# Read in HD data and dictionary (7153 obs. of 72 variables)
hd <- read.csv(file.path(data_dir, 'ipeds_hd2017.csv'), header = TRUE, na.strings = c('', ' ', 'NA'), stringsAsFactors = F,  colClasses = c('UNITID' = 'character', 'OPEID' = 'character', 'FIPS' = 'character'))
hd_dictionary <- read.csv(file.path(data_dir, 'ipeds_hd2017_dictionary.csv'), header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)
hd_values <- read.csv(file.path(data_dir, 'ipeds_hd2017_values.csv'), header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)

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
ic <- read.csv(file.path(data_dir, 'ipeds_ic2017_rv.csv'), header = TRUE, na.strings = c('', ' ', 'NA'), stringsAsFactors = F,  colClasses = c('UNITID' = 'character'))
ic_dictionary <- read.csv(file.path(data_dir, 'ipeds_ic2017_dictionary.csv'), header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)
ic_values <- read.csv(file.path(data_dir, 'ipeds_ic2017_values.csv'), header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F,  colClasses = c('codevalue' = 'character'))

ic <- label_df(ic, ic_dictionary, ic_values)

# Inspect relaffil (-2 is nonsectarian)
View(ic %>% distinct(cntlaffi, relaffil) %>% arrange(relaffil))
val_labels(ic$relaffil)

# Add religion variable
v_get_val <- Vectorize(function(x) val_label(ic$relaffil, x))
ic <- ic %>% mutate(
  relaffil_text = v_get_val(relaffil),
  religion_4 = case_when(
    relaffil == '-2' ~ 'nonsectarian',
    relaffil == '30' ~ 'catholic',
    # Baptist
    relaffil == '54' ~ 'conservative_christian',
    TRUE ~ 'other_religion'
  ),
  religion = case_when(
    relaffil == '-2' ~ 'nonsectarian',
    relaffil == '30' ~ 'catholic',
    # Jewish, Muslim, Undenominational, Unitarian Universalist, Other (none of the above)
    relaffil %in% c('80', '106', '88', '93', '99') ~ 'other',
    TRUE ~ 'christian'
  )
)


# Read in EF data and dictionary (127770 obs. of 65 variables)
ef <- read.csv(file.path(data_dir, 'ipeds_ef2017a_rv.csv'), header = TRUE, na.strings = c('', ' ', 'NA'), stringsAsFactors = F,  colClasses = c('UNITID' = 'character'))
ef_dictionary <- read.csv(file.path(data_dir, 'ipeds_ef2017a_dictionary.csv'), header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)
ef_values <- read.csv(file.path(data_dir, 'ipeds_ef2017a_values.csv'), header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F,  colClasses = c('codevalue' = 'character'))

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
  pct_white = efwhitt / (efwhitt + efbkaat + efhispt + efasiat + efaiant + efnhpit + ef2mort + efnralt) * 100,
  pct_black = efbkaat / (efwhitt + efbkaat + efhispt + efasiat + efaiant + efnhpit + ef2mort + efnralt) * 100,
  pct_hispanic = efhispt / (efwhitt + efbkaat + efhispt + efasiat + efaiant + efnhpit + ef2mort + efnralt) * 100,
  pct_asian = efasiat / (efwhitt + efbkaat + efhispt + efasiat + efaiant + efnhpit + ef2mort + efnralt) * 100,
  pct_amerindian = efaiant / (efwhitt + efbkaat + efhispt + efasiat + efaiant + efnhpit + ef2mort + efnralt) * 100,
  pct_nativehawaii = efnhpit / (efwhitt + efbkaat + efhispt + efasiat + efaiant + efnhpit + ef2mort + efnralt) * 100,
  pct_tworaces = ef2mort / (efwhitt + efbkaat + efhispt + efasiat + efaiant + efnhpit + ef2mort + efnralt) * 100,
  pct_non_res_alien = efnralt / (efwhitt + efbkaat + efhispt + efasiat + efaiant + efnhpit + ef2mort + efnralt) * 100
)


# Read in ADM data and dictionary (2075 obs. of 68 variables)
adm <- read.csv(file.path(data_dir, 'ipeds_adm2017_rv.csv'), header = TRUE, na.strings = c('', ' ', 'NA'), stringsAsFactors = F,  colClasses = c('UNITID' = 'character'))
adm_dictionary <- read.csv(file.path(data_dir, 'ipeds_adm2017_dictionary.csv'), header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)
adm_values <- read.csv(file.path(data_dir, 'ipeds_adm2017_values.csv'), header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F,  colClasses = c('codevalue' = 'character'))

adm <- label_df(adm, adm_dictionary, adm_values)

# Filter variables (39 remaining variables)
adm <- adm %>% select(-starts_with('x'))

# Add SAT composite variables
adm <- adm %>% mutate(
  satcm25 = satvr25 + satmt25,
  satcm75 = satvr75 + satmt75
)

var_label(adm$satcm25) <- 'SAT Composite 25th percentile score'
var_label(adm$satcm75) <- 'SAT Composite 75th percentile score'


# Read in IC_AY data and dictionary (4281 obs. of 235 variables)
ic_ay <- read.csv(file.path(data_dir, 'ipeds_ic2017_ay.csv'), header = TRUE, na.strings = c('', ' ', 'NA', '.'), stringsAsFactors = F,  colClasses = c('UNITID' = 'character'))
ic_ay_dictionary <- read.csv(file.path(data_dir, 'ipeds_ic2017_ay_dictionary.csv'), header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)

ic_ay <- label_df(ic_ay, ic_ay_dictionary)

# Filter variables (121 remaining variables)
ic_ay <- ic_ay %>% select(-starts_with('x'))

# Add tuition + fee variables
ic_ay <- ic_ay %>% mutate(
  tuit_fees_instate = tuition2 + fee2,
  tuit_fees_outstate = tuition3 + fee3
)

var_label(ic_ay$tuit_fees_instate) <- 'In-state average tuition + required fees for full-time undergraduates'
var_label(ic_ay$tuit_fees_outstate) <- 'Out-of-state average tuition + required fees for full-time undergraduates'


# Join tables
ipeds <- left_join(hd, ic, by = 'unitid') %>% 
  left_join(ef, by = 'unitid') %>% 
  left_join(adm, by = 'unitid') %>% 
  left_join(ic_ay, by = 'unitid')

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
saveRDS(ipeds, file = file.path(data_dir, 'ipeds_1718.RDS'))
