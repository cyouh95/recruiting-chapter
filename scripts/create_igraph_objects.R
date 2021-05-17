
library(igraph)
library(tidyverse)
library(labelled)

#rm(list = ls())

## ----------
## LOAD DATA
## ----------

# Recruiting events data from 43 univs (17 public research, 13 private national, 13 private liberal arts)
#events_data <- read.csv('./data/events_data_2020-07-27.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character', 'event_type' = 'character')) %>% as_tibble()
events_data_temp1 <- read.csv('./data/events_data_2020-10-20.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character', 'event_type' = 'character')) %>% as_tibble()
events_data_marquette <- read.csv('./data/events_data_marquette.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character', 'event_type' = 'character')) %>% as_tibble()
events_data <- events_data_temp1 %>% bind_rows(events_data_marquette)
rm(events_data_marquette,events_data_temp1)

# University data from IPEDS
univ_data <- readRDS('./data/ipeds_1718.RDS')
univ_info <- read.csv('./data/univ_data.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character', 'zip_code' = 'character')) %>% as_tibble() %>% 
  filter(!(univ_id %in% c('168218','199193','110653')))


# Private HS data from PSS
privhs_data_1718 <- readRDS('./data/pss_1718.RDS')
privhs_data_1516 <- readRDS('./data/pss_1516.RDS')
privhs_data_1314 <- readRDS('./data/pss_1314.RDS')
privhs_overrides <- read.csv('./data/pss_updated_id.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE) %>% as_tibble()

# Rankings data from Niche
niche_data <- read.csv('./data/niche_private.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE) %>% as_tibble()
niche_overrides <- read.csv('./data/niche_updated_id.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE) %>% as_tibble()

# Rankings data from US News & World Report
usnews_data <- read.csv('./data/usnews_rankings.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character')) %>% as_tibble()


## ----------
## PREP DATA
## ----------

# Focus on private HS visits by the univs
  # universities to exclude:
    # wellesley college; data seem suspect
    # NC State; univ_id ==199193
        #rationale: data seem suspect
    # UC Irvine: univ_id == 110653
      # rationale: we have four UCs: berkeley; san diego; irvine; riverside; don't want so many; Irvine and San Diego have same ranking but San Diego has more out-of-state visits and more private hs visits so keep that one; and keep riverside because it has very different rank
  # (exclude Wellesley and mismatched HS)
privhs_events <- events_data %>%
  filter(event_type == 'priv_hs', !(univ_id %in% c('168218','199193','110653')), pid != 55696) %>%
  select(univ_id, univ_state, event_type, school_id, event_location_name, event_city, event_state)

  
# Override school_id with updated 2017-18 PSS ID
get_pss_override <- function(x) {
  new_id <- privhs_overrides[privhs_overrides$old_id == x, ]$new_id
  ifelse(length(new_id) == 0, x, new_id)
}
v_get_pss_override <- Vectorize(get_pss_override, USE.NAMES = FALSE)

privhs_events <- privhs_events %>% mutate(school_id = v_get_pss_override(school_id))

privhs_events %>% glimpse()

# Combine 2017-18 PSS data w/ past years as needed
pss_missing_ncessch <- setdiff(privhs_events$school_id, privhs_data_1718$ncessch)
pss_1516_ncessch <- privhs_data_1516[privhs_data_1516$ncessch %in% pss_missing_ncessch, ]$ncessch
pss_1314_ncessch <- setdiff(pss_missing_ncessch, pss_1516_ncessch)

# criteria for private hs to be included in our analysis is 12th grade enrollment of 10 or more
# CRYSTAL MAY/JUNE 2021 - PLEASE CHECK THAT WE HAVE NON-MISSING VALUES OF THE VARIABLE total_12 
privhs_data <- privhs_data_1718 %>%
  dplyr::union(privhs_data_1516 %>% filter(ncessch %in% pss_1516_ncessch)) %>%
  dplyr::union(privhs_data_1314 %>% filter(ncessch %in% pss_1314_ncessch)) %>% filter(total_12>=10)



# merge privatehs_events to privhs_data; remove private high schools that have less than 10 12th graders; override object privhs_events

privhs_events <- privhs_events %>% inner_join(y=select(privhs_data,ncessch), by = c('school_id'='ncessch')) 


privhs_data %>% group_by(year) %>% count()  # 105 from 2015-16, 144 from 2013-14

# Select variables of interest from private HS data
privhs_df <- privhs_data %>%
  mutate(type = 'priv hs', control = 'private') %>%
  select(ncessch, name, city, state_code, region, religion, pct_white, pct_black, pct_hispanic, pct_asian, pct_amerindian, pct_nativehawaii, pct_tworaces, type, control)
val_labels(privhs_df$region)  # https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf

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

privhs_df <- privhs_df %>% left_join(dplyr::union(
    niche_df %>% select(ncessch, overall_niche_letter_grade, rank_within_category),
    niche_overrides %>% left_join(niche_df %>% select(guid, overall_niche_letter_grade, rank_within_category), by = 'guid') %>% select(-guid)
  ), by = 'ncessch'
)

# Export universe of private HS

privhs_universe <- privhs_data %>% left_join(dplyr::union(
    niche_df %>% select(ncessch, overall_niche_letter_grade, rank_within_category),
    niche_overrides %>% left_join(niche_df %>% select(guid, overall_niche_letter_grade, rank_within_category), by = 'guid') %>% select(-guid)
  ), by = 'ncessch'
)

saveRDS(privhs_universe, file = str_c('./data/privhs_universe.RDS'))  # all 2017-18 + used schools from past years

# Select variables of interest from univ data
get_abbrev <- function(x, y) {
  univ_abbrev <- univ_info[univ_info$univ_id == x, ]$univ_abbrev
  ifelse(length(univ_abbrev) == 0, y, univ_abbrev)
}
v_get_abbrev <- Vectorize(get_abbrev, USE.NAMES = FALSE)

univ_df <- univ_data %>% mutate(univ_abbrev = v_get_abbrev(univ_id, univ_name)) %>%
  select(univ_id, univ_abbrev, city, state_code, region, religion, pct_white, pct_black, pct_hispanic, pct_asian, pct_amerindian, pct_nativehawaii, pct_tworaces)

univ_df$state_code <- as.character(univ_df$state_code)

# Add ranking from US News & World Report data
usnews_df <- usnews_data %>%
  mutate(type = recode(source,
                       'national-liberal-arts-colleges' = 'lib arts',
                       'national-universities' = 'univ'
                       )) %>%
  select(univ_id, type, control, score_text, rank)
univ_df <- univ_df %>% left_join(usnews_df, by = 'univ_id')

# Create attributes dataframe
var_names <- c('school_id', 'school_name', 'city', 'state_code', 'region', 'religion', 'pct_white', 'pct_black', 'pct_hispanic', 'pct_asian', 'pct_amerindian', 'pct_nativehawaii', 'pct_tworaces', 'school_type', 'control', 'ranking', 'ranking_numeric')
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

  
  
# Check religion variable
attributes_df %>% filter(school_id %in% psi_vec) %>% select(religion) %>% table(useNA = 'ifany')
attributes_df %>% filter(school_id %in% privhs_vec) %>% select(religion) %>% table(useNA = 'ifany')

  #View(univ_data %>% filter(univ_id %in% univ_info$univ_id) %>% select(univ_id, univ_name, relaffil, relaffil_text, religion_4, religion))

# TODO: Check unmerged Niche data
attributes_df %>% filter(school_id %in% privhs_vec) %>% select(ranking) %>% table(useNA = 'always')
  #View(attributes_df %>% filter(school_id %in% privhs_vec, is.na(ranking)))

length(privhs_vec[!(privhs_vec %in% attributes_df$school_id)])


## ----------------------------
## CREATE ADDITIONAL VARIABLES
## ----------------------------

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
v_get_val <- Vectorize(function(x) str_to_lower(val_label(attributes_df$region, x)))
attributes_df <- attributes_df %>% mutate(
  region = v_get_val(region)
)

# Preview vertex attributes that will be merged
  #View(attributes_df %>% filter(school_id %in% psi_vec))
  #View(attributes_df %>% filter(school_id %in% privhs_vec))

# Export universe of private HS
saveRDS(attributes_df %>% filter(school_id %in% privhs_vec), file = str_c('./data/privhs_visited.RDS'))


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

v_get_loc <- Vectorize(function(x, y) ifelse(privhs_events[privhs_events$school_id == x, ]$event_state[1] == univ_info[univ_info$univ_id == y, ]$state_code, 'instate', 'outofstate'), USE.NAMES = FALSE)
v_get_states <- Vectorize(function(x, y) paste0(privhs_events[privhs_events$school_id == x, ]$event_state[1], '|', univ_info[univ_info$univ_id == y, ]$state_code), USE.NAMES = FALSE)
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
nrow(v_2mode %>% filter(is.na(ranking), !(name %in% niche_data$ncessch)))  # unmerged Niche


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
egos_hs[[1]] %>% class()
E(egos_hs[[1]])$order
privhs_vec

egos_psi_priv %>% length()
egos_psi_priv[[1]]
egos_psi_priv[1]


