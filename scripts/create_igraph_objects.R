library(igraph)
library(tidyverse)
library(labelled)


## ----------
## LOAD DATA
## ----------

# Recruiting events data from 43 univs (17 public research, 13 private national, 13 private liberal arts)
events_data <- read.csv('./data/events_data_2020-07-27.csv', header = TRUE, na.strings = '', colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character', 'event_type' = 'character')) %>% as_tibble()

# University data from IPEDS
univ_data <- readRDS('./data/ipeds_1718.RDS')
univ_info <- read.csv('./data/univ_data.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character', 'zip_code' = 'character')) %>% as_tibble()

# Private HS data from PSS
privhs_data <- readRDS('./data/pss_1718.RDS')

# Rankings data from Niche
niche_data  <- read.csv('./data/niche_private.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE) %>% as_tibble()

# Rankings data from US News & World Report
usnews_data <- read.csv('./data/usnews_rankings.csv', header = TRUE, na.strings = '', stringsAsFactors = FALSE, colClasses = c('univ_id' = 'character')) %>% as_tibble()


## ----------
## PREP DATA
## ----------

# Focus on private HS visits by the univs
privhs_events <- events_data %>% filter(event_type == 'priv_hs') %>%
  select(univ_id, event_type, school_id, event_location_name, event_city, event_state)

# Select variables of interest from private HS data
privhs_df <- privhs_data %>% select(ncessch, name, city, state_code, region, religion, pct_white, pct_black, pct_hispanic, pct_asian, pct_amerindian, pct_nativehawaii, pct_tworaces)
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
)) %>% select(ncessch, overall_niche_letter_grade, rank_within_category)
privhs_df <- privhs_df %>% left_join(niche_df)

# Select variables of interest from univ data
get_abbrev <- function(x, y) {
  univ_abbrev <- univ_info[univ_info$univ_id == x, 'univ_abbrev']$univ_abbrev
  ifelse(length(univ_abbrev) == 0, y, univ_abbrev)
}
v_get_abbrev <- Vectorize(get_abbrev)
univ_df <- univ_data %>% mutate(univ_abbrev = v_get_abbrev(univ_id, univ_name)) %>%
  select(univ_id, univ_abbrev, city, state_code, region, religion, pct_white, pct_black, pct_hispanic, pct_asian, pct_amerindian, pct_nativehawaii, pct_tworaces)

# Add ranking from US News & World Report data
usnews_df <- usnews_data %>% select(univ_id, score_text, rank)
univ_df <- univ_df %>% left_join(usnews_df)

# Create attributes dataframe
var_names <- c('school_id', 'school_name', 'city', 'state_code', 'region', 'religion', 'pct_white', 'pct_black', 'pct_hispanic', 'pct_asian', 'pct_amerindian', 'pct_nativehawaii', 'pct_tworaces', 'ranking', 'ranking_numeric')
names(privhs_df) <- var_names
names(univ_df) <- var_names
attributes_df <- dplyr::union(privhs_df, univ_df)

# Vectors of private HS and univs
univ_vec <- unique(privhs_events$univ_id) %>% str_sort(numeric = TRUE)
privu_vec <- unique((univ_info %>% filter(search_sector == 'PRIVATE'))$univ_id)
pubu_vec <- unique((univ_info %>% filter(search_sector == 'PUBLIC'))$univ_id)

privhs_vec <- unique(privhs_events$school_id) %>% str_sort(numeric = TRUE)
privhs_visited_by_privu_vec <- unique((privhs_events %>% filter(univ_id %in% privu_vec))$school_id)
privhs_visited_by_pubu_vec <- unique((privhs_events %>% filter(univ_id %in% pubu_vec))$school_id)

# Preview vertex attributes that will be merged
View(attributes_df %>% filter(school_id %in% univ_vec))
View(attributes_df %>% filter(school_id %in% privhs_vec))

# TODO: Check unmerged Niche data (61 unmerged, 1 truly missing data)
attributes_df %>% filter(school_id %in% privhs_vec) %>% select(ranking) %>% table(useNA = 'always')


## -------------------
## AFFILIATION MATRIX
## -------------------

# Create empty matrix: # of rows = # of HS, # of cols = # of univs
affiliation_matrix <- matrix(0, length(privhs_vec), length(univ_vec))
dimnames(affiliation_matrix) <- list(privhs_vec, univ_vec)

# Populate matrix
for (i in privhs_vec) {
  for (j in univ_vec) {
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

# Vertex list (`name` must come first)
v_2mode <- tibble(name = df$vertices$name, type = df$vertices$type)
v_2mode <- left_join(v_2mode, attributes_df, c('name' = 'school_id'))


## -----------------------------
## CREATE 2-MODE IGRAPH OBJECTS
## -----------------------------

# Recreate 2-mode graph with attributes
g_2mode <- graph_from_data_frame(d = e_2mode,
                                 directed = FALSE,
                                 vertices = v_2mode)

# Create 2-mode subgraph for private univs and their private HS visits only
g_2mode_privu <- induced_subgraph(
  graph = g_2mode,
  vids = V(g_2mode)[V(g_2mode)$name %in% c(privhs_visited_by_privu_vec, privu_vec)]
)

# Create 2-mode subgraph for public univs and their private HS visits only
g_2mode_pubu <- induced_subgraph(
  graph = g_2mode,
  vids = V(g_2mode)[V(g_2mode)$name %in% c(privhs_visited_by_pubu_vec, pubu_vec)]
)


## -----------------------------
## CREATE 1-MODE IGRAPH OBJECTS
## -----------------------------

# Create 1-mode graphs from g_2mode
projection <- bipartite_projection(graph = g_2mode,
                                   types = NULL,
                                   multiplicity = TRUE,
                                   which = c('both'),
                                   probe1 = NULL,
                                   remove.type = TRUE)

g_1mode_hs <- projection[['proj1']]  # all private HS visited
g_1mode_psi <- projection[['proj2']]  # all univs in sample

# Create 1-mode graphs from g_2mode_privu
projection <- bipartite_projection(graph = g_2mode_privu,
                                   types = NULL,
                                   multiplicity = TRUE,
                                   which = c('both'),
                                   probe1 = NULL,
                                   remove.type = TRUE)

g_1mode_hs_privu <- projection[['proj1']]  # all private HS visited by at least 1 private univ
g_1mode_psi_privu <- projection[['proj2']]  # all private univs in sample

# Create 1-mode graphs from g_2mode_pubu
projection <- bipartite_projection(graph = g_2mode_pubu,
                                   types = NULL,
                                   multiplicity = TRUE,
                                   which = c('both'),
                                   probe1 = NULL,
                                   remove.type = TRUE)

g_1mode_hs_pubu <- projection[['proj1']]  # all private HS visited by at least 1 public univ
g_1mode_psi_pubu <- projection[['proj2']]  # all public univs in sample


## --------------------------
## CREATE EGO IGRAPH OBJECTS
## --------------------------

# Create ego networks for each private HS from g_2mode
egos_hs <- make_ego_graph(graph = g_2mode,
                          order = 2,
                          nodes = V(g_2mode)[V(g_2mode)$type == FALSE],  # only include private HS
                          mindist = 0)

names(egos_hs) <- privhs_vec %>% str_sort()  # name the list of ego networks

for (i in 1:length(egos_hs)) {  # assign order as edge attribute
  E(egos_hs[[i]])$order <- if_else(str_detect(string = attr(E(egos_hs[[i]]), 'vnames'), pattern = names(egos_hs)[[i]]), 1, 2)
}

# Create ego networks for each univ from g_2mode
egos_psi <- make_ego_graph(graph = g_2mode,
                           order = 2,
                           nodes = V(g_2mode)[V(g_2mode)$type == TRUE],  # only include univs
                           mindist = 0)

names(egos_psi) <- univ_vec %>% str_sort()

for (i in 1:length(egos_psi)) {
  E(egos_psi[[i]])$order <- if_else(str_detect(string = attr(E(egos_psi[[i]]), 'vnames'), pattern = str_c('\\|', names(egos_psi)[[i]])), 1, 2)
}

# Create ego networks for each private HS from g_2mode_privu
egos_hs_privu <- make_ego_graph(graph = g_2mode_privu, order = 2, nodes = V(g_2mode_privu)[V(g_2mode_privu)$type == FALSE], mindist = 0)

names(egos_hs_privu) <- privhs_visited_by_privu_vec %>% str_sort()

for (i in 1:length(egos_hs_privu)) {
  E(egos_hs_privu[[i]])$order <- if_else(str_detect(string = attr(E(egos_hs_privu[[i]]), 'vnames'), pattern = names(egos_hs_privu)[[i]]), 1, 2)
}

# Create ego networks for each univ from g_2mode_privu
egos_psi_privu <- make_ego_graph(graph = g_2mode_privu, order = 2, nodes = V(g_2mode_privu)[V(g_2mode_privu)$type == TRUE], mindist = 0)

names(egos_psi_privu) <- privu_vec %>% str_sort()

for (i in 1:length(egos_psi_privu)) {
  E(egos_psi_privu[[i]])$order <- if_else(str_detect(string = attr(E(egos_psi_privu[[i]]), 'vnames'), pattern = str_c('\\|', names(egos_psi_privu)[[i]])), 1, 2)
}

# Create ego networks for each private HS from g_2mode_pubu
egos_hs_pubu <- make_ego_graph(graph = g_2mode_pubu, order = 2, nodes = V(g_2mode_pubu)[V(g_2mode_pubu)$type == FALSE], mindist = 0)

names(egos_hs_pubu) <- privhs_visited_by_pubu_vec %>% str_sort()

for (i in 1:length(egos_hs_pubu)) {
  E(egos_hs_pubu[[i]])$order <- if_else(str_detect(string = attr(E(egos_hs_pubu[[i]]), 'vnames'), pattern = names(egos_hs_pubu)[[i]]), 1, 2)
}

# Create ego networks for each univ from g_2mode_pubu
egos_psi_pubu <- make_ego_graph(graph = g_2mode_pubu, order = 2, nodes = V(g_2mode_pubu)[V(g_2mode_pubu)$type == TRUE], mindist = 0)

names(egos_psi_pubu) <- pubu_vec %>% str_sort()

for (i in 1:length(egos_psi_pubu)) {
  E(egos_psi_pubu[[i]])$order <- if_else(str_detect(string = attr(E(egos_psi_pubu[[i]]), 'vnames'), pattern = str_c('\\|', names(egos_psi_pubu)[[i]])), 1, 2)
}
