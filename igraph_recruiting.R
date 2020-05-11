library(igraph)
library(tidyverse)
library(Hmisc)

hs_data <- read.csv('./data/hs_data.csv', header = TRUE, na.strings='', colClasses = c('school_type' = 'character', 'ncessch' = 'character', 'state_code' = 'character', 'zip_code' = 'character'))
zip_data <- read.csv('./data/zip_to_state.csv', header = TRUE, na.strings='', colClasses = c('state_code' = 'character', 'zip_name' = 'character', 'zip_code' = 'character'))
data <- full_join(hs_data, zip_data, by = 'zip_code')

events <- read.csv('./data/events_data.csv', header = TRUE, na.strings='', colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character'))

univ_sample <- c('196097_req', '186380_req', '215293', '201885_req', '181464', '139959_req', '218663_req', '100751', '199193', '110635_req', '110653_req', '110671_req', '110680_req', '126614_req', '155317_req', '106397', '166629_req')
df <- select(events, univ_id_req, school_id, avgmedian_inc_2564, pct_white_zip, pct_black_zip, pct_asian_zip, pct_hispanic_zip, pct_amerindian_zip, pct_nativehawaii_zip, pct_tworaces_zip, pct_otherrace_zip) %>% filter(nchar(school_id) == 12, univ_id_req %in% univ_sample)

hs_list <- unique(df$school_id)
univ_list <- unique(df$univ_id_req)

# Create affiliation matrix (# of rows = # of HS, # of cols = # of univs)

m <- matrix(0, length(hs_list), length(univ_list))
dimnames(m) <- list(
  hs_list,
  univ_list
)

for (i in hs_list) {
  for (j in univ_list) {
    if (nrow(df %>% filter(school_id == i, univ_id_req == j)) > 0) {
      m[i, j] <- 1
    }
  }
}

affiliation_matrix <- m

# Collapse from two mode to one mode

two_mode_network <- graph.incidence(affiliation_matrix)
one_mode_network <- bipartite.projection(two_mode_network)

univ_graph <- one_mode_network$proj2

get.adjacency(univ_graph, sparse = FALSE, attr = 'weight')
plot(univ_graph, edge.label = E(univ_graph)$weight)

# Cluster univ graph

cw <- cluster_fast_greedy(univ_graph)
plot(cw, univ_graph, edge.label = E(univ_graph)$weight)

mem <- membership(cw)

for (i in unique(mem)) {
  
  univs <- names(mem[mem == i])
  
  # Plot each cluster
  
  univ_subgraph <- induced.subgraph(univ_graph, univs)
  plot(univ_subgraph, edge.label = E(univ_subgraph)$weight)
  
  # Affiliation matrix for cluster
  
  univ_matrix <- affiliation_matrix[, univs]
  univ_matrix <- cbind(univ_matrix, total = rowSums(univ_matrix))
  
  hs_union <- rownames(univ_matrix[univ_matrix[, 'total'] > 0, ])
  hs_intersect <- rownames(univ_matrix[univ_matrix[, 'total'] == length(univs), ])
  hs <- rownames(univ_matrix[univ_matrix[, 'total'] > 1, ]) # visited by more than 1 univ
  print(paste(length(hs_union), length(hs_intersect), length(hs)))
  
  # HS characteristics
  
  # Data not available yet for some ncessch that were manually inputted
  # hs_subset <- (hs_data %>% filter(ncessch %in% hs))$ncessch
  # print(hs[!(hs %in% hs_subset)])
  
  hs_subset <- (data %>% filter(ncessch %in% hs))
  print(describe(hs_subset$pct_white))
  print(describe(hs_subset$avgmedian_inc_2564))
}

  #in-state & out-of-state? 
