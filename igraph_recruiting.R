library(igraph)
library(tidyverse)
library(Hmisc)

hs_data <- read.csv('./data/hs_data.csv', header = TRUE, na.strings='', colClasses = c('school_type' = 'character', 'ncessch' = 'character', 'state_code' = 'character', 'zip_code' = 'character'))
zip_data <- read.csv('./data/zip_to_state.csv', header = TRUE, na.strings='', colClasses = c('state_code' = 'character', 'zip_name' = 'character', 'zip_code' = 'character'))
data <- full_join(hs_data, zip_data, by = 'zip_code')

#events <- read.csv('./data/events_data.csv', header = TRUE, na.strings='', colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character'))
events <- read.csv('./data/events_data_2020-07-27.csv', header = TRUE, na.strings='', colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character')) %>% as_tibble()

# character vector containing 
#univ_sample <- c('196097_req', '186380_req', '215293', '201885_req', '181464', '139959_req', '218663_req', '100751', '199193', '110635_req', '110653_req', '110671_req', '110680_req', '126614_req', '155317_req', '106397', '166629_req')

univ_sample <- c("100751", "106397", "110635_req", "110653_req", "110671_req", "110680_req", "115409", "120254", "123165", "126614_req", "126678", "127060", "128902", "139658", "139959_req", "147767", "152080","155317_req", "160755", "164924", "166629_req", "167835", "168148", "168218", "168342", "173902", "181464", "186380_req", "186867", "196097_req", "199193", "201645", "201885_req", "204501","215293", "216287", "216597", "218663_req", "221519", "223232", "228246", "228875", "230959")

df <- select(events, univ_id_req, school_id, avgmedian_inc_2564, pct_white_zip, pct_black_zip, pct_asian_zip, pct_hispanic_zip, pct_amerindian_zip, pct_nativehawaii_zip, pct_tworaces_zip, pct_otherrace_zip) %>% filter(nchar(school_id) == 12, univ_id_req %in% univ_sample)

#get list of all the unitids in events data
temp <- events %>% group_by(univ_id) %>% summarise(unitid = first(univ_id)) %>% mutate(univ_id_req = str_c(univ_id,"_req", sep = "")) %>% select(univ_id,univ_id_req) %>% print(n = 100)
#str(temp) 
temp %>% print(n=100)
temp %>% mutate(univ_id_req = str_c(univ_id,"_req", sep = ""))

temp %>% 
writeLines(temp$univ_id_req)
print(temp$univ_id_req, quote = TRUE)




print(temp$univ_id, quote = TRUE)
"100751" "106397" "110635_req" "110653_req" "110671_req" "110680_req" "115409" "120254" "123165" "126614_req" "126678" "127060" "128902" "139658" "139959_req" "147767" "152080"
"155317_req" "160755" "164924" "166629_req" "167835" "168148" "168218" "168342" "173902" "181464" "186380_req" "186867" "196097_req" "199193" "201645" "201885_req" "204501"
"215293" "216287" "216597" "218663_req" "221519" "223232" "228246" "228875" "230959"


print(temp$univ_id_req, quote = TRUE)
"100751_req" "106397_req" "110635_req" "110653_req" "110671_req" "110680_req" "115409_req" "120254_req" "123165_req" "126614_req" "126678_req" "127060_req"
"128902_req" "139658_req" "139959_req" "147767_req" "152080_req" "155317_req" "160755_req" "164924_req" "166629_req" "167835_req" "168148_req" "168218_req"
"168342_req" "173902_req" "181464_req" "186380_req" "186867_req" "196097_req" "199193_req" "201645_req" "201885_req" "204501_req" "215293_req" "216287_req"
"216597_req" "218663_req" "221519_req" "223232_req" "228246_req" "228875_req" "230959_req"


str(events)
options(max.print=99)
events %>% count(univ_id)
glimpse(events)

events %>% filter(univ_id %in% c("196097","181464")) %>% select(univ_id,univ_id_req, event_type) %>% print(n=3000)
  glimpse()


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
