# CLEAR MEMORY
rm(list = ls())

options(max.print=99)

library(igraph)
library(tidyverse)
library(Hmisc)

# read in hs data; has data on both publics and privates
  hs_data <- read.csv('./data/hs_data.csv', header = TRUE, na.strings='', colClasses = c('school_type' = 'character', 'ncessch' = 'character', 'state_code' = 'character', 'zip_code' = 'character'))
  hs_data %>% count(school_type)

# read in zip_code level data
  zip_data <- read.csv('./data/zip_to_state.csv', header = TRUE, na.strings='', colClasses = c('state_code' = 'character', 'zip_name' = 'character', 'zip_code' = 'character'))

# merge hs school-level data to zipcode-level data  
  data <- full_join(hs_data, zip_data, by = 'zip_code')

#read in data on visits by universities; one obs per university-event

  #events <- read.csv('./data/events_data.csv', header = TRUE, na.strings='', colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character'))
  events <- read.csv('./data/events_data_2020-07-27.csv', header = TRUE, na.strings='', colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character')) %>% as_tibble()

# Create data frame of events at private high schoools

  privhs_events <- events %>% filter(event_type == "priv_hs") %>% 
    select(univ_id,univ_id_req,contains("event"),cbsa_code,pid,school_id,determined_zip,categorized_location_type,latitude,longitude,locale_code,contains("school_type"))

  # VARS FROM NCES PRIVATE SCHOOL DATA (PSS); NOT KEEPING FOR NOW. BETTER TO GET DIRECTLY FROM A PSS DATASET
    #$ level_pri                    <int> NA, NA, 2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 3, 3, 2, NA, NA, NA, NA, NA, NA, NA, NA...
    #$ total_enrolled_pri           <dbl> NA, NA, 1113, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1604, 644, 1200, NA, NA, NA, NA, NA,...
    #$ total_students_pri           <dbl> NA, NA, 1113, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1604, 623, 1200, NA, NA, NA, NA, NA,...
    #$ total_09                     <dbl> NA, NA, 286, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 132, 72, 300, NA, NA, NA, NA, NA, NA,...
    #$ total_10                     <dbl> NA, NA, 314, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 135, 61, 300, NA, NA, NA, NA, NA, NA,...
    #$ total_11                     <dbl> NA, NA, 239, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 138, 67, 320, NA, NA, NA, NA, NA, NA,...
    #$ total_12                     <dbl> NA, NA, 274, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 121, 65, 280, NA, NA, NA, NA, NA, NA,...
    #$ pct_white_pri                <dbl> NA, NA, 54.7170, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 85.0998, 78.1701, 83.3333, NA, NA...
    #$ pct_black_pri                <dbl> NA, NA, 26.2354, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 5.7980, 9.6308, 10.8333, NA, NA, ...
    #$ pct_asian_pri                <dbl> NA, NA, 4.6721, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 4.8005, 6.7416, 3.3333, NA, NA, NA...
    #$ pct_hispanic_pri             <dbl> NA, NA, 9.7035, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.7481, 5.1364, 1.6667, NA, NA, NA...
    #$ pct_amerindian_pri           <dbl> NA, NA, 0.1797, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.0000, 0.0000, 0.1667, NA, NA, NA...
    #$ pct_nativehawaii_pri         <dbl> NA, NA, 0.0000, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.3741, 0.3210, 0.4167, NA, NA, NA...
    #$ pct_tworaces_pri             <dbl> NA, NA, 4.4924, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 3.1796, 0.0000, 0.2500, NA, NA, NA...
    #$ titlei_status_pri            <dbl> NA, NA, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA...
    #$ pct_to_4yr                   <dbl> NA, NA, 99, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 100, 100, 98, NA, NA, NA, NA, NA, NA, ...

  # FILTER CREATED BY CRYSTAL; THINK GOAL WAS TO GRAB VISITS TO PUBLIC HIGH SCHOOLS
    #df <- select(events, event_type, univ_id_req, school_id, avgmedian_inc_2564, pct_white_zip, pct_black_zip, pct_asian_zip, pct_hispanic_zip, pct_amerindian_zip, pct_nativehawaii_zip, pct_tworaces_zip, pct_otherrace_zip) %>% filter(nchar(school_id) == 12, univ_id_req %in% univ_sample)

events %>% count(event_type)
#df %>% count(event_type)
privhs_events %>% count(event_type)

# Create character vector containing unitid of universities in event data; element should have subscript "_req" if using requested data rather than scrapted data

  # programming approach
  univ_vec <- unique(events$univ_id_req) %>% str_sort(numeric = TRUE)

  # manual approach
  univ_sample <- c("100751", "106397", "110635_req", "110653_req", "110671_req", "110680_req", "115409", "120254", "123165", "126614_req", "126678", "127060", "128902", "139658", "139959_req", "147767", "152080","155317_req", "160755", "164924", "166629_req", "167835", "168148", "168218", "168342", "173902", "181464", "186380_req", "186867", "196097_req", "199193", "201645", "201885_req", "204501","215293", "216287", "216597", "218663_req", "221519", "223232", "228246", "228875", "230959") %>%
    str_sort(numeric = TRUE)
    # below is original line of code, containing only public universities
    #univ_sample <- c('196097_req', '186380_req', '215293', '201885_req', '181464', '139959_req', '218663_req', '100751', '199193', '110635_req', '110653_req', '110671_req', '110680_req', '126614_req', '155317_req', '106397', '166629_req')

    #assert these two are the same  
    str_sort(univ_vec,numeric = TRUE) == str_sort(univ_sample,numeric = TRUE)
      #str_sort(univ_vec,numeric = TRUE)
      #str_sort(univ_sample,numeric = TRUE)
  rm(univ_sample)
  
# Create vector containing ID of private high schools

  # check on values of school_id from data frame privhs_events
  
    # do all have 8 characters?
      all(nchar(privhs_events$school_id) ==8)
  
    # check high and low values of 500 obs
      #privhs_events %>% arrange(school_id,univ_id_req) %>% select(school_id,univ_id_req) %>% print(n=500) # sort school_id ascending
      #privhs_events %>% arrange(desc(school_id),univ_id_req) %>% select(school_id,univ_id_req) %>% print(n=500)  # sort school_id descending
      
      #NOTE: the highest values of school_id start w/ letters (e.g., "BB", "X", ETC.)

  # Create vector of unique high school IDs from dataframe privhs_vec
    #NOTE: below vector created from dataframe privhs_events; and thus only contains IDs of visited private high schools  
  
    privhs_vec <- unique(privhs_events$school_id) %>% str_sort(numeric = TRUE)
  
# Create affiliation matrix

  # create empty affiliation matrix, where  # of rows = # of HS, # of cols = # of univs
  m <- matrix(0, length(privhs_vec), length(univ_vec))
  #str(m)  
  
  # add dimension name attributes; row name = NCES high school ID; column name = univ_id_req
  dimnames(m) <- list(
    privhs_vec,
    univ_vec
  )
  #str(m)


  # populate adjacency matrix
  for (i in privhs_vec) { # for (i in c("00000044","00000226","00000237","00000714","00000849")) {
    for (j in univ_vec) {
      
      #writeLines(str_c("hs id=",i,"; univ id=",j))

      # for each combination of private high school and university, if number of by that university to that high school >0, replace value of matrix cell to the total number of visits by that university to that high school      
      if (nrow(privhs_events %>% filter(school_id == i, univ_id_req == j)) > 0) {
        m[i, j] <- privhs_events %>% filter(school_id == i, univ_id_req == j) %>% nrow()
      }
      
    }
  }

###### Create igraph objects for network analysis

  affiliation_matrix <- m
  str(affiliation_matrix)
  str(m)
  
# Create object for two mode [bipartite] analysis
  
  two_mode_network <- graph_from_incidence_matrix(affiliation_matrix, weighted = TRUE)
    # two_mode_network <- graph.incidence(affiliation_matrix) # is this same as above?
  class(two_mode_network) # class = igraph

# create object for one mode analysis
  
  one_mode_network <- bipartite.projection(two_mode_network)  
  
  class(one_mode_network) # class = list
  str(one_mode_network) # two lists (each w/ 10 elements); list 1 name = "proj1"; list 2 name = "proj2"
  
  # element name = proj 1; one-mode network where each HS is a node
  # ??? and edge defined as number of visits to that HS [? or is it number of universities that visited the HS]
  
  # below 3 lines are all equivalent
  class(one_mode_network[[1]])
  class(one_mode_network[["proj1"]])
  class(one_mode_network$proj1)
  
  hs_graph <- one_mode_network$proj1
  class(hs_graph)
  
  #get.adjacency(hs_graph, sparse = FALSE, attr = 'weight')
  #str(get.adjacency(hs_graph, sparse = FALSE, attr = 'weight')) # 1742 by 1742 matrix
  
  #plot(hs_graph, edge.label = E(univ_graph)$weight)
  
  # element name = proj2; one-mode network where each university is a node
  # ??? and edge defined as number of ???
  
  class(one_mode_network[[2]])
  
  univ_graph <- one_mode_network$proj2
  class(univ_graph)

#######################    
#investigate two_mode_network object
#######################  

# number of vertices and edges  
  V(two_mode_network) # number of vertices = 1785 = 43+1742 = number of universities + number of private hS that got at least one visit
    
  E(two_mode_network) # number of edges; each edge is a relation between a high school and a university
  str(E(two_mode_network)) # class = igraph edge sequence; type = integer vector of lendgth == 11034
  
  is_weighted(two_mode_network)
  
# print command    
  #print_all(two_mode_network)
  print(two_mode_network)
    
  summary(two_mode_network)
  
# print representations for graph
  
  # edge list representation
    igraph::as_edgelist(two_mode_network) # not sure if this is capturing weights (i.e., number of visits vs. presence of visit)
  
  # adjacency list representation
    as_adjacency_matrix(two_mode_network)
    
# investigate attributes using regular attributes() function
    
    attributes(two_mode_network) # has a class attribute; class = igraph
    
    attributes(V(two_mode_network))
    
    attributes(E(two_mode_network))
    
    
# investigate/modify vertext attributes using igraph attribute functions

  # vertex attributes    
    
    igraph::vertex_attr_names(graph = two_mode_network) # two attributes: "type" "name"
    
    vertex_attr(graph = two_mode_network, name = "name") # names of the nodes
      str(vertex_attr(graph = two_mode_network, name = "name")) # character vector of 1785 elements
      
    vertex_attr(graph = two_mode_network, name = "type") # 
      str(vertex_attr(graph = two_mode_network, name = "type")) # logical vector of 1785 elements which is number of private High schools (that got visits) + number of universities
      
      sum(vertex_attr(graph = two_mode_network, name = "type")) # sum = 43; so type == TRUE if element is a university
    
  # edge attributes
    
    igraph::edge_attr_names(graph = two_mode_network) # one attribute: weight 
  
    edge_attr(graph = two_mode_network, name = "weight")
      str(edge_attr(graph = two_mode_network, name = "weight")) # numeric vector of length = 11034; each element is the weight associated with that edge
  
##########################    
########################## descriptive analysis of igraph object univ_graph, where nodes are universities
##########################    

    
    
  get.adjacency(univ_graph, sparse = FALSE, attr = 'weight')
  str(get.adjacency(univ_graph, sparse = FALSE, attr = 'weight')) # 43 by 43 matrix
  
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
