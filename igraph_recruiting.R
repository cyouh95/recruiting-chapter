# CLEAR MEMORY
rm(list = ls())

options(max.print=999)

library(igraph)
library(tidyverse)
library(Hmisc)
library(labelled)
library(haven) 

#### READ IN CSV DATA/LOAD RDATA

# read in hs data; has data on both publics and privates
  #hs_data <- read.csv('./data/hs_data.csv', header = TRUE, na.strings='', colClasses = c('school_type' = 'character', 'ncessch' = 'character', 'state_code' = 'character', 'zip_code' = 'character'))
  #hs_data %>% count(school_type)

# read in zip_code level data
  zip_data <- read.csv('./data/zip_to_state.csv', header = TRUE, na.strings='', colClasses = c('state_code' = 'character', 'zip_name' = 'character', 'zip_code' = 'character'))

# merge hs school-level data to zipcode-level data  
  #data <- full_join(hs_data, zip_data, by = 'zip_code')
  
# load 2017-2018 PSS data
  load(file = "./data/pss_1718.RData")
  class(privatehs)
  glimpse(privatehs)
  
  #remove variables we definitely won't use
  privatehs <- privatehs %>% select(-starts_with("rep"),-starts_with("f_"))
  
    #privatehs %>% var_label() # variable labels
    #privatehs %>% val_labels() # variable labels
  
# university data  

#read in data on visits by universities; one obs per university-event

  #events <- read.csv('./data/events_data.csv', header = TRUE, na.strings='', colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character'))
  events <- read.csv('./data/events_data_2020-07-27.csv', header = TRUE, na.strings='', colClasses = c('univ_id' = 'character', 'univ_id_req' = 'character', 'school_id' = 'character')) %>% as_tibble()

#####  
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
  univ_vec
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
    str(privhs_vec) # length = 1,742 elements
  
# Create affiliation matrix

  # create empty affiliation matrix, where  # of rows = # of HS, # of cols = # of univs
  m <- matrix(0, length(privhs_vec), length(univ_vec))
  str(m)  
  
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
  rm(i,j)

###### Create igraph objects for network analysis

  affiliation_matrix <- m
  str(affiliation_matrix)
  str(m)
  rm(m)
  
# Create object for two mode [bipartite] analysis
  
  two_mode_network <- graph_from_incidence_matrix(affiliation_matrix, weighted = TRUE)
    # two_mode_network <- graph.incidence(affiliation_matrix) # is this same as above?
  class(two_mode_network) # class = igraph
  two_mode_network


# Create igraph object with vertex attributes based on school, university characteristics
  #STEPS: 
    # create an edge list dataset, using the two_mode_network igraph object already created:
    # create a vertext attributes dataset with "name" and "type", using two_mode_network igraph object already created:
      # merge in desired private school characteristics by ppin
      # merge in university characteristics by unitid
    # and then using igraph::graph_from_data_frame(), recreate the igraph object, but this time w/ vertex attributes

  
# CREATE AN EDGE LIST DATAFRAME
  elist_2mode <- as_tibble(x = cbind(as_edgelist(graph = two_mode_network, names = TRUE),E(two_mode_network)$weight), .name_repair = ~ c("ppin","unitid","weight")) %>%
    mutate(weight = as.integer(weight))
  str(elist_2mode)
  attributes(elist_2mode)
  
# CREATE A VERTICES DATA FRAME
  
  # basic inputs of "name" and "type" attribute
    V(two_mode_network)$name
    str(V(two_mode_network)$name)
    
    V(two_mode_network)$type
    str(V(two_mode_network)$type)

  # some old code    
    #as_tibble(x = V(two_mode_network)$type, .name_repair = ~ c("type")) %>% str()
    #enframe(x = V(two_mode_network)$type, name = NULL, value = "type") %>% str()
    #v_attr_2mode <- enframe(x = V(two_mode_network)$type, name = NULL, value = "type")
    
    tibble(name = V(two_mode_network)$name, type = V(two_mode_network)$type)
    #print(x = v_attr_2mode, n = 2000)
    #print(x = tibble(name = V(two_mode_network)$name, type = V(two_mode_network)$type), n=2000)

  # create data frame and add separate id variables for private high school (ppin) and universities (unitid)
    v_attr_2mode <- tibble(name = V(two_mode_network)$name, type = V(two_mode_network)$type) %>% 
      mutate(
        ppin = if_else(type == FALSE,name,NA_character_),
        unitid = if_else(type == TRUE,name,NA_character_),
      )
      str(v_attr_2mode)
    # check
      all(v_attr_2mode$name[v_attr_2mode$type == FALSE] == v_attr_2mode$ppin[v_attr_2mode$type == FALSE])
      
      all(v_attr_2mode$name[v_attr_2mode$type == TRUE] == v_attr_2mode$unitid[v_attr_2mode$type == TRUE])
      
  # merge in private high school characteristics
      # NOTE FOR IRMA/CRYSTAL, 8/27/2020
        # 278 private high schools that received at least one visit were not in the 2017-18 PSS data
        # below, I merge these 278 obs to high-school-level data from events.csv [all obs merge] and to high-school-level data from hs_data.csv [all obs merge]
        # One of you take the lead in investigating these 278 obs
          # confirm that these schools were in previous iteration of PSS      
          # we need to figure out why they were not in the 2017-18 PSS data
            # did the school close?
        # after investigating, need to decide what to do about these 278 schools
          # e.g., should we use non-missing data from previous iteration of PSS?

      
      glimpse(privatehs)
      all(nchar(privatehs$ncessch) ==8) # assert private school id variable is 8 characters long
      
    # create dataframe with subset of variables and rename variables with suffix _pss
      #privatehs %>% var_label() # variable labels
      #privatehs %>% val_labels() # variable labels
      privatehs_somevars <- privatehs %>% 
        select(ncessch,name,address,city,state_code,zip_code,pzip4,county_fips_code,county_name,address_2,pl_cit,pl_stabb,pl_zip,pl_zip4,region,state_fips_code,
               locale_code,latitude,longitude,logr2018,higr2018,pct_to_4yr,school_type,typology,relig,orient,diocese,level,total_students,size,total_09,total_10,
               total_11,total_12,total_enrolled,total_hispanic,total_white,total_black,total_asian,total_nativehawaii,total_amerindian,total_tworaces,
               total_teachers,community_type,pct_amerindian,pct_asian,pct_nativehawaii,pct_hispanic,pct_white,pct_black,pct_tworaces,sttch_rt)
      
    #add suffix _pss to all variables
        names(privatehs_somevars)
          #str(names(privatehs_somevars)) # character vector
        str_c(names(privatehs_somevars),"pss",sep="_")
          #paste(names(privatehs_somevars),"pss",sep="_") # same same

        names(privatehs_somevars) <- str_c(names(privatehs_somevars),"pss",sep="_")

    # perform left join
        attributes(privatehs_somevars$ncessch_pss)
        attributes(v_attr_2mode$ppin)
        
        v_attr_ljoin <- v_attr_2mode %>% left_join(privatehs_somevars, by = c("ppin" = "ncessch_pss"))
        glimpse(v_attr_ljoin)
        
        v_attr_ljoin %>% count(school_type_pss) # missing for 321 obs, so merge is not high quality [~278 are private schools that should have merged, 43 are universities that should not have merged]
        
      # anti join to check quality of merge
        # these are values of ppin that were in the "events" data, but not the 2017-18 PSS data
        school_ajoin <- v_attr_2mode %>% anti_join(privatehs_somevars, by = c("ppin" = "ncessch_pss")) %>%
          # get rid of the 43 obs where the vertex is a university rather than a high school
          filter(type == FALSE) %>% select(ppin)
  
          glimpse(school_ajoin)
        
        # See if you can get other data on characterics from the hs_data.csv data and from the events.csv dataset
        
            #data from hs_data.csv
            hs_data <- read.csv('./data/hs_data.csv', header = TRUE, na.strings='', colClasses = c('school_type' = 'character', 'ncessch' = 'character', 'state_code' = 'character', 'zip_code' = 'character')) %>%
              filter(school_type == "Private")
              
              hs_data %>% glimpse()
              hs_data %>% count(school_type)
              
              all(nchar(hs_data$ncessch) ==8) # do all have 8 characters
            
            #data from events_data_2020-07-27.csv
            hs_data2 <- events %>% filter(event_type == "priv_hs") %>% 
              select(school_id,event_location_name,event_state,event_city,school_type_pri,locale_code,level_pri,total_enrolled_pri,total_students_pri,total_09,total_10,total_11,total_12,pct_white_pri,pct_black_pri,pct_asian_pri,pct_hispanic_pri,pct_amerindian_pri,pct_nativehawaii_pri,pct_tworaces_pri,titlei_status_pri,pct_to_4yr) %>%
              arrange(school_id) %>%
              # keep only one obs per school_id
              group_by(school_id) %>% filter(row_number()==1) %>% mutate(one = 1)

              all(nchar(hs_data2$school_id) ==8) # do all have 8 characters?
              
              glimpse(hs_data2)
            
          # merge anti-join dataframe to hs data from events_data_2020-07-27.csv
            school_ajoin_ljoin_events <- school_ajoin %>% left_join(hs_data2, by = c("ppin" = "school_id"))
          
            glimpse(school_ajoin_ljoin_events)
            
            school_ajoin_ljoin_events %>% count(one) # conveys number/percent of obs that merge
            
            school_ajoin_ljoin_events %>% select(ppin,event_location_name,event_state,event_city,total_12,pct_to_4yr) %>% print(n=300)
            
          # merge anti-join dataframe to hs data hs_data.csv
            school_ajoin_ljoin_hsdata <- school_ajoin %>% left_join(hs_data, by = c("ppin" = "ncessch"))
            
            glimpse(school_ajoin_ljoin_hsdata)
            
            school_ajoin_ljoin_hsdata %>% count(school_type) # conveys number/percent of obs that merge
            
            school_ajoin_ljoin_hsdata %>% select(ppin,name,state_code,total_students,pct_white,pct_black,pct_hispanic,pct_asian) %>% print(n=300)
            
        # remove anti-join type datasets
          rm(school_ajoin,school_ajoin_ljoin_events,school_ajoin_ljoin_hsdata)
          rm(hs_data,hs_data2)



  # merge in university characteristics
    
    if_else(condition, true, false, missing = NULL)
    
    #print(v_attr_2mode, n=2000)
    str(v_attr_2mode)
  
    # next step, merge private high school and university characteristics to v_attr_2mode
    
      # from v_attr_2mode$name [character vector that has ID of school/university], create separate ID variables for private HS (ppin) and for university (unitid)
        # for unitid get rid of suffix "_req"
    
      # merge in private high school characteristics by ppin
    
      # merge in university characteristics by unitid
  
  # RECREATE IGRAPH OBJECT
  #?graph_from_data_frame
  #g_2mode <- graph_from_data_frame(d = elist_2mode, directed = FALSE, vertices = NULL)
  g_2mode <- graph_from_data_frame(
    d = elist_2mode, 
    directed = FALSE, 
    vertices = v_attr_2mode
  )
  is_bipartite(graph = g_2mode)
  
  g_2mode
  
  vertex_attr_names(g_2mode)
  
  vertex_attr(graph = g_2mode, name = "name")
  vertex_attr(graph = g_2mode, name = "type")
  
  edge_attr_names(g_2mode)
  
  edge_attr(graph = g_2mode, name = "weight")
  
  two_mode_network
  
  ecount(g_2mode)
  ecount(two_mode_network)
  
  vcount(g_2mode)
  vcount(two_mode_network)    
  
# create object for one mode analysis
  ?bipartite.projection
  one_mode_network <- bipartite.projection(two_mode_network)
  one_mode_network <- bipartite.projection(g_2mode)
  
  
  class(one_mode_network) # class = list
  str(one_mode_network) # two lists (each w/ 10 elements); list 1 name = "proj1"; list 2 name = "proj2"
  
  # element name = proj 1; one-mode network where each HS is a node
  # ??? and edge defined as number of visits to that HS [? or is it number of universities that visited the HS]
  
  # below 3 lines are all equivalent
  class(one_mode_network[[1]])
  class(one_mode_network[["proj1"]])
  class(one_mode_network$proj1)
  
  hs_graph <- one_mode_network$proj1
  vertex_attr_names(hs_graph)
  edge_attr_names(hs_graph)
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
