################################################################################
##
## [ PROJ ] Recruiting chapter for Stephen Burd
## [ FILE ] igraph_recruiting.R
## [ AUTH ] Ozan, Crystal, Irma
## [ INIT ] July 1, 2020
##
################################################################################

# CLEAR MEMORY
rm(list = ls())

options(max.print=2000)

## ---------------------------
## libraries
## ---------------------------

library(igraph)
library(tidyverse)
library(Hmisc)
library(labelled)
library(haven)

## ---------------------------
## Read in csv data; load R data
## ---------------------------


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



# Create data frame of events at private high schoools
  privhs_events <- events %>% filter(event_type == "priv_hs") %>% 
    select(univ_id,univ_id_req,contains("event"),cbsa_code,pid,school_id,determined_zip,categorized_location_type,latitude,longitude,locale_code,contains("school_type"))

events %>% count(event_type)
#df %>% count(event_type)
privhs_events %>% count(event_type)


## ---------------------------
## create vectors of private HS IDs (mode 1) and vector of university IDs (mode 2), which are inputs to igraph object
## ---------------------------

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

  # decision: don't want the ID number for universities to have suffix "_req"
  univ_vec <- str_extract(univ_vec, pattern = "\\d*") # this uses regular expressions to extract the "155317" from "155317_req"
  univ_vec
  
## ---------------------------
## Create affiliation matrix, which is input to igraph object
## ---------------------------
    
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
      if (nrow(privhs_events %>% filter(school_id == i, univ_id == j)) > 0) {
        m[i, j] <- privhs_events %>% filter(school_id == i, univ_id == j) %>% nrow()
      }
      
    }
  }
  rm(i,j)

## ---------------------------
## Create bipartite/two-mode igraph object
## ---------------------------
  
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

## ---------------------------
## Create data frames of vertex attributes, both HS characteristics and university characteristics
## ---------------------------
  
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
        #unitid = if_else(type == TRUE,name,NA_character_)
        unitid = if_else(type == TRUE,str_replace(string = name, pattern = "_req", replacement = ""),NA_character_) , # remove suffix "_req"
      ) 
      #str_replace(string = v_attr_2mode$unitid[v_attr_2mode$type == TRUE], pattern = "_req", replacement = "")
    
      str(v_attr_2mode)
      
    # check
      
      all(v_attr_2mode$name[v_attr_2mode$type == FALSE] == v_attr_2mode$ppin[v_attr_2mode$type == FALSE])
      
      #all(v_attr_2mode$name[v_attr_2mode$type == TRUE] == v_attr_2mode$unitid[v_attr_2mode$type == TRUE])

# next step, merge private high school and university characteristics to v_attr_2mode

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
               total_teachers,community_type,pct_amerindian,pct_asian,pct_nativehawaii,pct_hispanic,pct_white,pct_black,pct_tworaces,sttch_rt) %>%
        arrange(ncessch)
      
    #add suffix _pss to all variables
        names(privatehs_somevars)
          #str(names(privatehs_somevars)) # character vector
        str_c(names(privatehs_somevars),"pss",sep="_")
          #paste(names(privatehs_somevars),"pss",sep="_") # same same

        names(privatehs_somevars) <- str_c(names(privatehs_somevars),"pss",sep="_")
        privatehs_somevars %>% glimpse()
        #privatehs_somevars %>% arrange() %>% glimpse()
        
    # perform left join
        attributes(privatehs_somevars$ncessch_pss)
        attributes(v_attr_2mode$ppin)

        v_attr_2mode <- v_attr_2mode %>% left_join(privatehs_somevars, by = c("ppin" = "ncessch_pss"))
        #v_attr_ljoin <- v_attr_2mode %>% left_join(privatehs_somevars, by = c("ppin" = "ncessch_pss"))
        glimpse(v_attr_2mode)
        
        v_attr_2mode %>% count(school_type_pss) # missing for 321 obs, so merge is not high quality [~278 are private schools that should have merged, 43 are universities that should not have merged]
        
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
          #rm(v_attr_ljoin)
          rm(hs_data,hs_data2)

  # merge in university characteristics

    # read data          
    univ_data <- read.csv("./data/univ_data.csv", header = TRUE, na.strings='', colClasses = c('univ_id' = 'character')) %>% as_tibble() %>%
      # remove variables you know you won't use right away
      select(-fips_county_code,-county_name,-cbsa_code,-sector,-school_url,-search_sector,-search_length,-fips_state_code,-starts_with("pop_"))

    #add suffix _pss to all variables
      names(univ_data) <- str_c(names(univ_data),"ipeds",sep="_")
      #names(univ_data)
      
      univ_data %>% glimpse()

    # left join

      v_attr_2mode <- v_attr_2mode %>% left_join(univ_data, by = c("unitid" = "univ_id_ipeds"))

      v_attr_2mode %>% count(control_ipeds) # convey number of unitids that merged; looks good
    
      v_attr_2mode %>% glimpse()

## ---------------------------
## Create igraph object, but this time with vertex attributes consisting of HS and university characteristics
## ---------------------------
      
  # RECREATE IGRAPH OBJECT
    #?graph_from_data_frame
    elist_2mode %>% glimpse()
    v_attr_2mode %>% glimpse()
      
  g_2mode <- graph_from_data_frame(
    d = elist_2mode, 
    directed = FALSE, 
    vertices = v_attr_2mode
  )
  is_bipartite(graph = g_2mode)

  # compare to original igraph object you created  
    two_mode_network
    g_2mode # what does this error mean?: Error in seq_len(no) : argument must be coercible to non-negative integer
  
    ecount(g_2mode)
    ecount(two_mode_network)
    
    vcount(g_2mode)
    vcount(two_mode_network)    
    
    vertex_attr_names(g_2mode)
    vertex_attr_names(two_mode_network)
    
    vertex_attr(graph = g_2mode, name = "name")
    vertex_attr(graph = g_2mode, name = "type")
    
    edge_attr_names(g_2mode)
    edge_attr_names(two_mode_network)
    
    edge_attr(graph = g_2mode, name = "weight")
  
  rm(two_mode_network)
  
  # Create 2-mode objects that only contain vertices and edge associated with visits from private colleges/universities to private high schools
    g_2mode_privu <- induced_subgraph(
      graph = g_2mode,
      vids = V(g_2mode)[(V(g_2mode)$name %in% (unique((events %>% filter(univ_id %in% (unique((univ_data %>% filter(control_ipeds == 'Private not-for-profit'))$univ_id_ipeds)), event_type == 'priv_hs'))$school_id))) | (V(g_2mode)$type==TRUE & V(g_2mode)$control_ipeds == "Private not-for-profit")]
    )
  
  # Create 2-mode objects that only contain vertices and edge associated with visits from public universities to private high schools
    g_2mode_pubu <- induced_subgraph(
      graph = g_2mode,
      vids = V(g_2mode)[(V(g_2mode)$name %in% (unique((events %>% filter(univ_id %in% (unique((univ_data %>% filter(control_ipeds == 'Public'))$univ_id_ipeds)), event_type == 'priv_hs'))$school_id))) | (V(g_2mode)$type==TRUE & V(g_2mode)$control_ipeds == "Public")]
    )

  # code from crystal to identify vertices associated with private high schools that received at least one visit from a private college/university
    
    # identify the vertices associated with private colleges/universities
      #V(g_2mode)[V(g_2mode)$type==TRUE & V(g_2mode)$control_ipeds == "Private not-for-profit"]
      
    # identify verticies of private high schools that received at least one visit from a private college/university; this creates a character vector
      #unique((events %>% filter(univ_id %in% (unique((univ_data %>% filter(control_ipeds == 'Private not-for-profit'))$univ_id_ipeds)), event_type == 'priv_hs'))$school_id)

    # same as above but returns an object that is a vertex sequence (underlying value is numeric and associated with a name attribute containing ID)
      # V(g_2mode)[(V(g_2mode)$name %in% (unique((events %>% filter(univ_id %in% (unique((univ_data %>% filter(control_ipeds == 'Private not-for-profit'))$univ_id_ipeds)), event_type == 'priv_hs'))$school_id))) | (V(g_2mode)$type==TRUE & V(g_2mode)$control_ipeds == "Private not-for-profit")]
    
    g_2mode_privu
    g_2mode_pubu
    
    # checks that object created correctly
      vertex_attr_names(graph = g_2mode_privu)
      V(g_2mode_privu)$type %>% str()
      edge_attr_names(graph = g_2mode_privu)
      E(g_2mode_privu)$weight %>% str()
      is_weighted(g_2mode_privu)
      is_bipartite(g_2mode_privu)
       

## ---------------------------
## Create igraph objects for one-mode analysis, but this time with vertex attributes consisting of HS and university characteristics
## ---------------------------
    
# what is a projection?
    # from: https://toreopsahl.com/tnet/two-mode-networks/
    # Transforming a two-mode network into a one-mode network is often referred to as projection. Although it is preferable to analyse networks in their original form, few methods exist for analysing two-mode networks. As such, projection is the only way to analyse these networks. The process works by selecting one set of nodes, and linking two nodes if they are connected to the same node of the other set. 

# Prior to creating projection, investigate "weight" attribute in igraph object g_2mode
  is_weighted(g_2mode) # TRUE
    
  edge_attr(graph = g_2mode, name = "weight", index = E(g_2mode))
  
  class(E(g_2mode)$weight) # integer
  attributes(E(g_2mode)$weight) # null
  
  E(g_2mode)$weight
  
  ecount(g_2mode) # 11034 edges; and edges have values weight values of 1 or more
  
  str(edge_attr(graph = g_2mode, name = "weight", index = E(g_2mode))) # integer vector of length = 11034
  str(E(g_2mode)$weight)
  

  # interpreting what the wights represent
    
    # first, what does each edge represent
      # g_2mode is a bipartite igraph object where vertices consist of two modes:
        # mode 1 = private high schools
        # mode 2 = postsecondary institutions
      # because g_2mode is a bipartite igraph object, edges can only exist between vertices of different modes
        # i.e., a high school (a vertex from mode 1) can receive a vist from a university (a vertex from mode 2)
        # but a high school (mode 1) cannot receive a visit from another high school (mode 1)
        # and a university (mode 2) cannot visit another university (mode 2)
      # edges occur between pairs of vertices; each pair consists of one high school (mode 1) and one university (mode 2)
        # the presence of an edge means that there was at least one visit from that university (mode 2) to that high school (mode 1)
      # there are 11,034 edges
        # this means that there were 11,034 pairs (each pair consisting of a spcific HS and a specific university) where the university visited the high school at least once
  
    # what do weights represent
      # there are 11034 edges and the edge attribute "weight" is a numeric vector of length == 11,034
        # in other words, each edge has a weight
        ecount(g_2mode)
        str(E(g_2mode)$weight) # length = 11,034
      # For a given edge (unique pair of a high school and a university) the weight attribute represents the number of visits from that university to that high school
        # minimum weight is 1 meaning that for that pair, there was only one visit from the university to the high school
          min(E(g_2mode)$weight)
        # maximum weight is 6 meaning that for that pair of high school i and university j, there were 6 visits from the university to the high school
          max(E(g_2mode)$weight)
  
  max(E(g_2mode)$weight)
  
# familiarize yourself w/ igraph::bipartite.projectio()
   #?bipartite.projection
  # syntax
    # bipartite_projection(graph, types = NULL, multiplicity = TRUE, probe1 = NULL, which = c("both", "true", "false"), remove.type = TRUE)
  
  # arguments
    # graph: 
      # input graph object
    # types
      # An optional vertex type vector to use instead of the ‘type’ vertex attribute. You must supply this argument if the graph has no ‘type’ vertex attribute.
    # multiplicity
      # If TRUE, then igraph keeps the multiplicity of the edges as an edge attribute called ‘weight’. E.g. if there is an A-C-B and also an A-D-B triple in the bipartite graph (but no more X, such that A-X-B is also in the graph), then the multiplicity of the A-B edge in the projection will be 2.
      # own: don't understand this
    # which = = c("both", "true", "false")
      # A character scalar to specify which projection(s) to calculate. The default is to calculate both.
    # probe1
      # This argument can be used to specify the order of the projections in the resulting list. If given, then it is considered as a vertex id (or a symbolic vertex name); the projection containing this vertex will be the first one in the result list
    # remove.type
      # Logical scalar, whether to remove the type vertex attribute from the projections. This makes sense because these graphs are not bipartite any more. However if you want to combine them with each other (or other bipartite graphs), then it is worth keeping this attribute. By default it will be removed
  
  vertex_attr_names(g_2mode)
  edge_attr_names(g_2mode)
  
    
  
# Create object that contains both one-mode objects; resulting object is a list of two elements, each element is a one-mode igraph object
  projection  <- bipartite_projection(graph = g_2mode, types = NULL, multiplicity = TRUE, which = c("both"), probe1 = NULL, remove.type = TRUE)
    # note: if multiplicity = FALSE, then resulting igraph objects will not contain the edge attribute "weight"
      # for one mode projection where vertices = private high schools:
        # each edge will represent a pair of high schools i and j that for which at least one university in the sample visited both high schools i and j
        # and the "weight" attribute will represent the number of universities in the sample that visited both high school i and high school j
      # for one mode pprojection where vertices = universities:
        # each edge will represent a pair of universities i and j that visited at least one high school in common
        # and the "weight" attribute will represent the number of private high schools that received a visit from both university i and university j
  

  class(projection) # class = list
  #str(projection) # two lists (each w/ 10 elements); list 1 name = "proj1"; list 2 name = "proj2"

# Create one mode igraph object where vertices are private high schools
  g_1mode_hs <- projection[["proj1"]]
  
  # investigate
  class(g_1mode_hs)
  g_1mode_hs
  
  # number of nodes
  vcount(g_1mode_hs)
  
  # number of edges
  ecount(g_1mode_hs) # 745,145 edges
    
  # maximum number of edges (undirected graph) = n(n-1)/2
    vcount(g_1mode_hs)*(vcount(g_1mode_hs) -1)/2 # = 1742*(1742-1)/2 = 1,516,411
    # so the network contains 745,145 of the potential 1,516,411 number of edges
    
  vertex_attr_names(g_1mode_hs)
  edge_attr_names(g_1mode_hs)
  
  # delete vertex attributes that are associated with university characteristics
  
    # create character vector that contains names of the vertex attributes you want to delete (i.e., those that end w/ suffix "_ipeds")
      str(vertex_attr_names(g_1mode_hs)) # character vector of length = 86
      
      ipeds_vattr <- str_subset(string = vertex_attr_names(g_1mode_hs), pattern = "_ipeds")
      ipeds_vattr

    # create loop that iterates once for each vertix attribute with suffix "_ipeds"; for each iteration, the loop deletes vertex attribute (with assignment)
      for (i in ipeds_vattr){
        #print(i)
        #g2<-delete_vertex_attr(g2, i)
        g_1mode_hs <- delete_vertex_attr(graph = g_1mode_hs, name = i)
      }
      vertex_attr_names(g_1mode_hs)
      str(vertex_attr_names(g_1mode_hs)) # now fewer vertex attributes
      
      #rm(ipeds_vattr)
      rm(i)
      
  # what does each vertex/node represent
    # g_1mode_hs is an igraph object where each vertex represents a private high school
  #what does each "edge" represent?
    # the 1-mode data has "edges" between individual high schools
    # the presence of an edge connecting high school i and high school j means that at least one college/university visited both high school i and high school j

  # what does the edge attribute named "weight" from g1_mode_hs 
  
    # for a given edge (two high schools that received a visit from the same college/university for at least one postsecondary institution in our sample),
      # weight attribute identifies that number of postsecondary institutions that visited both of these high schools
  
    is_weighted(g_1mode_hs)
    E(g_1mode_hs)$weight
    
    ecount(g_1mode_hs) # 745,145 edges
    str(E(g_1mode_hs)$weight) # numeric vector of length = 745,145
    
    
    max(E(g_1mode_hs)$weight) # max edge weight = 28
      # interpretation: for this edge (i.e., a pair of high schools), there were 28 colleges/universities in our sample that visited both of these two high schools
    
  # QUESTION: how could/are the "weight" attribute from the orignal 2-mode igraph object g_2mode incorporated into the 1-mode object?
    
    #E(g_2mode)$weight
      # each edge in g_2mode represents at least one vist to a pariticular high school i (mode 1) from a particular university j (mode 2)
      # E(g_2mode)$weight represents the number of visits to the pariticular high school i (mode 1) from a particular university j (mode 2)
    
    #E(g_1mode_hs)$weight
      # the presence of an edge connecting high school i and high school j means that at least one college/university visited both high school i and high school j      
      # for an edge connecting high school i and high school j, E(g_1mode_hs)$weight represents the number of universities in the sample that visited both high school i and high school j

    # ANSWER: Because E(g_2mode)$weight is an attribute of an edge between a particular university and a particular high school;
      # don't see how E(g_2mode)$weight could become part of the one-mode igraph object where vertices represent high schools and edges represent pairs of high schools that both receive at least one visit from university i
    

  #get.adjacency(g_1mode_hs, sparse = FALSE, attr = 'weight')
  #str(get.adjacency(g_1mode_hs, sparse = FALSE, attr = 'weight')) # 1742 by 1742 matrix
  
  #plot(g_1mode_hs, edge.label = E(g_1mode_hs)$weight)

# Create one mode igraph object where vertices are postsecondary institutions
  
  g_1mode_psi <- projection[["proj2"]]
  
  # investigate
  class(g_1mode_psi)
  g_1mode_psi
  
  vcount(g_1mode_psi)
  ecount(g_1mode_psi)
  
  # maximum number of number of possible edges (undirected graph) = n(n-1)/2
    vcount(g_1mode_psi)*(vcount(g_1mode_psi) -1)/2 # = 43*(43 - 1)/2 = 903
    # so the network contains 897 of 903 potential edges; pretty saturated  
    
  
  vertex_attr_names(g_1mode_psi)
  edge_attr_names(g_1mode_psi)

  # delete vertex attributes that are associated with university characteristics
  
    # create character vector that contains names of the vertex attributes you want to delete (i.e., those that end w/ suffix "_ipeds")
      str(vertex_attr_names(g_1mode_psi)) # character vector of length = 86
      
      pss_vattr <- str_subset(string = vertex_attr_names(g_1mode_psi), pattern = "_pss")
      str(pss_vattr)

    # create loop that iterates once for each vertix attribute with suffix "_ipeds"; for each iteration, the loop deletes vertex attribute (with assignment)
      for (i in pss_vattr){
        #print(i)
        #g2<-delete_vertex_attr(g2, i)
        
        # modify code so that below line is only run if the vertex attribute exists
        g_1mode_psi <- delete_vertex_attr(graph = g_1mode_psi, name = i)
      }
      vertex_attr_names(g_1mode_psi)
      str(vertex_attr_names(g_1mode_psi)) # now fewer vertex attributes
      #rm(pss_vattr)
      rm(i)
  
  # what does each vertex/node represent
    # g_1mode_psi is a 1-mode igraph object where each vertex represents a postsecondary institution
  #what does each "edge" represent?
    # the 1-mode data has "edges" between individual postsecondary institutions
    # each edge will represent a pair of universities i and j that visited at least one high school i in common
  # what does edge attribute "weight" represent; E(g_1mode_psi)$weight
    # the "weight" attribute will represent the number of private high schools that received a visit from both university i and university j

  # investigating weight attribute
    is_weighted(g_1mode_psi)
    
    E(g_1mode_psi)$weight
    
    ecount(g_1mode_psi) # 897
    str(E(g_1mode_psi)$weight) # numeric vector of length = 897
    
    # minimum weight attached to each edge == 1
      # for this edge (the pair of university i and university j), only one private high school received a visit from both university i and university j
      min(E(g_1mode_psi)$weight) # = 1
      
    # minimum weight attached to each edge == 329
      # for this edge (the pair of university i and university j), there were 329 private high schools that received a visit from both university i and university j
      max(E(g_1mode_psi)$weight) # = 329
      

  # create one mode objects from g_2mode_privu and g_2mode_pubu
    
    # one mode objects from g_2mode_privu
      projection_privu  <- bipartite_projection(graph = g_2mode_privu, types = NULL, multiplicity = TRUE, which = c("both"), probe1 = NULL, remove.type = TRUE)
      
      g_1mode_psi_privu <- projection_privu[["proj2"]]
      g_1mode_hs_privu <- projection_privu[["proj1"]]
      
      
      class(g_1mode_psi_privu)
      class(g_1mode_hs_privu)
    
    # one mode objects from g_2mode_pubu
      projection_pubu  <- bipartite_projection(graph = g_2mode_pubu, types = NULL, multiplicity = TRUE, which = c("both"), probe1 = NULL, remove.type = TRUE)
      
      g_1mode_psi_pubu <- projection_pubu[["proj2"]]
      g_1mode_hs_pubu <- projection_pubu[["proj1"]]

      class(g_1mode_psi_pubu)
      class(g_1mode_hs_pubu)
          
    
    # remove vertex attributes we don't need from 1-mode objects
      pss_vattr
      ipeds_vattr
    
      # remove vertex attributes associated with private high schools from 1-mode objects where vertices are always universities 
      for (i in pss_vattr){

        g_1mode_psi_privu <- delete_vertex_attr(graph = g_1mode_psi_privu, name = i)
        g_1mode_psi_pubu <- delete_vertex_attr(graph = g_1mode_psi_pubu, name = i)
      }
      vertex_attr_names(g_1mode_psi_privu)
      vertex_attr_names(g_1mode_psi_pubu)
      
      # remove vertex attributes associated with universities from 1-mode objects where vertices are always private high schools
      for (i in ipeds_vattr){

        g_1mode_hs_privu <- delete_vertex_attr(graph = g_1mode_hs_privu, name = i)
        g_1mode_hs_pubu <- delete_vertex_attr(graph = g_1mode_hs_pubu, name = i)
      }
      vertex_attr_names(g_1mode_hs_privu)
      vertex_attr_names(g_1mode_hs_pubu)
      
      # remove work objects
      rm(i)
      rm(pss_vattr,ipeds_vattr)
      rm(projection,projection_privu,projection_pubu)
      rm(privhs_per_privunivs)
    
## ---------------------------
## Create ego network igraph objects 
## ---------------------------

#?igraph::make_ego_graph

# syntax:
  #make_ego_graph(graph, order = 1, nodes = V(graph), mode = c("all","out", "in"), mindist = 0)

#arguments      
  #graph	
    #The input graph.
  #order	
    #Integer giving the order of the neighborhood.
  #nodes	
    #The vertices for which the calculation is performed.
    # default is all nodes (i.e., nodes = V(graph))
  #mode	
    #Character constant, it specifies how to use the direction of the edges if a directed graph is analyzed. 
      #For ‘out’ only the outgoing edges are followed, so all vertices reachable from the source vertex in at most order steps are counted. For ‘"in"’ all vertices from which the source vertex is reachable in at most order steps are counted. ‘"all"’ ignores the direction of the edges. This argument is ignored for undirected graphs.
  #mindist	
    #The minimum distance to include the vertex in the result.  

# create objects, where each element is an ego network
      
  # Choosing which nodes from igraph object to make ego network for
    # PSI (make one ego network igraph object for each PSI)
      length(make_ego_graph(graph = g_2mode, order = 1, nodes = V(graph = g_2mode)[V(g_2mode)$type == TRUE], mindist = 0))
      
    # high school (make one ego network igraph object for each high school)
      length(make_ego_graph(graph = g_2mode, order = 1, nodes = V(graph = g_2mode)[V(g_2mode)$type == FALSE], mindist = 0))
      
      ego(graph = g_2mode, order =1, nodes = V(graph = g_2mode)[V(g_2mode)$type == TRUE], mindist = 0)[2] # 160 of 1785 vertices
      ego(graph = g_2mode, order =1, nodes = V(graph = g_2mode)[V(g_2mode)$type == TRUE], mindist = 1)[2] # 159 of 1785 vertices
      ego(graph = g_2mode, order =2, nodes = V(graph = g_2mode)[V(g_2mode)$type == TRUE], mindist = 1)[2] # 201 of 1785 vertices
      
      ego_size(graph = g_2mode, order =2, nodes = V(graph = g_2mode)[V(g_2mode)$type == TRUE], mindist = 1)[2] # 202 of 1785 vertices
      
  # create one ego network per private HS
    egos_hs <- make_ego_graph(graph = g_2mode, order = 2, nodes = V(graph = g_2mode)[V(g_2mode)$type == FALSE], mindist = 0)
    
    # assign name for each ego network
      # object has one element per ego network; assign name of element equal to the NCES school ID #
      #names(egos_hs)
      #length(egos_hs)
      #length(privhs_vec)
      names(egos_hs) <- privhs_vec
      
  # create one ego network per psi
    #?make_ego_graph
    egos_psi <- make_ego_graph(
      graph = g_2mode, 
      order = 2, # The neighborhood of a given order o of a vertex v includes all vertices which are closer to v than the order. Ie. order 0 is always v itself, order 1 is v plus its immediate neighbors, order 2 is order 1 plus the immediate neighbors of the vertices in order 1, etc.
      nodes = V(graph = g_2mode)[V(g_2mode)$type == TRUE], 
      mindist = 0 # default = 0 means include itself as a node
    )
    #str(egos_psi)
    #V(graph = g_2mode)[V(g_2mode)$type == TRUE]
    #as.integer(V(graph = g_2mode)[V(g_2mode)$type == TRUE])
    
    # assign name for each ego network
      # object has one element per ego network; assign name of element equal to the IPEDS ID #
      names(egos_psi) <- str_extract(univ_vec, pattern = "\\d*") # this uses regular expressions to extract the "_req" from "155317_req"
        #names(egos_psi) <- univ_vec # this doesn't extract "_req" from "155317_req"
  
    # remove attributes that don't have utility for ego network? 
      # no. potentially all of them could be used

    # investigate creating edge order as an edge attribute for one element (i.e., one ego network of a university) from object egos_psi
      
      #edge where the attribute name of the edge contains "106397"
        str_subset(string = attr(x = E(egos_psi[["106397"]]), which = "vnames"), pattern ="106397")
        str_detect(string = attr(x = E(egos_psi[["106397"]]), which = "vnames"), pattern ="106397")

      #edge where the attribute name of the edge contains does not contain "106397"
        str_subset(string = attr(x = E(egos_psi[["106397"]]), which = "vnames"), pattern ="^((?!106397).)*$")
      
    # loop through each element and assign order as an edge attribute
      # edge order = 1: edges for visits by ego to private high schools
      # edge order = 2: for private high schools visited by ego, which universities also visited the high school
        
      length(egos_psi)
      names(egos_psi)
      length(names(egos_psi))
      
      for (i in 1:length(egos_psi)) {
      
        writeLines(str_c("i=",i,"; univ id=",names(egos_psi)[[i]]))
        
        E(egos_psi[[i]])$order <- if_else(str_detect(string = attr(x = E(egos_psi[[i]]), which = "vnames"), pattern =names(egos_psi)[[i]]),1,2)

        # investigate        
          #print(edge_attr_names(egos_psi[[i]]))
        
          # summary table of edge attribute order
          print(table(E(egos_psi[[i]])$order))
      }
      rm(i)

    # new ego objects to create:
      # from g_2mode_privu create:
        # egos_hs_privu
        egos_hs_privu <- make_ego_graph(graph = g_2mode_privu, order = 2, nodes = V(graph = g_2mode_privu)[V(g_2mode_privu)$type == FALSE], mindist = 0)
        names(egos_hs_privu) <- unique((events %>% filter(univ_id %in% (unique((univ_data %>% filter(control_ipeds == 'Private not-for-profit'))$univ_id_ipeds)), event_type == 'priv_hs'))$school_id)
      
        # egos_psi_privu
        egos_psi_privu <- make_ego_graph(
          graph = g_2mode_privu, 
          order = 2, # The neighborhood of a given order o of a vertex v includes all vertices which are closer to v than the order. Ie. order 0 is always v itself, order 1 is v plus its immediate neighbors, order 2 is order 1 plus the immediate neighbors of the vertices in order 1, etc.
          nodes = V(graph = g_2mode_privu)[V(g_2mode_privu)$type == TRUE], 
          mindist = 0 # default = 0 means include itself as a node
        )
        
        names(egos_psi_privu) <- unique((univ_data %>% filter(control_ipeds == 'Private not-for-profit'))$univ_id_ipeds)
        
        for (i in 1:length(egos_psi_privu)) {
          writeLines(str_c("i=",i,"; univ id=",names(egos_psi_privu)[[i]]))
          E(egos_psi_privu[[i]])$order <- if_else(str_detect(string = attr(x = E(egos_psi_privu[[i]]), which = "vnames"), pattern =names(egos_psi_privu)[[i]]),1,2)
          print(table(E(egos_psi_privu[[i]])$order))
        }
      
      # from g_2mode_pubu create:
        # egos_hs_pubu
        egos_hs_pubu <- make_ego_graph(graph = g_2mode_pubu, order = 2, nodes = V(graph = g_2mode_pubu)[V(g_2mode_pubu)$type == FALSE], mindist = 0)
        names(egos_hs_pubu) <- unique((events %>% filter(univ_id %in% (unique((univ_data %>% filter(control_ipeds == 'Public'))$univ_id_ipeds)), event_type == 'priv_hs'))$school_id)
        
        # egos_psi_pubu
        egos_psi_pubu <- make_ego_graph(
          graph = g_2mode_pubu, 
          order = 2, # The neighborhood of a given order o of a vertex v includes all vertices which are closer to v than the order. Ie. order 0 is always v itself, order 1 is v plus its immediate neighbors, order 2 is order 1 plus the immediate neighbors of the vertices in order 1, etc.
          nodes = V(graph = g_2mode_pubu)[V(g_2mode_pubu)$type == TRUE], 
          mindist = 0 # default = 0 means include itself as a node
        )
        
        names(egos_psi_pubu) <- unique((univ_data %>% filter(control_ipeds == 'Public'))$univ_id_ipeds)
        
        for (i in 1:length(egos_psi_pubu)) {
          writeLines(str_c("i=",i,"; univ id=",names(egos_psi_pubu)[[i]]))
          E(egos_psi_pubu[[i]])$order <- if_else(str_detect(string = attr(x = E(egos_psi_pubu[[i]]), which = "vnames"), pattern =names(egos_psi_pubu)[[i]]),1,2)
          print(table(E(egos_psi_pubu[[i]])$order))
        }
      
