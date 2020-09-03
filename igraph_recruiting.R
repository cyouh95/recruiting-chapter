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

options(max.print=999)

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
    univ_vec
  rm(univ_sample)

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
      if (nrow(privhs_events %>% filter(school_id == i, univ_id_req == j)) > 0) {
        m[i, j] <- privhs_events %>% filter(school_id == i, univ_id_req == j) %>% nrow()
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
  
  , edges only exist between 
  
  max(E(g_2mode)$weight)
  
# familiarize yourself w/ igraph::bipartite.projectio()
   ?bipartite.projection
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
  
  temp <- bipartite_projection(graph = g_2mode, types = NULL, multiplicity = FALSE, which = c("both"), probe1 = NULL, remove.type = TRUE)
  temp_hs <- temp[["proj1"]]
  
  vertex_attr_names(temp_hs)
  edge_attr_names(temp_hs)
  
  
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

    # create loop that iterates once for each vertix attribute with suffix "_ipeds"; for each iteration, the loop deletes vertex attribute (with assignment)
      for (i in ipeds_vattr){
        #print(i)
        #g2<-delete_vertex_attr(g2, i)
        g_1mode_hs <- delete_vertex_attr(graph = g_1mode_hs, name = i)
      }
      vertex_attr_names(g_1mode_hs)
      str(vertex_attr_names(g_1mode_hs)) # now fewer vertex attributes
      
      rm(ipeds_vattr)
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
      rm(pss_vattr)
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
