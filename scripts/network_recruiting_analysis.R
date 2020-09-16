################################################################################
##
## [ PROJ ] < Burd recruiting chapter >
## [ FILE ] < network_recruiting_analysis.R >
## [ AUTH ] < Ozan Jaquette / ozanj >
## [ INIT ] < 9/4/2020 >
##
################################################################################

rm(list = ls())

options(max.print=9999)

## ---------------------------
## libraries
## ---------------------------

library(igraph)
library(tidyverse)
library(Hmisc)

## ---------------------------
## directory paths
## ---------------------------

getwd()
list.files()

data_dir <- file.path(".", "data")
scripts_dir <- file.path(".", "scripts")


## ---------------------------
## Run script that creates igraph objects
## ---------------------------

#file.path(scripts_dir,"create_igraph_objects.R")
source(file = file.path(scripts_dir,"create_igraph_objects.R"))


## ---------------------------
## Visits by private colleges and universities to private high schools
## ---------------------------

# objects wi will potentially use for analyses

  g_2mode_privu # 2-mode object, only visits by private colleges and universities
  g_1mode_hs_privu # 1 mode object, nodes = private high schools that received at least one visit from a private college/university
  g_1mode_psi_privu  # 1 mode object, nodes = private colleges/universities
  
  egos_psi_privu %>% length() # object that has one element per ego network for each private university
    # egos_psi_privu[["139658"]] # emory university

  egos_hs_privu %>% length() # object that has one element per ego network for each private high school
  

  # plotting ego network of emory university
    plot.igraph(
      x = egos_psi_privu[["139658"]], # egos_psi[["106397"]] = uarkansas
      #vertex.label = "",
      vertex.label = if_else(V(egos_psi_privu[["139658"]])$type, V(egos_psi_privu[["139658"]])$univ_abbrev_ipeds, ""),
      vertex.shape = if_else(V(egos_psi_privu[["139658"]])$type, "square", "circle"),
      vertex.color = if_else(V(egos_psi_privu[["139658"]])$type, "lightblue", "salmon"),
      vertex.size = if_else(V(egos_psi_privu[["139658"]])$type, 5, 3),
      edge.color = "lightgrey",
      edge.lty = if_else(E(egos_psi_privu[["139658"]])$order ==1, 0, 1),
      # main = "my plot name folks"
      layout = layout_with_kk, # layout_with_kk, layout_with_fr, layout_in_circle, layout_nicely
    )
    
      # playing with edge attributes
      egos_psi_privu[["139658"]] %>% E() # 2,549
      edge_attr_names(egos_psi_privu[["139658"]])
      
      E(egos_psi_privu[["139658"]])$weight
      E(egos_psi_privu[["139658"]])$order

      edge.color = "lightgrey", 
      edge.lty = 2, # 0 and “blank” mean no edges, 1 and “solid” are for solid lines, the other possible values are: 2 (“dashed”), 3 (“dotted”), 4 (“dotdash”), 5 (“longdash”), 6 (“twodash”).
    
    # plot ego network subgraph, order =1 only
      plot.igraph(
        x = subgraph.edges(graph = egos_psi_privu[["139658"]], eids = E(egos_psi_privu[["139658"]])[E(egos_psi_privu[["139658"]])$order==1]),
        #vertex.label = "",
        vertex.label = NA,
        vertex.shape = "circle",
        vertex.color = "salmon",
        vertex.size = 3,
        layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
        # main = "my plot name folks"
      )

    # plot ego network subgraph, order = 2 only
      plot.igraph(
        x = subgraph.edges(graph = egos_psi_privu[["139658"]], eids = E(egos_psi_privu[["139658"]])[E(egos_psi_privu[["139658"]])$order==2]),
        vertex.label = if_else(V(egos_psi_privu[["139658"]])$type, V(egos_psi_privu[["139658"]])$univ_abbrev_ipeds, ""),
        vertex.shape = if_else(V(egos_psi_privu[["139658"]])$type, "square", "circle"),
        vertex.color = if_else(V(egos_psi_privu[["139658"]])$type, "lightblue", "salmon"),
        vertex.size = if_else(V(egos_psi_privu[["139658"]])$type, 5, 3),
        layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
      )
      
      # which vertices included in graph of order = 2
        subgraph.edges(graph = egos_psi_privu[["139658"]], eids = E(egos_psi_privu[["139658"]])[E(egos_psi_privu[["139658"]])$order==2]) %>% V() %>% print(full = T)
         #subgraph.edges(graph = egos_psi_privu[["139658"]], eids = E(egos_psi_privu[["139658"]])[E(egos_psi_privu[["139658"]])$order==2]) %>% E() %>% print(full = T)
      
      # which verticies included in graph of order = 1 or 2
        egos_psi_privu[["139658"]] %>% V() %>% print(full = T)
      
         plot.igraph(
          x = ego_emory_order2, # emory
          vertex.label = if_else(V(ego_emory_order2)$type, V(ego_emory_order2)$univ_abbrev_ipeds, ""),
          vertex.shape = if_else(V(ego_emory_order2)$type, "square", "circle"),
          vertex.color = if_else(V(ego_emory_order2)$type, "lightblue", "salmon"),
          vertex.size = if_else(V(ego_emory_order2)$type, 5,3),
          layout = layout_with_kk,
          #layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
        )
          
# What would we show the reader? using Notre Dame as example; 
         
  #  overall approach: start local and simple and move towards more complicated
         
  # first plot ego network, and only high schools visited by notre dame
    # plot ego network subgraph, order =1 only

      # create object for order = 1 of nd ego network
      nd_order1 <- subgraph.edges(graph = egos_psi_privu[["152080"]], eids = E(egos_psi_privu[["152080"]])[E(egos_psi_privu[["152080"]])$order==1])

          
      #  !!!!CRYSTAL!!!! try having color of high school vertices determined by vertex attributes of high school
        # try creating small multiples that give different colors/shapes to nodes depending on values of specific vertex attributes

        vertex_attr_names(nd_order1)
        # religious affiliation
        # academic reputation as defined by niche grade
        # geographic region [northeast, midwest, west, south]
        # racial composition [3-4 categories depending on percent white]

      # plot
      plot.igraph(
        x = nd_order1,
        #vertex.label = "",
        vertex.label = if_else(V(nd_order1)$name =="152080", V(nd_order1)$univ_abbrev_ipeds, ""),
        vertex.shape = "circle",
        vertex.color = if_else(V(nd_order1)$name =="152080", "lightblue", "salmon"),
        vertex.size = if_else(V(nd_order1)$name =="152080", 6, 3),
        edge.lty = if_else(E(nd_order1)$weight ==1, 3, 1),
        edge.width = if_else(E(nd_order1)$weight ==1, .5, as.numeric(E(nd_order1)$weight)),
        layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle, layout_nicely
        # main = "my plot name folks"
      )
      
      
  # next, examine the private colleges and universities that visited the private high schools that notre dame visited
    plot.igraph(
      x = egos_psi_privu[["152080"]], # egos_psi[["106397"]] = uarkansas
      #vertex.label = "",
      vertex.label = if_else(V(egos_psi_privu[["152080"]])$type, V(egos_psi_privu[["152080"]])$univ_abbrev_ipeds, ""),
      vertex.shape = if_else(V(egos_psi_privu[["152080"]])$type, "circle", "circle"),
      vertex.color = if_else(V(egos_psi_privu[["152080"]])$type, "lightblue", "salmon"),
      vertex.size = if_else(V(egos_psi_privu[["152080"]])$type, 6, 2),
      edge.color = "lightgrey",
      edge.lty = if_else(E(egos_psi_privu[["152080"]])$order ==1, 0, 1),
      # main = "my plot name folks"
      layout = layout_with_kk, # layout_with_kk, layout_with_fr, layout_in_circle, layout_nicely
    )
    
    # maybe create separate small multiples to highlight different vertex attributes
    
  # [probably becomes one modest sized table of simple descriptive stats] next identify which high schools are most central in the notre dame ego network [MAYBE BASE OF g_2mode_priv rather than ego network of notre dame]
    
    # of the high schools that notre dame visited; which schools were also visited by lots of other private universities in our sample
    # maybe provide a table of descritpve statistics that convey which high high schools and high school characteristics are associated with being visited by many other universities (in addition to notre dame)
    
    # what are we trying to show here: which high schools visited by notre dame are visited by many private col/univ in our sample vs. high schools not visited by many private col/univ in our sample
    
    # rationale for basing this of ND ego network rather than g_2mode_privu:
      # because we just showed graphs of ND ego network
    # potential measures of centrality: degree centrality; closeness centrality; betweenness centrality
    
      # degree centrality = number of edges incident to vertex; i.e., number of neighbors of order =1
      
      egos_psi_privu[["152080"]] %>% vcount()
      
        # create vertex attributes of degree centrality
      
          V(egos_psi_privu[["152080"]])$degree <- degree(egos_psi_privu[["152080"]])
          V(egos_psi_privu[["152080"]])$strength <- strength(egos_psi_privu[["152080"]])
          V(egos_psi_privu[["152080"]])$closeness <- closeness(graph = egos_psi_privu[["152080"]], normalized = T)

          
            V(egos_psi_privu[["152080"]])$degree %>% length()
            V(egos_psi_privu[["152080"]]) %>% length()          
          
          # private schools

            degree(egos_psi_privu[["152080"]])[V(egos_psi_privu[["152080"]])$type == F] %>% sort(decreasing = T)
            
            V(egos_psi_privu[["152080"]])$degree %>% str() # numeric vector 
            V(egos_psi_privu[["152080"]])$degree[V(egos_psi_privu[["152080"]])$type == F] %>% sort(decreasing = T)
            
            temp <- data.frame(
              id = V(egos_psi_privu[["152080"]])$name,
              name_pss = V(egos_psi_privu[["152080"]])$name_pss,
              city = V(egos_psi_privu[["152080"]])$city_pss,
              state = V(egos_psi_privu[["152080"]])$state_code_pss,
              type = V(egos_psi_privu[["152080"]])$type,
              degree = V(egos_psi_privu[["152080"]])$degree,
              strength = V(egos_psi_privu[["152080"]])$strength,
              closeness = V(egos_psi_privu[["152080"]])$closeness,
              name_ipeds = V(egos_psi_privu[["152080"]])$univ_name_ipeds
            )
            # which high schools visited by notre dame were most visited by other universities
            temp %>% filter(type == F) %>% arrange(-degree) %>% select(id, name_pss, city, state, degree, strength)

            # which universities visited the greatest number of high schools that were also visited by Notre Dame            
            temp %>% filter(type == T) %>% arrange(-degree) %>% select(id, name_ipeds, degree, strength)


      # closeness centrality
        # Closeness centrality measures attempt to capture the notion that a vertex is ‘central’ if it is ‘close’ to many other vertices. 
          # The standard approach, introduced by Sabidussi [21], is to let the centrality vary inversely with a measure of the total distance 
          # of a vertex from all others,

            # closeness centrality, high schools
            temp %>% filter(type == F) %>% arrange(-closeness) %>% select(id, name_pss, city, state, closeness, degree, strength)

            # closeness centrality, universities
            temp %>% filter(type == T) %>% arrange(-closeness) %>% select(id, name_ipeds, closeness, degree, strength)
            
      # betweenness centrality
        # Betweenness centrality measures are aimed at summarizing the extent to which a vertex is located ‘between’ other pairs of vertices. These centralities are based upon the perspective that ‘importance’ relates to where a vertex is located with respect to the paths in the network graph. If we picture those paths as the routes by which, say, communication of some sort or another takes place, vertices that sit on many paths are likely more critical to the communication process.

        # don't think betweenness centrality or eigenvector centrality are relevant for off-campus recruiting networks          
            
# then move away from notre dame ego network and examine entire network of visits to private schools by private colleges/universities in our sample
    
    g_2mode_privu
    
    # what do we want to convey?
      # which high schools are most central; the characteristics of these high schools
        
      # which types of private colleges/universities visit types of high schools

    # create vertex attributes of degree centrality
        V(g_2mode_privu)$degree <- degree(g_2mode_privu)
        V(g_2mode_privu)$strength <- strength(g_2mode_privu)
        V(g_2mode_privu)$closeness <- closeness(graph = g_2mode_privu, normalized = T)
        
            temp <- data.frame(
              id = V(g_2mode_privu)$name,
              name_pss = V(g_2mode_privu)$name_pss,
              city = V(g_2mode_privu)$city_pss,
              state = V(g_2mode_privu)$state_code_pss,
              type = V(g_2mode_privu)$type,
              degree = V(g_2mode_privu)$degree,
              strength = V(g_2mode_privu)$strength,
              closeness = V(g_2mode_privu)$closeness,
              name_ipeds = V(g_2mode_privu)$univ_name_ipeds
            )
            temp %>% glimpse()
            # which high schools visited by notre dame were most visited by other universities
            temp %>% filter(type == F) %>% arrange(-degree) %>% select(id, name_pss, city, state, degree, strength)
    

      # closeness centrality
        # Closeness centrality measures attempt to capture the notion that a vertex is ‘central’ if it is ‘close’ to many other vertices. 
          # The standard approach, introduced by Sabidussi [21], is to let the centrality vary inversely with a measure of the total distance 
          # of a vertex from all others,

            # closeness centrality, high schools
            temp %>% filter(type == F) %>% arrange(-closeness) %>% select(id, name_pss, city, state, closeness, degree, strength)

    # which types of private colleges/universities visit types of high schools [focus on geography, academic reputation, religion, race]
        
        # ways to approach this:
            # what other simple descriptive statistics (network measures and/or non-network measures) could be used to get at above question?
              # review standard measures of homophily/heterophily
            # create communities using graph partitioning
            
            
          # START HERE 9/17/2020