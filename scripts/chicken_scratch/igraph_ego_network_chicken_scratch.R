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



      summary(E(egos_psi[[2]])$order)
      print(edge_attr_names(egos_psi[[43]]))
      edge_attr_names(egos_psi[[43]])
      
      print(table(E(egos_psi[[2]])$order))
      
      E(egos_psi[[i]])$order <- if_else(str_detect(string = attr(x = E(ego_uarkansas), which = "vnames"), pattern ="106397"),1,2)      
      
      str(attr(x = E(ego_uarkansas), which = "vnames"))
      
      str_detect(string = attr(x = E(egos_psi[[2]]), which = "vnames"), pattern =names(egos_psi)[[2]])
      
      summary(str_detect(string = attr(x = E(egos_psi[[2]]), which = "vnames"), pattern =names(egos_psi)[[2]]))
      summary(str_detect(string = attr(x = E(egos_psi[[2]]), which = "vnames"), pattern ="106397"))
      
      class(E(egos_psi[[2]]))
      
      names(egos_psi)[[2]]
      str(names(egos_psi)[[2]])
      
  # Create separate object for u arkansas ego network
    ego_uarkansas <- egos_psi[["106397"]]

    # see if you can create an edge attribute that is order of the network
      ego_uarkansas
      vcount(ego_uarkansas)
      ecount(ego_uarkansas)
      as.integer(E(ego_uarkansas))
      str(E(ego_uarkansas))
      
      attr(x = E(ego_uarkansas), which = "vnames")
      str(attr(x = E(ego_uarkansas), which = "vnames"))
      #edge where the attribute name of the edge contains "100751"

      str_subset(string = attr(x = E(ego_uarkansas), which = "vnames"), pattern ="106397")
      str_detect(string = attr(x = E(ego_uarkansas), which = "vnames"), pattern ="106397")
      #if_else(condition,true,false,missing=NULL) # 
      ifelse()
      E(ego_uarkansas)$order <- if_else(str_detect(string = attr(x = E(ego_uarkansas), which = "vnames"), pattern ="106397"),1,2)
      E(ego_uarkansas)$order
      
      str_subset(string = attr(x = E(ego_uarkansas), which = "vnames"), pattern ="106397")
        # str(str_subset(string = attr(x = E(ego_uarkansas), which = "vnames"), pattern ="106397"))
      
      #edge where the attribute name of the edge contains does not contain "100751"
      str(str_subset(string = attr(x = E(ego_uarkansas), which = "vnames"), pattern ="^((?!106397).)*$"))
    
  # graph ego network    
    plot.igraph(
      x = ego_uarkansas, 
      #vertex.label = "",
      vertex.label = if_else(V(ego_uarkansas)$type, V(ego_uarkansas)$univ_abbrev_ipeds, ""),
      vertex.shape = if_else(V(ego_uarkansas)$type, "square", "circle"),
      vertex.color = if_else(V(ego_uarkansas)$type, "lightblue", "salmon"),
      vertex.size = if_else(V(ego_uarkansas)$type, 5, 3),
      layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
      main = "my plot name folks"
    )
          
      
      #ego_uarkansas <- egos_psi[[2]]
      
      
      #examine edges
      ego_uarkansas
      ego(graph = ego_uarkansas, order =1)
      str(ego_size(graph = ego_uarkansas, order =0))
      
      class(ego_uarkansas)
      str(E(ego_uarkansas))
      attributes(E(ego_uarkansas))
      
      attr(x = E(ego_uarkansas), which = "vnames")
      str(attr(x = E(ego_uarkansas), which = "vnames"))
      
      
      str(str_subset(string = attr(x = E(ego_uarkansas), which = "vnames"), pattern ="!(106397)"))
      
        str(attr(x = E(ego_uarkansas), which = "vnames"))
        str(str_subset(string = attr(x = E(ego_uarkansas), which = "vnames"), pattern ="106397"))
        
        str_subset(string = attr(x = E(ego_uarkansas), which = "vnames"), (!pattern = "106397"))
        
        str(str_subset(string = attr(x = E(ego_uarkansas), which = "vnames"), !(pattern = "106397")))
      
      class(ego_uarkansas)
      ego_uarkansas
      str(ego_uarkansas)
      vcount(ego_uarkansas) # number of vertices; when order = 1; vertices = itself and all nodes that it has a direct ties with
        # vertices connected to ego network by 
        #ties of order = 1 will always be high schools because in off-campus recruiting network we created, universities do not visit other universities
        # ties of order = 2 will always be universities?
      ecount(ego_uarkansas) # number of edges; when order = 1
      vcount(ego_uarkansas)
        str(V(ego_uarkansas))

        ego_size(graph =ego_uarkansas, order = 0)
        ego_size(graph =ego_uarkansas, order = 1)
        ego_size(graph =ego_uarkansas, order = 2)
        ego_size(graph =ego_uarkansas, order = 3)
        ego_size(graph =ego_uarkansas, order = 4)
        
        
        ego(graph =ego_uarkansas, order = 0)
        ego(graph =ego_uarkansas, order = 1)
        
        ego(graph =ego_uarkansas, order = 0)[1]
        ego(graph =ego_uarkansas, order = 1)[1]
        ego(graph =ego_uarkansas, order = 2)[1] # ? what does this represent?
      
    
    
    edge_attr_names(ego_uarkansas)
    
    str(E(ego_uarkansas)$weight)
    E(ego_uarkansas)$weight
    
    
    vertex_attr_names(ego_uarkansas)
    
    V(ego_uarkansas)$type # false for all obs (nodes) except the last one which is u. arkansas
    V(ego_uarkansas)$control_ipeds # missing except for last node, which is u. arkansas
    V(ego_uarkansas)$school_type_pss
    V(ego_uarkansas)$state_code_pss # missing for last node (u. arkansas); and als NA for obs that didn't merge to 2017-18 pss
    
    
      
  
V(ego_uarkansas)$type
      
      str(ego_uarkansas)
      
      str(egos_psi[[2]])
      str(egos_psi[2])
      
      univ_data
      
      
      str(egos_hs[[10]])
      egos_hs[[10]]
      egos_hs[10]
      
      
    univ_vec
    
    
    egos_psi[["168342"]]
    str(egos_psi[["168342"]])
    
    names(egos_psi)
    str(egos_psi)
    class(egos_psi)
    length(egos_psi)
    #egos_psi_order2 <- make_ego_graph(graph = g_2mode, order = 2, nodes = V(graph = g_2mode)[V(g_2mode)$type == TRUE], mindist = 0)
    
    str(egos_psi)
    length(egos_psi[[1]])
    length(egos_psi[[1]][1])
    attributes(egos_psi[[1]][1])
    
    class(egos_psi[[1]])
    class(egos_psi[[1]][1])
    
    str(egos_psi[[1]][1])
    str(egos_psi[[1]])
    
    
    
    str(egos_psi[[2]])
    egos_psi[[2]]
    
    str(egos_psi_order2[[2]])
    egos_psi_order2[[2]]
    
    
    egos_psi[[1]]
    egos_psi_order2[[1]]
    
    length(egos_psi)
    names(egos_psi)
    
    egos_psi[[1]]
    str(egos_psi[[1]])
    attributes(egos_psi[[1]])
    vertex_attr_names(egos_psi[[1]])
    V(egos_psi[[1]])$name
    
    edge_attr_names(egos_psi[[1]])
    graph_attr_names(egos_psi[[1]])
    
    
    vertex_attr_names(egos_psi[[5]])
    V(egos_psi[[5]])$name
    
  # create one ego network per private HS
    egos_hs <- make_ego_graph(graph = g_2mode, order = 1, nodes = V(graph = g_2mode)[V(g_2mode)$type == FALSE], mindist = 0)
    
    
length(make_ego_graph(graph = g_2mode, order = 1, nodes = V(graph = g_2mode)[V(g_2mode)$type == TRUE], mindist = 0))
length(make_ego_graph(graph = g_2mode, order = 1, nodes = V(graph = g_2mode)[V(g_2mode)$type == FALSE], mindist = 0))
V(graph = g_2mode)
str(V(graph = g_2mode)) # named integer vector
V(graph = g_2mode)[V(g_2mode)$type == TRUE]

str(V(graph = g_2mode)[V(g_2mode)$type == TRUE])

?igraph::V

temp <- make_ego_graph(graph = g_2mode, order = 1, nodes = V(graph = g_2mode)[V(g_2mode)$type == TRUE], mindist = 0)
temp2 <- temp[[1]]

str(temp[[1]])
class(temp[[1]])

class(temp2)
temp2

class(temp2)
ecount(temp2)
vcount(temp2)

class(egos_psi[[4]])
ecount(egos_psi[[4]])
vcount(egos_psi[[4]])
str(egos_psi[[4]])

str(egos_psi[[5]])
egos_psi[[5]]