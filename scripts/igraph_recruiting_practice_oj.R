

################################################################################
##
## [ PROJ ] < Burd recruiting chapter >
## [ FILE ] < igraph_recruiting_practice_oj.R >
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

###########################
########################### 

## ---------------------------
## Practice network visualization
## ---------------------------

# resources I am drawing from:
  # Bipartite/Two-Mode Networks in igraph by Phil Murphy & Brendan Knapp
    # https://rpubs.com/pjmurphy/317838
  # ?plot.igraph
  # igraph manual on plotting
    # ?igraph.plotting


# igraph approaches for visualization
    # ?plot.igraph
    # igraph manual on plotting
      # ?igraph.plotting

# syntax for plot.igraph:
  # plot(x, axes = FALSE, add = FALSE, xlim = c(-1, 1), ylim = c(-1, 1), mark.groups = list(), mark.shape = 1/2,
    #mark.col = rainbow(length(mark.groups), alpha = 0.3),mark.border = rainbow(length(mark.groups), alpha = 1), mark.expand = 15, ...)

# Three ways to give values to the parameters of the plot functions
  # 1. supply named arguments to the plotting commands
  # 2. assign vertex, edge, and graph attributes to the graph
    # The second way is to assign vertex, edge and graph attributes to the graph. These attributes have no prefix, ie. the color of the vertices is taken from the color vertex attribute and the color of the edges from the color edge attribute. 
  # 3. igraph_options
    # setting parameters using igraph_options is "useful if you want all or most of your graphs to have the same look, vertex size, vertex color, etc. Then you don't need to set these at every plotting, and you also don't need to assign vertex/edge attributes to every graph."
    # 
  
# Setting vertex parameters
  # when setting vertex parameters within igraph.plot() or within igraph_options(), the  ‘vertex.’ prefix needs to be added
  # the value of vertex varameter may be a scalar (i.e., single value) that is valid for every vertex OR a vecter with a separate value for each vertex (shorter vectors recycled which you probably don't want)

  # some particular vertex parameters
    # label.family: font family to be used for vertex labels
    # label.font: the font within the font family to be used for vertex labels
    # label.cex: font size for vertex labels
    # label.dist: distance of the label from the center of the vertex
    # label.degree: defines position of the vertex labels relative to center of the vertices (interpreted as an angle in radian)
    # label.color: color of the labels
    

  # set plot aesthetics (e.g., color, font, labels, etc.) by setting aesthetics as vertex attributes

  # one mode object
    vertex_attr_names(g_1mode_psi)
    
    V(g_1mode_psi)$color <- "green"
    plot(x = g_1mode_psi)

    plot(
      x = g_1mode_psi, 
      vertex.label = V(g_2mode)$univ_abbrev_ipeds)
    )    

    # color of node should depend on whether it is a public or private university
    plot(
      x = g_1mode_psi, 
      vertex.color = if_else(V(g_1mode_psi)$control_ipeds == "Public", "green", "red"),
      vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds
    )


      vertex.label = if_else(V(g_2mode)$type, V(g_2mode)$univ_abbrev_ipeds, "")
  
      V(g_1mode_psi)$control_ipeds
      str(V(g_1mode_psi)$control_ipeds)
      
  # set aesthetics of vertex attributes in two-mode object
    is_bipartite(g_2mode)
    V(g_2mode)$type
    
    vertex_attr_names(g_2mode)
    
    #plot(x = g_2mode)
    
    # create vertex attributes that will be used as aesthetics in plots
      V(g_2mode)$color <- if_else(V(g_2mode)$type, "lightblue", "salmon")
      V(g_2mode)$shape <- if_else(V(g_2mode)$type, "square", "circle")
      V(g_2mode)$size <- if_else(V(g_2mode)$type, 20, 2)
    
    # figuring out how to assign institution name as an aesthetic    
      V(g_2mode)$name
      V(g_2mode)$univ_abbrev_ipeds
      if_else(V(g_2mode)$type, V(g_2mode)$univ_abbrev_ipeds, "")
    
    plot(
      x = g_2mode, 
      vertex.label = if_else(V(g_2mode)$type, V(g_2mode)$univ_abbrev_ipeds, "")
    )

# edge parameters
    
  #Edge parameters require to add the ‘edge.’ prefix when used as arguments or set by igraph_options. The edge parameters:
    
  # one mode objects
    #this time let's set them using igraph_options
    igraph_options()
    
    igraph_options(
      edge.color = "lightgrey", 
      edge.lty = 2, # 0 and “blank” mean no edges, 1 and “solid” are for solid lines, the other possible values are: 2 (“dashed”), 3 (“dotted”), 4 (“dotdash”), 5 (“longdash”), 6 (“twodash”).
    )
    #igraph_options(edge.color = "darkgrey", edge.arrow.size=0.5)
    
    plot.igraph(
      x = g_1mode_psi, 
      vertex.color = if_else(V(g_1mode_psi)$control_ipeds == "Public", "green", "red"),
      vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds,
      main = "my plot name folks"
    )
    
    # try making edge width a function of weight attribute
    edge_attr_names(g_1mode_psi)
    str(E(g_1mode_psi)$weight)
    
    igraph_options(
      edge.color = "darkgrey", 
      edge.lty = 1 # 0 and “blank” mean no edges, 1 and “solid” are for solid lines, the other possible values are: 2 (“dashed”), 3 (“dotted”), 4 (“dotdash”), 5 (“longdash”), 6 (“twodash”).
    )    
    
    plot.igraph(
      x = g_1mode_psi, 
      vertex.color = if_else(V(g_1mode_psi)$control_ipeds == "Public", "green", "red"),
      vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds,
      edge.width = log(E(g_1mode_psi)$weight)
    )    
    
    # with a multidimensional scaling layout of vertices
    plot.igraph(
      x = g_1mode_psi, 
      vertex.color = if_else(V(g_1mode_psi)$control_ipeds == "Public", "green", "red"),
      vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds,
      edge.width = log(E(g_1mode_psi)$weight),
      layout = layout_with_kk
    ) 
    
    # with a spring embedder layout
    plot.igraph(
      x = g_1mode_psi, 
      vertex.color = if_else(V(g_1mode_psi)$control_ipeds == "Public", "green", "red"),
      vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds,
      edge.width = log(E(g_1mode_psi)$weight),
      layout = layout_with_fr
    )  
    
  
    # make vertex area proportional to vertex strength (defined as sum of edge weights of the adjacent edges)
      # ?igraph::strength
      
      strength(g_1mode_psi)
      log(strength(g_1mode_psi))
      sqrt(strength(g_1mode_psi))
      str(strength(g_1mode_psi))
      
    plot.igraph(
      x = g_1mode_psi, 
      vertex.color = if_else(V(g_1mode_psi)$control_ipeds == "Public", "green", "red"),
      vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds,
      # vertex.size = area taken by each vertex
      vertex.size = sqrt(strength(g_1mode_psi))/10, # strength(g_1mode_psi)/1000, # sqrt(strength(g_1mode_psi))/10, # log(strength(g_1mode_psi)),
      # width of edge lines determined by weight of edges
      edge.width = sqrt(E(g_1mode_psi)$weight)/5, # log(E(g_1mode_psi)$weight),
      layout = layout_with_fr
    )      
    
    
  # two mode objects
    plot(
      x = g_2mode, 
      vertex.label = if_else(V(g_2mode)$type, V(g_2mode)$univ_abbrev_ipeds, "")
    )
  
    
# layout
  # from ?igraph.plotting
    # layout specifies how vertices will be placed on the plot
    # layout can be specified either as a numeric matrix or as a function
    # specifying layout as numeric matrix
      # If it is a numeric matrix, then the matrix has to have one line for each vertex, specifying its coordinates.
      # The matrix should have at least two columns, for the x and y coordinates, and it can also have third column, this will be the z coordinate for 3D plots and it is ignored for 2D plots.
    # specifying layout as a function
      # If layout is a function, this function will be called with the graph as the single parameter to determine the actual coordinates. 
      # The function should return a matrix with two or three columns. For the 2D plots the third column is ignored.
    
  # experimenting with layout outside of plotting
    
    #layout_in_circle()
      layout_in_circle(graph = g_1mode_psi)
      class(layout_in_circle(graph = g_1mode_psi)) # class = matrix
      str(layout_in_circle(graph = g_1mode_psi)) # numeric matrix with 43 rows and two columns
        # first column gives x coordinates, second column gives y coordinates
      
      # applying to g_1mode_hs igraph object and the 2-mode object
        str(layout_in_circle(graph = g_1mode_hs)) # 1742 rows, two columns; first column gives x coordinates, second column gives y coordinates
        str(layout_in_circle(graph = g_2mode)) # 1785 rows, two columns
    
      
  # experimenting with layout on 1-mode objects
    plot.igraph(
      x = g_1mode_psi, 
      vertex.color = if_else(V(g_1mode_psi)$control_ipeds == "Public", "green", "red"),
      vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds,
      layout = layout_nicely,
      main = "my plot name folks"
    )
    
    plot.igraph(
      x = g_1mode_psi, 
      vertex.color = if_else(V(g_1mode_psi)$control_ipeds == "Public", "green", "red"),
      vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds,
      layout = layout_in_circle,
      main = "my plot name folks"
    )   
    
    # spring embedder method layout
      # If a literal system of balls connected by springs is disrupted, thereby stretching some of the springs and compressing others, upon being let go it will return to its natural state
    #So-called spring-embedder methods of graph drawing define a notion of force for each vertex in the graph depending, at the very least, on the positions of pairs of vertices and the distances between them, and seek to iteratively update the placement of vertices until a vector of net forces across vertices converges
    
    plot.igraph(
      x = g_1mode_psi, 
      vertex.color = if_else(V(g_1mode_psi)$control_ipeds == "Public", "green", "red"),
      vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds,
      layout = layout_with_fr,
      main = "my plot name folks"
    )    
    
    # Alternatively, motivated by the fact that it is possible to associate the collection of forces in spring systems with an overall system energy, another common approach to generating layouts is that of energy-placement methods.
    plot.igraph(
      x = g_1mode_psi, 
      vertex.color = if_else(V(g_1mode_psi)$control_ipeds == "Public", "green", "red"),
      vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds,
      layout = layout_with_kk,
    )        
    
    # "Tree" layouts
    plot.igraph(
      x = g_1mode_psi, 
      vertex.color = if_else(V(g_1mode_psi)$control_ipeds == "Public", "green", "red"),
      vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds,
      layout = layout_as_tree,
    )        
    
    # radial
    plot.igraph(
      x = g_1mode_psi, 
      vertex.color = if_else(V(g_1mode_psi)$control_ipeds == "Public", "green", "red"),
      vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds,
      layout = layout_as_tree(g_1mode_psi, circular = TRUE),
    )        
    
    plot.igraph(
      x = g_1mode_psi, 
      vertex.color = if_else(V(g_1mode_psi)$control_ipeds == "Public", "green", "red"),
      vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds,
      layout = layout_in_circle,
    )
  # experimenting with layout on 2-mode objects
    
    
    #We can then use layout_with_graphopt to minimize vertex overlap when we plot().
    plot(
      x = g_2mode,
      layout = layout_with_graphopt,
      vertex.label = if_else(V(g_2mode)$type, V(g_2mode)$univ_abbrev_ipeds, "")
    )  
    
    #Alternatively, we can use the bipartite-specific layout.
    plot(
      x = g_2mode,
      layout = layout.bipartite,
      vertex.label = if_else(V(g_2mode)$type, V(g_2mode)$univ_abbrev_ipeds, "")
    )      
    
    
  ###########
    
  V(g_1mode_psi)$univ_abbrev_ipeds
  
  plot(x = g_1mode_psi, vertex.size = 20, vertex.frame.color = "pink", vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds)
  
  plot(x = g_1mode_psi, vertex.size = 20, vertex.color = "SkyBlue2", vertex.frame.color = "pink", vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds)
  
  plot(x = g_1mode_psi, axes = FALSE, add = FALSE, xlim = c(-1, 1), ylim = c(-1, 1), mark.shape = 1/2, mark.expand = 15)    
  
  plot(x = g_1mode_psi, vertex.size = 20, vertex.color = "SkyBlue2", vertex.frame.color = "pink", vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds)
  
  plot(x = g_1mode_psi, axes = FALSE, add = FALSE, xlim = c(-1, 1), ylim = c(-1, 1), mark.shape = 1/2, mark.expand = 15, vertex.size = 20)
  
       mark.groups = list(), 
    mark.col = rainbow(length(mark.groups), alpha = 0.3),mark.border = rainbow(length(mark.groups), alpha = 1), mark.expand = 15)
## ---------------------------
## Practice descriptive analysis of network graph characteristics from Kolaczyk and Csardi (2020), chapter 4
## ---------------------------
      

      

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


####################################
####################################
#################################### GOING FROM DATA TO NETWORKS
####################################
####################################

# CREATE A NETWORK OBJECT FROM THE LAWYER DATA FROM SANDS LIBRARY

  library(sand)


  
  g.lazega <- graph_from_data_frame(
    d = elist.lazega, # elist.lazega is a data frame containing an edge list of the network.
    directed="FALSE", # undirected graph object 
    vertices=v.attr.lazega # v.attr.lazega is a data frame containing the vertex attributes only.
  )

  ecount(g.lazega) # 115 edges
  vcount(g.lazega) # 36 nodes
  
  # data frame with two variables V1 and V2 and 115 roes
  str(elist.lazega) 
  attributes(elist.lazega) # has row.names attribute; has "names" attribute w/ value V1 and V2
  
  # data frame with 36 obs and 9 variables; each observation represents a node (36 total nodes); each variable is a vertex attribute
  v.attr.lazega
  str(v.attr.lazega)
  
E(g.lazega)
str(E(g.lazega))
class(E(g.lazega))
typeof(E(g.lazega)) # integer

################## GOING FROM DATA TO NETWORKS, ONE MODE CASE

# THREE BASIC FORMATS FOR CREATION OF NETWORK OBJECTS: ADJACENCY LISTS; EDGE LISTS; ADJACENCY MATRICES

# ADJACENCY LIST
  # GENERAL
    # An adjacency list representation of a graph G is simply an array of size Nv, ordered with respect to the ordering of the vertices in V, 
      #each element of which is a list, where the ith list contains the set of all vertices j for which there is an edge from i to j . 
      #This is the representation usually used by igraph, evident in the output from the function print_all in the examples above.

  # ONE MODE
    as_adj_list(graph = g.lazega)
    
    str(as_adj_list(graph = g.lazega)) # object is a list with 36 elements; each element represents a node
    
    # examine one of the 36 elements; specifically the element for vertex 3
      # contents of this element is a vector whose length is equal to the number of vertices that share an edge with V3
      # V3 shares an edge with 3 other vertices (V18, V25, V28), so the length of the element is = 3
    str(as_adj_list(graph = g.lazega)["V3"]) # a list of 1
    str(as_adj_list(graph = g.lazega)[["V3"]]) # named int vector 
    length(as_adj_list(graph = g.lazega)[["V3"]]) # length = 3; 
    
  # TWO MODE
    
    

# EDGE LIST
  # GENERAL
    #An edge list is a simple two-column list of all vertex pairs that are joined by an edge.
  # ONE MODE
    as_edgelist(graph = g.lazega, names = TRUE)
    
    class(as_edgelist(graph = g.lazega, names = TRUE)) # class = matrix
    str(as_edgelist(graph = g.lazega, names = TRUE)) # character vector matrix with 115 rows and two columns
    
    # argument names = FALSE
    as_edgelist(graph = g.lazega, names = FALSE)
    str(as_edgelist(graph = g.lazega, names = FALSE)) # numeric vector matrix w/ 115 rows and two columns
  # TWO MODE

# ADJACENCY MATRIX
  # GENERAL
    # for 1-mode network w/ N vertices, the adjacency matrix has dimensions N X N
      #the rows are the V vertices and the columns are the V vertices
      # and each cell in the matrix indicates whether the two vertices are adjacent (connected by an edge)
  # ONE MODE
    as_adjacency_matrix(graph = g.lazega)
    str(as_adjacency_matrix(graph = g.lazega))
    class(as_adjacency_matrix(graph = g.lazega)) # class = dgCmatrix
    typeof(as_adjacency_matrix(graph = g.lazega)) # S4
    
    # argument sparse = FALSE
    as_adjacency_matrix(graph = g.lazega, sparse = FALSE)
    str(as_adjacency_matrix(graph = g.lazega, sparse = FALSE))
    
    as_adjacency_matrix(graph = g.lazega, type = "both")
    as_adjacency_matrix(graph = g.lazega, type = "both")
    

################## GOING FROM DATA TO NETWORKS, TWO MODE CASE

# THREE BASIC FORMATS FOR CREATION OF NETWORK OBJECTS: ADJACENCY LISTS; EDGE LISTS; ADJACENCY MATRICES

# ADJACENCY MATRIX [if weighted, then called an incidence matrix rather than an adjacency matrix]
    
  # ONE MODE
    as_adjacency_matrix(graph = g.lazega, sparse = FALSE, type = "both")
    as_adjacency_matrix(graph = g.lazega, sparse = FALSE, type = "upper")
    as_adjacency_matrix(graph = g.lazega, sparse = FALSE, type = "lower")
    
  # TWO MODE
    
    # CREATE A SAMPLE BIPARTITE ADJACENCY MATRIX
    
      A <- matrix(c(2,4,0,0,1,2,1,0,0,0,4,5,0,0,1,2,0,0,6,2), byrow = T, nrow = 5, ncol = 4)
      class(A)
      typeof(A)
      A
      rownames(A) <- letters[c(1:nrow(A))]
      colnames(A) <- LETTERS[c(1:ncol(A))]
      print(A)

      g <- graph.incidence(incidence = A, weighted = TRUE)
      g <- graph_from_incidence_matrix(incidence = A, weighted = TRUE)
      g # Parameters of created graph can be seen. This graph is UN-B which mean that this graph is U = undirected, N = named, W = weighted, B = bipartite, 9 = # of vertex, 11 = # of edges, and there are connections.
      
      V(g)$type
      str(V(g)$type) # length = to number of vertices
      
# ADJACENCY LIST

  # ONE MODE
      as_adj_list(graph = g.lazega)
      
      str(as_adj_list(graph = g.lazega)) # object is a list with 36 elements; each element represents a node
      
      # examine one of the 36 elements; specifically the element for vertex 3
      # contents of this element is a vector whose length is equal to the number of vertices that share an edge with V3
      # V3 shares an edge with 3 other vertices (V18, V25, V28), so the length of the element is = 3
      str(as_adj_list(graph = g.lazega)["V3"]) # a list of 1
      str(as_adj_list(graph = g.lazega)[["V3"]]) # named int vector 
      length(as_adj_list(graph = g.lazega)[["V3"]]) # length = 3; 
    
  # TWO MODE
    
    
    
# EDGE LIST

  # GENERAL
      #An edge list is a simple two-column list of all vertex pairs that are joined by an edge.
  
    # ONE MODE
      as_edgelist(graph = g.lazega, names = TRUE)
    
  # TWO MODE
    #two column edgelist: 
      #first vertex set in first column second vertex set in second column and rows represent connections; in weighted case there are recurring pairs of nodes
    #three column edgelist (weighted graphs): 
      #first vertex set in first column second vertex set in second column and connection weights in third column      
    
      # what the data look like on recruiting data
        #two columns: first column = IDs representing vertices from mode =1; second column = IDs representing vertices from mode =2
        # within a single row, you see output like this: [3,] "00000226" "106397"; that means that private HS "00000226" got a visit from university "106397"
      options(max.print=99)
      
      as_edgelist(graph = two_mode_network, names = TRUE)
      as_edgelist(graph = two_mode_network, names = FALSE)
      two_mode_network
      E(two_mode_network)$weight
      
      str(as_edgelist(graph = two_mode_network, names = TRUE))
      str(E(two_mode_network)$weight)
      
      as_tibble(as_edgelist(graph = two_mode_network, names = TRUE))
      
      #try adding weight column to edge list matrix
      cbind(as_edgelist(graph = two_mode_network, names = TRUE),E(two_mode_network)$weight)
      
      ## S3 method for class 'matrix'
      as_tibble(x = as_edgelist(graph = two_mode_network, names = TRUE), .name_repair = NULL) %>% str()
      as_tibble(x = as_edgelist(graph = two_mode_network, names = TRUE), .name_repair = "minimal") %>% str()
      
      as_tibble(x = as_edgelist(graph = two_mode_network, names = TRUE), .name_repair = ~ c("ppin","unitid")) %>% str()
      
      as_tibble(x = cbind(as_edgelist(graph = two_mode_network, names = TRUE),E(two_mode_network)$weight), .name_repair = ~ c("ppin","unitid","weight")) %>% str()
      as_tibble(x = cbind(as_edgelist(graph = two_mode_network, names = FALSE),E(two_mode_network)$weight), .name_repair = ~ c("ppin","unitid","weight")) %>% str()
########
######## STEPS TO FOLLOW WEDS 8/26, CREATING BIPARTITE IGRAPH OBJECT USING igraph::graph_from_data_frame()
######## 
########
########
########
      
      g.lazega <- graph_from_data_frame(
        d = elist.lazega, # elist.lazega is a data frame containing an edge list of the network.
        directed="FALSE", # undirected graph object 
        vertices=v.attr.lazega # v.attr.lazega is a data frame containing the vertex attributes only.
      )

      
  # CREATE AN EDGE LIST DATAFRAME
      elist_2mode <- as_tibble(x = cbind(as_edgelist(graph = two_mode_network, names = TRUE),E(two_mode_network)$weight), .name_repair = ~ c("ppin","unitid","weight")) %>%
        mutate(weight = as.integer(weight))
      str(elist_2mode)
      attributes(elist_2mode)
      
  # CREATE A VERTICES DATA FRAME
    # NULL FOR NOW
      V(two_mode_network)$name
      str(V(two_mode_network)$name)
      
      V(two_mode_network)$type
      str(V(two_mode_network)$type)
      
      #as_tibble(x = V(two_mode_network)$type, .name_repair = ~ c("type")) %>% str()
      #enframe(x = V(two_mode_network)$type, name = NULL, value = "type") %>% str()
      #v_attr_2mode <- enframe(x = V(two_mode_network)$type, name = NULL, value = "type")
      
      tibble(name = V(two_mode_network)$name, type = V(two_mode_network)$type)
      #print(x = v_attr_2mode, n = 2000)
      #print(x = tibble(name = V(two_mode_network)$name, type = V(two_mode_network)$type), n=2000)
      
      v_attr_2mode <- tibble(name = V(two_mode_network)$name, type = V(two_mode_network)$type)
      v_attr_2mode
      str(v_attr_2mode)
      
  # RECREATE IGRAPH OBJECT
    #?graph_from_data_frame
    #g_2mode <- graph_from_data_frame(d = elist_2mode, directed = FALSE, vertices = NULL)
    g_2mode <- graph_from_data_frame(
      d = elist_2mode, 
      directed = FALSE, 
      vertices = v_attr_2mode
    )
    is_bipartite(graph = g_2mode)
    
    elist_2mode
    v_attr_2mode
    
    g_2mode
    two_mode_network
    
    ecount(g_2mode)
    ecount(two_mode_network)
    
    vcount(g_2mode)
    vcount(two_mode_network)    
###########################
###########################    
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

###### Create igraph objects for network analysis

affiliation_matrix <- m
str(affiliation_matrix)
str(m)

# Create object for two mode [bipartite] analysis

two_mode_network <- graph_from_incidence_matrix(affiliation_matrix, weighted = TRUE)
# two_mode_network <- graph.incidence(affiliation_matrix) # is this same as above?
class(two_mode_network) # class = igraph

is_bipartite(graph = two_mode_network)

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

  # investigate vertex attributes    

    igraph::vertex_attr_names(graph = two_mode_network) # two attributes: "type" "name"
    
    vertex_attr(graph = two_mode_network, name = "name") # names of the nodes
    str(vertex_attr(graph = two_mode_network, name = "name")) # character vector of 1785 elements
    
    vertex_attr(graph = two_mode_network, name = "type") # 
    str(vertex_attr(graph = two_mode_network, name = "type")) # logical vector of 1785 elements which is number of private High schools (that got visits) + number of universities
    
    sum(vertex_attr(graph = two_mode_network, name = "type")) # sum = 43; so type == TRUE if element is a university
  
  #adding/modify vertex attributes
    vertex_attr_names(graph = two_mode_network)
    
    #syntax
      #vertex_attr(graph, name, index = V(graph)) <- value
    
    
    #test w/ a fictitious attribute
      vertex_attr(graph = two_mode_network, name = "test", index = V(two_mode_network)) <- c(1,2,3)
      vertex_attr(graph = two_mode_network, name = "test") # 
      str(vertex_attr(graph = two_mode_network, name = "test")) # note that values are recycled to match length of number of vertices

      #delete vertex attribute "test"
      vertex_attr_names(graph = two_mode_network)
      delete_vertex_attr(graph = two_mode_network, name = "test") # note: doesn't change object because you did not assign
      vertex_attr_names(graph = two_mode_network)
      
      two_mode_network <- delete_vertex_attr(graph = two_mode_network, name = "test")
      vertex_attr_names(graph = two_mode_network)
      
      
# edge attributes

  igraph::edge_attr_names(graph = two_mode_network) # one attribute: weight 
  
  edge_attr(graph = two_mode_network, name = "weight")
  igraph::edge_attr(graph = two_mode_network, name = "weight", index = E(two_mode_network)) # same same
  
  str(edge_attr(graph = two_mode_network, name = "weight")) # numeric vector of length = 11034; each element is the weight associated with that edge
  
  max(edge_attr(graph = two_mode_network, name = "weight")) # max = 6
  
  
# graph attributes

two_mode_network
graph_attr_names(two_mode_network) # none

##########################    
########################## descriptive analysis of igraph object univ_graph, where nodes are universities
##########################    

#from this: file:///C:/Users/ozanj/Downloads/igraphtutorialeng.html
#https://mariliagaiarsa.weebly.com/uploads/3/8/6/2/38628397/igraphtutorialeng.html

library(bipartite)

pp<-bezerra2009

str(pp) # matrix 13 by 13
attributes(pp)

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------