

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
      vertex.label = V(g_2mode)$univ_abbrev_ipeds
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
      V(g_2mode)$size <- if_else(V(g_2mode)$type, 5, 2)
    
    # figuring out how to assign institution name as an aesthetic    
      V(g_2mode)$name
      V(g_2mode)$univ_abbrev_ipeds
      if_else(V(g_2mode)$type, V(g_2mode)$univ_abbrev_ipeds, "")
    
    plot(
      x = g_2mode, 
      vertex.label = if_else(V(g_2mode)$type, V(g_2mode)$univ_abbrev_ipeds, ""),
      layout = layout_with_kk
    )

# edge parameters
    
  #Edge parameters require to add the ‘edge.’ prefix when used as arguments or set by igraph_options. The edge parameters:
    
  # one mode objects
    #this time let's set them using igraph_options

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
    
  # objects
      # g_2
      # g_1mode_psi
      # g_1mode_hs

    vertex_attr_names(g_1mode_hs)
    plot.igraph(
      x = g_1mode_hs, 
      vertex.color = NA,
      vertex.label = NA,
      layout = layout_nicely,
    )

    plot(
      x = g_2mode, 
      vertex.label = if_else(V(g_2mode)$type, V(g_2mode)$univ_abbrev_ipeds, "")
    )
                    
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

# graphing large networks

  # one mode
    
    # conventional methods: here, layout_with_kk
    plot.igraph(
      x = g_1mode_psi, 
      vertex.color = if_else(V(g_1mode_psi)$control_ipeds == "Public", "green", "red"),
      vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds,
      layout = layout_with_kk,
    )
    
    # visualization method VxOrd for visualizing large networks
      # VxOrd [4], a visualization package produced by Sandia Labs, is an enhanced version of the spring-embedder methodology. 
        # It attempts to place vertices in clusters on the two-dimensional plane, with the help of sophisticated optimization methods to 
        # more efficiently search the space of possible graph drawings and a grid that helps reduce computation time from the typical O(N2v ) down to O(Nv). 
      #In addition, it employs edge-cutting criteria, designed towards producing drawings that balance the detail with which both local 
        #and global structure are shown.
      plot.igraph(
        x = g_1mode_psi, 
        vertex.color = 5,
        vertex.label = NA,
        layout = layout_with_drl
      )    
    # doesn't look that much different than layout_with_kk
      
    # recommendations for graphing large networks from Csardi chapter 3
      
      # basically, recommends "coarsening" a network graph, which means reducing number of nodes by "replacing groups of vertices with single meta-vertices"
      # this can be done by using some vertex attribute as the meta-node (e.g., US news prestige tier) or by using graph partitioning methods (i.e., community detection)
    
# Experiment graphing ego networks

  # try plotting ego network for one university
    # to do: modify plot to eliminate lines for edge order = 1
    plot.igraph(
      x = egos_psi[["100751"]], # egos_psi[["106397"]] = uarkansas
      #vertex.label = "",
      vertex.label = if_else(V(egos_psi[["100751"]])$type, V(egos_psi[["100751"]])$univ_abbrev_ipeds, ""),
      vertex.shape = if_else(V(egos_psi[["100751"]])$type, "square", "circle"),
      vertex.color = if_else(V(egos_psi[["100751"]])$type, "lightblue", "salmon"),
      vertex.size = if_else(V(egos_psi[["100751"]])$type, 5, 3),
      layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
      main = "my plot name folks"
    )

    # create subgraph based on edges rather than based on vertices?

      subgraph.edges(
        graph = egos_psi[["139658"]], 
        eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==1]
      )
      
      subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==1])
      subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==1]) %>% vcount()
      subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==1]) %>% ecount()
      
      subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==1]) %>% V() %>% print(full = T)


      E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==1]
      E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==2]

      subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==2])
      
      subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==2]) %>% vcount()
      subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==2]) %>% ecount()
      
      #plot
      length(egos_psi)
      class(egos_psi[[3]])
      
      plot.igraph(
        x = egos_psi[["139658"]], # emory
        #vertex.label = "",
        vertex.label = if_else(V(egos_psi[["139658"]])$type, V(egos_psi[["139658"]])$univ_abbrev_ipeds, ""),
        vertex.shape = if_else(V(egos_psi[["139658"]])$type, "square", "circle"),
        vertex.color = if_else(V(egos_psi[["139658"]])$type, "lightblue", "salmon"),
        vertex.size = if_else(V(egos_psi[["139658"]])$type, 5, 3),
        layout = layout_with_kk,
        #layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
      )
      
      # plot subgraph, order =1
      plot.igraph(
        x = subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==1]), # emory
        vertex.label = NA,
        vertex.shape = "circle",
        vertex.color = "lightblue",
        vertex.size = 5,        
        layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
      )
      
      # plot subgraph, order = 2
        egos_psi[["139658"]] # emory
        
        # vertices
        subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==2]) %>% V() %>% print(full = T)
        
        # edges
        subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==2]) %>% E() %>% print(full = T)
        
        plot.igraph(
          x = subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==2]), # emory
          vertex.label = NA,
          vertex.shape = "circle",
          vertex.color = "lightblue",
          vertex.size = 5,        
          layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
        )      
        
        ego_emory_order2 <- subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==2])
        
        plot.igraph(
          x = ego_emory_order2, # emory
          vertex.label = if_else(V(ego_emory_order2)$type, V(ego_emory_order2)$univ_abbrev_ipeds, ""),
          vertex.shape = if_else(V(ego_emory_order2)$type, "square", "circle"),
          vertex.color = if_else(V(ego_emory_order2)$type, "lightblue", "salmon"),
          vertex.size = if_else(V(ego_emory_order2)$type, 5,3),
          layout = layout_with_kk,
          #layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
        )
        
      # compare to plot that includes edges of order 1 and order 2
      plot.igraph(
        x = egos_psi[["139658"]], # emory
        #vertex.label = "",
        vertex.label = if_else(V(egos_psi[["139658"]])$type, V(egos_psi[["139658"]])$univ_abbrev_ipeds, ""),
        vertex.shape = if_else(V(egos_psi[["139658"]])$type, "square", "circle"),
        vertex.color = if_else(V(egos_psi[["139658"]])$type, "lightblue", "salmon"),
        vertex.size = if_else(V(egos_psi[["139658"]])$type, 5, 3),
        layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
      )
        
                    
  # properties of ego networks that networks people research
    #note:  alters = nodes ego is directly connected to (order =1)
    
    #Size
      #How many total alters?
      # based on ego-alter ties  
    #Diversity
      # variation in alter attributes
    # homophily/heterophily
      # ties to alters that are the same or different from ego
      # compare ego attributes to alter attributes
      # ego network homophily index
        # canonical: share of ties that are same race vs. different race as you
        # university reruiting ego networks
          # order = 1 (ties from ego to private high schools)
            # share of alters (privae high schools) that are high prestige vs. low-prestige
          # order = 2 (of the high schools visited by ego, which universities also visited those high schools)
            # e.g., of high schools visited by notre dame university, how similar to notre dame university (on some metric like religious affiliation) were other universities that visited those high schools
      # ego network heterogeneity index (from Blau)
        # this is same as structural diversity inex you used for tuition rich mission poor paper
    #Composition
      #Prop. of certain types of alter
      # based on ego-alter ties, and bringing in attributes as necessary
    #Clustering
      #How many ties between alters?      
      # based on alter-alter ties
      # clustering coefficient: 
        # number of alters who have ties to one another compared to number of possible ties between your alters
        # = (number of edges of order = 2)/(N*(N-1)/2)
        # own: likely operationalize differently for bipartite networks
    # centrality
      # for order = 2: of high schools visited by ego (order =1), which high schools are most visited by other universities in your sample
      # different measures of centrality
        # degree centrality
        # k-path centrality
        # closeness centrality
        # betweenness centrality
          # don't know how relevant this would be for bipartite networks of off-campus recruiting
        # eigenvector centrality
    
##
##        
## ---------------------------
## Kolaczyk and Csardi (2020), chapter 4: descriptive analysis of network graph characteristics from 
## ---------------------------
##
##
    
## ---------------------------
## 4.2 vertex and edge characteristics
## ---------------------------
    
# vertex degree
  
  # definition of vertex degree:
    # degree of a vertex d_v is the number of edges the vertex is directly connected to
    
    
  # one mode (vertices = universities)
    # vertices = universities
    # edges = whether both universities visited at least one high school in common
    
    vcount(g_1mode_psi)
    ecount(g_1mode_psi)
    
    # degree(graph) creates a named numeric vector of length = V, with each element = number of edges directly connected to the vector
    degree(g_1mode_psi)
    str(degree(g_1mode_psi))
    
    # historgram of degree    
    hist(
      degree(g_1mode_psi), 
      col="lightblue", 
      xlim=c(0,50),
      xlab="Vertex Degree", 
      ylab="Frequency", 
      main=""
    )

  # one mode (vertices = high schools)
    # vertices = high schools
    # edges = whether two high schools were visited by at least one university in common
      # number of potential edges associated with a single high school is total number of vertices (V) minus 1 = 1742-1 = 1741

    vcount(g_1mode_hs)
    ecount(g_1mode_hs)
    
    # degree(graph) creates a named numeric vector of length = V, with each element = number of edges directly connected to the vector
    degree(g_1mode_hs)
    length(degree(g_1mode_hs)) # length = 1742; degree is a vertex-level measure
    max(degree(g_1mode_hs)) # max = 1636
      # for this high school i, there were 1,636 high schools that received at least one visit from a university that also visited high school i
    str(degree(g_1mode_hs))
    
    # historgram of degree    
    hist(
      degree(g_1mode_hs), 
      col="lightblue", 
      xlim=c(0,2000),
      xlab="Vertex Degree", 
      ylab="Frequency", 
      main=""
    )
            
  # 2- mode object
    # vertices = high schools (mode 1) and universities (mode 2)
      # total number of vertices = 1785
    # edges = whether a particular high school i (mode 1) was visited by a particular university j (mode 2)

    vcount(g_2mode)
    ecount(g_2mode)
    
    # degree
      length(degree(g_2mode)) # length = 1785

      # mode 1 = high schools
        length(degree(g_2mode)[V(g_2mode)$type == FALSE]) # 1742
        degree(g_2mode)[V(g_2mode)$type == FALSE] # each element of degree represents the number of universities in our sample that visited the high school
        max(degree(g_2mode)[V(g_2mode)$type == FALSE]) # max = 31; a private high school that was visited by 31 universities in our sample
        
      # mode 2 = universities
        length(degree(g_2mode)[V(g_2mode)$type == TRUE]) # 43
        degree(g_2mode)[V(g_2mode)$type == TRUE] # each element of degree represents the number of private high schools the university visited
        max(degree(g_2mode)[V(g_2mode)$type == TRUE]) # max = 718; a university (alabama) visited 718 private high schools


  # ego network of universities, using emory as example ego
    # keep order of 0, 1, and 2; 
      # order = 0 is ego (itself); 
      # order = 1 = which private high schools (alters) the university (ego) visited; 
      # order = 2 = of high schools the university visited, which universities visited that set of high schools
    
      class(egos_psi)
      length(egos_psi)
      names(egos_psi)
      
    # emory university
        egos_psi[["139658"]]
        class(egos_psi[["139658"]])
      
    # vertices = private high schools and universities
      vcount(egos_psi[["139658"]])
      V(egos_psi[["139658"]])
      
    # vertex attributes
      vertex_attr_names(egos_psi[["139658"]])
      V(egos_psi[["139658"]])$name
      V(egos_psi[["139658"]])$control_ipeds
      
      # attributes of ego
      V(egos_psi[["139658"]])$control_ipeds[V(egos_psi[["139658"]])$name == "139658"]
      V(egos_psi[["139658"]])$state_code_ipeds[V(egos_psi[["139658"]])$name == "139658"]
      
    # edges = a visit from ego to a high school (alters, order =1), or a visit from a university that is not the ego to the alters of the ego (order =2)
      ecount(egos_psi[["139658"]])
      
      str(E(egos_psi[["139658"]]))
      str(E(egos_psi[["139658"]])$order)
      
      # edge attribute order identifies whether distance from ego
      table(E(egos_psi[["139658"]])$order)

      # print edges when order =1
        # when order = 1, each edge represents a private high school visited by the ego
        E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==1]
        print(E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==1],  full = TRUE) # print all edges

      # print edges when order =2
        # when order = 2, each edge represents 1+ visits from a university to a high school (alter) that was visited by the ego
        print(E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==2],  full = TRUE) # print all edges
            
    # degree
      length(degree(egos_psi[["139658"]])) # length = 303
      vcount(egos_psi[["139658"]]) # 303  
      
      degree(egos_psi[["139658"]]) # each element represents the number of edges associated with that vertex
      
      max(degree(egos_psi[["139658"]]))
      
    # vertex attribute "type", whether the vector is a private high school or a university
      table(V(egos_psi[["139658"]])$type)
      
    # degree, type == private high school (FALSE)
      
      length(degree(egos_psi[["139658"]])[V(egos_psi[["139658"]])$type==FALSE]) # 260
      
      # note that degree includes visits of order = 1 and order = 2
      
      # degree includes presence of a visit from ego to the HS (order =1, which counts as degree of 1) and number of other universities that visited the HS
      degree(egos_psi[["139658"]])[V(egos_psi[["139658"]])$type==FALSE] 
      degree(egos_psi[["139658"]], v = V(egos_psi[["139658"]])$type==FALSE) # same same

      # max. = 31; 
      max(degree(egos_psi[["139658"]])[V(egos_psi[["139658"]])$type==FALSE]) # max = 31; a high school visited by the ego was visited by 31 total universities (including ego)
      
    # degree, type == TRUE (university)
      
      length(degree(egos_psi[["139658"]])[V(egos_psi[["139658"]])$type==T]) # 43
      
      degree(egos_psi[["139658"]])[V(egos_psi[["139658"]])$type==TRUE] 
      degree(egos_psi[["139658"]], v = V(egos_psi[["139658"]])$type==TRUE) # same same
      
      

  # ego network of high schools, choose "Choate" as the ego ID = 00233261
    # keep edges of order of 0, 1, and 2; 
      # order = 0 is ego (itself, the private HS); 
      # order = 1 = which universities (alters) visited the private high school (ego)
      # order = 2 = of the universities that visited the private high school (ego), which private high schools did that set of universities visit
    
      length(egos_hs)
      
    # Choate
      class(egos_hs[["00233261"]])
      egos_hs[["00233261"]]

    # vertices = private high schools and universities
      # if vertex is a university, this is a university that visited choate (i.e., alters of the ego choate)
      # if a vertex is a HS (not including Choate), this is a high school that was visited by at least one of the universities that visited choate
      vcount(egos_hs[["00233261"]])
      
          # edges = a visit from ego to a high school (alters, order =1), or a visit from a university that is not the ego to the alters of the ego (order =2)

    # edges represent presence of a visit to a particular high school from a particular university
      ecount(egos_hs[["00233261"]])

      
      # edge attribute order identifies whether distance from ego
      table(E(egos_hs[["00233261"]])$order) # you didn't create order attribute


    # degree
      length(degree(egos_hs[["00233261"]])) # length = 1453
      vcount(egos_hs[["00233261"]]) # 1453
      
      degree(egos_hs[["00233261"]]) # each element represents the number of edges associated with that vertex
      
      max(degree(egos_hs[["00233261"]]))
      
    # vertex attribute "type", whether the vector is a private high school or a university
      table(V(egos_hs[["00233261"]])$type)
      
      
    # degree, type == TRUE (university)
      
      length(degree(egos_hs[["00233261"]])[V(egos_hs[["00233261"]])$type==T]) # 27; means that 27 of the universities in our sample visited choate
      
      # for each university that visited choate, degree represents the number of private high schools they visited (including choate)
      degree(egos_hs[["00233261"]])[V(egos_hs[["00233261"]])$type==TRUE] 
      degree(egos_hs[["00233261"]], v = V(egos_hs[["00233261"]])$type==TRUE) # same same
      
      max(degree(egos_hs[["00233261"]])[V(egos_hs[["00233261"]])$type==TRUE]) # max = 595; 
      
    #create sub-graph from ego network
      str(V(egos_hs[["00233261"]])[V(egos_hs[["00233261"]])$type==TRUE]) 
      V(egos_hs[["00233261"]])[V(egos_hs[["00233261"]])$type==TRUE]
    #as.integer(temp)

    #?induced_subgraph
    induced_subgraph(
      graph = egos_hs[["00233261"]],
      vids = V(egos_hs[["00233261"]])[V(egos_hs[["00233261"]])$type==TRUE]
    )    

    # problem is that this subgraph has no edges      
      induced_subgraph(graph = egos_hs[["00233261"]], vids = V(egos_hs[["00233261"]])[V(egos_hs[["00233261"]])$type==TRUE])
      
      class(induced_subgraph(graph = egos_hs[["00233261"]], vids = V(egos_hs[["00233261"]])[V(egos_hs[["00233261"]])$type==TRUE]))
      ecount(induced_subgraph(graph = egos_hs[["00233261"]], vids = V(egos_hs[["00233261"]])[V(egos_hs[["00233261"]])$type==TRUE]))
      vcount(induced_subgraph(graph = egos_hs[["00233261"]], vids = V(egos_hs[["00233261"]])[V(egos_hs[["00233261"]])$type==TRUE]))
      
      induced_subgraph(graph = egos_hs[["00233261"]], vids = V(egos_hs[["00233261"]])[V(egos_hs[["00233261"]])$type==FALSE])


    # plot ego network of high school
      plot(
        x = egos_hs[["00233261"]],
        vertex.label = if_else(V(egos_hs[["00233261"]])$type, V(egos_hs[["00233261"]])$univ_abbrev_ipeds, ""),
        vertex.color = if_else(V(egos_hs[["00233261"]])$type, "lightblue", "salmon"),
        vertex.size = if_else(V(egos_hs[["00233261"]])$type, 5, 3)
        
      )
    # try plotting subgraph
      plot(
        x = egos_hs[["00233261"]],
        vertex.label = if_else(V(egos_hs[["00233261"]])$type, V(egos_hs[["00233261"]])$univ_abbrev_ipeds, ""),
        vertex.color = if_else(V(egos_hs[["00233261"]])$type, "lightblue", "salmon"),
        vertex.size = if_else(V(egos_hs[["00233261"]])$type, 5, 3)
        
      )
      
    
    

    

    # degree, type == private high school (FALSE)
      
      length(degree(egos_hs[["00233261"]])[V(egos_hs[["00233261"]])$type==FALSE]) # 1426
      
      # note that degree includes visits of order = 1 and order = 2
      
      # degree includes presence of a visit from ego to the HS (order =1, which counts as degree of 1) and number of other universities that visited the HS
      degree(egos_hs[["00233261"]])[V(egos_hs[["00233261"]])$type==FALSE]
      degree(egos_hs[["00233261"]], v = V(egos_hs[["00233261"]])$type==FALSE) # same same

      # max.
      max(degree(egos_hs[["00233261"]])[V(egos_hs[["00233261"]])$type==FALSE]) # max = 27; a high school visited by the ego was visited by 31 total universities (including ego)
      
# vertex strength
        
  # definition of vertex strength:
    # for weighted networks, vertex strength = sum of weights of edges incident to a given vertex
    
    
  # one mode (vertices = universities)
    # vertices = universities
    # edges = whether both universities visited at least one high school in common
    
    vcount(g_1mode_psi)
    ecount(g_1mode_psi)
    
    # strength
    degree(g_1mode_psi)
    strength(g_1mode_psi)
    str(degree(g_1mode_psi))
    
    # historgram of degree    
    hist(
      strength(g_1mode_psi), 
      col="lightblue", 
      xlab="Vertex strength", 
      ylab="Frequency", 
      main=""
    )


  # 2- mode object
    # vertices = high schools (mode 1) and universities (mode 2)
      # total number of vertices = 1785
    # edges = whether a particular high school i (mode 1) was visited by a particular university j (mode 2)

    vcount(g_2mode)
    ecount(g_2mode)
    
    # strength
      length(degree(g_2mode)) # length = 1785

      # mode 1 = high schools
        length(degree(g_2mode)[V(g_2mode)$type == FALSE]) # 1742
        length(strength(g_2mode)[V(g_2mode)$type == FALSE]) # 1742
        
        degree(g_2mode)[V(g_2mode)$type == FALSE]
        strength(g_2mode)[V(g_2mode)$type == FALSE]
        

      # mode 2 = universities
        degree(g_2mode)[V(g_2mode)$type == T]
        strength(g_2mode)[V(g_2mode)$type == T]


# PLOTTING DEGREE CENTRALITY
        
    # idea: central vertices are the ones w/ the most edges
    
    # when would we want to use this?
      
      # for g_2mode, would want to know which high schools are the most central
      # for g_1mode_hs, would want to know which high schools are connected to other high schools by a visit from a common university
      # for ego network where ego = university, would want to know degree centrality of high schools the university visited; that is, which other universities visited these high schools
        
        
        degree(g_2mode)
        
        degree(g_2mode)[V(g_2mode)$type == F] # high schools
        max(degree(g_2mode)[V(g_2mode)$type == F]) # 

      # plot 2 mode, with vertex size determined by degree        
      plot(
        x = g_2mode, 
        vertex.label = if_else(V(g_2mode)$type, V(g_2mode)$univ_abbrev_ipeds, ""),
        vertex.shape = if_else(V(g_2mode)$type, "square", "circle"),
        vertex.color = if_else(V(g_2mode)$type, "lightblue", "salmon"),
        vertex.size = if_else(V(g_2mode)$type, 5, degree(g_2mode)/3),
        layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
        
      )

  # for ego network where ego = university, would want to know degree centrality of high schools the university visited; that is, which other universities visited these high schools

    # PLOT EGO NETWORK, INCLUDING ORDER 1 AND 2      
      plot.igraph(
          x = egos_psi[["139658"]], # emory
          #vertex.label = "",
          vertex.label = if_else(V(egos_psi[["139658"]])$type, V(egos_psi[["139658"]])$univ_abbrev_ipeds, ""),
          vertex.shape = if_else(V(egos_psi[["139658"]])$type, "square", "circle"),
          vertex.color = if_else(V(egos_psi[["139658"]])$type, "lightblue", "salmon"),
          vertex.size = if_else(V(egos_psi[["139658"]])$type, 5, 3),
          layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
        )
      
      egos_psi[["139658"]] %>% vcount()
      egos_psi[["139658"]] %>% ecount()

      # plot ego network order = 1
      plot.igraph(
        x = subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==1]), # emory
        vertex.label = NA,
        vertex.shape = "circle",
        vertex.color = "lightblue",
        vertex.size = 5,        
        layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
      )
      
      subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==1]) %>% degree()
      
      # plot subgraph, order = 2
        ego_emory_order2 <- subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==2])
        
        degree(ego_emory_order2)[V(ego_emory_order2)$type ==T]
        degree(ego_emory_order2)[V(ego_emory_order2)$type ==F]

        # without making vertex size based on degree centrality        
        plot.igraph(
          x = ego_emory_order2, # emory
          vertex.label = if_else(V(ego_emory_order2)$type, V(ego_emory_order2)$univ_abbrev_ipeds, ""),
          vertex.shape = if_else(V(ego_emory_order2)$type, "square", "circle"),
          vertex.color = if_else(V(ego_emory_order2)$type, "lightblue", "salmon"),
          vertex.size = if_else(V(ego_emory_order2)$type, 5,3),
          layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
        )

        # making vertex size based on degree centrality        
        plot.igraph(
          x = ego_emory_order2, # emory
          vertex.label = if_else(V(ego_emory_order2)$type, V(ego_emory_order2)$univ_abbrev_ipeds, ""),
          vertex.shape = if_else(V(ego_emory_order2)$type, "square", "circle"),
          vertex.color = if_else(V(ego_emory_order2)$type, "lightblue", "salmon"),
          vertex.size = if_else(V(ego_emory_order2)$type, 5, degree(ego_emory_order2)/3),
          layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
        )        
# vertex centrality
  # Many questions that might be asked about a vertex in a network graph essentially seek to understand its ‘importance’ in the network
    
        

  # closeness centrality
    # Closeness centrality measures attempt to capture the notion that a vertex is ‘central’ if it is ‘close’ to many other vertices. 
      # The standard approach, introduced by Sabidussi [21], is to let the centrality vary inversely with a measure of the total 
      # distance of a vertex from all others,        
    #?closeness
        
    # for which igraph objects would closeness centrality be important for?
        
        # basically, interested in closeness centrality of high schools (which high schools are close to other vertices in terms of length of path)
    
    #closeness(graph, vids = V(graph), weights = NULL, normalized = FALSE)
      closeness(graph = g_1mode_psi, vids = V(g_1mode_psi), normalized = FALSE)
    closeness(graph = g_1mode_psi, vids = V(g_1mode_psi), normalized = TRUE)
    
    vertex_attr_names(g_1mode_psi)
    V(g_1mode_psi)$control_ipeds
    V(g_1mode_psi)$control_ipeds == "Public"
    
    closeness(graph = g_1mode_psi, vids = V(g_1mode_psi)[V(g_1mode_psi)$control_ipeds == "Public"], normalized = FALSE)
    closeness(graph = g_1mode_psi, vids = V(g_1mode_psi)[V(g_1mode_psi)$control_ipeds == "Private not-for-profit"], normalized = FALSE)
    
    # igraph object = g_1mode_psi
      close_1mode_psi <- closeness(graph = g_1mode_psi, vids = V(g_1mode_psi), normalized = TRUE)
      
      close_1mode_psi
      max(close_1mode_psi)
      min(close_1mode_psi)
        
    # igraph object = g_1mode_hs
      close_1mode_hs <- closeness(graph = g_1mode_hs, vids = V(g_1mode_hs), normalized = TRUE)
      
      close_1mode_hs
      max(close_1mode_hs)
      min(close_1mode_hs)
      V(g_1mode_hs)[close_1mode_hs > .61] # vertex id = 01160835
      
    # igraph object = g_2mode
      close_2mode <- closeness(graph = g_2mode, vids = V(g_2mode), normalized = TRUE)
      
      close_2mode
      
      # high schools
      close_2mode[V(g_2mode)$type == FALSE]
      max(close_2mode[V(g_2mode)$type == FALSE])
      min(close_2mode[V(g_2mode)$type == FALSE])

      # universities
      close_2mode[V(g_2mode)$type == T]
      max(close_2mode[V(g_2mode)$type == T])
            
  # betweenness centrality
    # Betweenness centrality measures are aimed at summarizing the extent to which a vertex is located ‘between’ other pairs of vertices.
    ?betweenness
        
    # not sure how important betweenness centrality or eigen_centrality are to our analyses
      
  # eigenvector centrality
    # centrality measures that are based on notions of ‘status’ or ‘prestige’ or ‘rank.’ That is, they seek to capture the idea that 
      # the more central the neighbors of a vertex are, themore central that vertex itself is.
    ?eigen_centrality
        
# characterizing edges    
    
    # skipped
      
## ---------------------------
## 4.3 Characterizing network cohesion
## ---------------------------
    
  #What is network cohesion
    # "network cohesion" defined as the extent to which subsets of vertices are cohesive—or ‘stuck together’— with respect to the relation defining edges in the network graph."
    # own: what is meant by "with respect to the relation defining edges in the network graph"

  # Examples: Do friends of a given actor in a social network tend to be friends of one another as well? [tie = whether two vertices are friends in a social network]

# complete graph and complete bipartite graph
      
  # complete graph (1 mode)
    #- https://en.wikipedia.org/wiki/Complete_graph
    #- "In the mathematical field of graph theory, a complete graph is a simple undirected graph in which every pair of distinct vertices is connected by a unique edge"
    #- own: imagine a graph $G$ that consists of vertices A, B, C, D; in a complete graph, each vertex is connected to all other vertices. so - assuming an undirected graph - we would have the following edges: A-B, A-C, A-D, B-C, B-D, C-D      
      
  # complete bipartite graph (also called a biclique): 
    # A complete bipartite graph is a graph whose vertices can be partitioned into two subsets V1 and V2 such that no edge has both endpoints in the same subset, and every possible edge that could connect vertices in different subsets is part of the graph. 
    # Example: 
      # mode 1 consists of vertices A, B, C
      # mode 2 consists of vertices 1, 2, 3, 4
      # edges consist of:
        # A-1, A-2, A-3, A-4
        # B-1, B-2, B-3, B-4
        # C-1, C-2, C-3, C-4
      
            
      
# Define subgraph
  # - consider a graph $G$ consisting of a set of vertices $V$ and the set of edges/relations $E$ connecting vertices. 
  # - Imagine we choose a subset of vertices $V'$ within larger set $V$
  # - then the subgraph consists of all vertices $V'$ and all their relations $E'$ which are a subset of the full set of relations $E$            

# Many ways to define what counts as network cohesion
  # - local cohesion = triads
  # - cohesion defined explicitly = clicks
  # - cohesion defined implicitly = "clusters" or "communities"

# Canonical example of a subgraph is a "clique"
  # cliques are "cliques are complete subgraphs and hence are subsets of vertices that are fully cohesive, in the sense that every vertex within the subset is connected by one edge to every other vertex within the subset"            
  # can use igraph::cliques() to identify cliques in a 1-mode network
      
  # we can only apply cliques() function to 1-mode networks. let's try g_1mode_hs where:
    # vertices = high schools
    # edges between two high schools: at least one university visited both high schools
      
    g_1mode_hs
    # cliques(g_1mode_hs) # note that cliques of larger sizes include cliques of smaller sizes
    
  # maximal clique is a clique that is not a subset of a smaller clique
  # max_cliques() # 
    
    #- a "maximal" clique is a clique that is not a subset of a larger clique
    
    #`igraph::max_cliques()`: "finds all maximal cliques in the input graph. A clique in maximal if it cannot be extended to a larger clique. The largest cliques are always maximal, but a maximal clique is not neccessarily the largest."
    
    #- syntax: `max_cliques(graph, min = NULL, max = NULL, subset = NULL, file = NULL)`
    #- where:
      #- `subset`. If not NULL, then it must be a vector of vertex ids, numeric or symbolic if the graph is named. The algorithm is run from these vertices only, so only a subset of all maximal cliques is returned
      #- `file`. If not NULL, then it must be a file name, i.e. a character scalar. The output of the algorithm is written to this file. (If it exists, then it will be overwritten.)
    
      
# packages/functions to identify bicliques in a bipartite network
  install.packages("biclique")
  library(biclique)
  ?biclique
  # from https://bmcresnotes.biomedcentral.com/articles/10.1186/s13104-020-04955-0
    # Biclique consists of four R functions. The core function, bi.clique, invokes an efficient algorithm to enumerate maximal bicliques. Three utility functions, bi.format, bi.print, and bi.degree, provide formatting and output support.

  ?bi.clique # the core function         
      
    # system.file("extdata", "example1.el", package = "biclique")

  library(bipartite)
  ?bipartite

# DYADS AND TRIADS
  
  # note: both dyad_census and triad_census are meant for directed graphs
  
  # dyads
    # dyads have two possible states in undirected graphs: null and present
    # dyads have three possible states in directed graphs: null; asymmetric; mutual
  # census of dyads: for each pair of vertices, identify the state
  
  #let's try g_1mode_hs where:
    # vertices = high schools
    # edges between two high schools: at least one university visited both high schools
      
    g_1mode_hs
    # run on 1 mode psi data
    dyad_census(g_1mode_psi)
    
    dyad_census(g_1mode_hs) # seems to be more for directed graphs
    ?dyad_census
  
  # note: both dyad_census and triad_census are meant for directed graphs
    # triad_census(g_1mode_psi)
    
# DENSITY AND RELATED NOTIONS OF RELATIVE FREQUENCY
  
  # "density"
    # density is the frequency of realized edges (i.e., edges that exist) relative to potential edges
  
  #sub-graph H
    #- graph $ G = (V,E)$; subgraph $H$ consists of a subset of vertices from $V$ denotes $V_H$ and all pairs the edges $E_H$ that connect pairs of vertices in $V_H$
  
  
  #density of a subgraph $H$ (for an undirected graph)
    # E- $den(H) = \frac{|E_H|}{|V_H|(|V_H|-1)/2}$
    # E- __"The value of den(H) will lie between zero and one and provides a measure of how close H is to being a clique."__
  
  #?edge_density of entire network
    edge_density(g_1mode_psi)
    edge_density(g_1mode_hs)
    
  # the neighborhood function
    # syntax: neighborhood(graph, order = 1, nodes = V(graph), mode = c("all", "out", "in"), mindist = 0)
  
  # create ego network for a single high school
    V(graph = g_1mode_hs)
    as.integer(V(graph = g_1mode_hs))
    #as.integer(V(graph = g_2mode)[V(g_2mode)$type == TRUE])
    
  # identify the neighbors of a particular private high school
    # you can do this using numeric node id number or the vertex name attribute attached to particular id number
    
    #using node id number
    neighborhood(
      graph = g_1mode_hs, 
      order = 1, 
      nodes = c(1),
      mindist = 0
    )
    
    # using vertex name attribute attached to particular id number
      # choate == "00233261"
      # HOLY SPIRIT CATHOLIC SCHOOL == "00000044"; in tuscaloosa AL
    
    # holy spirit catholic school in tuscaloosa AL
    neighborhood(
      graph = g_1mode_hs, 
      order = 1, 
      nodes = c("00000044"),
      mindist = 0
    )
    
    # holy spirit catholic school and Choate
    neighborhood(
      graph = g_1mode_hs, 
      order = 1, 
      nodes = c("00000044","00233261"),
      mindist = 0
    )
    
    # create ego network for holy spirit catholic school
      
      # identifying vertices that are immediate neighbors
      neighborhood(graph = g_1mode_hs, order = 1, nodes = c("00000044"), mindist = 0) # list of one element
      neighborhood(graph = g_1mode_hs, order = 1, nodes = c("00000044"), mindist = 0)[[1]] # named numeric vector
      
      # create ego network subgraph [one step]
      induced_subgraph(
        graph = g_1mode_hs,
        vids = neighborhood(graph = g_1mode_hs, order = 1, nodes = c("00000044"), mindist = 0)[[1]]    
      )
      
      # assign object
      holy_spirit <- induced_subgraph(
        graph = g_1mode_hs,
        vids = neighborhood(graph = g_1mode_hs, order = 1, nodes = c("00000044"), mindist = 0)[[1]]    
      )
      class(holy_spirit)
      holy_spirit
        vcount(holy_spirit)*(vcount(holy_spirit)-1)/2
        ecount(holy_spirit)
        ecount(holy_spirit)/(vcount(holy_spirit)*(vcount(holy_spirit)-1)/2)
    
      choate <- induced_subgraph(
        graph = g_1mode_hs,
        vids = neighborhood(graph = g_1mode_hs, order = 1, nodes = c("00233261"), mindist = 0)[[1]]    
      )
      choate
      
      # "00000226" = john carroll catholic high school in birmingham AL
      john_carroll <- induced_subgraph(
        graph = g_1mode_hs,
        vids = neighborhood(graph = g_1mode_hs, order = 1, nodes = c("00000226"), mindist = 0)[[1]]    
      )
      john_carroll
      
        vcount(john_carroll)*(vcount(john_carroll)-1)/2
        ecount(john_carroll)
        ecount(john_carroll)/(vcount(john_carroll)*(vcount(john_carroll)-1)/2)
        
      edge_density(john_carroll)
      
      edge_density(g_1mode_hs)
      edge_density(holy_spirit)
      edge_density(choate)
    
# clustering coefficient/transitivity
  #?transitivity
    #Transitivity measures the probability that the adjacent vertices of a vertex are connected. This is sometimes also called the clustering coefficient.
      
  # calculate transitivity for entire network
    transitivity(
      graph = g_1mode_hs,
      type = "global",
      vids = NULL,
      weights = NULL,
      #isolates = "zero", # isolates = c("NaN", "zero")
    )

    # weighted transitivity; calculates transitivity for each vertex
    edge_attr_names(g_1mode_hs)
    transitivity(
      graph = g_1mode_hs,
      type = "weighted",
      vids = NULL,
      weights = NULL,
      #isolates = "zero", # isolates = c("NaN", "zero")
    )
    
  # calculate transitivity for specific local vertices
  transitivity(
    graph = g_1mode_hs,
    type = "local",
    vids = c("00000044","00233261"),
    weights = NULL,
    #isolates = "zero", # isolates = c("NaN", "zero")
  )    

  # calculates weighted transitivity for specific local vertices  
  transitivity(
    graph = g_1mode_hs,
    type = "weighted",
    vids = c("00000044","00233261"),
    weights = NULL,
    #isolates = "zero", # isolates = c("NaN", "zero")
  )
  
# CONNECTIVITY, CUTS, AND FLOWS  
  
  # Recall that a graph G is said to be connected if every vertex is reachable from every other (i.e., if for any two vertices, there exists a walk between the two), and that a connected component of a graph is a maximally connected subgraph

  # all are connected
  is_connected(g_1mode_psi)
  is_connected(g_1mode_hs)
  is_connected(g_2mode)
  
  #?deompose
    # decomposes graph object into separate graph objects for each component
    # result is a "list of graph objects"; presumably one graph object per component
    # if the graph is connected, then decompose(graph) will yield the original graph object cuz it can't be decomposed
  decompose(g_1mode_psi)
  str(decompose(g_1mode_psi))
  
  # a census of connected components and how many vertices in each
  table(sapply(decompose(g_1mode_psi), vcount))
  
  table(sapply(decompose(g_1mode_hs), vcount))
  
  table(sapply(decompose(g_2mode), vcount))
  
  # biggest connected component is the "giant component"; since our igraph objects only have a single component (i.e., all vertices are connected)  
    # then the giant component is the same thing as the graph
  
  # "small world property"
    # a celebrated characteristic observed in the giant component of many real-world networks is the so-called smallworld property,which refers to the situation
      # wherein (a) the shortest-path distance between pairs of vertices is generally quite small, but (b) the clustering is relatively high.
  
  # ?mean_distance
    # mean_distance calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices (both ways for directed graphs)
  mean_distance(g_1mode_psi)
  mean_distance(g_1mode_hs)
  mean_distance(g_2mode)
  
  #?diameter
    #The diameter of a graph is the length of the longest geodesic.
  diameter(g_1mode_psi) # 31 is max. which set of pairs is this?
  diameter(g_1mode_hs) # 3 is max
  diameter(g_2mode) # 10 is max
  
  
## ---------------------------
## 4.4 graph partitioning (community detection)
## ---------------------------
 
# overview of what is partitioning and why useful     
  # "Partitioning" —broadly speaking—refers to the segmentation of a set of elements into ‘natural’ subsets

  #How partitioning is deemed useful in network analysis
    #- "In the analysis of network graphs, partitioning is a useful tool for finding, in an unsupervised fashion, subsets of vertices that demonstrate a ‘cohesiveness’ with respect to the underlying relational patterns."
  #what is meant by a "cohesive" subset of vertices (conceptually)?
    #- "A ‘cohesive’ subset of vertices generally is taken to refer to a subset of vertices that [satisfy two broad conditions]:
    # - (i) are well connected among themselves, 
    # - (ii) and, at the same time, are relatively well separated from the remaining vertices."
  
#Two well-established classes of methods for "community detection"

#1. methods based on adaptations of hierarchical clustering
#1. methods based on spectral partitioning

#what Zach says [from 8/14/2020 email] on whether to prefer hierarchical clustering vs. spectral partitioning approach:

  #- "I forget precisely what spectral partitioning means.  
  #I like hierarchical clustering when starting out because it’s intuitive and produces decent looking charts.  
    # Modularity maximization, the most common one now, is a type of hierarchical clustering."  
  
#?cluster_fast_greedy  
  
kc_1mode_psi <- cluster_fast_greedy(g_1mode_psi)

kc_1mode_psi
str(kc_1mode_psi) # a list of two elmenents, one for each community

length(kc_1mode_psi)
sizes(kc_1mode_psi)

membership(kc_1mode_psi)

plot(
  kc_1mode_psi,
  g_1mode_psi,
  vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds
)

  # in addition to the two communities the dend plot shows which vertex within a commmnity is particularly similar to one another
   # hierarchical clustering methods actually produce, as the name indicates, an entire hierarchy of nested partitions of the graph, not just a single partition
  
  library(ape)
  dendPlot(kc_1mode_psi, mode="phylo")
  
  #dendPlot(kc_1mode_psi, mode="phylo",#vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds)

  # how to do "Modularity maximization" which Zach said was the most common type of hierarchical clustering
    ?cluster_optimal
    # syntax
      #cluster_optimal(graph, weights = NULL)
    #Arguments
      # graph	
        # The input graph. Edge directions are ignored for directed graphs.
      # weights	
        # Optional positive weight vector for optimizing weighted modularity. If the graph has a weight edge attribute, then this is used by default. Supply NA to ignore the weights of a weighted graph. Larger edge weights correspond to stronger connections.  

  coptimal_1mode_psi <- cluster_optimal(graph = g_1mode_psi)

  coptimal_1mode_psi
  str(coptimal_1mode_psi) # a list of two elmenents, one for each community
  
  length(coptimal_1mode_psi)
  sizes(coptimal_1mode_psi)
  
  membership(coptimal_1mode_psi)
  membership(kc_1mode_psi)
  
  plot(
    coptimal_1mode_psi,
    g_1mode_psi,
    vertex.label = V(g_1mode_psi)$univ_abbrev_ipeds
  )
  
# cluster fast and greedy applied to 1-mode HS object
  # motivating idea: which high schools are similar to one another with respect to high schools being visited by one or more universities in common

  c_1mode_hs <- cluster_fast_greedy(g_1mode_hs)

  length(c_1mode_hs) # 4 communities
  sizes(c_1mode_hs) # of size (respectively): 62, 609, 33, 1038

  c_1mode_hs
  str(c_1mode_hs)
  
  membership(c_1mode_hs)

plot(
  c_1mode_hs,
  g_1mode_hs,
  vertex.label = NA,
  vertex.size = 2
)

  # what would you do with this? plotting seems not helpful for so many vertices. 
    # think you would merge back to original igraph object as a vertex attribute
    # and then compare how characteristics of vertices across different communities

# possible to apply fast and greedy to 2-mode object?

c_2mode <- cluster_fast_greedy(g_2mode)

  length(c_2mode) # 5
  sizes(c_2mode) # of size (respectively): 35, 483, 463, 377, 77

  c_2mode
  str(c_2mode)
  
  membership(c_2mode)
  
  # it is possible to run the function but not sure if results are worthwhile
  
# fast and greedy applied to ego networks?

  emory_order1 <- subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==1])
  
  emory_order2 <- subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==2])
  
  # plot emory order 1
      plot.igraph(
        #x = subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==1]), # emory
        x = emory_order1,
        vertex.label = NA,
        vertex.shape = "circle",
        vertex.color = "lightblue",
        vertex.size = 5,        
        layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
      )

      emory_order1 %>% V()
      emory_order1 %>% E()
      
  # plot emory order 2

        plot.igraph(
          #x = subgraph.edges(graph = egos_psi[["139658"]], eids = E(egos_psi[["139658"]])[E(egos_psi[["139658"]])$order==2]), # emory
          x = emory_order2,
          vertex.label = NA,
          vertex.shape = "circle",
          vertex.color = "lightblue",
          vertex.size = 5,        
          layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle,
        )      
            
      emory_order2 %>% V()
      emory_order2 %>% E()
      
    # try to create communities from emory order 2
      c_emory_order2 <- cluster_fast_greedy(emory_order2)
      
      length(c_emory_order2) # 4
      sizes(c_emory_order2) # 34, 100, 104, 62
    
      c_emory_order2
      str(c_emory_order2)
      
      membership(c_emory_order2)
      

      plot(
        c_emory_order2,
        emory_order2,
        #vertex.shape <- if_else(V(emory_order2)$type, "square", "circle"),
        vertex.size <- if_else(V(emory_order2)$type, 5, 2),
        vertex.label = if_else(V(emory_order2)$type, V(emory_order2)$univ_abbrev_ipeds, ""),
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
  
## ---------------------------
## 4.5 assortativity and mixing
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