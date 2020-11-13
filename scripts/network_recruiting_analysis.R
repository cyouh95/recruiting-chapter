library(igraph)
library(tidyverse)

options(max.print=2000)
## -------------
## LOAD OBJECTS
## -------------

# Run create_igraph_objects.R
scripts_dir <- file.path('.', 'scripts')
source(file = file.path(scripts_dir, 'create_igraph_objects.R'))

# Summary of objects (1742 private HS + 43 univs = 1785 vertices total)
  # 2-mode igraph objects
    # g_2mode (1785 vertices + 11034 edges)
    # g_2mode_privu (1356 vertices + 7310 edges)
    # g_2mode_pubu (1481 vertices + 3724 edges)
  # 1-mode igraph objects [HS]
    # g_1mode_hs (1742 vertices + 745145 edges)
    # g_1mode_hs_privu (1330 vertices + 520723 edges)
    # g_1mode_hs_pubu (1464 vertices + 448865)
  # 1-mode igraph objects [PSI]
    # g_1mode_psi (43 vertices + 897 edges)
    # g_1mode_psi_privu (26 vertices + 325 edges)
    # g_1mode_psi_pubu (17 vertices + 130 edges)
  # Ego igraph objects [HS]
    # egos_hs (1742 ego graphs)
    # egos_hs_privu (1330 ego graphs)
    # egos_hs_pubu (1464 ego graphs)
  # Ego igraph objects [PSI]
    # egos_psi (43 ego graphs)
    # egos_psi_privu (26 ego graphs)
    # egos_psi_pubu (17 ego graphs)


## --------------------
## EGO IGRAPH FUNCTION (can utilize order =1, order =2, or both)
## --------------------

plot_ego_graph <- function(univ_id, characteristic, values, keys, colors = c('lightblue', 'lightgreen', 'violet', 'yellow'), title = univ_info[univ_info$univ_id == univ_id, ] %>% select(univ_abbrev) %>% as.character(), graph_order = 'both', margin = 0) {

  network <- egos_psi_privu[[univ_id]]
    
  # network object
  if (graph_order != 'both') {  # order == 1 or 2
    network <- subgraph.edges(graph = network, eids = E(network)[E(network)$order == graph_order]) # create subgraph network object if order != "both"
  }

  # Vertex color  
    if (is.na(characteristic)==TRUE & graph_order %in% c(1,2)  ) { # no characteristics; order = 1 or 2
      vertex_color <- if_else(V(network)$type, 'lightblue', 'salmon') # do not use characteristic to determine vertex color/shape
      # note: if order = 2, ego is not included as a vertex
    } else if (is.na(characteristic)==TRUE & graph_order %in% c('both')) {  # no characteristics; order == both
      vertex_color <- case_when(
        vertex_attr(network, "type") == FALSE ~ 'salmon',
        vertex_attr(network, "type") == TRUE &  vertex_attr(network, "name") != univ_id ~ 'lightblue',
        vertex_attr(network, "type") == TRUE &  vertex_attr(network, "name") == univ_id ~ 'purple',
      )
    } else {  # yes characteristics
      color_palette <- colors
      names(color_palette) <- values
      
      vertex_color <- recode(vertex_attr(network, characteristic), !!!color_palette) # use characteristic to determine vertex color/shape
    }

  # vertex size; edge width, color, line type
  if (graph_order %in% c('both',2)) {  # graph order is 'both' 1 and 2; or graph order == 2
    vertex_size <- case_when(
      vertex_attr(network, "type") == FALSE ~ 2,
      vertex_attr(network, "type") == TRUE &  vertex_attr(network, "name") != univ_id ~ 10,
      vertex_attr(network, "type") == TRUE &  vertex_attr(network, "name") == univ_id ~ 15,
    )
    
    edge_width <- if_else(E(network)$order == 1, 0.5, 0)
    edge_color <- 'lightgrey'
    edge_lty <- 5

  } else {  # graph order == 1
    vertex_size <- if_else(V(network)$type, 15, 2)
    
    edge_width <- if_else(E(network)$weight == 1, 0.5, as.numeric(E(network)$weight)^2)
    edge_color <- if_else(E(network)$weight == 1, 'lightgrey', 'black')
    edge_lty <- if_else(E(network)$weight == 1, 5, 1)

  }
  
  # graph layout
    # giving graph layout it's own set of if/else conditions because we will play w/ layout a lot]
    # layout_with_kk, layout_with_fr, layout_in_circle, layout_nicely
  if (graph_order == 1) { 
    graph_layout <- layout_nicely
  } else if (graph_order == 2) {  
    graph_layout <- layout_nicely
  } else {  # order == both
    graph_layout <- layout_with_kk
    #graph_layout <- layout_with_fr
  }


  plot.igraph(
    x = network,
    # vertex attributes
    vertex.shape = 'circle',
    vertex.label = if_else(V(network)$type, V(network)$school_name, ''),
    vertex.color = vertex_color,
    vertex.size = vertex_size,
    vertex.frame.color = if_else(V(network)$type, 'lightgray', 'lightgray'),  
    # edge attributes
    edge.color = edge_color,
    edge.lty = edge_lty,
    edge.width = edge_width,
    layout = graph_layout,  
    main = str_to_title(title),
    margin = margin # -0.2
  )  

  if (is.na(characteristic)==FALSE) {  # if characteristic ! FALSE then create legend    
    legend(
      x = .8,
      y = 0.0,
      legend = keys,
      fill = colors,
      bty = 'n'  # box drawn around legend: 'o' [default] = solid line box; 'n' = no box
    )
  }
  
}
 #dev.off() # to fix this: Error in .Call.graphics(C_palette, value) : invalid graphics state




par(mfrow=c(1, 1))  # resets to single plot
plot_ego_graph(univ_id = "160755", characteristic = NA, values = NA, keys = NA, colors = NA, graph_order = 1)

plot_ego_graph(univ_id = "160755", characteristic = NA, values = NA, keys = NA, colors = NA, title, graph_order = 1)
plot_ego_graph(univ_id = "160755", characteristic = NA, values = NA, keys = NA, colors = NA, title = "", graph_order = 2)
plot_ego_graph(univ_id = "160755", characteristic = NA, values = NA, keys = NA, colors = NA, title = "", graph_order = "both")

plot_ego_graph(univ_id = "160755", characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), title = "geographic region", graph_order = 1)
plot_ego_graph(univ_id = "160755", characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), title = "geographic region", graph_order = 2)
plot_ego_graph(univ_id = "160755", characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), title = "geographic region", graph_order = 'both')


#For catholic, show order equals both. one big graph of one univ and maybe characteristic equals religious
  vertex_attr_names(g_2mode)
  
  table(V(g_2mode)$religion, useNA = "always")
  table(V(g_2mode)$religion_4, useNA = "always")

  vertex_attr_names(egos_psi_privu[["216597"]])
  
  table(V(egos_psi_privu[["216597"]])$religion, useNA = "always")
  table(V(egos_psi_privu[["216597"]])$religion_4, useNA = "always")
  

  # villanova ego network, religion overlayed
  pdf("assets/figures/villanova_religion.pdf", paper = "a4r") # open file
  
    par(mar=c(5, 4, 4, 2) + 0.1) # default margins
    #par(mar=c(0, 0, 0, 0) + 0.1, mai=c(0, 0, 0, 0))
  
    plot_ego_graph(univ_id = "216597", characteristic = 'religion_4', values = c('catholic', 'christian', 'nonsectarian', 'other'), keys = c('Catholic', 'Christian', 'Nonsectarian', 'Other'), title = "", graph_order = 'both', margin = -0.3)
    
  dev.off() # close the file  


# show order = both of one interesting university, with an overlay
# EMORY
par(mfrow=c(1, 1))  # resets to single plot 
pdf("assets/figures/emory_region.pdf", paper = "a4r") # open file
  
  #par(mar=c(5, 4, 4, 2) + 0.1) # default margins
  par(mar=c(0, 0, 0, 0) + 0.1, mai=c(0, 0, 0, 0))
  
  # show order = 2 of one interesting university, with an overlay
  plot_ego_graph(univ_id = "139658", characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), title = "", graph_order = 'both', margin = 0.0) 
    #plot_ego_graph(univ_id = "139658", characteristic = NA, values = NA, keys = NA, colors = NA, graph_order = 'both', margin = -0.4)    
    
dev.off() # close the file  

par(mfrow=c(2, 2))
plot_ego_graph(univ_id = "160755", characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), title = "geographic region", graph_order = 1)
plot_ego_graph(univ_id = "160755", characteristic = 'religion', values = c('catholic', 'conservative_christian', 'nonsectarian', 'other_religion'), keys = c('Catholic', 'Conservative Christian', 'Nonsectarian', 'Other'), title = "religious affiliation", graph_order = 1)
#plot_ego_graph(univ_id = "160755", characteristic = 'rank_cat1', values = c('c1_top100','c2_top200','c3_A+','c4_ltA+'), keys = c('Rank top 100', 'Rank 100-200', 'A+', 'A or below'), title = "Academic reputation", graph_order = 1)
plot_ego_graph(univ_id = "160755", characteristic = 'rank_cat2', values = c('c1_top200','c2_A+','c3_A','c4_ltA'), keys = c('Rank top 200', 'A+', 'A', 'A- or below'), title = "Academic reputation", graph_order = 1)
plot_ego_graph(univ_id = "160755", characteristic = 'pct_blacklatinxnative_cat', values = c('c1_lt10','c2_10to25','c3_25to50','c4_50+'), keys = c('LT 10%', '10-25%', '25-50%', 'GT 50%'), title = "percent black, latinx, or Native", graph_order = 1)
#plot_ego_graph(univ_id = "160755", characteristic = 'pct_white_cat', values = c('c1_lt50','c2_50to75','c3_75to85','c4_85+'), keys = c('LT 50% white', '50-75% white', '75-85% white', 'GT 85% white'), title = "percent white", graph_order = 1)


# ORDER = 1
par(mfrow=c(2, 2))
plot_ego_graph(ego_network, characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), title = "geographic region", graph_order = 1)
plot_ego_graph(ego_network, characteristic = 'religion', values = c('catholic', 'conservative_christian', 'nonsectarian', 'other_religion'), keys = c('Catholic', 'Conservative Christian', 'Nonsectarian', 'Other'), title = "religious affiliation", graph_order = 1)
#plot_ego_graph(ego_network, characteristic = 'rank_cat1', values = c('c1_top100','c2_top200','c3_A+','c4_ltA+'), keys = c('Rank top 100', 'Rank 100-200', 'A+', 'A or below'), title = "Academic reputation", graph_order = 1)
plot_ego_graph(ego_network, characteristic = 'rank_cat2', values = c('c1_top200','c2_A+','c3_A','c4_ltA'), keys = c('Rank top 200', 'A+', 'A', 'A- or below'), title = "Academic reputation", graph_order = 1)
plot_ego_graph(ego_network, characteristic = 'pct_blacklatinxnative_cat', values = c('c1_lt10','c2_10to25','c3_25to50','c4_50+'), keys = c('LT 10%', '10-25%', '25-50%', 'GT 50%'), title = "percent black, latinx, or Native", graph_order = 1)
#plot_ego_graph(ego_network, characteristic = 'pct_white_cat', values = c('c1_lt50','c2_50to75','c3_75to85','c4_85+'), keys = c('LT 50% white', '50-75% white', '75-85% white', 'GT 85% white'), title = "percent white", graph_order = 1)

# ORDER = BOTH
par(mfrow=c(2, 2))
plot_ego_graph(ego_network, characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), title = "geographic region", graph_order = 'both')
plot_ego_graph(ego_network, characteristic = 'religion', values = c('catholic', 'conservative_christian', 'nonsectarian', 'other_religion'), keys = c('Catholic', 'Conservative Christian', 'Nonsectarian', 'Other'), title = "religious affiliation", graph_order = 'both')
#plot_ego_graph(ego_network, characteristic = 'rank_cat1', values = c('c1_top100','c2_top200','c3_A+','c4_ltA+'), keys = c('Rank top 100', 'Rank 100-200', 'A+', 'A or below'), title = "Academic reputation", graph_order = 'both')
plot_ego_graph(ego_network, characteristic = 'rank_cat2', values = c('c1_top200','c2_A+','c3_A','c4_ltA'), keys = c('Rank top 200', 'A+', 'A', 'A- or below'), title = "Academic reputation", graph_order = 'both')
plot_ego_graph(ego_network, characteristic = 'pct_blacklatinxnative_cat', values = c('c1_lt10','c2_10to25','c3_25to50','c4_50+'), keys = c('LT 10%', '10-25%', '25-50%', 'GT 50%'), title = "percent black, latinx, or Native", graph_order = 'both')
#plot_ego_graph(ego_network, characteristic = 'pct_white_cat', values = c('c1_lt50','c2_50to75','c3_75to85','c4_85+'), keys = c('LT 50% white', '50-75% white', '75-85% white', 'GT 85% white'), title = "percent white", graph_order = 'both')

#par(mfrow=c(1, 1))  # resets to single plot

# MESSING AROUND WITH THE RANKING TABLE



## ------------------------------
## WRITE COMMUNITY DETECTION FUNCTION
## ------------------------------

# FUNCTION TO RUN HIERARCHICAL CLUSTER ANALYSIS
  # STEPS IN HIERARCHICAL CLUSTER ANALYSIS:
  #create adjacency matrix of 1-mode university object
  # create "distance matrix" using stats::dist() function
  # conduct hierarchical clustering using stats::hclust
  # cut the dendrogram to create the desired number of clusters using stats::cutree
  # merge the cluster assignments to the igraph object as a vertex attribute


create_hclust <- function(network, mode, k = NULL, h = NULL, dist_method = "euclidean", hclust_method = "complete") {

  # mode = psi or hs
  if (mode == "hs") {
    proj <- "proj1"
  } else {  
    
    proj <- "proj2"
    
  }

  #create adjacency matrix of 1-mode university object
  hclust_obj <- as_adjacency_matrix(
    graph = bipartite.projection(network)[[proj]],  # creates one-mode object w/ mode = universities
      sparse = FALSE, 
      attr = 'weight'
    ) %>%
    # create "distance matrix" using stats::dist() function
    dist(
      #x = affil_1mode_psi , # 	a numeric matrix, data frame or "dist" object.
      method = dist_method, # the distance measure to be used. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski". default = euclidian where Usual distance between the two vectors (2 norm aka L_2), sqrt(sum((x_i - y_i)^2)).
      diag = TRUE, # FALSE, # logical value indicating whether the diagonal of the distance matrix should be printed by print.dist. default = FALSE
      upper = TRUE, # logical value indicating whether the upper triangle of the distance matrix should be printed by print.dist.
      p = 2 # The power of the Minkowski distance. default = 2
    ) %>% 
    # conduct hierarchical clustering using stats::hclust
    hclust(
      #d = dist_1mode_psi, # a dissimilarity structure as produced by dist() function
      method = hclust_method, # default = complete; the agglomeration method to be used. This should be (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
      members = NULL # NULL or a vector with length size of d. See the ‘Details’ section. default = NUKK
    ) 
    
    # plot dendrogram associated with hclust object
    hclust_obj %>% as.dendrogram() %>% plot(horiz = TRUE)
    
    # cut the dendrogram to create the desired number of clusters using stats::cutree
    cutree(
      tree = hclust_obj, # a tree as produced by hclust. cutree() only expects a list with components merge, height, and labels, of appropriate content each.
      k = k, # NULL, # default = NULL an integer scalar or vector with the desired number of groups
      h = h # default = NULL numeric scalar or vector with heights where the tree should be cut
    )

}
# create_hclust <- function(network, mode, k = NULL, h = NULL, dist_method = "euclidean", hclust_method = "complete") {
# where
  # k = desired number of groups, hierarchical cluster analysis only, default = NULL
  # h = numeric scalar with heights where tree should be cut to create groups, hierarchical cluster analysis only, default = NULL


create_hclust(network = g_2mode, mode = "psi", k = 4, h = NULL) %>% table()
create_hclust(network = g_2mode, mode = "psi", k = 4, h = NULL, hclust_method = "average") %>% table()

create_hclust(network = g_2mode, mode = "psi", k = 6, h = NULL, hclust_method = "complete") %>% table()

#create_hclust(network = g_2mode, mode = "psi", k = NULL, h = 600)

## ---------------------------
## PLOT 2-MODE IGRAPH OBJECT; PUBLIC AND PRIVATE COLLEGES/UNIVERSITIES AND ALL HIGH SCHOOLS THEY VISIT
## ---------------------------



# which layout to choose? ask Russ?
  # on layout_with_kk as an example of an "energy displacement" method, from Kolaczyk & Csardi 2020, pg 32-33
    # motivated by the fact that it is possible to associate the collection of forces in spring systems with an overall system energy, another common approach to generating layouts is that of energy-placement methods. An energy, as a function of vertex positions, ostensibly is defined using expressions motivated by those found in physics. A vertex placement is chosen which minimizes the total system energy. A physical system with minimum energy is typically in its most relaxed state, and hence the assertion here is that a graph drawn according to similar principles should be visually appealing. 
    # says that methods based on multidimensional scaling (MDS) are of the "energy displacement" type
    # layout_with_kk is a commonly used variant of multidimensional scaling approach
  # on layout_with_fr as an example of "spring-embedder methods", from Kolaczyk & Csardi 2020, pg 32
    #Often more effective for creating useful drawings are layouts based on exploiting analogies between the relational structure in graphs and the forces among elements in physical systems. One approach in this area, and the earliest proposed, is to introduce attractive and repulsive forces by associating vertices with balls and edges with springs. If a literal system of balls connected by springs is disrupted, thereby stretching some of the springs and compressing others, upon being let go it will return to its natural state. So-called spring-embedder methods of graph drawing define a notion of force for each vertex in the graph depending, at the very least, on the positions of pairs of vertices and the distances between them, and seek to iteratively update the placement of vertices until a vector of net forces across vertices converges. 
    #The method of Fruchterman and Reingold [6] is a commonly used example of this type.

par(mfrow=c(1, 1))  # resets to single plot
#graph_layout <- layout_with_kk
graph_layout <- layout_with_fr

pdf("assets/figures/plot_g_2mode.pdf", paper = "a4r") # open file

# par(mar=c(5, 4, 4, 2) + 0.1) # default margins
par(mar=c(0, 0, 0, 0) + 0.1, mai=c(0, 0, 0, 0))
plot(
  x = g_2mode, 
  vertex.label = if_else(V(g_2mode)$type, V(g_2mode)$school_name, ''),
  vertex.color = case_when(
      vertex_attr(g_2mode, "type") == FALSE ~ 'salmon',
      vertex_attr(g_2mode, "type") == TRUE &  vertex_attr(g_2mode, "name") %in% privu_vec ~ 'green',
      vertex_attr(g_2mode, "type") == TRUE &  vertex_attr(g_2mode, "name") %in% pubu_vec  ~ 'violet',
    ),
  vertex.frame.color = if_else(V(g_2mode)$type, 'lightgray', 'lightgray'),  
  vertex.shape = if_else(V(g_2mode)$type, 'circle', 'circle'),
  vertex.size = if_else(V(g_2mode)$type, 2.5, .5),
  edge.lty = 3, # 0 (“blank”), 1 (“solid”), 2 (“dashed”), 3 (“dotted”), 4 (“dotdash”), 5 (“longdash”), 6 (“twodash”).
  edge.lty = .5,
  edge.color = 'lightgrey',
  layout = graph_layout,
  margin = -0.65
)

dev.off() # close the file

## ---------------------------
## PLOT 2-MODE IGRAPH OBJECTS
## ---------------------------



# analysis = whether to run hierarchical cluster analysis or fast_and_greeedy
# k = number of groups to specify
# h = height to determine groups 
  #for cluster_fast_and_greedy() h refers to number of merges to determine cluster; 
    # seems like n-2 equals number of merges in cluster_fast_and_greedy() %>% membership()

  
save_2mode_plot <- function(network, pubu_visits = 'all', privu_visits = 'all', layout = layout_with_fr, c_analysis, k = NULL, h = NULL, hclust_method = "ward.D", plot_margin = -0.5, plot_name = "plot") {

  # all visits for private; out-of-state only for publics; 
  if (pubu_visits == 'outst' & privu_visits != 'outst') {
    
    network <- subgraph.edges(graph = network, eids = E(network)[(E(network)$visiting_univ == "public" & E(network)$visit_loc == "outofstate") | E(network)$visiting_univ == "private"]) 
  }
  
  # out-of-state visits only for both public and private colleges/universities
  if (pubu_visits == 'outst' & privu_visits == 'outst') {
    
    network <- subgraph.edges(graph = network, eids = E(g_2mode)[E(network)$visit_loc == "outofstate"])
  }
  
  # print summary of network to check that network object has the right number of vertices and edges
    #summary(network)

  #network_name <- deparse(substitute(network)) # why doesn't this work?
  #print(network_name)

  # run desired cluster analysis to create communities
    # create vertex attribute "cluster_psi" that is assigned only for vertices where V(network)$type == TRUE    
  if (c_analysis == "hclust") { # run hierarchical cluster analysis

      vertex_attr(graph = network, name = "cluster_psi", index = V(network)$name[V(network)$type == TRUE]) <- create_hclust(network = network, mode = "psi", k = k, h = h, hclust_method = hclust_method)
      
  } else {  # run igraph::fast_and_greedy() cluster analysis
    
    c_obj <- bipartite.projection(network)[["proj2"]] %>% cluster_fast_greedy() 
    
    dendPlot(c_obj, mode="phylo")

    if (is.null(k)==1 & is.null(h)==1) { # if k or h not specified, use membership() to determine number of groups
      
      m_obj <- c_obj %>% membership()
      
    } else if (is.null(k)==0 & is.null(h)==1) { # if k specified and h not specified, use k to determine number of groups
      
      m_obj <- c_obj %>% cut_at(no = k)
      
    } else if (is.null(k)==1 & is.null(h)==0) { # if k not specified and h specified, use h to determine number of groups
      
      m_obj <- c_obj %>% cut_at(steps = h)
    }
    # print(m_obj)
    
    vertex_attr(graph = network, name = "cluster_psi", index = V(network)$name[V(network)$type == TRUE]) <- m_obj
    
  }
  #print(V(network)$cluster_psi)
    
  # create vertex attribute for value of cluster with high schools having value = 0
  vertex_attr(graph = network, name = "cluster") <- if_else(V(network)$type, as.integer(vertex_attr(graph = network, name = "cluster_psi")), 0L)
    
    # merge the cluster assignments to the igraph object as a vertex attribute
    #vertex_attr(graph = g_2mode, name = "temp") <- (as.data.frame(V(g_2mode)$name) %>% left_join(y = df_cut_1mode_psi, by = c(`V(g_2mode)$name` = "univ_id")))$cluster
    
    #print(V(network)$cluster)
    #print(V(network)$cluster_psi[V(network)$type==TRUE])
    
    tibble(name = V(network)$school_name, cluster = V(network)$cluster, type = V(network)$type) %>%
      filter(type == TRUE) %>% select(name, cluster) %>% arrange(cluster, name) %>% print(n = 50) # print obs
    V(network)$cluster %>% table() %>% print() # print table
    
    #vertex_attr_names(g_2mode)
    vertex_color <- recode(vertex_attr(graph = network, name = "cluster"),
      `0` = "salmon",
      `1` = 'lightblue',
      `2` = 'lightgreen',      
      `3` = 'violet',
      `4` = 'yellow',
    )
    #print(vertex_color)
    
  #pdf(str_c('assets/figures/', plot_name), paper = "a4r")
  
  par(mar=c(0, 0, 0, 0) + 0.1, mai=c(0, 0, 0, 0))
  
  par(mfrow=c(1, 1))

  plot(
    x = network, 
    vertex.label = if_else(V(network)$type, V(network)$school_name, ''),
    vertex.color = vertex_color,
    vertex.frame.color = if_else(V(network)$type, 'lightgray', 'lightgray'),
    vertex.shape = 'circle',
    vertex.size = if_else(V(network)$type, 3, .75),
    edge.lty = 3,
    edge.lty = 0.5,
    edge.color = 'lightgrey',
    layout = layout,
    margin = plot_margin
  )
    
  #dev.off()
}

# priv
save_2mode_plot(g_2mode_privu, c_analysis = "hclust", k=4, h = NULL, hclust_method = "ward.D", plot_margin = -0.74, plot_name = "plot_2mode_privu.pdf")

# pub
save_2mode_plot(g_2mode_pubu, c_analysis = "hclust", k=4, h = NULL, hclust_method = "ward.D", plot_margin = -0.74, plot_name = "plot_2mode_privu.pdf")

# pub out-of-state
save_2mode_plot(g_2mode_pubu, pubu_visits = 'outst', c_analysis = "hclust", k=4, h = NULL, hclust_method = "ward.D", plot_margin = -0.74, plot_name = "plot_2mode_privu.pdf")

# all
save_2mode_plot(g_2mode, c_analysis = "hclust", k=4, h = NULL, hclust_method = "ward.D", plot_margin = -0.74, plot_name = "plot_2mode_privu.pdf")

# all, out-of-state pub
save_2mode_plot(g_2mode, pubu_visits = 'outst', c_analysis = "hclust", k=4, h = NULL, hclust_method = "ward.D", plot_margin = -0.74, plot_name = "plot_2mode_privu.pdf")

# all, out-of-state all
save_2mode_plot(g_2mode, pubu_visits = 'outst', privu_visits = 'outst', c_analysis = "hclust", k=4, h = NULL, hclust_method = "ward.D", plot_margin = -0.74, plot_name = "plot_2mode_privu.pdf")



## ---------------------------
## WRITE FUNCTION TO PLOT 1-MODE IGRAPH OBJECTS
## ---------------------------

save_1mode_plot <- function(network, mode, pubu_visits = 'all', privu_visits = 'all', layout = layout_with_fr, c_analysis, k = NULL, h = NULL, hclust_method = "ward.D", plot_margin = 0.0, plot_name = "plot") {

  # create subgraph of 2-mode network depending on whether including or excluding out-of-state visits from particular types of institutions
  
  if (pubu_visits == 'outst' & privu_visits != 'outst') { # all visits for private; out-of-state only for publics; 
    
    network <- subgraph.edges(graph = network, eids = E(network)[(E(network)$visiting_univ == "public" & E(network)$visit_loc == "outofstate") | E(network)$visiting_univ == "private"]) 
  } else if (pubu_visits == 'outst' & privu_visits == 'outst') { # out-of-state visits only for both public and private colleges/universities
    
    network <- subgraph.edges(graph = network, eids = E(g_2mode)[E(network)$visit_loc == "outofstate"])
  }
    # print summary of network to check that network object has the right number of vertices and edges
      #summary(network)

  # projection for mode = psi or hs
  if (mode == "psi") {
    proj <- "proj2" # for obtaining the 1-mode object from the 2-mode object
    
  } else {  
    
    proj <- "proj1"
    
  }

  network2 <- network # save 2-mode network object as network2 before you overwrite object "network" with 1-mode projection
  
  network <- bipartite.projection(network)[[proj]] # create 1-mode object from 2-mode object
    

  # run desired cluster analysis to create communities
    # create vertex attribute "cluster_psi" that is assigned only for vertices where V(network)$type == TRUE    
  if (c_analysis == "hclust") { # run hierarchical cluster analysis

      vertex_attr(graph = network, name = "cluster") <- create_hclust(network = network2, mode = mode, k = k, h = h, hclust_method = hclust_method) # create vertex attribute for cluster
      #create_hclust(network = network2, mode = mode, k = k, h = h)
      #vertex_attr(graph = network, name = "cluster_psi", index = V(network)$name[V(network)$type == TRUE]) <- create_hclust(network = network, mode = "psi", k = k, h = h)    
      
  } else {  # run igraph::fast_and_greedy() cluster analysis
    
    c_obj <- network %>% cluster_fast_greedy() 
    
    if (mode == "psi") {
      
      dendPlot(c_obj, mode="phylo")
    }
    
    if (is.null(k)==1 & is.null(h)==1) { # if k or h not specified, use membership() to determine number of groups
      
      m_obj <- c_obj %>% membership()
      
    } else if (is.null(k)==0 & is.null(h)==1) { # if k specified and h not specified, use k to determine number of groups
      
      m_obj <- c_obj %>% cut_at(no = k)
      
    } else if (is.null(k)==1 & is.null(h)==0) { # if k not specified and h specified, use h to determine number of groups
      
      m_obj <- c_obj %>% cut_at(steps = h)
    }
    # print(m_obj)
    
    vertex_attr(graph = network, name = "cluster") <- m_obj # create vertex attribute for cluster
    
  }
  
  tibble(name = V(network)$school_name, cluster = V(network)$cluster) %>%
    select(name, cluster) %>% arrange(cluster, name) %>% print(n = 50) # print obs
    
  V(network)$cluster %>% table() %>% print() # print table
  
    #vertex_attr_names(g_2mode)
    vertex_color <- recode(vertex_attr(graph = network, name = "cluster"),
      `1` = 'lightblue',
      `2` = 'lightgreen',      
      `3` = 'violet',
      `4` = 'yellow',
    )
    #print(vertex_color)
  
  # modify plot depending on whether mode = psi or hs
  if (mode == "psi") {

    vertex_size <- 5
    vertex_label <- V(network)$school_name
    
  } else {  
    
    vertex_size <- 1
    vertex_label <- ''
  }
    
  pdf(str_c('assets/figures/', plot_name), paper = "a4r")
  
  par(mar=c(0, 0, 0, 0) + 0.1, mai=c(0, 0, 0, 0))
  
  par(mfrow=c(1, 1))

  plot(
    x = network, 
    vertex.label = vertex_label,
    vertex.color = vertex_color,
    #vertex.frame.color = if_else(V(network)$type, 'black', 'lightgray'),
    vertex.frame.color = 'lightgray',
    vertex.shape = 'circle',
    vertex.size = vertex_size,
    edge.lty = 3,
    edge.lty = 0.5,
    edge.color = 'lightgrey',
    layout = layout,
    margin = plot_margin
  )
    
  dev.off()
}


#### MODE = PSI

# priv
save_1mode_plot(g_2mode_privu, mode = "psi", layout = layout_with_fr, c_analysis = "hclust", k=4, h = NULL, hclust_method = "ward.D", plot_margin = 0.0, plot_name = "plot_1mode_privu.pdf")

# pub
save_1mode_plot(g_2mode_pubu, mode = "psi", layout = layout_with_fr, c_analysis = "hclust", k=4, h = NULL, hclust_method = "ward.D", plot_margin = 0.0, plot_name = "plot_1mode_pubu.pdf")

# pub out-of-state
save_1mode_plot(g_2mode_pubu, mode = "psi", pubu_visits = 'outst', layout = layout_with_fr, c_analysis = "hclust", k=4, h = NULL, hclust_method = "ward.D", plot_margin = 0.0, plot_name = "plot_1mode_pubu_outst.pdf")

# all
save_1mode_plot(g_2mode, mode = "psi", layout = layout_with_fr, c_analysis = "hclust", k=4, h = NULL, hclust_method = "ward.D", plot_margin = 0.0, plot_name = "plot_1mode_all.pdf")

# all, out-of-state pub
save_1mode_plot(g_2mode, mode = "psi", pubu_visits = 'outst', layout = layout_with_fr, c_analysis = "hclust", k=4, h = NULL, hclust_method = "ward.D", plot_margin = 0.0, plot_name = "plot_1mode_all_pubu_outst.pdf")

# all, out-of-state all
#save_1mode_plot(g_2mode, mode = "psi", pubu_visits = 'outst', privu_visits = 'outst', layout = layout_with_fr, c_analysis = "hclust", k=4, h = NULL, hclust_method = "ward.D", plot_margin = 0.0, plot_name = "plot_2mode_all.pdf")

#### MODE = HS

#save_1mode_plot(g_2mode_pubu, mode = "hs", layout = layout_with_fr, c_analysis = "fast", plot_margin = 0.0, plot_name = "plot_2mode_all.pdf")


## ------------------------------
## TABLE FROM EGO IGRAPH OBJECTS
## ------------------------------


create_ego_table <- function(twomode_network, ego_networks, univs, c_analysis, race_var = 'pct_blacklatinxnative_cat', ranking_var = 'rank_cat2', k = NULL, h = NULL) {
  
  ego_tbl <- data.frame(univ_id = character(0), univ_name = character(0), cluster = character(0), univ_type = character(0), univ_ranking = character(0), characteristics = character(0),
                        region_1 = character(0), region_2 = character(0), region_3 = character(0), region_4 = character(0),
                        religion_1 = character(0), religion_2 = character(0), religion_3 = character(0), religion_4 = character(0),
                        race_1 = character(0), race_2 = character(0), race_3 = character(0), race_4 = character(0),
                        ranking_1 = character(0), ranking_2 = character(0), ranking_3 = character(0), ranking_4 = character(0),
                        stringsAsFactors=FALSE)
  
  if (c_analysis == 'fast') {
    member <- membership(cluster_fast_greedy(twomode_network))
  } else {
    member <- create_hclust(network = twomode_network, mode = 'psi', k = k, h = h)
  }
  
  for (i in seq_along(univs)) {
    ego_network <- ego_networks[[univs[[i]]]]
    ego_network_order1 <- subgraph.edges(graph = ego_network, eids = E(ego_network)[E(ego_network)$order==1])
    
    ego_df <- as.data.frame(vertex_attr(ego_network_order1))
    
    univ_characteristics <- ego_df %>% filter(type == TRUE)
    
    hs_characteristics <- ego_df %>% filter(type == FALSE)
    num_hs <- nrow(hs_characteristics)
    
    # Region
    pct_hs_region_1 <- sprintf('%.1f%%', nrow(hs_characteristics %>% filter(region == 1)) / num_hs * 100)
    pct_hs_region_2 <- sprintf('%.1f%%', nrow(hs_characteristics %>% filter(region == 2)) / num_hs * 100)
    pct_hs_region_3 <- sprintf('%.1f%%', nrow(hs_characteristics %>% filter(region == 3)) / num_hs * 100)
    pct_hs_region_4 <- sprintf('%.1f%%', nrow(hs_characteristics %>% filter(region == 4)) / num_hs * 100)
    
    # Religion
    pct_hs_religion_1 <- sprintf('%.1f%%', nrow(hs_characteristics %>% filter(religion == 'catholic')) / num_hs * 100)
    pct_hs_religion_2 <- sprintf('%.1f%%', nrow(hs_characteristics %>% filter(religion == 'conservative_christian')) / num_hs * 100)
    pct_hs_religion_3 <- sprintf('%.1f%%', nrow(hs_characteristics %>% filter(religion == 'nonsectarian')) / num_hs * 100)
    pct_hs_religion_4 <- sprintf('%.1f%%', nrow(hs_characteristics %>% filter(religion == 'other_religion')) / num_hs * 100)
    
    # Race
    race_vals <- levels(as.factor(hs_characteristics[[race_var]]))
    for (j in seq_along(race_vals)) {
      assign(paste0('pct_hs_race_', j), sprintf('%.1f%%', nrow(hs_characteristics %>% filter(get(race_var) == race_vals[[j]])) / num_hs * 100))
    }
    
    # Ranking
    ranking_vals <- levels(as.factor(hs_characteristics[[ranking_var]]))
    for (j in seq_along(ranking_vals)) {
      assign(paste0('pct_hs_ranking_', j), sprintf('%.1f%%', nrow(hs_characteristics %>% filter(get(ranking_var) == ranking_vals[[j]])) / num_hs * 100))
    }
    
    ego_tbl[i, ] <- c(as.character(univ_characteristics$name), as.character(univ_characteristics$school_name), member[names(member) == as.character(univ_characteristics$name)],
                      as.character(univ_characteristics$school_type), as.character(univ_characteristics$ranking_numeric),
                      str_c(val_label(univ_df$region, univ_characteristics$region), univ_characteristics$religion, univ_characteristics[[race_var]], univ_characteristics$ranking, sep = '|'),
                      pct_hs_region_1, pct_hs_region_2, pct_hs_region_3, pct_hs_region_4,
                      pct_hs_religion_1, pct_hs_religion_2, pct_hs_religion_3, pct_hs_religion_4,
                      pct_hs_race_1, pct_hs_race_2, pct_hs_race_3, pct_hs_race_4,
                      pct_hs_ranking_1, pct_hs_ranking_2, pct_hs_ranking_3, pct_hs_ranking_4)
  }
  
  ego_tbl <- ego_tbl %>% arrange(as.numeric(cluster), univ_type, as.numeric(univ_ranking))
  
  # merge in number of visits to private high schools (total, inst, outst)
    ego_tbl <- events_data %>% filter(event_type == "priv_hs") %>% group_by(univ_id) %>% 
      summarize(phs_visits_tot = n()) %>% right_join(y = ego_tbl)
      
    ego_tbl <- events_data %>% filter(event_type == "priv_hs", univ_state == event_state) %>% group_by(univ_id) %>% 
      summarize(phs_visits_in = n()) %>% right_join(y = ego_tbl)
    
    ego_tbl <- events_data %>% filter(event_type == "priv_hs", univ_state != event_state) %>% group_by(univ_id) %>% 
      summarize(phs_visits_out = n()) %>% right_join(y = ego_tbl)  
    
  # change order of variables
    ego_tbl <- ego_tbl %>% relocate(univ_id, univ_name, cluster, univ_type, univ_ranking, phs_visits_tot, phs_visits_in, phs_visits_out, characteristics)
    
  names(ego_tbl) <- c('ID', 'University', 'Cluster', 'Type', 'Rank', 'total', 'in_state', 'out_state','Characteristics',
                      'Northeast', 'Midwest', 'South', 'West',
                      'Catholic', 'Conserv', 'Nonsect', 'Other',
                      race_vals, ranking_vals)
  
  ego_tbl
}

ego_table_privu <- create_ego_table(g_2mode_privu, egos_psi_privu, privu_vec, c_analysis = 'fast')
saveRDS(ego_table_privu, file = './assets/tables/table_ego_privu.RDS')

ego_table_pubu <- create_ego_table(g_2mode_pubu, egos_psi_pubu, pubu_vec, c_analysis = 'fast')
saveRDS(ego_table_pubu, file = './assets/tables/table_ego_pubu.RDS')

ego_table_privu_test <- create_ego_table(g_2mode_privu, egos_psi_privu, privu_vec, c_analysis = 'hclust', k = 4)

# count number of total, in-state, and out-of-state visits to private high schools
events_data %>% left_join(y = univ_df, by = c("univ_id" = "school_id")) %>% filter(event_type == "priv_hs") %>% group_by(school_name) %>% count() %>% View() # all

events_data %>% left_join(y = univ_df, by = c("univ_id" = "school_id")) %>% filter(event_type == "priv_hs", univ_state == event_state) %>% group_by(school_name) %>% count() %>% View() # in-state

events_data %>% left_join(y = univ_df, by = c("univ_id" = "school_id")) %>% filter(event_type == "priv_hs", univ_state != event_state) %>% group_by(school_name) %>% count() %>% View() # out-state

events_data %>% glimpse()

## ---------------------------------
## TABLE FROM 2-MODE IGRAPH OBJECTS
## ---------------------------------

create_agg_table <- function(network, characteristic) {
  network %>% select_('degree_band', characteristic) %>%
    group_by_('degree_band', characteristic) %>%
    summarize(cnt = n()) %>%
    mutate(pct = sprintf('%.1f%%', cnt / sum(cnt) * 100)) %>%
    select_('degree_band', characteristic, 'pct') %>%
    spread_(characteristic, 'pct') %>%
    rename_at(vars(-degree_band), ~ paste0(characteristic, '_', .))
}

create_2mode_table <- function(twomode_network, race_var = 'pct_blacklatinxnative', ranking_var = 'rank_cat2') {
  
  twomode_df <- igraph::as_data_frame(twomode_network, 'vertices')
  
  twomode_df$degree <- degree(twomode_network)
  twomode_df$strength <- strength(twomode_network)
  twomode_df$closeness <- closeness(twomode_network, normalized = T)
  
  # Filter for private HS (temporarily get rid of unmerged rows)
  twomode_df <- twomode_df %>% filter(type == F, !is.na(school_name))
  
  # Different degree_band for private and public university visits
  if (setequal(twomode_df$name, privu_vec)) {
    twomode_df <- twomode_df %>%
      mutate(degree_band = case_when(degree >= 15 ~ '15+',
                                     degree >= 10 ~ '10-14',
                                     degree >= 5 ~ '5-9',
                                     TRUE ~ as.character(degree)))
  } else {
    twomode_df <- twomode_df %>%
      mutate(degree_band = case_when(degree > 5 ~ '6+',
                                     TRUE ~ as.character(degree)))
  }
  
  # Full table
  full_twomode_table <- twomode_df %>% arrange(-degree) %>%
    select_('name', 'school_name', 'city', 'state_code', 'region', 'religion', race_var, 'ranking', 'ranking_numeric', 'degree', 'closeness', 'strength')
  
  # Aggregated table
  agg_twomode_table <- twomode_df %>%
    group_by(degree_band) %>%
    summarise(Count = n())

  agg_twomode_table <- left_join(agg_twomode_table, create_agg_table(twomode_df, 'region'))
  agg_twomode_table <- left_join(agg_twomode_table, create_agg_table(twomode_df, 'religion'))
  agg_twomode_table <- left_join(agg_twomode_table, create_agg_table(twomode_df, paste0(race_var, '_cat')))
  agg_twomode_table <- left_join(agg_twomode_table, create_agg_table(twomode_df, ranking_var))
  
  band_order <- c('15+', '10-14', '5-9', '6+', '5', '4', '3', '2', '1')
  agg_twomode_table <- agg_twomode_table[order(match(agg_twomode_table$degree_band, band_order)), ]
  
  names(agg_twomode_table) <- c('Degree', 'Count',
                                'Northeast', 'Midwest', 'South', 'West',
                                'Catholic', 'Conservative Christian', 'Nonsectarian', 'Other',
                                levels(as.factor(twomode_df[[paste0(race_var, '_cat')]])),
                                levels(as.factor(twomode_df$rank_cat2)), 'rank_NA')
  
  list(full_table = full_twomode_table, agg_table = agg_twomode_table)
}

twomode_both <- create_2mode_table(g_2mode)
saveRDS(twomode_both$full_table, file = './assets/tables/table_2mode_both.RDS')
saveRDS(twomode_both$agg_table, file = './assets/tables/table_2mode_agg_both.RDS')

twomode_privu <- create_2mode_table(g_2mode_privu)
saveRDS(twomode_privu$full_table, file = './assets/tables/table_2mode_privu.RDS')
saveRDS(twomode_privu$agg_table, file = './assets/tables/table_2mode_agg_privu.RDS')

twomode_pubu <- create_2mode_table(g_2mode_pubu)
saveRDS(twomode_pubu$full_table, file = './assets/tables/table_2mode_pubu.RDS')
saveRDS(twomode_pubu$agg_table, file = './assets/tables/table_2mode_agg_pubu.RDS')


## -----------------------------------
## CLUSTER FROM 2-MODE IGRAPH OBJECTS
## -----------------------------------

# Community cluster
vertex_attr_names(g_2mode_privu)

c_2mode_privu <- cluster_fast_greedy(g_2mode_privu)
class(c_2mode_privu)
length(c_2mode_privu)
sizes(c_2mode_privu)

membership(c_2mode_privu)

length(V(g_2mode_privu)$type)
membership(c_2mode_privu)[V(g_2mode_privu)$type==T]
table(membership(c_2mode_privu)[V(g_2mode_privu)$type==T])

plot(c_2mode_privu,
     g_2mode_privu,
     vertex.label = if_else(V(g_2mode_privu)$type, V(g_2mode_privu)$school_name, ''),
     vertex.shape = if_else(V(g_2mode_privu)$type, 'square', 'circle'),
     vertex.size = if_else(V(g_2mode_privu)$type, 5, 2),
     layout = layout_with_kk,
     edge.lty = 0)


## -----------------------------------
## CLUSTER FROM 1-MODE IGRAPH OBJECTS
## -----------------------------------

# Community cluster
vertex_attr_names(g_1mode_psi)

c_1mode_psi <- cluster_fast_greedy(g_1mode_psi)
length(c_1mode_psi)
sizes(c_1mode_psi)
membership(c_1mode_psi)

plot(
  c_1mode_psi,
  g_1mode_psi,
  vertex.label = V(g_1mode_psi)$school_name,
  col = ifelse(V(g_1mode_psi)$name %in% privu_vec, 'yellow', 'green'),
  vertex.size = 5,
  layout = layout_nicely,
  edge.lty = 0
)
