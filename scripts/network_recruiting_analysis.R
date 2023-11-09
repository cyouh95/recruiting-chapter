library(igraph)
library(tidyverse)

options(max.print = 2000)


## -------------
## LOAD OBJECTS
## -------------

# Run create_igraph_objects.R
scripts_dir <- file.path('.', 'scripts')
source(file = file.path(scripts_dir, 'create_igraph_objects.R'))

# [private colleges and universities] Create 1-mode graphs from g_2mode_priv
# [public universities] Create 1-mode graphs from g_2mode_pubu
# [public and private universities] Create 1-mode graphs from g_2mode_u
# [private universities] Create 1-mode graphs from g_2mode_privu
# [private colleges] Create 1-mode graphs from g_2mode_privc  

# Summary of objects (1890 private HS + 41 univs = 1931 vertices total)
  # 2-mode igraph objects - vertices is sum of vertices of below 1-mode graphs
    # g_2mode (1931 vertices + 11804 edges)
    # g_2mode_priv (1488 vertices + 7980 edges)
    # g_2mode_pubu (1564 vertices + 3824 edges)
    # g_2mode_u (1869 vertices + 8952 edges)
    # g_2mode_privu (1365 vertices + 5128 edges)
    # g_2mode_privc (889 vertices + 2852 edges)
  # 1-mode igraph objects [HS]
    # g_1mode_hs (1890 vertices + 858702 edges)
    # g_1mode_priv_hs (1462 vertices + 606659 edges)
    # g_1mode_pubu_hs (1549 vertices + 499055 edges)
    # g_1mode_u_hs (1840 vertices + 811755 edges)
    # g_1mode_privu_hs (1351 vertices + 541204 edges)
    # g_1mode_privc_hs (877 vertices + 215317 edges)
  # 1-mode igraph objects [PSI]
    # g_1mode_psi (41 vertices + 817 edges)
    # g_1mode_priv_psi (26 vertices + 325 edges)
    # g_1mode_pubu_psi (15 vertices + 102 edges)
    # g_1mode_u_psi (29 vertices + 403 edges)
    # g_1mode_privu_psi (14 vertices + 91 edges)
    # g_1mode_privc_psi (12 vertices + 66 edges)
  # Ego igraph objects [HS]
    # egos_hs (1890 ego graphs)
    # egos_hs_priv (1462 ego graphs)
    # egos_hs_pubu (1549 ego graphs)
    # egos_hs_u (1840 ego graphs)
    # egos_hs_privu (1351 ego graphs)
    # egos_hs_privc (877 ego graphs)
  # Ego igraph objects [PSI]
    # egos_psi (41 ego graphs)
    # egos_psi_priv (26 ego graphs)
    # egos_psi_pubu (15 ego graphs)
    # egos_psi_u (29 ego graphs)
    # egos_psi_privu (14 ego graphs)
    # egos_psi_privc (12 ego graphs)


## --------------------------
## CHARACTERISTICS VARIABLES
## --------------------------

region_values <- c('northeast', 'midwest', 'south', 'west')
region_keys <- c('Northeast', 'Midwest', 'South', 'West')
region_title <- 'Region'

religion_values <- c('catholic', 'christian', 'nonsectarian', 'other')
religion_keys <- c('Catholic', 'Christian', 'Nonsectarian', 'Other')
religion_title <- 'Religion'

race_values <- c('c1_lt10', 'c2_10to20', 'c3_20to50', 'c4_50+')
race_keys <- c('LT 10%', '10-20%', '20-50%', 'GT 50%')
race_title <- '% Black/Latinx/Native'

ranking_values <- c('c1_top200', 'c2_A+', 'c3_A', 'c4_ltA')
ranking_keys <- c('Rank top 200', 'A+', 'A', 'A- or below')
ranking_title <- 'Ranking'

enroll_values <- c('c1_lt50', 'c2_50to100', 'c3_100to150', 'c4_gt150')
enroll_keys <- c('LT 50', '50-100', '100-150', 'GT 150')
enroll_title <- '12th grade enrollment'


## -------------------
## SAVE PLOT FUNCTION
## -------------------

# FUNCTION: save_plot(graph_object, plot_name, <paper>, <mar>, <mai>)
  # graph_object: call function that plots the graph
  # plot_name: name of file with extension
  # paper: paper type (default: a4r)
  # mar: margin in lines (default: c(0, 0, 0, 0))
  # mai: margin in inches (default: c(0, 0, 0, 0))

save_plot <- function(graph_object, plot_name, paper = 'a4r', mar = c(0, 0, 0, 0), mai = c(0, 0, 0, 0)) {
  
  pdf(str_c('./assets/figures/', plot_name), paper = paper)
  par(mfrow = c(1, 1))
  par(mar = mar + 0.1, mai = mai)
  
  graph_object
  
  dev.off()
}


## ------------------------
## PLOT EGO IGRAPH OBJECTS
## ------------------------

# FUNCTION: plot_ego_graph(ego_networks, univ_id, <characteristic>, <values>, <keys>, <colors>, <title>, <graph_order>, <margin>)
  # ego_networks: ego network objects
  # univ_id: univ to plot
  # characteristic: characteristic variable (default: NULL)
  # values: values of characteristic variable (default: NULL)
  # keys: legend text for characteristic variable (default: NULL)
  # colors: palette for vertices (default: lightblue, lightgreen, violet, yellow)
  # title: plot title (default: name of specified univ)
  # graph_order: order of graph (default: both)
  # margin: plot margin (default: 0.0)

# RETURNS: ego plot of the specified univ

plot_ego_graph <- function(ego_networks, univ_id, characteristic = NULL, values = NULL, keys = NULL, colors = c('lightblue', 'lightgreen', 'violet', 'yellow'), title = as.character(univ_df[univ_df$school_id == univ_id, 'school_name']), graph_order = 'both', margin = 0.0) {

  network <- ego_networks[[univ_id]]
  
  if (graph_order != 'both') {  # graph order == 1 or 2
    # Note: If order == 2, ego is not included as a vertex
    network <- subgraph.edges(graph = network, eids = E(network)[E(network)$order == graph_order])
  }

  # Vertex colors
  if (is.null(characteristic) && graph_order %in% c(1, 2)) {
    vertex_color <- if_else(V(network)$type, 'lightblue', 'salmon')
  } else if (is.null(characteristic) && graph_order == 'both') {
    vertex_color <- case_when(
      vertex_attr(network, 'type') == FALSE ~ 'salmon',
      vertex_attr(network, 'type') == TRUE & vertex_attr(network, 'name') != univ_id ~ 'lightblue',
      vertex_attr(network, 'type') == TRUE & vertex_attr(network, 'name') == univ_id ~ 'purple'
    )
  } else {  # characteristics
    color_palette <- colors
    names(color_palette) <- values
    
    vertex_color <- recode(vertex_attr(network, characteristic), !!!color_palette)
  }

  # Vertex size + Edge width, color, line type
  if (graph_order %in% c('both', 2)) {
    vertex_size <- case_when(
      vertex_attr(network, 'type') == FALSE ~ 2,
      vertex_attr(network, 'type') == TRUE & vertex_attr(network, 'name') != univ_id ~ 10,
      vertex_attr(network, 'type') == TRUE & vertex_attr(network, 'name') == univ_id ~ 15,
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
  
  # Graph layout (layout_with_kk, layout_with_fr, layout_in_circle, layout_nicely)
  if (graph_order == 1) { 
    graph_layout <- layout_nicely
  } else if (graph_order == 2) {  
    graph_layout <- layout_nicely
  } else {
    graph_layout <- layout_with_kk
  }

  # Plot graph
  plot.igraph(
    x = network,
    # vertex attributes
    vertex.shape = 'circle',
    vertex.label = if_else(V(network)$type, V(network)$school_name, ''),
    vertex.color = vertex_color,
    vertex.size = vertex_size,
    vertex.frame.color = 'lightgray',
    # edge attributes
    edge.color = edge_color,
    edge.lty = edge_lty,
    edge.width = edge_width,
    layout = graph_layout,  
    main = str_to_title(title),
    margin = margin
  )  

  # Legend
  if (!is.null(characteristic)) {
    legend(
      x = 0.8,
      y = 0.0,
      legend = keys,
      fill = colors,
      bty = 'n'
    )
  }
}

# Create plots
# par(mfrow = c(1, 1))
# par(mar = c(5, 4, 4, 2) + 0.1)

# No characteristics
plot_ego_graph(egos_psi_privu, '160755', graph_order = 1)
plot_ego_graph(egos_psi_privu, '160755', graph_order = 2)
plot_ego_graph(egos_psi_privu, '160755', graph_order = 'both')

# Notre Dame 152080

save_plot(
  plot_ego_graph(egos_psi_u, '152080', characteristic = 'religion', values = religion_values, keys = religion_keys, 
                 title = religion_title, graph_order = 'both', margin = -.3),
  plot_name = 'nd_religtion.pdf')

# Characteristics
plot_ego_graph(egos_psi_privu, '160755', characteristic = 'region', values = region_values, keys = region_keys, title = region_title, graph_order = 1)
plot_ego_graph(egos_psi_privu, '160755', characteristic = 'region', values = region_values, keys = region_keys, title = region_title, graph_order = 2)
plot_ego_graph(egos_psi_privu, '160755', characteristic = 'region', values = region_values, keys = region_keys, title = region_title, graph_order = 'both')

# Small multiples
par(mfrow = c(2, 2))

plot_ego_graph(egos_psi_privu, '160755', characteristic = 'region', values = region_values, keys = region_keys, title = region_title, graph_order = 1)
plot_ego_graph(egos_psi_privu, '160755', characteristic = 'religion', values = religion_values, keys = religion_keys, title = religion_title, graph_order = 1)
plot_ego_graph(egos_psi_privu, '160755', characteristic = 'pct_blacklatinxnative_cat', values = race_values, keys = race_keys, title = race_title, graph_order = 1)
plot_ego_graph(egos_psi_privu, '160755', characteristic = 'rank_cat2', values = ranking_values, keys = ranking_keys, title = ranking_title, graph_order = 1)

# Villanova + religion
# save_plot(plot_ego_graph(egos_psi_privu, '216597', characteristic = 'religion', values = religion_values, keys = religion_keys, title = '', graph_order = 'both'),
#           plot_name = 'villanova_religion.pdf')

# Emory + region
# save_plot(plot_ego_graph(egos_psi_privu, '139658', characteristic = 'region', values = region_values, keys = region_keys, title = '', graph_order = 'both'),
#           plot_name = 'emory_region.pdf')


## ------------------------------
## COMMUNITY DETECTION FUNCTIONS
## ------------------------------

# FUNCTION: create_hclust(twomode_network, mode, <dist_method>, <hclust_method>, <k>, <h>, <plot_tree>)
  # twomode_network: 2-mode object
  # mode: cluster HS or univs (hs, psi)
  # dist_method: distance measure to be used in stats::dist() (default: euclidean)
  # hclust_method: agglomeration method to be used in stats::hclust() (default: ward.D)
  # k: number of clusters to form (default: NULL)
  # h: height of tree to cut at (default: NULL)
  # plot_tree: whether to plot dendrogram (default: FALSE)

# RETURNS: named vector indicating cluster that HS or univ belong to
  # data: cluster number
  # names: HS or univ ID

create_hclust <- function(twomode_network, mode, dist_method = 'euclidean', hclust_method = 'ward.D', k = NULL, h = NULL, plot_tree = FALSE) {

  # Cluster for HS or univs
  if (mode == 'hs') {
    proj <- 'proj1'
    type <- FALSE
  } else {  # mode == 'psi'
    proj <- 'proj2'
    type <- TRUE
  }

  # Create cluster object
  hclust_obj <- as_adjacency_matrix(  # create adjacency matrix of 1-mode object
    graph = bipartite.projection(twomode_network)[[proj]],
    sparse = FALSE, 
    attr = 'weight'
  ) %>% dist(  # create distance matrix using stats::dist()
    method = dist_method,
  ) %>% hclust(  # conduct hierarchical clustering using stats::hclust()
    method = hclust_method,
  ) 
  
  # Plot dendrogram associated with hclust object using school name as labels
  if (plot_tree) {
    hclust_obj2 <- hclust_obj
    hclust_obj2$labels <- V(twomode_network)$school_name[V(twomode_network)$type == type]
    
    par(mar = c(3, 1, 1, 8))
    hclust_obj2 %>% as.dendrogram() %>% plot(horiz = TRUE)
  }
  
  # Cut the dendrogram to create the desired number of clusters using stats::cutree()
  cutree(
    tree = hclust_obj,
    k = k,
    h = h
  )
}

# Usage
create_hclust(twomode_network = g_2mode, mode = 'psi', k = 4, plot_tree = TRUE)
create_hclust(twomode_network = g_2mode, mode = 'psi', k = 4) %>% table()

create_hclust(twomode_network = g_2mode_pubu, mode = 'psi', k = 4, plot_tree = TRUE)
# FUNCTION: create_fast_greedy(twomode_network, mode, <no>, <steps>, <plot_tree>)
  # twomode_network: 2-mode object
  # mode: cluster HS or univs (hs, psi)
  # no: number of clusters to form (default: NULL)
  # steps: number of merge operations to perform (default: NULL)
  # plot_tree: whether to plot dendrogram (default: FALSE)

# RETURNS: named vector indicating cluster that HS or univ belong to
  # data: cluster number
  # names: HS or univ ID

create_fast_greedy <- function(twomode_network, mode, no = NULL, steps = NULL, plot_tree = FALSE) {
  
  # Cluster for HS or univs
  if (mode == 'hs') {
    proj <- 'proj1'
    type <- FALSE
  } else {  # mode == 'psi'
    proj <- 'proj2'
    type <- TRUE
  }
  
  # Create cluster object
  c_obj <- bipartite.projection(twomode_network)[[proj]] %>% cluster_fast_greedy() 
  
  # Plot object
  if (plot_tree) {
    dendPlot(c_obj, mode = 'phylo')
  }
  
  # Get membership
  if (is.null(no) && is.null(steps)) {
    m_obj <- c_obj %>% membership()
  } else if (!is.null(no) && is.null(steps)) {
    m_obj <- c_obj %>% cut_at(no = no)
  } else if (is.null(no) && !is.null(steps)) {
    m_obj <- c_obj %>% cut_at(steps = steps)
  }
  
  # Create named vector
  names(m_obj) <- V(twomode_network)$name[V(twomode_network)$type == type]
  m_obj
}

# Usage
#create_fast_greedy(twomode_network = g_2mode, mode = 'psi', no = 4, plot_tree = TRUE)
#create_fast_greedy(twomode_network = g_2mode, mode = 'psi', steps = 38, plot_tree = TRUE)


## ---------------------------
## PLOT 2-MODE IGRAPH OBJECTS
## ---------------------------

# FUNCTION: plot_2mode_graph(twomode_network, c_analysis, <k>, <h>, <no>, <steps>, <values>, <keys>, <colors>, <pubu_visits>, <privu_visits>, <layout>, <margin>)
  # twomode_network: 2-mode object
  # c_analysis: type of cluster analysis (hclust, fast, or characteristic variable)
  # k: number of clusters (default: NULL)
  # h: height to cut tree (default: NULL)
  # no: number of clusters to form (default: NULL)
  # steps: number of merge operations to perform (default: NULL)
  # values: values of characteristic variable (default: NULL)
  # keys: legend text for characteristic variable (default: NULL)
  # colors: palette for vertices (default: lightblue, lightgreen, violet, yellow)
  # pubu_visits: which HS visited by public univs to include (instate, outofstate, default: all)
  # privu_visits: which HS visited by private univs to include (instate, outofstate, default: all)
  # layout: plot layout (default: layout_with_fr)
  # margin: plot margin (default: 0.0)

# RETURNS: 2-mode plot colored by cluster or specified characteristics

plot_2mode_graph <- function(twomode_network, c_analysis, k = NULL, h = NULL, no = NULL, steps = NULL, values = NULL, keys = NULL, colors = c('lightblue', 'lightgreen', 'violet', 'yellow'), pubu_visits = 'all', privu_visits = 'all', layout = layout_with_fr, margin = 0.0) {
  
  # Cluster analysis to identify communities
  if (pubu_visits != 'all' & privu_visits == 'all') {
    twomode_network <- subgraph.edges(graph = twomode_network, eids = E(twomode_network)[(E(twomode_network)$visiting_univ == 'public' & E(twomode_network)$visit_loc == pubu_visits) | E(twomode_network)$visiting_univ == 'private'])
  } else if (pubu_visits == 'all' & privu_visits != 'all') {
    twomode_network <- subgraph.edges(graph = twomode_network, eids = E(twomode_network)[E(twomode_network)$visiting_univ == 'public' | (E(twomode_network)$visiting_univ == 'private' & E(twomode_network)$visit_loc == privu_visits)])
  } else if (pubu_visits != 'all' & privu_visits != 'all') {
    twomode_network <- subgraph.edges(graph = twomode_network, eids = E(twomode_network)[(E(twomode_network)$visiting_univ == 'public' & E(twomode_network)$visit_loc == pubu_visits) | (E(twomode_network)$visiting_univ == 'private' & E(twomode_network)$visit_loc == privu_visits)])
  }
  
  summary(twomode_network)
  
  if (c_analysis == 'fast') {
    member <- create_fast_greedy(twomode_network = twomode_network, mode = 'psi', no = no, steps = steps)
  } else if (c_analysis == 'hclust') {
    member <- create_hclust(twomode_network = twomode_network, mode = 'psi', k = k, h = h)
  }
  
  # Vertex colors
  if (c_analysis %in% c('hclust', 'fast')) {
    vertex_attr(graph = twomode_network, name = 'cluster_psi', index = V(twomode_network)$name[V(twomode_network)$type == TRUE]) <- member
    vertex_attr(graph = twomode_network, name = 'cluster') <- if_else(V(twomode_network)$type, as.integer(vertex_attr(graph = twomode_network, name = 'cluster_psi')), 0L)
    
    vals <- vertex_attr(graph = twomode_network, name = 'cluster') %>% unique() %>% sort()
    color_palette <- c('salmon', colors)
    names(color_palette) <- vals
    
    vertex_color <- recode(vertex_attr(twomode_network, 'cluster'), !!!color_palette)
  } else {  # characteristics
    color_palette <- colors
    names(color_palette) <- values
    
    vertex_color <- recode(vertex_attr(twomode_network, c_analysis), !!!color_palette)
  }
  
  
  # starting coordinates of graph
    # layout_with_fr(g, coords); coords argument = "Optional starting positions for the vertices. If this argument is not NULL then it should be an appropriate matrix of starting coordinates"
  coords_kk <- layout_with_kk(graph = twomode_network)
    #print(coords_kk) # print layout_with_kk coordinates; these remain the same
    #print(layout_with_fr(graph = twomode_network)) # print layout_with_fr coordinates; these change each time
  
  # Plot graph
  plot(
    x = twomode_network, 
    vertex.label = if_else(V(twomode_network)$type, V(twomode_network)$school_name, ''),
    vertex.color = vertex_color,
    vertex.frame.color = 'lightgray',
    vertex.shape = 'circle',
    vertex.size = if_else(V(twomode_network)$type, 3, 0.75),
    edge.lty = 3,
    #edge.lty = 0.5,
    edge.color = 'lightgrey',
    #coords = coords_kk, # starting positions for vertices
    layout = layout(twomode_network, coords = coords_kk),
    margin = margin
  )
  
  # Legend
  if (!c_analysis %in% c('hclust', 'fast')) {
    legend(
      x = 0.2,
      y = 0.0,
      legend = keys,
      fill = color_palette,
      bty = 'n'
    )
  }
}

# Usage



plot_2mode_graph(g_2mode_u, c_analysis = 'region', values = region_values, keys = region_keys, margin = -.8)
plot_2mode_graph(g_2mode_u, c_analysis = 'region', values = region_values, keys = region_keys, layout = layout_with_fr, margin = -.7)
plot_2mode_graph(g_2mode_u, c_analysis = 'region', values = region_values, keys = region_keys, layout = layout_with_kk, margin = -.6)

plot_2mode_graph(g_2mode_privu, c_analysis = 'hclust', k = 4)
plot_2mode_graph(g_2mode_pubu, c_analysis = 'fast', steps = 11, colors = c('red', 'orange', 'yellow', 'green', 'blue', 'purple'))


# public and private universities

# save_plot(plot_2mode_graph(g_2mode_u, c_analysis = 'hclust', k = 5, colors = c('lightblue', 'green', 'violet', 'yellow','coral'), margin = -.75),
#           plot_name = 'plot_2mode_u_k5.pdf')
# 
# save_plot(plot_1mode_graph(g_2mode_u, mode = 'psi', c_analysis = 'hclust', k = 5, colors = c('lightblue', 'green', 'violet', 'yellow','coral')),
#           plot_name = 'plot_1mode_u_k5.pdf')

# Region characteristic
# save_plot(plot_2mode_graph(g_2mode, c_analysis = 'region', values = region_values, keys = region_keys, margin = -0.6),
#           plot_name = 'plot_2mode_all_region.pdf')

# Private univs
# save_plot(plot_2mode_graph(g_2mode_privu, c_analysis = 'hclust', k = 4, margin = -0.74),
#           plot_name = 'plot_2mode_privu.pdf')

# Public univs
# save_plot(plot_2mode_graph(g_2mode_pubu, c_analysis = 'hclust', k = 4, margin = -0.74),
#           plot_name = 'plot_2mode_pubu.pdf')

# Public univs, out-of-state
# save_plot(plot_2mode_graph(g_2mode_pubu, pubu_visits = 'outofstate', c_analysis = 'hclust', k = 4, margin = -0.74),
#           plot_name = 'plot_2mode_pubu_outst.pdf')

# All
# save_plot(plot_2mode_graph(g_2mode, c_analysis = 'hclust', k = 4, margin = -0.74),
#           plot_name = 'plot_2mode_all.pdf')

# All, out-of-state public univs
# save_plot(plot_2mode_graph(g_2mode, pubu_visits = 'outofstate', c_analysis = 'hclust', k = 4, margin = -0.74),
#           plot_name = 'plot_2mode_all_pubu_outst.pdf')

# All, out-of-state both
# save_plot(plot_2mode_graph(g_2mode, pubu_visits = 'outofstate', privu_visits = 'outofstate', c_analysis = 'hclust', k = 4, margin = -0.74),
#           plot_name = 'plot_2mode_all_outst.pdf')


## ---------------------------
## PLOT 1-MODE IGRAPH OBJECTS
## ---------------------------

# FUNCTION: plot_1mode_graph(twomode_network, mode, c_analysis, <k>, <h>, <no>, <steps>, <colors>, <pubu_visits>, <privu_visits>, <layout>, <margin>)
  # twomode_network: 2-mode object
  # mode: cluster HS or univs (hs, psi)
  # c_analysis: type of cluster analysis (hclust, fast, or characteristic like region)
  # k: number of clusters (default: NULL)
  # h: height to cut tree (default: NULL)
  # no: number of clusters to form (default: NULL)
  # steps: number of merge operations to perform (default: NULL)
  # colors: palette for vertices (default: lightblue, lightgreen, violet, yellow)
  # pubu_visits: which HS visited by public univs to include (instate, outofstate, default: all)
  # privu_visits: which HS visited by private univs to include (instate, outofstate, default: all)
  # layout: plot layout (default: layout_with_fr)
  # margin: plot margin (default: 0.0)

# RETURNS: 1-mode plot colored by cluster

plot_1mode_graph <- function(twomode_network, mode, c_analysis, k = NULL, h = NULL, no = NULL, steps = NULL, colors = c('lightblue', 'lightgreen', 'violet', 'yellow'), pubu_visits = 'all', privu_visits = 'all', layout = layout_with_fr, margin = 0.1) {
  
  # Cluster analysis to identify communities
  if (pubu_visits != 'all' & privu_visits == 'all') {
    twomode_network <- subgraph.edges(graph = twomode_network, eids = E(twomode_network)[(E(twomode_network)$visiting_univ == 'public' & E(twomode_network)$visit_loc == pubu_visits) | E(twomode_network)$visiting_univ == 'private'])
  } else if (pubu_visits == 'all' & privu_visits != 'all') {
    twomode_network <- subgraph.edges(graph = twomode_network, eids = E(twomode_network)[E(twomode_network)$visiting_univ == 'public' | (E(twomode_network)$visiting_univ == 'private' & E(twomode_network)$visit_loc == privu_visits)])
  } else if (pubu_visits != 'all' & privu_visits != 'all') {
    twomode_network <- subgraph.edges(graph = twomode_network, eids = E(twomode_network)[(E(twomode_network)$visiting_univ == 'public' & E(twomode_network)$visit_loc == pubu_visits) | (E(twomode_network)$visiting_univ == 'private' & E(twomode_network)$visit_loc == privu_visits)])
  }
  
  summary(twomode_network)
  
  if (c_analysis == 'fast') {
    member <- create_fast_greedy(twomode_network = twomode_network, mode = mode, no = no, steps = steps)
  } else if (c_analysis == 'hclust') {
    member <- create_hclust(twomode_network = twomode_network, mode = mode, k = k, h = h)
  }
  
  # Create 1-mode object
  if (mode == 'hs') {
    proj <- 'proj1'
    vertex_size <- 1
    vertex_label <- ''
  } else {  # mode == 'psi'
    proj <- 'proj2'
    vertex_size <- 8
    vertex_label <- V(twomode_network)[V(twomode_network)$type]$school_name
  }
  
  network <- bipartite.projection(twomode_network)[[proj]]
  
  # Vertex colors
  vertex_attr(graph = network, name = 'cluster') <- member
  
  color_palette <- colors
  names(color_palette) <- vertex_attr(graph = network, name = 'cluster') %>% unique() %>% sort()
  
  vertex_color <- recode(vertex_attr(network, 'cluster'), !!!color_palette)
  
  # starting coordinates of graph
    # layout_with_fr(g, coords); coords argument = "Optional starting positions for the vertices. If this argument is not NULL then it should be an appropriate matrix of starting coordinates"
  coords_kk <- layout_with_kk(graph = network)
    #print(coords_kk) # print layout_with_kk coordinates; these remain the same
    #print(layout_with_fr(graph = network)) # print layout_with_fr coordinates; these change each time
  
  # Plot graph
  plot(
    x = network, 
    vertex.label = vertex_label,
    vertex.color = vertex_color,
    vertex.frame.color = 'lightgray',
    vertex.shape = 'circle',
    vertex.size = vertex_size,
    edge.lty = 1, # 0 and “blank” mean no edges, 1 and “solid” are for solid lines, the other possible values are: 2 (“dashed”), 3 (“dotted”), 4 (“dotdash”), 5 (“longdash”), 6 (“twodash”).
    #edge.lty = 0.5,
    edge.width = log(as.numeric(E(network)$weight)/10),
    #edge.width = 1,
    edge.color = 'lightgrey',
    #coords = coords_kk, # starting positions for vertices
    layout = layout(network, coords = coords_kk),
    #layout=layout_with_fr(network, coords = coords_kk),
    margin = margin
  )
}

#### Usage
  # privu
    plot_1mode_graph(g_2mode_privu, mode = 'psi', c_analysis = 'hclust', k = 4)
    plot_1mode_graph(g_2mode_privu, mode = 'psi', c_analysis = 'fast', no = 4)
    plot_1mode_graph(g_2mode_privu, mode = 'psi', c_analysis = 'fast', no = 5, colors = c('lightblue', 'green', 'violet', 'yellow','coral'))
    plot_1mode_graph(g_2mode_privu, mode = 'psi', c_analysis = 'hclust', k = 6, colors = c('lightblue', 'green', 'violet', 'yellow','coral','pink'))
    
  # public
    plot_1mode_graph(g_2mode_pubu, mode = 'psi', c_analysis = 'hclust', k = 4)
    plot_1mode_graph(g_2mode_pubu, mode = 'psi', c_analysis = 'fast', no = 4)

  # u
    plot_1mode_graph(g_2mode_u, mode = 'psi', c_analysis = 'hclust', k = 5, colors = c('lightblue', 'green', 'violet', 'yellow','coral'))
    plot_1mode_graph(g_2mode_u, mode = 'psi', c_analysis = 'fast', no = 4)
    plot_1mode_graph(g_2mode_u, mode = 'psi', c_analysis = 'fast', no = 5, colors = c('lightblue', 'green', 'violet', 'yellow','coral'))
    plot_1mode_graph(g_2mode_u, mode = 'psi', c_analysis = 'fast', no = 6, colors = c('lightblue', 'green', 'violet', 'yellow','coral','pink'))
    plot_1mode_graph(g_2mode_u, mode = 'psi', c_analysis = 'fast', no = 7, colors = c('lightblue', 'green', 'violet', 'yellow','coral','pink','tan'))
    
plot_1mode_graph(g_2mode_pubu, mode = 'psi', c_analysis = 'hclust', k = 5, colors = c('lightblue', 'green', 'violet', 'yellow','coral'))
plot_1mode_graph(g_2mode_u, mode = 'psi', c_analysis = 'hclust', k = 5, colors = c('lightblue', 'green', 'violet', 'yellow','coral'))

plot_1mode_graph(g_2mode, mode = 'psi', c_analysis = 'hclust', k = 6, colors = c('lightblue', 'green', 'violet', 'yellow','coral','pink'))



# Private univs
save_plot(plot_1mode_graph(g_2mode_privu, mode = 'psi', c_analysis = 'fast', no = 4),
          plot_name = 'plot_1mode_privu.pdf')

# private colleges

save_plot(plot_1mode_graph(g_2mode_privc, mode = 'psi', c_analysis = 'fast', no = 4),
          plot_name = 'plot_1mode_privc.pdf')

# Public univs
save_plot(plot_1mode_graph(g_2mode_pubu, mode = 'psi', c_analysis = 'fast', no = 4),
          plot_name = 'plot_1mode_pubu.pdf')


# private and public universities
save_plot(plot_1mode_graph(g_2mode_u, mode = 'psi', c_analysis = 'fast', no = 4),
          plot_name = 'plot_1mode_u.pdf')

save_plot(plot_1mode_graph(g_2mode_u, mode = 'psi', c_analysis = 'fast', no = 5, colors = c('lightblue', 'green', 'violet', 'yellow','coral')),
          plot_name = 'plot_1mode_u_k5.pdf')

# Private and public universities; out-of-state visits only for public universities
# save_plot(plot_1mode_graph(g_2mode_u, mode = 'psi', pubu_visits = 'outofstate', c_analysis = 'hclust', k = 4),
#           plot_name = 'plot_1mode_u_pubu_outst.pdf')

# All
save_plot(plot_1mode_graph(g_2mode, mode = 'psi', c_analysis = 'hclust', k = 4),
          plot_name = 'plot_1mode_all.pdf')

save_plot(plot_1mode_graph(g_2mode, mode = 'psi', c_analysis = 'hclust', k = 5, colors = c('lightblue', 'green', 'violet', 'yellow','coral')),
          plot_name = 'plot_1mode_all_k5.pdf')

save_plot(plot_1mode_graph(g_2mode, mode = 'psi', c_analysis = 'hclust', k = 6, colors = c('lightblue', 'green', 'violet', 'yellow','coral','pink')),
          plot_name = 'plot_1mode_all_k6.pdf')

# All, out-of-state public univs
# save_plot(plot_1mode_graph(g_2mode, mode = 'psi', pubu_visits = 'outofstate', c_analysis = 'hclust', k = 4),
#           plot_name = 'plot_1mode_all_pubu_outst.pdf')

# All, out-of-state both
# save_plot(plot_1mode_graph(g_2mode, mode = 'psi', pubu_visits = 'outofstate', privu_visits = 'outofstate', c_analysis = 'hclust', k = 4),
#           plot_name = 'plot_1mode_all_outst.pdf')


## ---------------------------
## TABLE OF MATRIX OF NUMBER OF PRIVATE HIGH SCHOOLS VISITED IN COMMON
## ---------------------------

privhs_events_unique <- privhs_events %>%
  left_join(univ_info %>% select(univ_id, univ_abbrev, classification), by = 'univ_id') %>% 
  group_by(classification, univ_id, univ_abbrev) %>% 
  summarise(count = n_distinct(school_id)) %>% 
  arrange(desc(count))

create_1mode_table <- function(onemode_network, univ_type) {
  
  privhs_events_unique_df <- privhs_events_unique %>% filter(classification %in% univ_type)
  
  # Get N = number of unique private HS visited by each univ (order univ same way as igraph)
  num_unique_privhs_visited <- privhs_events_unique_df[match(vertex_attr(graph=onemode_network, name='name'), privhs_events_unique_df$univ_id), ]$count

  vertex_attr(graph=onemode_network, name='name') <- str_c(vertex_attr(graph=onemode_network, name='school_name'), ' (N=', num_unique_privhs_visited, ')')

  onemode_matrix <- as_adjacency_matrix(
    graph = onemode_network,
    type = "both",
    attr = "weight",
    names = TRUE,
    sparse = FALSE
  )
  
  # Sort by number of visits
  g_df <- igraph::as_data_frame(onemode_network, 'vertices')
  univs_order_by_privhs <- g_df[match(privhs_events_unique_df$univ_abbrev, g_df$school_name),]$name 
  onemode_df <- as.data.frame(onemode_matrix[univs_order_by_privhs, univs_order_by_privhs])
  
  rownames(onemode_df) <- privhs_events_unique_df$univ_abbrev
  
  for(i in seq_along(onemode_df)) {
    total_visits <- as.numeric(str_extract(names(onemode_df)[[i]], '\\d+'))
    
    # Replace self count from 0 to N = number of unique private HS visited
    onemode_df[[i]][[i]] <- total_visits
  }
  
  onemode_pct_df <- onemode_df
  
  for (i in seq_along(onemode_pct_df)) {
    total_visits <- as.numeric(str_extract(names(onemode_pct_df)[[i]], '\\d+'))
    
    # Replace column w/ pct
    onemode_pct_df[[i]] <- round(onemode_pct_df[[i]] / total_visits * 100, 1)
  }
  
  list(onemode_df = onemode_df, onemode_pct_df = onemode_pct_df)
}


# Public univs
table_1mode_pubu <- create_1mode_table(g_1mode_pubu_psi, 'public_research')
saveRDS(table_1mode_pubu, file = './assets/tables/table_1mode_pubu.RDS')

View(table_1mode_pubu$onemode_df)  # count matrix
View(table_1mode_pubu$onemode_pct_df)  # pct matrix

# Checks (edges *are* actually unique private HS visited in common)
(privhs_events %>% filter(univ_id == '100751'))$school_id %>% length()  # U of Alabama has 1039 private HS visits total (non-unique)
(privhs_events %>% filter(univ_id == '100751'))$school_id %>% unique() %>% length()  # Visits are to 759 unique private HS (unique)

(privhs_events %>% filter(univ_id == '218663'))$school_id %>% length()  # U of S.Carolina has 498 private HS visits total (non-unique)
(privhs_events %>% filter(univ_id == '218663'))$school_id %>% unique() %>% length()  # Visits are to 396 unique private HS (unique)

intersect((privhs_events %>% filter(univ_id == '100751'))$school_id, (privhs_events %>% filter(univ_id == '218663'))$school_id) %>% length()  # 284 private HS are the same between them (unique)
intersect(unique((privhs_events %>% filter(univ_id == '100751'))$school_id), unique((privhs_events %>% filter(univ_id == '218663'))$school_id)) %>% length()  # Same as above

# Private univs
table_1mode_privu <- create_1mode_table(g_1mode_privu_psi, 'private_national')
saveRDS(table_1mode_privu, file = './assets/tables/table_1mode_privu.RDS')

# Private colleges
table_1mode_privc <- create_1mode_table(g_1mode_privc_psi, 'private_libarts')
saveRDS(table_1mode_privc, file = './assets/tables/table_1mode_privc.RDS')

# Public and private universities
table_1mode_u <- create_1mode_table(g_1mode_u_psi, c('public_research', 'private_national'))

intersect((privhs_events %>% filter(univ_id == '100751'))$school_id, (privhs_events %>% filter(univ_id == '152080'))$school_id) %>% length()  # 326 private HS are the same between U of Alabama & Notre Dame (unique)

clusters_u <- create_fast_greedy(twomode_network = g_2mode_u, mode = 'psi', no = 4, plot_tree = F)
get_cluster <- Vectorize(function(u) clusters_u[[u]])

clusters_df <- univ_info %>% filter(classification != 'private_libarts') %>% select(univ_abbrev, classification, univ_id) %>% mutate(cluster = get_cluster(univ_id)) %>% arrange(cluster)
table_1mode_u$clusters_df <- clusters_df

saveRDS(table_1mode_u, file = './assets/tables/table_1mode_u.RDS')


## ------------------------------
## TABLE FROM EGO IGRAPH OBJECTS
## ------------------------------

# FUNCTION: create_ego_table(twomode_network, ego_networks, univs, c_analysis, <k>, <h>, <no>, <steps>, <pubu_visits>, <privu_visits>, <race_var>, <ranking_var>)
  # twomode_network: 2-mode object
  # ego_networks: ego network objects
  # univs: univs to include in table
  # c_analysis: type of cluster analysis (hclust, fast)
  # k: number of clusters (default: NULL)
  # h: height to cut tree (default: NULL)
  # no: number of clusters to form (default: NULL)
  # steps: number of merge operations to perform (default: NULL)
  # pubu_visits: which HS visited by public univs to include (instate, outofstate, default: all)
  # privu_visits: which HS visited by private univs to include (instate, outofstate, default: all)
  # race_var: race variable to use (default: pct_blacklatinxnative_cat)
  # ranking_var: ranking variable to use (default: rank_cat2)

# RETURNS: dataframe of one row per univ containing summary of visited HS characteristics

get_characteristic_pct <- function(hs_df, characteristic_var, characteristic_vals) {
  output <- vector(mode = 'character', length = length(characteristic_vals))
  
  for (j in seq_along(characteristic_vals)) {
    output[[j]] <- sprintf('%.1f%%', nrow(hs_df %>% filter(get(characteristic_var) == characteristic_vals[[j]])) / nrow(hs_df) * 100)
  }
  
  output
}

create_ego_table <- function(twomode_network, ego_networks, univs, c_analysis, k = NULL, h = NULL, no = NULL, steps = NULL, pubu_visits = 'all', privu_visits = 'all', race_var = 'pct_blacklatinxnative_cat', ranking_var = 'rank_cat2', enroll_var = 'enroll_cat1') {

  # Cluster analysis to identify communities
  if (pubu_visits != 'all' & privu_visits == 'all') {
    twomode_network <- subgraph.edges(graph = twomode_network, eids = E(twomode_network)[(E(twomode_network)$visiting_univ == 'public' & E(twomode_network)$visit_loc == pubu_visits) | E(twomode_network)$visiting_univ == 'private'])
  } else if (pubu_visits == 'all' & privu_visits != 'all') {
    twomode_network <- subgraph.edges(graph = twomode_network, eids = E(twomode_network)[E(twomode_network)$visiting_univ == 'public' | (E(twomode_network)$visiting_univ == 'private' & E(twomode_network)$visit_loc == privu_visits)])
  } else if (pubu_visits != 'all' & privu_visits != 'all') {
    twomode_network <- subgraph.edges(graph = twomode_network, eids = E(twomode_network)[(E(twomode_network)$visiting_univ == 'public' & E(twomode_network)$visit_loc == pubu_visits) | (E(twomode_network)$visiting_univ == 'private' & E(twomode_network)$visit_loc == privu_visits)])
  }
  
  #summary(twomode_network)
  
  if (c_analysis == 'fast') {
    member <- create_fast_greedy(twomode_network = twomode_network, mode = 'psi', no = no, steps = steps)
  } else {  # c_analysis == 'hclust'
    member <- create_hclust(twomode_network = twomode_network, mode = 'psi', k = k, h = h)
  }
  
  # Characteristics variables
  twomode_df <- igraph::as_data_frame(twomode_network, 'vertices')
  
  #print(twomode_df)
  #print(twomode_df[[enroll_var]])
  
  region_vals <- levels(as.factor(twomode_df$region))
  religion_vals <- levels(as.factor(twomode_df$religion))
  race_vals <- levels(as.factor(twomode_df[[race_var]]))
  ranking_vals <- levels(as.factor(twomode_df[[ranking_var]]))
  
  enroll_vals <- levels(as.factor(twomode_df[[enroll_var]]))
  #print(enroll_vals)
  
  # Create empty dataframe to populate with stats from ego graphs
  col_names <- c('univ_id', 'univ_name', 'cluster', 'type', 'rank', 'control', 'total', 'in_state', 'out_of_state', 'characteristics',
                 region_vals, religion_vals, race_vals, ranking_vals, enroll_vals)
  
  ego_tbl <- data.frame(matrix(NA_character_, ncol = length(col_names), nrow = 0, dimnames = list(NULL, col_names)), stringsAsFactors = F, check.names = F)
    
  # Populate ego table
  for (i in seq_along(univs)) {
    univ_id <- univs[[i]]
    ego_network <- ego_networks[[univ_id]]
    
    if (univ_id %in% pubu_vec && pubu_visits != 'all') {
      ego_network_order1 <- subgraph.edges(graph = ego_network, eids = E(ego_network)[E(ego_network)$order == 1 & E(ego_network)$visit_loc == pubu_visits])
      privhs_visits_var <- if_else(pubu_visits == 'outofstate', 'privhs_visits_out', 'privhs_visits_in')
    } else if (univ_id %in% privu_vec && privu_visits != 'all') {
      ego_network_order1 <- subgraph.edges(graph = ego_network, eids = E(ego_network)[E(ego_network)$order == 1 & E(ego_network)$visit_loc == privu_visits])
      privhs_visits_var <- if_else(privu_visits == 'outofstate', 'privhs_visits_out', 'privhs_visits_in')
    } else {
      ego_network_order1 <- subgraph.edges(graph = ego_network, eids = E(ego_network)[E(ego_network)$order == 1])
      privhs_visits_var <- 'all'
    }
    
    ego_df <- igraph::as_data_frame(ego_network_order1, 'vertices')
    
    univ_characteristics <- ego_df %>% filter(type == TRUE)
    hs_characteristics <- ego_df %>% filter(type == FALSE)
    
    #print(hs_characteristics)
    
    ego_tbl[i, ] <- c(univ_characteristics$name, univ_characteristics$school_name, member[names(member) == univ_characteristics$name],
                      univ_characteristics$school_type, univ_characteristics$ranking_numeric, univ_characteristics$control,
                      if_else(privhs_visits_var == 'all', univ_characteristics$privhs_visits_tot, univ_characteristics[[privhs_visits_var]]),
                      if_else(privhs_visits_var == 'privhs_visits_out', 0L, univ_characteristics$privhs_visits_in),
                      if_else(privhs_visits_var == 'privhs_visits_in', 0L, univ_characteristics$privhs_visits_out),
                      str_c(univ_characteristics$region, univ_characteristics$religion, univ_characteristics[[str_sub(race_var, end = -5)]], univ_characteristics$ranking_numeric, univ_characteristics$enroll, sep = '|'),
                      get_characteristic_pct(hs_characteristics, 'region', region_vals),
                      get_characteristic_pct(hs_characteristics, 'religion', religion_vals),
                      get_characteristic_pct(hs_characteristics, race_var, race_vals),
                      get_characteristic_pct(hs_characteristics, ranking_var, ranking_vals),
                      get_characteristic_pct(hs_characteristics, enroll_var, enroll_vals))
  }
  
  ego_tbl %>% arrange(as.numeric(cluster), type, as.numeric(rank))
}

# Save tables


# ego_table_pubu <- create_ego_table(g_2mode_pubu, egos_psi_pubu, pubu_vec, c_analysis = 'hclust', k = 4)
# saveRDS(ego_table_pubu, file = './assets/tables/table_ego_pubu.RDS')
# ego_table_pubu
# 
# ego_table_pubu_outst <- create_ego_table(g_2mode_pubu, egos_psi_pubu, pubu_vec, c_analysis = 'hclust', k = 4, pubu_visits = 'outofstate')
# saveRDS(ego_table_pubu_outst, file = './assets/tables/table_ego_pubu_outst.RDS')
# ego_table_pubu_outst
# 
# 
# ego_table_privu <- create_ego_table(g_2mode_privu, egos_psi_privu, privu_vec, c_analysis = 'hclust', k = 4)
# saveRDS(ego_table_privu, file = './assets/tables/table_ego_privu.RDS')
# ego_table_privu
# 
# ego_table_privc <- create_ego_table(g_2mode_privc, egos_psi_privc, privc_vec, c_analysis = 'hclust', k = 4)
# saveRDS(ego_table_privc, file = './assets/tables/table_ego_privc.RDS')
# ego_table_privc
# 
# ego_table_univ <- create_ego_table(g_2mode_u, egos_psi_u, univ_vec, c_analysis = 'hclust', k = 4)
# saveRDS(ego_table_univ, file = './assets/tables/table_ego_univ.RDS')
# ego_table_univ
# 
# ego_table_univ_pubu_outst <- create_ego_table(g_2mode_u, egos_psi_u, univ_vec, c_analysis = 'hclust', k = 4, pubu_visits = 'outofstate', privu_visits = 'all')
# saveRDS(ego_table_univ_pubu_outst, file = './assets/tables/table_ego_univ_pubu_outst.RDS')
# ego_table_univ_pubu_outst


ego_table_all <- create_ego_table(g_2mode, egos_psi, psi_vec, c_analysis = 'hclust', k = 4)
saveRDS(ego_table_all, file = './assets/tables/table_ego_all.RDS')
ego_table_all


# ego_table_all_pubu_outst <- create_ego_table(g_2mode, egos_psi, psi_vec, c_analysis = 'hclust', k = 4, pubu_visits = 'outofstate', privu_visits = 'all')
# saveRDS(ego_table_all_pubu_outst, file = './assets/tables/table_ego_all_pubu_outst.RDS')
# ego_table_all_pubu_outst


## ---------------------------------
## TABLE FROM 2-MODE IGRAPH OBJECTS
## ---------------------------------

# FUNCTION: create_2mode_table(twomode_network, <race_var>, <ranking_var>)
  # twomode_network: 2-mode object
  # race_var: race variable to use (default: pct_blacklatinxnative_cat)
  # ranking_var: ranking variable to use (default: rank_cat2)

# RETURNS: list containing the full and aggregated dataframes
  # $full_table: table of top private HS ordered by degree
  # $agg_table: table of top private HS grouped by degree band

create_agg_table <- function(network, characteristic) {
  network %>% select_('degree_band', characteristic) %>%
    group_by_('degree_band', characteristic) %>%
    summarise(cnt = n()) %>%
    mutate(pct = sprintf('%.1f%%', cnt / sum(cnt) * 100)) %>%
    select_('degree_band', characteristic, 'pct') %>%
    spread_(characteristic, 'pct')
}

create_2mode_table <- function(twomode_network, race_var = 'pct_blacklatinxnative_cat', ranking_var = 'rank_cat2') {
  
  twomode_df <- igraph::as_data_frame(twomode_network, 'vertices')
  
  twomode_df$degree <- degree(twomode_network)
  twomode_df$strength <- strength(twomode_network)
  twomode_df$closeness <- closeness(twomode_network, normalized = T)
  
  # Filter for private HS
  twomode_df <- twomode_df %>% filter(type == F)
  
  # Create degree_band categorical variable
  twomode_df <- twomode_df %>% mutate(degree_band = case_when(
    degree > 5 ~ '6+',
    TRUE ~ as.character(degree)
  ))

  # Full table
  full_twomode_table <- twomode_df %>% arrange(-degree, -strength) %>%
    select_('name', 'school_name', 'city', 'state_code', 'region', 'religion', str_sub(race_var, end = -5), 'ranking', 'ranking_numeric', 'degree', 'closeness', 'strength')
  
  # Aggregated table
  agg_twomode_table <- twomode_df %>%
    group_by(degree_band) %>%
    summarise(count = n())

  agg_twomode_table <- left_join(agg_twomode_table, create_agg_table(twomode_df, 'region'))
  agg_twomode_table <- left_join(agg_twomode_table, create_agg_table(twomode_df, 'religion'))
  agg_twomode_table <- left_join(agg_twomode_table, create_agg_table(twomode_df, race_var))
  agg_twomode_table <- left_join(agg_twomode_table, create_agg_table(twomode_df, ranking_var))
  
  band_order <- c('6+', '5', '4', '3', '2', '1')
  agg_twomode_table <- agg_twomode_table[order(match(agg_twomode_table$degree_band, band_order)), ]
  
  list(full_table = full_twomode_table, agg_table = agg_twomode_table)
}

# Save tables
# saveRDS(twomode_both$full_table, file = './assets/tables/table_2mode_both.RDS')
# saveRDS(twomode_both$agg_table, file = './assets/tables/table_2mode_agg_both.RDS')
# 
# twomode_privu <- create_2mode_table(g_2mode_privu)
# saveRDS(twomode_privu$full_table, file = './assets/tables/table_2mode_privu.RDS')
# saveRDS(twomode_privu$agg_table, file = './assets/tables/table_2mode_agg_privu.RDS')
# 
# twomode_pubu <- create_2mode_table(g_2mode_pubu)
# saveRDS(twomode_pubu$full_table, file = './assets/tables/table_2mode_pubu.RDS')
# saveRDS(twomode_pubu$agg_table, file = './assets/tables/table_2mode_agg_pubu.RDS')
