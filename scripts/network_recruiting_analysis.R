library(igraph)
library(tidyverse)


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


## ------------------------
## PLOT EGO IGRAPH OBJECTS
## ------------------------

# Notre Dame ego network
univ_id <- '152080'
characteristic <- 'region'

ego_network <- egos_psi_privu[[univ_id]]
vertex_attr_names(ego_network)

univ_characteristic <- (univ_df %>% filter(school_id == univ_id))[[characteristic]]

plot(
  x = ego_network,
  vertex.label = if_else(V(ego_network)$type, V(ego_network)$school_name, ''),
  vertex.shape = if_else(V(ego_network)$type, 'square', 'circle'),
  vertex.color = if_else(vertex_attr(ego_network, characteristic) == univ_characteristic, 'lightblue', 'lightgrey'),
  vertex.size = if_else(V(ego_network)$type, 6, 2),
  edge.color = 'lightgrey',
  edge.lty = if_else(E(ego_network)$order == 1, 0, 1),
  main = paste('Ego network for', (univ_df %>% filter(school_id == univ_id))$school_name),
  layout = layout_with_kk,  # layout_with_kk, layout_with_fr, layout_in_circle, layout_nicely
)


## ---------------------------
## PLOT 2-MODE IGRAPH OBJECTS
## ---------------------------

# Private univs and the private HS they visited
vertex_attr_names(g_2mode_privu)

plot(
  x = g_2mode_privu, 
  vertex.label = if_else(V(g_2mode_privu)$type, V(g_2mode_privu)$school_name, ''),
  vertex.color = if_else(V(g_2mode_privu)$type, 'lightblue', 'salmon'),
  vertex.shape = if_else(V(g_2mode_privu)$type, 'square', 'circle'),
  vertex.size = if_else(V(g_2mode_privu)$type, 5, 2),
  edge.lty = 0,
  layout = layout_with_kk,
  margin = -0.4
)


## ---------------------------------
## TABLE FROM 2-MODE IGRAPH OBJECTS
## ---------------------------------

# Create table of characteristics
vertex_attr_names(g_2mode_privu)

V(g_2mode_privu)$degree <- degree(g_2mode_privu)
V(g_2mode_privu)$strength <- strength(g_2mode_privu)
V(g_2mode_privu)$closeness <- closeness(graph = g_2mode_privu, normalized = T)

t_2mode_privu <- data.frame(
  school_id = V(g_2mode_privu)$name,
  school_name = V(g_2mode_privu)$school_name,
  # city = V(g_2mode_privu)$city,
  # state = V(g_2mode_privu)$state_code,
  pct_white = V(g_2mode_privu)$pct_white,
  type = V(g_2mode_privu)$type,
  degree = V(g_2mode_privu)$degree,
  strength = V(g_2mode_privu)$strength,
  closeness = V(g_2mode_privu)$closeness
)

View(t_2mode_privu %>% filter(type == F) %>% arrange(-closeness) %>% select(school_id, school_name, pct_white, closeness, degree, strength))


## -----------------------------------
## CLUSTER FROM 2-MODE IGRAPH OBJECTS
## -----------------------------------

# Community cluster
vertex_attr_names(g_2mode_privu)

c_2mode_privu <- cluster_fast_greedy(g_2mode_privu)
length(c_2mode_privu)
sizes(c_2mode_privu)
membership(c_2mode_privu)

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
