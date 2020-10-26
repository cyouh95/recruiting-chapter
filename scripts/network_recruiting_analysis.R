library(igraph)
library(tidyverse)

options(max.print=9999)
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


## ------------------
## DRAFTING ANALYSIS
## ------------------

# choosing potential universities to for ego network

vertex_attr_names(g_1mode_psi_privu)
vertex_attr_names(g_1mode_hs)
V(g_1mode_psi_privu)$school_name
V(g_1mode_hs)$ranking

#events_data %>% left_join(y = univ_df, by = c("univ_id" = "school_id")) %>% group_by(school_name) %>% count() %>% View()

events_data %>% left_join(y = univ_df, by = c("univ_id" = "school_id")) %>% group_by(school_name) %>% count() %>% View()

# Create ego network and ego network order =1

  # which university to be the ego network
    #univ_id <- '152080' # Notre Dame ego network
    univ_id <- "160755" # Tulane # why does Tulane ego network order = 1 contain several universities from our sample?
    #univ_id <- "228246" # Southern methodist university
    #univ_id <- "216597" # Villanova

    
  # ego network
  ego_network <- egos_psi_privu[[univ_id]]

# create subgraph for order = 1 only [not really necessary]
  
    ego_network_order1 <- subgraph.edges(graph = ego_network, eids = E(ego_network)[E(ego_network)$order==1])
    ego_network_order1
    
    table(V(ego_network_order1)$rank_cat2, useNA = "always")
  
### create variables for academic reputation and for racial composition that will be use in ego graph plot

  # CUT THIS CODE HERE AND PUT IT IN create_igraph_objects.R INSTEAD    
  

# investigate vertex characteristics
  
    vertex_attr_names(ego_network_order1)
    
  # investigate characteristics of all high schools that received a visit 
    #(from any public or private in college/university in our sample)

    table(V(g_1mode_hs)$region, useNA = "always") # 1 = Northeast   2= Midwest     3 = South      4 = West 
    table(V(g_1mode_hs)$religion, useNA = "always")
    table(V(g_1mode_hs)$ranking, useNA = "always")

    summary(V(g_1mode_hs)$pct_white)
  
  # investigate characteristics of ego college/university
    
    table(V(ego_network_order1)$region, useNA = "always") # 1 = Northeast   2= Midwest     3 = South      4 = West 
    table(V(ego_network_order1)$religion, useNA = "always")
    
    table(V(ego_network_order1)$ranking, useNA = "always")
    table(V(ego_network_order1)$ranking_numeric, useNA = "always")
    
    
  # compare all high schools that got a visit to schools that got a visit from ego
    
    proportions(table(V(g_1mode_hs)$region, useNA = "always")) # 1 = Northeast   2= Midwest     3 = South      4 = West 
    proportions(table(V(ego_network_order1)$region, useNA = "always")) # 
    
    proportions(table(V(g_1mode_hs)$religion, useNA = "always")) # has 4 categories
    proportions(table(V(ego_network_order1)$religion, useNA = "always"))
    
    proportions(table(V(g_1mode_hs)$ranking, useNA = "always")) # turn this into 4 categoriexs
    proportions(table(V(ego_network_order1)$ranking, useNA = "always"))
    

## --------------------
## EGO IGRAPH FUNCTION (can utilize order =1, order =2, or both)
## --------------------

# function to plot ego graph

plot_ego_graph <- function(network, characteristic, values, keys, colors = c('blue', 'purple', 'red', 'green'), title = '', graph_order = 'both') {

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
      vertex_attr(network, "type") == TRUE &  vertex_attr(network, "name") != univ_id ~ 12,
      vertex_attr(network, "type") == TRUE &  vertex_attr(network, "name") == univ_id ~ 18,
    )
    
    edge_width <- if_else(E(network)$order == 1, 0.5, 0)
    edge_color <- 'lightgrey'
    edge_lty <- 5

  } else {  # graph order == 1
    vertex_size <- if_else(V(network)$type, 18, 4)
    
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
  }


  plot.igraph(
    x = network,
    # vertex attributes
    vertex.shape = 'circle',
    vertex.label = if_else(V(network)$type, V(network)$school_name, ''),
    vertex.color = vertex_color,
    vertex.size = vertex_size,
    # edge attributes
    edge.color = edge_color,
    edge.lty = edge_lty,
    edge.width = edge_width,
    layout = graph_layout,  
    main = str_to_title(title),
    margin = 0.0 # -0.2
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
# dev.off() # to fix this: Error in .Call.graphics(C_palette, value) : invalid graphics state
    
par(mfrow=c(1, 1))  # resets to single plot

plot_ego_graph(ego_network, characteristic = NA, values = NA, keys = NA, colors = NA, title = "", graph_order = 1)
plot_ego_graph(ego_network, characteristic = NA, values = NA, keys = NA, colors = NA, title = "", graph_order = 2)
plot_ego_graph(ego_network, characteristic = NA, values = NA, keys = NA, colors = NA, title = "", graph_order = "both")

plot_ego_graph(ego_network, characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), title = "geographic region", graph_order = 1)
plot_ego_graph(ego_network, characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), title = "geographic region", graph_order = 2)
plot_ego_graph(ego_network, characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), title = "geographic region", graph_order = 'both')


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

## ---------------------------
## PLOT 2-MODE IGRAPH OBJECTS
## ---------------------------

# Private univs and the private HS they visited
vertex_attr_names(g_2mode_privu)

par(mfrow=c(1, 1))  # resets to single plot

# which layout to choose? ask Russ?
  # on layout_with_kk as an example of an "energy displacement" method, from Kolaczyk & Csardi 2020, pg 32-33
    # motivated by the fact that it is possible to associate the collection of forces in spring systems with an overall system energy, another common approach to generating layouts is that of energy-placement methods. An energy, as a function of vertex positions, ostensibly is defined using expressions motivated by those found in physics. A vertex placement is chosen which minimizes the total system energy. A physical system with minimum energy is typically in its most relaxed state, and hence the assertion here is that a graph drawn according to similar principles should be visually appealing. 
    # says that methods based on multidimensional scaling (MDS) are of the "energy displacement" type
    # layout_with_kk is a commonly used variant of multidimensional scaling approach
  # on layout_with_fr as an example of "spring-embedder methods", from Kolaczyk & Csardi 2020, pg 32
    #Often more effective for creating useful drawings are layouts based on exploiting analogies between the relational structure in graphs and the forces among elements in physical systems. One approach in this area, and the earliest proposed, is to introduce attractive and repulsive forces by associating vertices with balls and edges with springs. If a literal system of balls connected by springs is disrupted, thereby stretching some of the springs and compressing others, upon being let go it will return to its natural state. So-called spring-embedder methods of graph drawing define a notion of force for each vertex in the graph depending, at the very least, on the positions of pairs of vertices and the distances between them, and seek to iteratively update the placement of vertices until a vector of net forces across vertices converges. 
    #The method of Fruchterman and Reingold [6] is a commonly used example of this type.

#graph_layout <- layout_with_kk
graph_layout <- layout_with_fr

# open file
pdf("")
plot(
  x = g_2mode_privu, 
  vertex.label = if_else(V(g_2mode_privu)$type, V(g_2mode_privu)$school_name, ''),
  vertex.color = if_else(V(g_2mode_privu)$type, 'lightblue', 'salmon'),
  vertex.shape = if_else(V(g_2mode_privu)$type, 'circle', 'circle'),
  vertex.size = if_else(V(g_2mode_privu)$type, 3, 1),
  edge.lty = 3, # 0 (“blank”), 1 (“solid”), 2 (“dashed”), 3 (“dotted”), 4 (“dotdash”), 5 (“longdash”), 6 (“twodash”).
  edge.lty = .5,
  edge.color = 'lightgrey',
  layout = graph_layout,
  margin = -0.8
)


## ------------------------------
## TABLE FROM EGO IGRAPH OBJECTS
## ------------------------------

ego_table <- data.frame(univ_id = character(0), characteristics = character(0),
                        private_hs_region = character(0), private_hs_religion = character(0),
                        private_hs_race = character(0), private_hs_ranking = character(0),
                        stringsAsFactors=FALSE)

for (i in seq_along(privu_vec)) {
  ego_network <- egos_psi_privu[[privu_vec[[i]]]]
  ego_network_order1 <- subgraph.edges(graph = ego_network, eids = E(ego_network)[E(ego_network)$order==1])
  
  ego_df <- as.data.frame(vertex_attr(ego_network_order1))
  
  univ_characteristics <- ego_df %>% filter(type == TRUE)
  
  privhs_characteristics <- ego_df %>% filter(type == FALSE)
  num_privhs <- nrow(privhs_characteristics)
  
  pct_privhs_region <- sprintf('%.1f%%', nrow(privhs_characteristics %>% filter(region == univ_characteristics$region)) / num_privhs * 100)
  pct_privhs_religion <- sprintf('%.1f%%', nrow(privhs_characteristics %>% filter(religion == univ_characteristics$religion)) / num_privhs * 100)
  pct_privhs_race <- sprintf('%.1f%%', nrow(privhs_characteristics %>% filter(pct_white_cat == univ_characteristics$pct_white_cat)) / num_privhs * 100)
  pct_privhs_ranking <- sprintf('%.1f%%', nrow(privhs_characteristics %>% filter(rank_cat1 == 'c1_top100')) / num_privhs * 100)
  
  ego_table[i, ] <- c(as.character(univ_characteristics$school_name),
                      str_c(val_label(univ_df$region, univ_characteristics$region), univ_characteristics$religion, univ_characteristics$pct_white_cat, univ_characteristics$ranking, sep = '|'),
                      pct_privhs_region, pct_privhs_religion, pct_privhs_race, pct_privhs_ranking)
}

View(ego_table)


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
  city = V(g_2mode_privu)$city,
  state = V(g_2mode_privu)$state_code,
  region = V(g_2mode_privu)$region,
  religion = V(g_2mode_privu)$religion,
  pct_blacklatinxnative = V(g_2mode_privu)$pct_blacklatinxnative,
  race = V(g_2mode_privu)$pct_blacklatinxnative_cat,
  ranking_score = V(g_2mode_privu)$ranking,
  ranking_numeric = V(g_2mode_privu)$ranking_numeric,
  ranking = V(g_2mode_privu)$rank_cat1,
  type = V(g_2mode_privu)$type,
  degree = V(g_2mode_privu)$degree,
  strength = V(g_2mode_privu)$strength,
  closeness = V(g_2mode_privu)$closeness
)

# temporarily get rid of unmerged rows
t_2mode_privu <- t_2mode_privu %>% filter(type == F, !is.na(school_name)) %>%
  mutate(degree_band = case_when(degree >= 15 ~ '15+',
                                 degree >= 10 ~ '10-14',
                                 degree >= 5 ~ '5-9',
                                 TRUE ~ as.character(degree)))

full_2mode_table <- t_2mode_privu %>% arrange(-degree) %>%
  select(school_id, school_name, city, state, region, religion, pct_blacklatinxnative, ranking_score, ranking_numeric, degree, closeness, strength)

View(full_2mode_table)

saveRDS(full_2mode_table, file = './assets/tables/table_2mode.RDS')
# readr::write_file(knitr::kable(head(full_2mode_table), 'latex'), 'assets/tables/table_2mode.tex')

# Aggregated table
characteristic <- 'race'

agg_2mode_table <- t_2mode_privu %>% select_('degree_band', characteristic) %>%
  group_by_('degree_band', characteristic) %>%
  summarize(cnt = n()) %>%
  mutate(pct = sprintf('%.1f%%', cnt / sum(cnt) * 100)) %>%
  select_('degree_band', characteristic, 'pct') %>%
  spread_(characteristic, 'pct') %>%
  rename_at(vars(-degree_band), ~ paste0(characteristic, '_', .))

band_order <- c('15+', '10-14', '5-9', '5', '4', '3', '2', '1')
agg_2mode_table <- agg_2mode_table[order(match(agg_2mode_table$degree_band, band_order)), ]

View(agg_2mode_table)

full_2mode_table %>% str()
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
