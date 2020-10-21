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
    univ_id <- '152080' # Notre Dame ego network
    #univ_id <- "160755" # Tulane # why does Tulane ego network order = 1 contain several universities from our sample?
    #univ_id <- "228246" # Southern methodist university
    #univ_id <- "216597" # Villanova

    
  # ego network
  ego_network <- egos_psi_privu[[univ_id]]
  
  # subgraph for order = 1 only
  
    ego_network_order1 <- subgraph.edges(graph = ego_network, eids = E(ego_network)[E(ego_network)$order==1])
    ego_network_order1

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
    
    
    summary(V(ego_network_order1)$pct_white)
    
    

  # compare all high schools that got a visit to schools that got a visit from ego
    
    proportions(table(V(g_1mode_hs)$region, useNA = "always")) # 1 = Northeast   2= Midwest     3 = South      4 = West 
    proportions(table(V(ego_network_order1)$region, useNA = "always")) # 
    
    proportions(table(V(g_1mode_hs)$religion, useNA = "always")) # has 4 categories
    proportions(table(V(ego_network_order1)$religion, useNA = "always"))
    
    proportions(table(V(g_1mode_hs)$ranking, useNA = "always")) # turn this into 4 categoriexs
    proportions(table(V(ego_network_order1)$ranking, useNA = "always"))
    
    
    summary(V(g_1mode_hs)$pct_white)
    summary(V(ego_network_order1)$pct_white)

    

## --------------------
## EGO IGRAPH FUNCTION (order = 1)
## --------------------

### create variables for academic reputation and for racial composition that will be use in ego graph plot
    
  vertex_attr_names(ego_network_order1)
  
  # racial composition

    # pct white
    ego_network_order1 <- delete_vertex_attr(ego_network_order1, "pct_white_cat")
    vertex_attr(graph = ego_network_order1, name = "pct_white_cat") <- case_when(
      vertex_attr(ego_network_order1, "pct_white") <50 ~ "c1_lt50",
      vertex_attr(ego_network_order1, "pct_white") >=50 & vertex_attr(ego_network_order1, "pct_white") <75 ~ "c2_50to75",
      vertex_attr(ego_network_order1, "pct_white") >=75 & vertex_attr(ego_network_order1, "pct_white") <85 ~ "c3_75to85",
      vertex_attr(ego_network_order1, "pct_white") >=85 ~ "c4_85+"
    )
    
    table(V(ego_network_order1)$pct_white_cat, useNA = "always")
  
  #pct black/latinx/native
  
    ego_network_order1 <- delete_vertex_attr(ego_network_order1, "pct_blacklatinxnative")
    vertex_attr(graph = ego_network_order1, name = "pct_blacklatinxnative") <- vertex_attr(graph = ego_network_order1, name = "pct_black") + vertex_attr(graph = ego_network_order1, name = "pct_hispanic") + 
      vertex_attr(graph = ego_network_order1, name = "pct_amerindian") + vertex_attr(graph = ego_network_order1, name = "pct_nativehawaii")
  
    summary(V(ego_network_order1)$pct_blacklatinxnative)
    
    ego_network_order1 <- delete_vertex_attr(ego_network_order1, "pct_blacklatinxnative_cat")
  
    vertex_attr(graph = ego_network_order1, name = "pct_blacklatinxnative_cat") <- case_when(
      vertex_attr(ego_network_order1, "pct_blacklatinxnative") <10 ~ "c1_lt10",
      vertex_attr(ego_network_order1, "pct_blacklatinxnative") >=10 & vertex_attr(ego_network_order1, "pct_blacklatinxnative") <25 ~ "c2_10to25",
      vertex_attr(ego_network_order1, "pct_blacklatinxnative") >=25 & vertex_attr(ego_network_order1, "pct_blacklatinxnative") <50 ~ "c3_25to50",
      vertex_attr(ego_network_order1, "pct_blacklatinxnative") >=50 ~ "c4_50+"
    )
    
    table(V(ego_network_order1)$pct_blacklatinxnative_cat, useNA = "always")
    proportions(table(V(ego_network_order1)$pct_blacklatinxnative_cat, useNA = "always"))
    
  # academic reputation/ranking
    # note that rank_cat1 is NA for the ego
    ego_network_order1 <- delete_vertex_attr(ego_network_order1, "rank_cat1")
    vertex_attr(graph = ego_network_order1, name = "rank_cat1") <- case_when(
      vertex_attr(ego_network_order1, "ranking_numeric") <=100 & vertex_attr(ego_network_order1, "ranking") == "A+"  ~ "c1_top100",
      vertex_attr(ego_network_order1, "ranking_numeric") >100 & vertex_attr(ego_network_order1, "ranking_numeric") <=200 & vertex_attr(ego_network_order1, "ranking") == "A+"  ~ "c2_top200",
      vertex_attr(ego_network_order1, "ranking_numeric") >200 & vertex_attr(ego_network_order1, "ranking") == "A+"  ~ "c3_A+",
      vertex_attr(ego_network_order1, "ranking") != "A+" & is.na(vertex_attr(ego_network_order1, "ranking"))==0  ~ "c4_ltA+"
    )  

# function to plot ego graph order = 1

plot_ego_graph <- function(network, characteristic, values, keys, colors = c('blue', 'purple', 'red', 'green'), title) {
  
  color_palette <- colors
  names(color_palette) <- values
  
  plot.igraph(
    x = network,
    vertex.label = if_else(V(network)$type, V(network)$school_name, ''),
    vertex.color = recode(vertex_attr(network, characteristic), !!!color_palette),
    vertex.size = if_else(V(network)$type, 16, 4),
    edge.color = if_else(E(network)$weight == 1, 'lightgrey', 'black'),
    edge.lty = if_else(E(network)$weight == 1, 5, 1),
    edge.width = if_else(E(network)$weight == 1, 0.5, as.numeric(E(network)$weight)^2),
    layout = layout_nicely,  # layout_with_kk, layout_with_fr, layout_in_circle, layout_nicely
    #main = paste('Ego network for', (univ_df %>% filter(school_id == univ_id))$school_name),
    main = str_to_title(title),
    margin = -0.2
  )  
  
  legend(
    x = 1,
    y = -0.1,
    legend = keys,
    fill = colors,
    bty = 'n'  # box drawn around legend: 'o' [default] = solid line box; 'n' = no box
  )
}

table(V(ego_network_order1)$pct_white_cat, useNA = "always")
table(V(ego_network_order1)$pct_blacklatinxnative_cat, useNA = "always")
table(V(ego_network_order1)$rank_cat1, useNA = "always")

# potentially call function here that creates desired race and ranking variables
par(mfrow=c(2, 2))
plot_ego_graph(ego_network_order1, characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), title = "geographic region")
plot_ego_graph(ego_network_order1, characteristic = 'religion', values = c('catholic', 'conservative_christian', 'nonsectarian', 'other_religion'), keys = c('Catholic', 'Conservative Christian', 'Nonsectarian', 'Other'), title = "religious affiliation")
plot_ego_graph(ego_network_order1, characteristic = 'rank_cat1', values = c('c1_top100','c2_top200','c3_A+','c4_ltA+'), keys = c('Rank top 100', 'Rank 100-200', 'A+', 'A or below'), title = "Academic reputation")
plot_ego_graph(ego_network_order1, characteristic = 'pct_blacklatinxnative_cat', values = c('c1_lt10','c2_10to25','c3_25to50','c4_50+'), keys = c('LT 10%', '10-25%', '25-50%', 'GT 50%'), title = "percent black, latinx, or Native")
#plot_ego_graph(ego_network_order1, characteristic = 'pct_white_cat', values = c('c1_lt50','c2_50to75','c3_75to85','c4_85+'), keys = c('LT 50% white', '50-75% white', '75-85% white', 'GT 85% white'), title = "percent white")

par(mfrow=c(1, 1))  # resets to single plot

plot_ego_graph(ego_network_order1, characteristic = 'religion', values = c('catholic', 'conservative_christian', 'nonsectarian', 'other_religion'), keys = c('Catholic', 'Conservative Christian', 'Nonsectarian', 'Other'))

# MESSING AROUND WITH THE RANKING TABLE


## ---------------------------
## PLOT 2-MODE IGRAPH OBJECTS
## ---------------------------

# Private univs and the private HS they visited
vertex_attr_names(g_2mode_privu)

par(mfrow=c(1, 1))  # resets to single plot

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
  city = V(g_2mode_privu)$city,
  state = V(g_2mode_privu)$state_code,
  pct_white = V(g_2mode_privu)$pct_white,
  type = V(g_2mode_privu)$type,
  degree = V(g_2mode_privu)$degree,
  strength = V(g_2mode_privu)$strength,
  closeness = V(g_2mode_privu)$closeness
)

View(t_2mode_privu %>% filter(type == F) %>% arrange(-closeness) %>% select(school_id, school_name, city, state, pct_white, closeness, degree, strength))


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
