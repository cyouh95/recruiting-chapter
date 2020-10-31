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

events_data %>% left_join(y = univ_df, by = c("univ_id" = "school_id")) %>% group_by(school_name) %>% count()

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
    #univ_id <- "160755" # Tulane # why does Tulane ego network order = 1 contain several universities from our sample?
    #univ_id <- "228246" # Southern methodist university
    univ_id <- "216597" # Villanova

    
  # ego network
  ego_network <- egos_psi_privu[[univ_id]]

  
  vertex_attr_names(ego_network)
  
  table(V(ego_network)$religion,  useNA = "always")
  
  V(ego_network)$name[V(ego_network)$type == TRUE]
  
proportions(table(V(ego_network_order1)$ranking, useNA = "always"))  

V(ego_network)$religion[V(ego_network)$type == TRUE]
V(ego_network)$religion[V(ego_network)$type == FALSE]

univ_id <- "216597" # Villanova
# ego network
ego_network <- egos_psi_privu[[univ_id]]


vertex_attr(ego_network, "religion")

vertex_attr(ego_network, "religion") <- case_when(
        vertex_attr(network, "type") == FALSE ~ 'salmon',
        vertex_attr(network, "type") == TRUE &  vertex_attr(network, "name") != univ_id ~ 'lightblue',
        vertex_attr(network, "type") == TRUE &  vertex_attr(network, "name") == univ_id ~ 'purple',
      )

      
# The U.S. Department of Education classifies conservative Christian schools
# as those that have membership in at least one of four associations (Kena et al., 2016): 
  # Accelerated Christian Education
  # American Association of Christian Schools
  # Association of Christian Schools International
  # Oral Roberts University Education Fellowship

# Texas Christian University
  # https://en.wikipedia.org/wiki/Texas_Christian_University
  # https://en.wikipedia.org/wiki/Christian_Church_(Disciples_of_Christ)

# Southern Methodist university
  # https://en.wikipedia.org/wiki/Southern_Methodist_University
  # https://en.wikipedia.org/wiki/United_Methodist_Church

# Baylor, baptist
  # https://en.wikipedia.org/wiki/Baylor_University
  # https://en.wikipedia.org/wiki/Baptist_General_Convention_of_Texas



plot_ego_graph <- function(univ_id, characteristic, values, keys, colors = c('blue', 'purple', 'red', 'green'), title = univ_info[univ_info$univ_id == univ_id, ] %>% select(univ_abbrev) %>% as.character(), graph_order = 'both', margin = 0) {

  network <- egos_psi_privu[[univ_id]]

  # recode religion for specific private universities
  vertex_attr(network, "religion") <- case_when(
    vertex_attr(network, "name") %in% c("173902","221519","139658","228246","223232","228875")==0 ~ vertex_attr(network, "religion"),
    vertex_attr(network, "name")=="173902" ~ "other_religion", # Macalester presbyterian_church_(usa)
    vertex_attr(network, "name")=="221519" ~ "other_religion", # Sewanee protestant_episcopa
    vertex_attr(network, "name")=="139658" ~ "other_religion", # Emory united_methodist
    vertex_attr(network, "name")=="228246" ~ "other_religion", # SMU united_methodist
    vertex_attr(network, "name")=="223232" ~ "conservative_christian", # Baylor baptist
    vertex_attr(network, "name")=="228875" ~ "other_religion" # TCU christian_church_(disciples_of_christ)
  )
    
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
      vertex_attr(network, "type") == FALSE ~ 3,
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
    #graph_layout <- layout_with_fr
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
 dev.off() # to fix this: Error in .Call.graphics(C_palette, value) : invalid graphics state




par(mfrow=c(1, 1))  # resets to single plot
plot_ego_graph(univ_id = "160755", characteristic = NA, values = NA, keys = NA, colors = NA, graph_order = 1)

plot_ego_graph(univ_id = "160755", characteristic = NA, values = NA, keys = NA, colors = NA, title, graph_order = 1)
plot_ego_graph(univ_id = "160755", characteristic = NA, values = NA, keys = NA, colors = NA, title = "", graph_order = 2)
plot_ego_graph(univ_id = "160755", characteristic = NA, values = NA, keys = NA, colors = NA, title = "", graph_order = "both")

plot_ego_graph(univ_id = "160755", characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), title = "geographic region", graph_order = 1)
plot_ego_graph(univ_id = "160755", characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), title = "geographic region", graph_order = 2)
plot_ego_graph(univ_id = "160755", characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), title = "geographic region", graph_order = 'both')

events_data %>% left_join(y = univ_df, by = c("univ_id" = "school_id")) %>% filter(event_type == "priv_hs") %>% group_by(school_name) %>% count() %>% View()

events_data %>% glimpse()
events_data %>% left_join(y = univ_df, by = c("univ_id" = "school_id")) %>% group_by(school_name) %>% count()

# focusing on variation within a cluster [cluster 1]
  # swarthmore "216287"
  # colorado college "126678"
  # oberlin "204501"
  # connecticut college ""128902

#For cluster 1, show small multiples of ranking. Maybe one outlier. Show ego graph order 1. Replace later with bar graph  

pdf("assets/figures/rank_cluster1.pdf") # open file

par(mar=c(5, 4, 4, 2) + 0.1) # default margins
#par(mar=c(0, 0, 0, 0) + 0.1, mai=c(0, 0, 0, 0))

par(mfrow=c(2, 2))

plot_ego_graph(univ_id = "216287", characteristic = 'rank_cat2', values = c('c1_top200','c2_A+','c3_A','c4_ltA'), keys = c('Rank top 200', 'A+', 'A', 'A- or below'), graph_order = 1)
plot_ego_graph(univ_id = "126678", characteristic = 'rank_cat2', values = c('c1_top200','c2_A+','c3_A','c4_ltA'), keys = c('Rank top 200', 'A+', 'A', 'A- or below'), graph_order = 1)
plot_ego_graph(univ_id = "204501", characteristic = 'rank_cat2', values = c('c1_top200','c2_A+','c3_A','c4_ltA'), keys = c('Rank top 200', 'A+', 'A', 'A- or below'), graph_order = 1)
plot_ego_graph(univ_id = "115409", characteristic = 'rank_cat2', values = c('c1_top200','c2_A+','c3_A','c4_ltA'), keys = c('Rank top 200', 'A+', 'A', 'A- or below'), graph_order = 1)
# 115409
dev.off() # close the file

#For catholic, show order equals both. one big graph of one univ and maybe characteristic equals religious

  # villanova ego network, religion overlayed
  pdf("assets/figures/villanova_religion.pdf", paper = "a4r") # open file
  
    par(mar=c(5, 4, 4, 2) + 0.1) # default margins
    #par(mar=c(0, 0, 0, 0) + 0.1, mai=c(0, 0, 0, 0))
  
    plot_ego_graph(univ_id = "216597", characteristic = 'religion', values = c('catholic', 'conservative_christian', 'nonsectarian', 'other_religion'), keys = c('Catholic', 'Conservative Christian', 'Nonsectarian', 'Other'), title = "", graph_order = 'both', margin = -0.3)
    
  dev.off() # close the file  


# show order = both of one interesting university, with an overlay
# EMORY
par(mfrow=c(1, 1))  # resets to single plot 
pdf("assets/figures/emory_region.pdf") # open file
  
  par(mar=c(5, 4, 4, 2) + 0.1) # default margins
  #par(mar=c(0, 0, 0, 0) + 0.1, mai=c(0, 0, 0, 0))
  
  # show order = 2 of one interesting university, with an overlay
  plot_ego_graph(univ_id = "139658", characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), title = "", graph_order = 'both', margin = -0.3) 
    #plot_ego_graph(univ_id = "139658", characteristic = NA, values = NA, keys = NA, colors = NA, graph_order = 'both', margin = -0.4)    
    
dev.off() # close the file  
  # williams 168342  
  # middlebury 230959
  # 221519 Sewanee 
  # 120254 Occidental 
  # 147767 Northwestern
  # 139658 Emory
  # 160755 Tulane
  # 223232 baylor

# baylor
plot_ego_graph(univ_id = "223232", characteristic = 'religion', values = c('catholic', 'conservative_christian', 'nonsectarian', 'other_religion'), keys = c('Catholic', 'Conservative Christian', 'Nonsectarian', 'Other'), title = "", graph_order = 'both', margin = -0.3)
    
# Maybe for TEXAS cluster show small multiples of race 

pdf("assets/figures/race_cluster4.pdf") # open file

  par(mar=c(5, 4, 4, 2) + 0.1) # default margins
  #par(mar=c(0, 0, 0, 0) + 0.1, mai=c(0, 0, 0, 0))
  
  par(mfrow=c(2, 2))

  # southern schools from cluster 4
  plot_ego_graph(univ_id = "228246", characteristic = 'pct_blacklatinxnative_cat', values = c('c1_lt10','c2_10to25','c3_25to50','c4_50+'), keys = c('LT 10%', '10-25%', '25-50%', 'GT 50%'), graph_order = 1)
  plot_ego_graph(univ_id = "223232", characteristic = 'pct_blacklatinxnative_cat', values = c('c1_lt10','c2_10to25','c3_25to50','c4_50+'), keys = c('LT 10%', '10-25%', '25-50%', 'GT 50%'), graph_order = 1)
  plot_ego_graph(univ_id = "228875", characteristic = 'pct_blacklatinxnative_cat', values = c('c1_lt10','c2_10to25','c3_25to50','c4_50+'), keys = c('LT 10%', '10-25%', '25-50%', 'GT 50%'), graph_order = 1)
  plot_ego_graph(univ_id = "127060", characteristic = 'pct_blacklatinxnative_cat', values = c('c1_lt10','c2_10to25','c3_25to50','c4_50+'), keys = c('LT 10%', '10-25%', '25-50%', 'GT 50%'), graph_order = 1)  
      
dev.off() # close the file
  
#plot_ego_graph(univ_id = "216597", characteristic = 'religion', values = c('catholic', 'conservative_christian', 'nonsectarian', 'other_religion'), keys = c('Catholic', 'Conservative Christian', 'Nonsectarian', 'Other'), title = "religious affiliation", graph_order = 1)
#plot_ego_graph(univ_id = "216597", characteristic = NA, values = NA, keys = NA, colors = NA, graph_order = 'both')
  
pdf("assets/figures/region_cluster1.pdf") # open file

par(mar=c(5, 4, 4, 2) + 0.1) # default margins
#par(mar=c(0, 0, 0, 0) + 0.1, mai=c(0, 0, 0, 0))

par(mfrow=c(2, 2))
plot_ego_graph(univ_id = "216287", characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), graph_order = 1)
plot_ego_graph(univ_id = "126678", characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), graph_order = 1)
plot_ego_graph(univ_id = "204501", characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), graph_order = 1)
plot_ego_graph(univ_id = "128902", characteristic = 'region', values = c(1, 2, 3, 4), keys = c('Northeast', 'Midwest', 'South', 'West'), graph_order = 1)

dev.off() # close the file

pdf("assets/figures/race_cluster1.pdf") # open file

par(mar=c(5, 4, 4, 2) + 0.1) # default margins
#par(mar=c(0, 0, 0, 0) + 0.1, mai=c(0, 0, 0, 0))

par(mfrow=c(2, 2))
plot_ego_graph(univ_id = "216287", characteristic = 'pct_blacklatinxnative_cat', values = c('c1_lt10','c2_10to25','c3_25to50','c4_50+'), keys = c('LT 10%', '10-25%', '25-50%', 'GT 50%'), graph_order = 1)
plot_ego_graph(univ_id = "126678", characteristic = 'pct_blacklatinxnative_cat', values = c('c1_lt10','c2_10to25','c3_25to50','c4_50+'), keys = c('LT 10%', '10-25%', '25-50%', 'GT 50%'), graph_order = 1)
plot_ego_graph(univ_id = "204501", characteristic = 'pct_blacklatinxnative_cat', values = c('c1_lt10','c2_10to25','c3_25to50','c4_50+'), keys = c('LT 10%', '10-25%', '25-50%', 'GT 50%'), graph_order = 1)
plot_ego_graph(univ_id = "128902", characteristic = 'pct_blacklatinxnative_cat', values = c('c1_lt10','c2_10to25','c3_25to50','c4_50+'), keys = c('LT 10%', '10-25%', '25-50%', 'GT 50%'), graph_order = 1)

dev.off() # close the file


pdf("assets/figures/rank_cluster1.pdf") # open file

par(mar=c(5, 4, 4, 2) + 0.1) # default margins
#par(mar=c(0, 0, 0, 0) + 0.1, mai=c(0, 0, 0, 0))
par(mfrow=c(2, 2))
plot_ego_graph(univ_id = "216287", characteristic = 'rank_cat2', values = c('c1_top200','c2_A+','c3_A','c4_ltA'), keys = c('Rank top 200', 'A+', 'A', 'A- or below'), graph_order = 1)
plot_ego_graph(univ_id = "126678", characteristic = 'rank_cat2', values = c('c1_top200','c2_A+','c3_A','c4_ltA'), keys = c('Rank top 200', 'A+', 'A', 'A- or below'), graph_order = 1)
plot_ego_graph(univ_id = "204501", characteristic = 'rank_cat2', values = c('c1_top200','c2_A+','c3_A','c4_ltA'), keys = c('Rank top 200', 'A+', 'A', 'A- or below'), graph_order = 1)
plot_ego_graph(univ_id = "128902", characteristic = 'rank_cat2', values = c('c1_top200','c2_A+','c3_A','c4_ltA'), keys = c('Rank top 200', 'A+', 'A', 'A- or below'), title = "Academic reputation", graph_order = 1)

dev.off() # close the file


# ORDER = 1

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

vertex_attr_names(g_2mode)

#graph_layout <- layout_with_kk
graph_layout <- layout_with_fr

pdf("assets/figures/plot_g_2mode.pdf") # open file

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
  vertex.shape = if_else(V(g_2mode)$type, 'circle', 'circle'),
  vertex.size = if_else(V(g_2mode)$type, 3, 1),
  edge.lty = 3, # 0 (“blank”), 1 (“solid”), 2 (“dashed”), 3 (“dotted”), 4 (“dotdash”), 5 (“longdash”), 6 (“twodash”).
  edge.lty = .5,
  edge.color = 'lightgrey',
  layout = graph_layout,
  margin = -0.7
)

dev.off() # close the file



c_2mode_privu <- cluster_fast_greedy(g_2mode_privu)
m_2mode_privu <- membership(c_2mode_privu)



library(plyr)
colors_2mode_privu <- revalue(as.character(m_2mode_privu), c(
  '1' = 'red',
  '2' = 'lightblue',
  '3' = 'green',
  '4' = 'yellow'
))
detach(package:plyr, unload = TRUE)

pdf("assets/figures/plot_g_2mode_privu.pdf") # open file

# par(mar=c(5, 4, 4, 2) + 0.1) # default margins
par(mar=c(0, 0, 0, 0) + 0.1, mai=c(0, 0, 0, 0))

# graph_layout <- layout_with_kk
graph_layout <- layout_with_fr

plot(
  x = g_2mode_privu, 
  vertex.label = if_else(V(g_2mode_privu)$type, V(g_2mode_privu)$school_name, ''),
  # vertex.color = if_else(V(g_2mode_privu)$type, 'green', 'salmon'),
  vertex.color = colors_2mode_privu,
  vertex.frame.color = if_else(V(g_2mode_privu)$type, 'black', 'lightgray'),
  vertex.shape = if_else(V(g_2mode_privu)$type, 'circle', 'circle'),
  vertex.size = if_else(V(g_2mode_privu)$type, 3, 1),
  edge.lty = 3, # 0 (“blank”), 1 (“solid”), 2 (“dashed”), 3 (“dotted”), 4 (“dotdash”), 5 (“longdash”), 6 (“twodash”).
  edge.lty = .5,
  edge.color = 'lightgrey',
  layout = graph_layout,
  margin = -0.7
)

dev.off() # close the file


## ---------------------------
## PLOT 2-MODE IGRAPH OBJECTS
## ---------------------------

par(mfrow=c(1, 1))

save_2mode_plot <- function(network, plot_name, plot_margin = -0.5) {
  
  cluster_2mode <- cluster_fast_greedy(network)
  membership_2mode <- membership(cluster_2mode)
  
  library(plyr)
  colors_2mode_privu <- revalue(as.character(membership_2mode), c(
    '1' = 'red',
    '2' = 'lightblue',
    '3' = 'green',
    '4' = 'yellow'
  ))
  detach(package:plyr, unload = TRUE)
  
  pdf(paste0('assets/figures/', plot_name))
  
  par(mar=c(0, 0, 0, 0) + 0.1, mai=c(0, 0, 0, 0))
  
  plot(
    x = network, 
    vertex.label = if_else(V(network)$type, V(network)$school_name, ''),
    vertex.color = colors_2mode_privu,
    vertex.frame.color = if_else(V(network)$type, 'black', 'lightgray'),
    vertex.shape = 'circle',
    vertex.size = if_else(V(network)$type, 3, 1),
    edge.lty = 3,
    edge.lty = 0.5,
    edge.color = 'lightgrey',
    layout = layout_with_fr,
    margin = plot_margin
  )
  
  dev.off()
}

save_2mode_plot(g_2mode, 'plot_2mode.pdf', plot_margin = -0.6)
save_2mode_plot(g_2mode_privu, 'plot_2mode_privu.pdf', plot_margin = -0.7)
save_2mode_plot(g_2mode_pubu, 'plot_2mode_pubu.pdf')


## ------------------------------
## TABLE FROM EGO IGRAPH OBJECTS
## ------------------------------

create_ego_table <- function(twomode_network, ego_networks, univs, race_var = 'pct_blacklatinxnative_cat', ranking_var = 'rank_cat2') {
  
  ego_tbl <- data.frame(univ_id = character(0), univ_name = character(0), cluster = character(0), univ_type = character(0), univ_ranking = character(0), characteristics = character(0),
                        region_1 = character(0), region_2 = character(0), region_3 = character(0), region_4 = character(0),
                        religion_1 = character(0), religion_2 = character(0), religion_3 = character(0), religion_4 = character(0),
                        race_1 = character(0), race_2 = character(0), race_3 = character(0), race_4 = character(0),
                        ranking_1 = character(0), ranking_2 = character(0), ranking_3 = character(0), ranking_4 = character(0),
                        stringsAsFactors=FALSE)
  
  member <- membership(cluster_fast_greedy(twomode_network))
  
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
  
  names(ego_tbl) <- c('ID', 'University', 'Cluster', 'Type', 'Rank', 'Characteristics',
                      'Northeast', 'Midwest', 'South', 'West',
                      'Catholic', 'Conserv', 'Nonsect', 'Other',
                      race_vals, ranking_vals)
  
  ego_tbl
}

ego_table_privu <- create_ego_table(g_2mode_privu, egos_psi_privu, privu_vec)
saveRDS(ego_table_privu, file = './assets/tables/table_ego_privu.RDS')

ego_table_pubu <- create_ego_table(g_2mode_pubu, egos_psi_pubu, pubu_vec)
saveRDS(ego_table_pubu, file = './assets/tables/table_ego_pubu.RDS')


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
