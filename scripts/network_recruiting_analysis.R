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


## ------------------------
## PLOT EGO IGRAPH OBJECTS
## ------------------------

# choosing potential universities to for ego network

vertex_attr_names(g_1mode_psi_privu)
V(g_1mode_psi_privu)$school_name

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

    
### create a function to plot the ego network, order = 1 with

ego_plot <- function(attribute, values, colors, legend) {

  characteristic <- attribute

  #vertex_color_text <- recode(vertex_attr(ego_network_order1, characteristic), `1` = 'blue', `2` = 'purple', `3` = 'red', `4` = 'green')
  
  print(attribute)
 
   
  print(values)

  print(colors)

  # below, want to create the text to supply code for vertex.color
  vertex_color_text <- ""
  for(i in c(1,2,3,4)) {
  
    vertex_color_text <- str_c(vertex_color_text,"vertex_attr(ego_network_order1, characteristic) ==", values[i]," ~ ", "'", colors[i], "'", ",", sep = "", collapse = NULL)
    
  }
  vertex_color_text <- str_c("case_when(",vertex_color_text,")", sep = "", collapse = NULL)
  print(vertex_color_text)
}

ego_plot(attribute = "region", values = c(1,2,3,4), colors = c('blue','purple', 'red','green'), legend = c("Northeast","Midwest","South","West"))

values <- c(1,2,3,4)
colors <- c('blue','purple', 'red','green')
legend <- c("Northeast","Midwest","South","West")


vertex_color_text <- ""
for(i in c(1,2,3,4)) {

  vertex_color_text <- str_c(vertex_color_text,"vertex_attr(ego_network_order1, characteristic) ==", values[i]," ~ ", "'", colors[i], "'", ",", sep = "", collapse = NULL)
  
}
vertex_color_text <- str_c("case_when(",vertex_color_text,")", sep = "", collapse = NULL)
print(vertex_color_text)

case_when(vertex_attr(ego_network_order1, characteristic) ==1 ~ 'blue',vertex_attr(ego_network_order1, characteristic) ==2 ~ 'purple',vertex_attr(ego_network_order1, characteristic) ==3 ~ 'red',vertex_attr(ego_network_order1, characteristic) ==4 ~ 'green',)

case_when(vertex_attr(ego_network_order1, characteristic) ==1 ~ 'blue',vertex_attr(ego_network_order1, characteristic) ==2 ~ 'purple',vertex_attr(ego_network_order1, characteristic) ==3 ~ 'red',vertex_attr(ego_network_order1, characteristic) ==4 ~ 'green',)

recode(vertex_attr(ego_network_order1, characteristic), `1` = 'blue', `2` = 'purple', `3` = 'red', `4` = 'green')

case_when(
  vertex_attr(ego_network_order1, characteristic) == 1 ~ "blue",
  vertex_attr(ego_network_order1, characteristic) == 2 ~ "purple",
  vertex_attr(ego_network_order1, characteristic) == 3 ~ "red",
  vertex_attr(ego_network_order1, characteristic) == 4 ~ "green",
)

str_c("vertex_attr(ego_network_order1, characteristic) ==", values[1]," ~ ", colors[1], ",", sep = "", collapse = NULL)

V(ego_network_order1)$characteristic == 1
vertex_attr(ego_network_order1, characteristic) == 1

  plot.igraph(
    x = ego_network_order1,
    vertex.label = if_else(V(ego_network_order1)$type, V(ego_network_order1)$school_name, ""),
    vertex.shape = if_else(V(ego_network_order1)$type, "circle","circle"),
    #vertex.color = recode(vertex_attr(ego_network_order1, characteristic), `1` = 'blue', `2` = 'purple', `3` = 'red', `4` = 'green'),
    vertex.color = vertex_color_text,
    #vertex.color = recode(vertex_attr(ego_network_order1, characteristic), vertex_color_text),
    vertex.size = if_else(V(ego_network_order1)$type, 16, 4),
    edge.color = if_else(E(ego_network_order1)$weight ==1, "lightgrey", "black"),
    edge.lty = if_else(E(ego_network_order1)$weight ==1, 5, 1),
    edge.width = if_else(E(ego_network_order1)$weight ==1, .5, as.numeric(E(ego_network_order1)$weight)^2),
    layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle, layout_nicely
    main = paste('Ego network for', (univ_df %>% filter(school_id == univ_id))$school_name),
    margin = -0.2 # -0.2
  )  
  
  legend( # based on vertex color; should change this to vertex shape
    x = 1,
    y = -.5,
    #"bottomright", # placement of legend
    legend = c("Northeast","Midwest","South","West"), # legend text
    fill = c("blue","purple","red","green"), # legend color fill
    bty = "n" # box drawn around legend; "o" [default] = solid line box; "n" = no box
  )

}

ego_plot(v_attr = "region")


### plot ego network, order = 1, characteristic = religious affiliation
characteristic <- 'religion'

proportions(table(V(ego_network_order1)$religion, useNA = "always"))

vertex_color_text <- recode(vertex_attr(ego_network_order1, characteristic), 'catholic' = 'blue', 'conservative_christian' = 'purple', 'nonsectarian' = 'red', 'other_religion' = 'green'),
plot.igraph(
  x = ego_network_order1,
  vertex.label = if_else(V(ego_network_order1)$type, V(ego_network_order1)$school_name, ""),
  vertex.shape = if_else(V(ego_network_order1)$type, "circle","circle"),
  #vertex.color = if_else(vertex_attr(ego_network_order1, characteristic) == univ_characteristic, 'lightblue', 'lightgrey'),  
  #vertex.color = recode(vertex_attr(ego_network_order1, characteristic), 'catholic' = 'blue', 'conservative_christian' = 'purple', 'nonsectarian' = 'red', 'other_religion' = 'green'),
  vertex.color = vertex_col
  #vertex.color = recode(vertex_attr(ego_network_order1, characteristic), vertex_color_text),
  vertex.size = if_else(V(ego_network_order1)$type, 16, 4),
  edge.color = if_else(E(ego_network_order1)$weight ==1, "lightgrey", "black"),
  edge.lty = if_else(E(ego_network_order1)$weight ==1, 5, 1),
  edge.width = if_else(E(ego_network_order1)$weight ==1, .5, as.numeric(E(ego_network_order1)$weight)^2),
  layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle, layout_nicely
  main = paste('Ego network for', (univ_df %>% filter(school_id == univ_id))$school_name),
  margin = -0.2 # -0.2
)
legend( # based on vertex color; should change this to vertex shape
  x = 1,
  y = -.5,
  #"bottomright", # placement of legend
  legend = c("Catholic","Conservative Christian","Nonsetarian","Other religion"), # legend text
  fill = c("blue","purple","red","green"), # legend color fill
  bty = "n" # box drawn around legend; "o" [default] = solid line box; "n" = no box
)


### plot ego network, order = 1, characteristic = region

    
    
characteristic <- 'region'
univ_characteristic <- (univ_df %>% filter(school_id == univ_id))[[characteristic]]
univ_characteristic
str(univ_characteristic)

vertex_color_text <- "`1` = 'blue', `2` = 'purple', `3` = 'red', `4` = 'green'"
vertex_color_text
writeLines(vertex_color_text)

vertex_color_text <- recode(vertex_attr(ego_network_order1, characteristic), `1` = 'blue', `2` = 'purple', `3` = 'red', `4` = 'green')
vertex_color_text
recode(vertex_attr(ego_network_order1, characteristic), vertex_color_text)

plot.igraph(
  x = ego_network_order1,
  vertex.label = if_else(V(ego_network_order1)$type, V(ego_network_order1)$school_name, ""),
  vertex.shape = if_else(V(ego_network_order1)$type, "circle","circle"),
  #vertex.color = if_else(vertex_attr(ego_network_order1, characteristic) == univ_characteristic, 'lightblue', 'lightgrey'),  
  #vertex.color = recode(vertex_attr(ego_network_order1, characteristic), `1` = 'blue', `2` = 'purple', `3` = 'red', `4` = 'green'),
  vertex.color = vertex_color_text,
  #vertex.color = recode(vertex_attr(ego_network_order1, characteristic), vertex_color_text),
  vertex.size = if_else(V(ego_network_order1)$type, 16, 4),
  edge.color = if_else(E(ego_network_order1)$weight ==1, "lightgrey", "black"),
  edge.lty = if_else(E(ego_network_order1)$weight ==1, 5, 1),
  edge.width = if_else(E(ego_network_order1)$weight ==1, .5, as.numeric(E(ego_network_order1)$weight)^2),
  layout = layout_nicely, # layout_with_kk, # layout = layout_in_circle, layout_nicely
  main = paste('Ego network for', (univ_df %>% filter(school_id == univ_id))$school_name),
  margin = -0.2 # -0.2
)

legend( # based on vertex color; should change this to vertex shape
  x = 1,
  y = -.5,
  #"bottomright", # placement of legend
  legend = c("Northeast","Midwest","South","West"), # legend text
  fill = c("blue","purple","red","green"), # legend color fill
  bty = "n" # box drawn around legend; "o" [default] = solid line box; "n" = no box
)

# messing around with vertex color based on vertex attribute
  #legend("right", legend = c("Northeast","Midwest","South","West"), fill = c("blue","purple","red","green"))
  #vertex.color = if_else(vertex_attr(ego_network_order1, characteristic) == univ_characteristic, 'lightblue', 'lightgrey'),
  
  #if_else(vertex_attr(ego_network_order1, characteristic) == univ_characteristic, 'lightblue', 'lightgrey')
  #vertex_attr(ego_network_order1, characteristic)
  #vertex_attr(ego_network_order1, characteristic) == univ_characteristic
  #vertex.color = recode(vertex_attr(ego_network_order1, characteristic), `1` = "blue", `2` = "purple", `3` = "red", `4` = "green")

# NOTE TO CRYSTAL:
  # currently using vertex color to display vertex attribute (region, 4 categories)
  # can you replace this w/ using vertex shape; so we would need four shapes
    # let's go with: triangle, circle, square, and .... maybe diamond? 





# plot: order 1 (high school) and order 2 (universities that visited those high schools)



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
  margin = -0.3
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
  city = V(g_2mode_privu)$city,
  state = V(g_2mode_privu)$state_code,
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
