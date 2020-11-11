library(igraph)
library(tidyverse)

options(max.print=2000)
## -------------
## LOAD OBJECTS
## -------------

# Run create_igraph_objects.R
scripts_dir <- file.path('.', 'scripts')
source(file = file.path(scripts_dir, 'create_igraph_objects.R'))



library(ape)  

#bipartite.projection(g_2mode_privu)[["proj2"]] %>% cluster_fast_greedy() %>% membership()

vertex_attr_names(bipartite.projection(g_2mode)[["proj2"]])

edge_attr_names(bipartite.projection(g_2mode)[["proj2"]])
edge_attr_names(g_1mode_psi)

ecount(bipartite.projection(g_2mode)[["proj2"]])
ecount(g_1mode_psi)


as_adjacency_matrix(
  graph = bipartite.projection(g_2mode)[["proj2"]],  # creates one-mode object w/ mode = universities
  sparse = FALSE, 
  attr = 'weight'
)
affil_1mode_psi %>% str()


E(bipartite.projection(g_2mode)[["proj2"]])$weight
E(g_1mode_psi)$weight

bipartite.projection(g_2mode)[["proj2"]]

c_obj <- cluster_fast_greedy(
  graph = bipartite.projection(g_2mode)[["proj2"]],
  merges = TRUE,
  modularity = TRUE,
  membership = TRUE,
  weights = E(bipartite.projection(g_2mode)[["proj2"]])$weight
)

c_obj %>% membership()


# investigating communities object
c_2mode <- bipartite.projection(g_2mode)[["proj2"]] %>% cluster_fast_greedy() 
c_2mode %>% membership() %>% table()
c_2mode %>% str()

c_2mode$merges %>% str()
c_2mode$modularity %>% str()

c_2mode %>% str()
c_2mode %>% class()

c_2mode %>% cut_at(steps = 41) %>% table()

bipartite.projection(g_2mode_pubu)[["proj2"]] %>% cluster_fast_greedy() %>% cut_at(steps = 16) %>% table()

cut_2mode <- c_2mode %>% cut_at(no = 2)
cut_2mode %>% str()


bipartite.projection(g_2mode)[["proj2"]] %>% cluster_fast_greedy() %>% cut_at(no =4) %>% table()
g_1mode_psi %>% cluster_fast_greedy() %>% cut_at(no =4) %>% table()

bipartite.projection(g_2mode_privu)[["proj2"]] %>% cluster_fast_greedy() %>% cut_at(no = 2) %>% table()
g_1mode_psi_privu %>% cluster_fast_greedy() %>% cut_at(no =2) %>% table()


bipartite.projection(g_2mode_pubu)[["proj2"]] %>% cluster_fast_greedy() %>% cut_at(no = 2) %>% table()
g_1mode_psi_pubu %>% cluster_fast_greedy() %>% cut_at(no =2) %>% table()


# one-mode = high school
bipartite.projection(g_2mode)[["proj1"]] %>% cluster_fast_greedy() %>% membership() %>% table()
g_1mode_hs %>% cluster_fast_greedy() %>% membership() %>% table()

bipartite.projection(g_2mode)[["proj1"]] %>% cluster_fast_greedy() %>% cut_at(no =4) %>% table()
bipartite.projection(g_2mode)[["proj1"]] %>% cluster_fast_greedy() %>% cut_at(no =3) %>% table()
bipartite.projection(g_2mode)[["proj1"]] %>% cluster_fast_greedy() %>% cut_at(no =5) %>% table()


bipartite.projection(g_2mode_privu)[["proj1"]] %>% cluster_fast_greedy() %>% membership() %>% table()
bipartite.projection(g_2mode_pubu)[["proj1"]] %>% cluster_fast_greedy() %>% membership() %>% table()

# 
# dendogram

d_2mode <- bipartite.projection(g_2mode)[["proj2"]] %>% cluster_fast_greedy() %>% as.dendrogram(hang = -1, use.modularity = FALSE)
d_2mode
d_2mode %>% summary()
d_2mode %>% str()

# graph dendogram by applying dendPlot() function to community object

bipartite.projection(g_2mode)[["proj2"]] %>% cluster_fast_greedy() %>% dendPlot()
bipartite.projection(g_2mode)[["proj2"]] %>% cluster_fast_greedy() %>% dendPlot(mode = "phylo")
bipartite.projection(g_2mode)[["proj2"]] %>% cluster_fast_greedy() %>% dendPlot(mode = "hclust")
bipartite.projection(g_2mode)[["proj2"]] %>% cluster_fast_greedy() %>% dendPlot(mode = "dendrogram")
bipartite.projection(g_2mode)[["proj2"]] %>% cluster_fast_greedy() %>% dendPlot(mode = "auto")


    
# graph dendogram by creating dendogram object and applying plot to dendogram object
bipartite.projection(g_2mode)[["proj2"]] %>% cluster_fast_greedy() %>% as.dendrogram(hang = -1, use.modularity = FALSE) %>% plot()
    
