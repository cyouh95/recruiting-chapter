library(igraph)
library(tidyverse)

options(max.print=2000)
## -------------
## LOAD OBJECTS
## -------------

# Run create_igraph_objects.R
scripts_dir <- file.path('.', 'scripts')
source(file = file.path(scripts_dir, 'create_igraph_objects.R'))



## ------------------------------
## PLAYING WITH COMMUNITY DETECTION
## ------------------------------

g_2mode

# function igraph::as_incidence_matrix "This function can return a sparse or dense incidence matrix of a bipartite network. The incidence matrix is an n times m matrix, n and m are the number of vertices of the two kinds."
as_incidence_matrix(
  graph = g_2mode,
  types = NULL,
  attr = NULL,
  names = TRUE,
  sparse = FALSE
) %>% str()

# creates data frame where each observation is a college/university and each variable is a high school ID
df_temp_univ <- as_incidence_matrix(
  graph = g_2mode,
  types = NULL,
  attr = NULL,
  names = TRUE,
  sparse = FALSE
) %>% t() %>% as.data.frame(
  row.names = NULL, 
  optional = FALSE,
  make.names = TRUE,
  stringsAsFactors = FALSE
)

df_temp_univ %>% dim() # 43 obs, 1742 variables

# try performing principal component analysis on object
pca_univ <- prcomp(
  x = df_temp_univ, 
  retx = TRUE, # a logical value indicating whether the rotated variables should be returned. default TRUE
  center = TRUE, # a logical value indicating whether the variables should be shifted to be zero centered. default TRUE. Alternately, a vector of length equal the number of columns of x can be supplied. The value is passed to scale.
  scale. = TRUE, # a logical value indicating whether the variables should be scaled to have unit variance before the analysis takes place. The default is FALSE for consistency with S, but in general scaling is advisable
  tol = NULL, # a value indicating the magnitude below which components should be omitted. (Components are omitted if their standard deviations are less than or equal to tol times the standard deviation of the first component.) With the default null setting, no components are omitted (unless rank. is specified less than min(dim(x)).)
  rank. = NULL # default = NULL # optionally, a number specifying the maximal rank, i.e., maximal number of principal components to be used. Can be set as alternative or in addition to tol, useful notably when the desired rank is considerably smaller than the dimensions of the matrix.
) 

summary(pca_univ)



# creates data frame where each observation is a high school and each variable is a university ID
df_temp %>% glimpse()
df_temp <- as_incidence_matrix(
  graph = g_2mode,
  types = NULL,
  attr = NULL,
  names = TRUE,
  sparse = FALSE
) %>% as.data.frame(
  row.names = NULL, 
  optional = FALSE,
  make.names = TRUE,
  stringsAsFactors = FALSE
) 

df_temp %>% select(`100751`,`106397`)


# 
#bipartite.projection(g_2mode)[["proj1"]] # one mode object, mode = high schools
#bipartite.projection(g_2mode)[["proj2"]] # one mode object, mode = universities


#create adjacency matrix of 1-mode university object
  # create adacency matrix
  # convert matrix to data frame. ? necessary?
  #rm(df_1mode_psi)
affil_1mode_psi <- as_adjacency_matrix(
  graph = bipartite.projection(g_2mode)[["proj2"]],  # creates one-mode object w/ mode = universities
  sparse = FALSE, 
  attr = 'weight'
)
affil_1mode_psi %>% str()

# create "distance matrix" using stats::dist() function
  # what is distance matrix?
    # https://online.stat.psu.edu/stat555/node/86/#:~:text=Clustering%20starts%20by%20computing%20a,is%20distance%20zero%20from%20itself).
      # Clustering starts by computing a distance between every pair of units that you want to cluster.  A distance matrix will be symmetric (because the distance between x and y is the same as the distance between y and x) and will have zeroes on the diagonal (because every item is distance zero from itself).  The table below is an example of a distance matrix.  Only the lower triangle is shown, because the upper triangle can be filled in by reflection.
dist_1mode_psi <- dist(
  x = affil_1mode_psi , # 	a numeric matrix, data frame or "dist" object.
  method = "euclidean", # the distance measure to be used. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski". default = euclidian where Usual distance between the two vectors (2 norm aka L_2), sqrt(sum((x_i - y_i)^2)).
  diag = TRUE, # FALSE, # logical value indicating whether the diagonal of the distance matrix should be printed by print.dist. default = FALSE
  upper = TRUE, # logical value indicating whether the upper triangle of the distance matrix should be printed by print.dist.
  p = 2 # The power of the Minkowski distance. default = 2
)
dist_1mode_psi %>% str()
# conduct hierarchical clustering using stats::hclust
  # https://www.datacamp.com/community/tutorials/hierarchical-clustering-R
m <- "complete" # "average" "complete"
hclust_1mode_psi <- hclust(
  d = dist_1mode_psi, # a dissimilarity structure as produced by dist() function
  method = m, # default = complete; the agglomeration method to be used. This should be (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
  members = NULL # NULL or a vector with length size of d. See the ‘Details’ section. default = NUKK
)
hclust_1mode_psi %>% class()
hclust_1mode_psi %>% str()

hclust_1mode_psi %>% as.dendrogram() %>% plot(horiz = TRUE)



hclust_1mode_psi %>% as.() %>%   dendPlot(mode = "auto")
  as.dendrogram(hang = -1, use.modularity = FALSE) %>%  dendPlot(mode = "auto")
# cut the dendrogram to create the desired number of clusters using stats::cutree
  # https://www.datacamp.com/community/tutorials/hierarchical-clustering-R
  # note: At least one of k or h must be specified, k overrides h if both are given.
  # choosing number of clusters by specifying k
  # choosing number of clusters by specifiying height
    # how is "height" defined
      # 
    # from http://ccicada.org/wp-content/uploads/2017/06/Community-Detection-with-Hierarchical-Clustering-Algorithms-Feb-3-2017.pdf 
    # Note. One way to subdivide a network into clusters is to choose a height along the y-axis of the dendrogram, and draw a horizontal line at that height (see the red line in the above dendrogram), and count how many vertical lines it crosses. Each vertical line corresponds to a grouping of clusters, and the members of that grouping are represented by the branches of the dendrogram that lie below the line

cut_1mode_psi <- cutree(
  tree = hclust_1mode_psi, # a tree as produced by hclust. cutree() only expects a list with components merge, height, and labels, of appropriate content each.
  #k = 3, # NULL, # default = NULL an integer scalar or vector with the desired number of groups
  h = 600 # default = NULL numeric scalar or vector with heights where the tree should be cut
)
cut_1mode_psi
cut_1mode_psi %>% str() # this is just a named atomic vector



df_cut_1mode_psi <- tibble(
  univ_id = names(cut_1mode_psi),
  cluster = cut_1mode_psi
)

vertex_attr_names(g_2mode)
g_2mode <- delete_vertex_attr(g_2mode, "temp")
vertex_attr(graph = g_2mode, name = "temp") <- (as.data.frame(V(g_2mode)$name) %>% left_join(y = df_cut_1mode_psi, by = c(`V(g_2mode)$name` = "univ_id")))$cluster
# seems like the problem that vertex attribute is a list rather than an atomic vector is not an issue. if problem arises again, see this:
  # https://stackoverflow.com/questions/49728990/how-can-i-add-a-data-frame-as-vertex-attributes-with-matching-ids-in-igraph

vertex_attr(graph = g_2mode, name = "temp")
V(g_2mode)$temp
V(g_2mode)$type




V(g_2mode)$name[V(g_2mode)$type == TRUE]

(as.data.frame(V(g_2mode)$name) %>% left_join(y = df_cut_1mode_psi, by = c(`V(g_2mode)$name` = "univ_id")))$cluster %>% str()

vertex_attr(graph = g_2mode, name = "temp") %>% as.numeric()

# a different approach, but this one doesn't work either
set_vertex_attr(graph = g_2mode, name = "temp", index = V(g_2mode)[V(g_2mode)$type == TRUE], df_cut_1mode_psi$cluster)

vertex_attr(graph = g_2mode, name = "temp")
vertex_attr(graph = g_2mode, name = "temp") %>% as.numeric()

V(g_2mode) %>% str()
V(g_2mode)[V(g_2mode)$type == TRUE] %>% str()
df_cut_1mode_psi %>% str()




# starting w/ the one-mode igraph objects previously created

df_temp3 <- as_adjacency_matrix(
  graph = g_1mode_psi,  
  sparse = FALSE, 
  attr = 'weight'
) %>% as.data.frame(
  row.names = NULL, 
  optional = FALSE,
  make.names = TRUE,
  stringsAsFactors = FALSE
) 

df_temp2 %>% select(`100751`,`106397`)
df_temp3 %>% select(`100751`,`106397`)


# we want to know which universities are visiting the same high schools;

g_2mode
g_1mode_hs

V(g_1mode_hs
  
  
  
  
################ SOMETHING ELSE

twomode_both <- create_2mode_table(g_2mode)
saveRDS(twomode_both$full_table, file = './assets/tables/table_2mode_both.RDS')
saveRDS(twomode_both$agg_table, file = './assets/tables/table_2mode_agg_both.RDS')
  
full <- twomode_both$full_table

full %>% filter(state_code == "NE") %>% select(name, school_name, religion, pct_blacklatinxnative, ranking_numeric, degree, strength)


events_data %>% left_join(y = univ_df, by = c("univ_id" = "school_id")) %>% filter(event_type == "priv_hs" & school_id == "02161678") %>%
  select(univ_id, school_name)


events_data %>% filter(event_type == "priv_hs" & school_id == "02161678") %>% select(univ_id)

events_data %>% left_join(y = univ_df, by = c("univ_id" = "school_id")) %>% filter(event_type == "priv_hs") %>% group_by(school_name) %>% count() %>% View()

%>% select()
glimpse(events_data)
full %>% glimpse()
 