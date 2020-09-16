################################################################################
##
## [ PROJ ] < Burd recruiting chapter >
## [ FILE ] < network_recruiting_analysis.R >
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


## ---------------------------
## Visits by private colleges and universities to private high schools
## ---------------------------

# objects wi will potentially use for analyses
  g_2mode_privu # 2-mode object, only visits by private colleges and universities
  g_1mode_hs_privu # 1 mode object, nodes = private high schools that received at least one visit from a private college/university
  g_1mode_psi_privu  # 1 mode object, nodes = private colleges/universities
  
  egos_psi %>% length() # ego networks; a list of one element per university
    egos_psi[["139658"]]
  egos_hs %>% length() # ego networks; a list of one element per high school
  
    # 2-mode
    # 1-mode, vertices = high schools
    # 1-mode, vertices = universities
    # ego networks (already created)

