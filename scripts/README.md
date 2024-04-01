# Scripts

This directory contains scripts used to generate cleaned datasets, create analysis results, and more. Each script is described in more detail below.


**Generate cleaned datasets**

The following scripts use raw data downloaded in [`data/`](https://github.com/cyouh95/recruiting-chapter/tree/master/data) to generate cleaned datasets saved in the same directory:

- `ipeds_data_saving.R`
- `ccd_data_saving.R`
- `pss_data_saving.R`
- `acs_collect_via_API_tract.py`


**Create analysis results**

Scripts to replicate the analyses should be called in the following order:

1. `create_igraph_objects.R`
1. `network_recruiting_analysis.R`
1. `create_figures.R`

The tables and figures created from these scripts are saved in [`assets/tables/`](https://github.com/cyouh95/recruiting-chapter/tree/master/assets/tables) and [`assets/figures/`](https://github.com/cyouh95/recruiting-chapter/tree/master/assets/figures), respectively.


**Miscellaneous scripts**

`vignette_lede_rihe.R` is used to print out statistics used in the opening vignette of the [manuscript](https://github.com/cyouh95/recruiting-chapter?tab=readme-ov-file#manuscript). 

`intro_map.R` is used to create map (saved in [`assets/maps/`](https://github.com/cyouh95/recruiting-chapter/tree/master/assets/maps)) to accompany the opening vignette, as presented in the [slideshow](https://github.com/cyouh95/recruiting-chapter?tab=readme-ov-file#slideshow).
