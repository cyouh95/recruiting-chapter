library(tidyverse)
library(gridExtra)


data_dir <- file.path('.', 'data')
tables_dir <- file.path('.', 'assets', 'tables')
figures_dir <- file.path('.', 'assets', 'figures')

region_values <- c('northeast', 'midwest', 'south', 'west')
region_keys <- c('Northeast', 'Midwest', 'South', 'West')
region_title <- 'Region'
region_var <- 'region'

religion_values <- c('catholic', 'christian', 'nonsectarian', 'other')
religion_keys <- c('Catholic', 'Christian', 'Nonsectarian', 'Other')
religion_title <- 'Religion'
religion_var <- 'religion'

race_values <- c('c1_lt10', 'c2_10to20', 'c3_20to50', 'c4_50+')
race_keys <- c('LT 10%', '10-20%', '20-50%', 'GT 50%')
race_title <- '% Black/Latinx/Native'
race_var <- 'pct_blacklatinxnative_cat'

ranking_values <- c('c1_top200', 'c2_A+', 'c3_A', 'c4_ltA')
ranking_keys <- c('Rank top 200', 'A+', 'A', 'A- or below')
ranking_title <- 'Ranking'
ranking_var <- 'rank_cat2'

enroll_values <- c('c1_lt50', 'c2_50to100', 'c3_100to150', 'c4_gt150')
enroll_keys <- c('LT 50', '50-100', '100-150', 'GT 150')
enroll_title <- '12th grade enrollment'
enroll_var <- 'enroll_cat1'

var_names <- c(region_var, religion_var, race_var, ranking_var, enroll_var)
var_labels <- c(region_title, religion_title, race_title, ranking_title, enroll_title)
var_values <- list(region_values, religion_values, race_values, ranking_values, enroll_values)
var_keys <- list(region_keys, religion_keys, race_keys, ranking_keys, enroll_keys)


## ------------------------------
## PLOT USING EGO IGRAPH OBJECTS
## ------------------------------

universe_df <- readRDS(file.path(data_dir, 'privhs_universe.RDS'))
visited_df <- readRDS(file.path(data_dir, 'privhs_visited.RDS'))

ego_df <- readRDS(file.path(tables_dir, 'table_ego_all.RDS')) %>% 
  mutate(
    univ_region = str_split(characteristics, '\\|', simplify = T)[,1],
    univ_religion = str_split(characteristics, '\\|', simplify = T)[,2],
    univ_race = round(as.numeric(str_split(characteristics, '\\|', simplify = T)[,3]), 1),
    univ_rank = as.numeric(str_split(characteristics, '\\|', simplify = T)[,4]),
    univ_enroll = format(as.numeric(str_split(characteristics, '\\|', simplify = T)[,5]), big.mark = ',')
  )


plot_characteristic <- function(plot_type, var_name, label, characteristic, label_text, control = 'public', type = 'univ') {
  
  sub_df_visited <- visited_df %>% group_by(get(var_name)) %>% 
    summarize(`Private HS, 1+ visit` = n()) %>% 
    pivot_longer(cols = 'Private HS, 1+ visit',
                 names_to = 'universe',
                 values_to = 'value')
  names(sub_df_visited) <- c('key', 'univ_name', 'value')
  
  sub_df_universe <- universe_df %>% group_by(get(var_name)) %>% 
    summarize(`Private HS, universe` = n()) %>% 
    pivot_longer(cols = 'Private HS, universe',
                 names_to = 'universe',
                 values_to = 'value')
  names(sub_df_universe) <- c('key', 'univ_name', 'value')
  
  sub_ego_df <- ego_df %>% filter(control == !!control, type == !!type) %>% 
    mutate(univ_name = str_c(univ_name, ' (', get(str_c('univ_', plot_type)), ')'))
  
  sub_df_univs <- sub_ego_df %>% 
    select(univ_name, all_of(characteristic)) %>%
    gather(key, value, characteristic) %>%
    mutate(value = parse_number(value)) %>% 
    select(key, univ_name, value)
  
  sub_df <- bind_rows(sub_df_universe, sub_df_visited, sub_df_univs) %>% filter(!is.na(key))
  
  # sort univs first by characteristic if region or religion, then by ranking
  if (plot_type %in% c('region', 'religion')) {
    univ_order <- (sub_ego_df[order(match(sub_ego_df[[str_c('univ_', plot_type)]], characteristic), sub_ego_df$univ_rank), ])$univ_name
  } else {
    univ_order <- (sub_ego_df[order(sub_ego_df$univ_rank), ])$univ_name
  }
  
  sub_df$univ_name <- factor(sub_df$univ_name, levels = rev(c('Private HS, universe', 'Private HS, 1+ visit', univ_order)))
  sub_df$key <- factor(sub_df$key, levels = characteristic)
  
  ggplot(sub_df, aes(fill = key, y = value, x = univ_name)) + 
    geom_bar(stat='identity', position = position_fill(reverse = TRUE), width = 0.7) +
    ggtitle('Title') +
    xlab('') + ylab('') +
    labs(fill = label) +
    scale_fill_manual(labels = label_text, values = c('#F8766D', '#7CAE00', '#00BFC4', '#C77CFF')) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    scale_y_continuous(labels = scales::percent, expand = c(0, 0, 0.05, 0)) +
    theme(legend.position = 'top',
          legend.title = element_text(size = 6, face = 'bold'),
          legend.text = element_text(size = 6),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(0, 0, -5, -30),
          legend.key.size = unit(0.3, 'cm'),
          panel.background = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 8),
          plot.title = element_text(color = 'white')) +
    coord_flip()
}


plot_types <- c('region', 'religion', 'race', 'rank', 'enroll')

for (i in seq_along(plot_types)) {
  a <- plot_characteristic(plot_types[[i]], var_names[[i]], var_labels[[i]], var_values[[i]], var_keys[[i]])
  b <- plot_characteristic(plot_types[[i]], var_names[[i]], var_labels[[i]], var_values[[i]], var_keys[[i]], control = 'private', type = 'univ')
  c <- plot_characteristic(plot_types[[i]], var_names[[i]], var_labels[[i]], var_values[[i]], var_keys[[i]], control = 'private', type = 'lib arts')
  
  # pubu + privu + privc
  pdf(file.path(figures_dir, str_c('ego_network_', plot_types[[i]], '_pubu_privu_privc.pdf')))
  par(mar=c(0, 0, 0, 0) + 0.1, mai = c(0, 0, 0, 0))
  grid.arrange(a, b, c, nrow = 3, ncol = 1)
  dev.off()
  
  # pubu + privu
  pdf(file.path(figures_dir, str_c('ego_network_', plot_types[[i]], '_pubu_privu.pdf')))
  par(mar = c(0, 0, 0, 0) + 0.1, mai = c(0, 0, 0, 0))
  grid.arrange(a, b, nrow = 2, ncol = 1)
  dev.off()
}
