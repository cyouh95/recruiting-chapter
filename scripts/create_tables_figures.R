library(tidyverse)
library(gridExtra)


# File paths
data_dir <- file.path('.', 'data')
tables_dir <- file.path('.', 'assets', 'tables')
figures_dir <- file.path('.', 'assets', 'figures')

# Characteristic variables
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

# Load data
univ_sample_df <- readRDS(file.path(data_dir, 'univ_sample.RDS')) %>% arrange(desc(classification), rank)

univ_public_research <- (univ_sample_df %>% filter(classification == 'public_research'))$univ_abbrev
univ_private_national <- (univ_sample_df %>% filter(classification == 'private_national'))$univ_abbrev
univ_private_libarts <- (univ_sample_df %>% filter(classification == 'private_libarts'))$univ_abbrev

events_df <- readRDS(file.path(data_dir, 'events_data.RDS'))

pubhs_universe_df <- readRDS(file.path(data_dir, 'pubhs_universe.RDS'))
pubhs_visited_df <- pubhs_universe_df %>% filter(ncessch %in% events_df$school_id)

privhs_universe_df <- readRDS(file.path(data_dir, 'privhs_universe.RDS'))
privhs_visited_df <- readRDS(file.path(data_dir, 'privhs_visited.RDS'))

ego_df <- readRDS(file.path(tables_dir, 'table_ego_all.RDS')) %>% 
  mutate(
    univ_region = str_split(characteristics, '\\|', simplify = T)[,1],
    univ_religion = str_split(characteristics, '\\|', simplify = T)[,2],
    univ_race = round(as.numeric(str_split(characteristics, '\\|', simplify = T)[,3]), 1),
    univ_rank = as.numeric(str_split(characteristics, '\\|', simplify = T)[,4]),
    univ_enroll = format(as.numeric(str_split(characteristics, '\\|', simplify = T)[,5]), big.mark = ',')
  )

color_palette <- c('#d1b38e', '#8eb9d1', '#b48ed1', '#afd18e')


## -------------------
## SAVE PLOT FUNCTION
## -------------------

# FUNCTION: save_plot(graph_object, plot_name, <paper>, <mar>, <mai>)
# graph_object: call function that plots the graph
# plot_name: name of file with extension
# paper: paper type (default: a4r)
# mar: margin in lines (default: c(0, 0, 0, 0))
# mai: margin in inches (default: c(0, 0, 0, 0))

save_plot <- function(graph_object, plot_name, paper = 'a4r', mar = c(0, 0, 0, 0), mai = c(0, 0, 0, 0)) {
  
  pdf(file.path(figures_dir, plot_name), paper = paper)
  par(mfrow = c(1, 1))
  par(mar = mar + 0.1, mai = mai)
  
  print(graph_object)
  
  dev.off()
}


## --------------------------------------
## NUMBER OF EVENTS BY TYPE AND LOCATION
## --------------------------------------

events_count <- events_df %>%
  mutate(event_type = case_when(
    event_type %in% c('pub_hs', 'priv_hs') ~ event_type,
    event_type == 'cc' & event_loc == 'instate' ~ event_type,
    TRUE ~ 'other'
  )) %>%
  select(univ_abbrev, event_loc, event_type) %>% 
  group_by(univ_abbrev, event_loc, event_type) %>% 
  summarise(count = n()) %>% 
  ungroup()

# View(events_count[order(match(events_count$univ_abbrev, univ_sample_df$univ_abbrev)), ])

events_count$event_type <- factor(events_count$event_type, levels = c('pub_hs', 'priv_hs', 'cc', 'other'))
events_count$univ_abbrev <- factor(events_count$univ_abbrev, levels = rev(univ_sample_df$univ_abbrev))

plot_event_count <- function(univ_sample, in_lim = 0, out_lim = 0, hjust = 0.1, text_offset = 15) {
  sub_events_count <- events_count %>% filter(univ_abbrev %in% univ_sample) %>% 
    mutate(
      event_type = recode_factor(
        event_type,
        'pub_hs' = 'Public HS',
        'priv_hs' = 'Private HS',
        'cc' = 'Community College',
        'other' = 'Other'
      )
    )
  
  ggplot(sub_events_count, aes(univ_abbrev)) +
    geom_bar(data = subset(sub_events_count, event_loc == 'instate'), aes(y = -count, fill = event_type), stat = 'identity', position = position_stack(reverse = T), width = 0.5) +
    geom_bar(data = subset(sub_events_count, event_loc == 'outofstate'), aes(y = count, fill = event_type), stat = 'identity', position = position_stack(reverse = T), width = 0.5) +
    geom_hline(yintercept = 0, colour = 'grey90', linetype = 'dotted') +
    geom_text(data = subset(sub_events_count, event_loc == 'instate'), aes(y = -count, label = format(abs(stat(y)), big.mark = ',')), stat = 'summary', fun = sum, hjust = 1, nudge_y = -text_offset, size = 3) +
    geom_text(data = subset(sub_events_count, event_loc == 'outofstate'), aes(y = count, label = ifelse(abs(stat(y)) >= 1000, format(abs(stat(y)), big.mark = ','), str_pad(abs(stat(y)), 4, 'right'))), stat = 'summary', fun = sum, hjust = 0, nudge_y = text_offset, size = 3) +
    xlab('') + ylab('') + 
    expand_limits(y = c(-in_lim, out_lim)) +
    scale_fill_manual(values = color_palette, name = 'Event type') +
    theme(
      panel.background = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      plot.subtitle = element_text(hjust = hjust, size = 10)
    ) +
    labs(subtitle = 'In-state                   Out-of-state') +
    coord_flip()
}


save_plot(plot_event_count(univ_public_research, in_lim = 1500, out_lim = 4340, hjust = 0.17, text_offset = 40), 'events_count_pubu.pdf')
save_plot(plot_event_count(univ_private_national, in_lim = 600, out_lim = 1350, hjust = 0.23), 'events_count_privu.pdf')
save_plot(plot_event_count(univ_private_libarts, in_lim = 250, out_lim = 900, text_offset = 10), 'events_count_privc.pdf')


## --------------------------------------
## NUMBER OF PUBLIC VS PRIVATE HS VISITS
## --------------------------------------

plot_hs_count <- function(univ_sample, in_lim = 0, out_lim = 0, vjust = 1.5, text_offset = 10) {
  sub_events_count <- events_count %>% filter(univ_abbrev %in% univ_sample)
  sub_events_count$univ_abbrev <- factor(sub_events_count$univ_abbrev, levels = rev(univ_sample))
  
  sub_events_privhs <- subset(sub_events_count, event_type == 'priv_hs')
  sub_events_pubhs <- subset(sub_events_count, event_type == 'pub_hs')
  
  sub_events_privhs_pct <- sub_events_privhs %>%
    pivot_wider(names_from = event_loc, values_from = count) %>% 
    mutate(
      tot_hs = instate + outofstate,
      pct_outofstate = round(outofstate / tot_hs * 100),
      label_text_cnt = tot_hs,
      label_text_pct = str_c(pct_outofstate, '% out-of-state')
    )
  
  sub_events_pubhs_pct <- sub_events_pubhs %>%
    pivot_wider(names_from = event_loc, values_from = count) %>% 
    mutate(
      tot_hs = instate + outofstate,
      pct_outofstate = round(outofstate / tot_hs * 100),
      label_text_cnt = tot_hs,
      label_text_pct = str_c(pct_outofstate, '% out-of-state')
    )
  
  sub_events_privhs <- sub_events_privhs %>% mutate(event_loc = recode(
    event_loc,
    'instate' = 'In-state',
    'outofstate' = 'Out-of-state'
  ))
  
  sub_events_pubhs <- sub_events_pubhs %>% mutate(event_loc = recode(
    event_loc,
    'instate' = 'In-state',
    'outofstate' = 'Out-of-state'
  ))
  
  ggplot() +
    geom_bar(data = sub_events_privhs,
             mapping = aes(x = univ_abbrev, y = count, fill = event_loc), 
             stat = 'identity', 
             position = 'stack', 
             width = 0.35) +
    geom_text(data = sub_events_privhs_pct,
              aes(x = univ_abbrev, y = in_lim, label = str_c('Private HS (N=', label_text_cnt, ')')),
              hjust = 0, size = 2.5) +
    geom_text(data = sub_events_privhs_pct,
              aes(x = univ_abbrev, y = tot_hs + text_offset, label = label_text_pct),
              hjust = 0, size = 2.5) +
    geom_bar(data = sub_events_pubhs,
             mapping = aes(x = as.numeric(univ_abbrev) - 0.4, y = count, fill = event_loc), 
             stat = 'identity', 
             position = 'stack', 
             width = 0.35) +
    geom_text(data = sub_events_pubhs_pct,
              aes(x = as.numeric(univ_abbrev) - 0.4, y = in_lim, label = str_c('Public HS (N=', label_text_cnt, ')')),
              hjust = 0, size = 2.5) +
    geom_text(data = sub_events_pubhs_pct,
              aes(x = as.numeric(univ_abbrev) - 0.4, y = tot_hs + text_offset, label = label_text_pct),
              hjust = 0, size = 2.5) +
    scale_fill_manual(values = color_palette, name = 'Event location') +
    theme(
      panel.background = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(vjust = vjust),
      axis.ticks = element_blank()
    ) +
    xlab('') + ylab('') + 
    expand_limits(y = c(in_lim, out_lim)) +
    coord_flip()
}


save_plot(plot_hs_count(univ_public_research, in_lim = -1050, out_lim = 3300, vjust = 1.2, text_offset = 20), 'events_hs_count_pubu.pdf')
save_plot(plot_hs_count(univ_private_national, in_lim = -300, out_lim = 930, text_offset = 6), 'events_hs_count_privu.pdf')
save_plot(plot_hs_count(univ_private_libarts, in_lim = -200, out_lim = 630, vjust = 1.6, text_offset = 4), 'events_hs_count_privc.pdf')



# OLD VERSION
plot_hs_count_by_loc <- function(univ_sample, in_lim = 0, out_lim = 0, vjust = 1.5) {
  sub_events_count <- events_count %>% filter(univ_abbrev %in% univ_sample)
  sub_events_count$univ_abbrev <- factor(sub_events_count$univ_abbrev, levels = rev(univ_sample))
  
  sub_events_instate <- subset(sub_events_count, event_loc == 'instate' & event_type %in% c('pub_hs', 'priv_hs'))
  sub_events_outofstate <- subset(sub_events_count, event_loc == 'outofstate' & event_type %in% c('pub_hs', 'priv_hs'))
  
  sub_events_instate_pct <- sub_events_instate %>%
    pivot_wider(names_from = event_type, values_from = count) %>% 
    mutate(
      tot_hs = priv_hs + pub_hs,
      pct_priv = round(priv_hs / tot_hs * 100),
      label_text_cnt = tot_hs,
      label_text_pct = str_c(pct_priv, '% priv')
    )
  
  sub_events_outofstate_pct <- sub_events_outofstate %>%
    pivot_wider(names_from = event_type, values_from = count) %>% 
    mutate(
      tot_hs = priv_hs + pub_hs,
      pct_priv = round(priv_hs / tot_hs * 100),
      label_text_cnt = tot_hs,
      label_text_pct = str_c(pct_priv, '% priv')
    )
  
  ggplot() +
    geom_bar(data = sub_events_instate, 
             mapping = aes(x = univ_abbrev, y = count, fill = event_type), 
             stat = 'identity', 
             position = 'stack', 
             width = 0.35) +
    geom_text(data = sub_events_instate_pct,
              aes(x = univ_abbrev, y = in_lim, label = str_c('In-state (N=', label_text_cnt, ')')),
              hjust = 0, size = 2.5) +
    geom_text(data = sub_events_instate_pct,
              aes(x = univ_abbrev, y = tot_hs + 5, label = label_text_pct),
              hjust = 0, size = 2.5) +
    geom_bar(data = sub_events_outofstate, 
             mapping = aes(x = as.numeric(univ_abbrev) - 0.4, y = count, fill = event_type), 
             stat = 'identity', 
             position = 'stack', 
             width = 0.35) +
    geom_text(data = sub_events_outofstate_pct,
              aes(x = as.numeric(univ_abbrev) - 0.4, y = in_lim, label = str_c('Out-of-state (N=', label_text_cnt, ')')),
              hjust = 0, size = 2.5) +
    geom_text(data = sub_events_outofstate_pct,
              aes(x = as.numeric(univ_abbrev) - 0.4, y = tot_hs + 5, label = label_text_pct),
              hjust = 0, size = 2.5) +
    theme(
      panel.background = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(vjust = vjust),
      axis.ticks = element_blank()
    ) +
    xlab('') + ylab('') + 
    expand_limits(y = c(in_lim, out_lim)) +
    coord_flip()
}


# save_plot(plot_hs_count_by_loc(univ_public_research, in_lim = -1400, out_lim = 3650, vjust = 1.2), 'events_hs_count_pubu.pdf')
# save_plot(plot_hs_count_by_loc(univ_private_national, in_lim = -500, out_lim = 1200), 'events_hs_count_privu.pdf')
# save_plot(plot_hs_count_by_loc(univ_private_libarts, in_lim = -300, out_lim = 850, vjust = 1.6), 'events_hs_count_privc.pdf')


## ------------------------------------------
## ACTUAL VS PROPORTIONAL HIGH SCHOOL VISITS
## ------------------------------------------

table(events_df$univ_id)
pubhs_universe_df %>% nrow()
privhs_universe_df %>% nrow()


plot_actual_proportional <- function(univ_sample, in_lim = 0, out_lim = 0, vjust = 1.5, text_offset = 5) {
  
  sub_df_actual <- data.frame(
    univ_abbrev = character(),
    event_type = character(),
    count = numeric(0),
    stringsAsFactors=FALSE
  )
  
  sub_df_proportional <- data.frame(
    univ_abbrev = character(),
    event_type = character(),
    count = numeric(0),
    stringsAsFactors=FALSE
  )
  
  i <- 1
  
  for (uab in univ_sample) {
    print(uab)
    uid <- (univ_sample_df %>% filter(univ_abbrev == uab))$univ_id
    ust <- (univ_sample_df %>% filter(univ_abbrev == uab))$state_code
    
    visited_pub_hs <- (events_df %>% filter(univ_id == uid, event_type == 'pub_hs'))$school_id
    visited_pub_state <- (pubhs_universe_df %>% filter(ncessch %in% visited_pub_hs))$state_code %>% unique()
    universe_pub_hs <- (pubhs_universe_df %>% filter(state_code %in% visited_pub_state))$ncessch %>% unique()
    
    visited_pub_state_out <- (pubhs_universe_df %>% filter(ncessch %in% visited_pub_hs, state_code != ust))$state_code %>% unique()
    visited_pub_hs_out <- (pubhs_universe_df %>% filter(ncessch %in% visited_pub_hs, state_code != ust))$ncessch %>% unique()
    universe_pub_hs_out <- (pubhs_universe_df %>% filter(state_code %in% visited_pub_state_out))$ncessch %>% unique()
    
    visited_priv_hs <- (events_df %>% filter(univ_id == uid, event_type == 'priv_hs'))$school_id
    visited_priv_state <- (privhs_universe_df %>% filter(school_id %in% visited_priv_hs))$state_code %>% unique()
    universe_priv_hs <- (privhs_universe_df %>% filter(state_code %in% visited_priv_state))$school_id %>% unique()
    
    visited_priv_state_out <- (privhs_universe_df %>% filter(school_id %in% visited_priv_hs, state_code != ust))$state_code %>% unique()
    visited_priv_hs_out <- (privhs_universe_df %>% filter(school_id %in% visited_priv_hs, state_code != ust))$school_id %>% unique()
    universe_priv_hs_out <- (privhs_universe_df %>% filter(state_code %in% visited_priv_state_out))$school_id %>% unique()
    
    visited_hs <- c(visited_pub_hs, visited_priv_hs)
    visited_hs_out <- c(visited_pub_hs_out, visited_priv_hs_out)
    
    universe_hs <- c(universe_pub_hs, universe_priv_hs)
    universe_hs_out <- c(universe_pub_hs_out, universe_priv_hs_out)
    
    sub_df_actual[i, ] <- c(uab, 'priv_hs', length(visited_priv_hs))
    sub_df_actual[i + 1, ] <- c(uab, 'pub_hs', length(visited_pub_hs))
    
    sub_df_proportional[i, ] <- c(uab, 'priv_hs', length(universe_priv_hs) / length(universe_hs) * length(visited_hs))
    sub_df_proportional[i + 1, ] <- c(uab, 'pub_hs', length(universe_pub_hs) / length(universe_hs) * length(visited_hs))
    
    i <- i + 2
    
    # writeLines(str_c('Total HS visited (unique): ', length(visited_hs)))
    # writeLines(str_c('  % private HS (actual): ', round(length(visited_priv_hs) / length(visited_hs) * 100, 1)))
    # writeLines(str_c('  % private HS (proportional): ', round(length(universe_priv_hs) / length(universe_hs) * 100, 1)))
    
    # writeLines(str_c('Total out-of-state HS visited (unique): ', length(visited_hs_out)))
    # writeLines(str_c('  % private HS (actual): ', round(length(visited_priv_hs_out) / length(visited_hs_out) * 100, 1)))
    # writeLines(str_c('  % private HS (proportional): ', round(length(universe_priv_hs_out) / length(universe_hs_out) * 100, 1)))
  }
  
  sub_df_actual$count <- as.numeric(sub_df_actual$count)
  sub_df_proportional$count <- as.numeric(sub_df_proportional$count)
  
  sub_df_actual$univ_abbrev <- factor(sub_df_actual$univ_abbrev, levels = rev(univ_sample))
  sub_df_proportional$univ_abbrev <- factor(sub_df_proportional$univ_abbrev, levels = rev(univ_sample))
  
  sub_df_actual_pct <- sub_df_actual %>%
    pivot_wider(names_from = event_type, values_from = count) %>% 
    mutate(
      tot_hs = priv_hs + pub_hs,
      pct_priv = round(priv_hs / tot_hs * 100),
      label_text_cnt = tot_hs,
      label_text_pct = str_c(pct_priv, '%')
    )
  
  sub_df_proportional_pct <- sub_df_proportional %>%
    pivot_wider(names_from = event_type, values_from = count) %>% 
    mutate(
      tot_hs = priv_hs + pub_hs,
      pct_priv = round(priv_hs / tot_hs * 100),
      label_text_cnt = tot_hs,
      label_text_pct = str_c(pct_priv, '%')
    )
  
  sub_df_actual <- sub_df_actual %>% mutate(event_type = recode_factor(
    event_type,
    'pub_hs' = 'Public HS',
    'priv_hs' = 'Private HS'
  ))
  
  sub_df_proportional <- sub_df_proportional %>% mutate(event_type = recode_factor(
    event_type,
    'pub_hs' = 'Public HS',
    'priv_hs' = 'Private HS'
  ))
  
  ggplot() +
    geom_bar(data = sub_df_actual, 
             mapping = aes(x = univ_abbrev, y = count, fill = event_type), 
             stat = 'identity', 
             position = position_stack(reverse = F), 
             width = 0.35) +
    geom_text(data = sub_df_actual_pct,
              aes(x = univ_abbrev, y = in_lim, label = 'Actual'),
              hjust = 0, size = 2.5) +
    geom_text(data = sub_df_actual_pct,
              aes(x = univ_abbrev, y = priv_hs + text_offset, label = label_text_pct),
              hjust = 0, size = 2.5) +
    geom_text(data = sub_df_actual_pct,
              aes(x = as.numeric(univ_abbrev) - 0.2, y = tot_hs + text_offset, label = str_c('N=', label_text_cnt)),
              hjust = 0, size = 2.5) +
    geom_bar(data = sub_df_proportional, 
             mapping = aes(x = as.numeric(univ_abbrev) - 0.4, y = count, fill = event_type), 
             stat = 'identity', 
             position = position_stack(reverse = F), 
             width = 0.35) +
    geom_text(data = sub_df_proportional_pct,
              aes(x = as.numeric(univ_abbrev) - 0.4, y = in_lim, label = 'Proportional'),
              hjust = 0, size = 2.5) +
    geom_text(data = sub_df_proportional_pct,
              aes(x = as.numeric(univ_abbrev) - 0.4, y = priv_hs + text_offset, label = label_text_pct),
              hjust = 0, size = 2.5) +
    scale_fill_manual(values = color_palette, name = 'School type') +
    theme(
      panel.background = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(vjust = vjust),
      axis.ticks = element_blank()
    ) +
    xlab('') + ylab('') + 
    expand_limits(y = c(in_lim, out_lim)) +
    coord_flip()
}


save_plot(plot_actual_proportional(univ_public_research, in_lim = -700, out_lim = 3850, text_offset = 15), 'events_hs_actual_proportional_pubu.pdf')
save_plot(plot_actual_proportional(univ_private_national, in_lim = -300, out_lim = 1300), 'events_hs_actual_proportional_privu.pdf')
save_plot(plot_actual_proportional(univ_private_libarts, in_lim = -200, out_lim = 900), 'events_hs_actual_proportional_privc.pdf')


## ------------------------------------------------------
## CHARACTERISTICS OF PRIVATE HS VISITED BY UNIVERSITIES
## ------------------------------------------------------

# Ego tables for private HS visits currently show % for unique private HS (does not recount multiple visits to same HS)
# View(events_df %>% filter(event_type == 'priv_hs', univ_id == '152080') %>% left_join(privhs_universe_df, by = 'school_id') %>% group_by(region) %>% summarise(count = n_distinct(school_id)) %>% mutate(pct = count / sum(count)))

unique_visited_schools <- events_df %>% 
  distinct() %>% 
  group_by(univ_id, univ_abbrev, event_type) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = 'event_type',
    values_from = 'count',
    values_fill = 0
  )

plot_characteristic_pubhs <- function(plot_type, var_name, label, characteristic, label_text, control = 'public', type = 'univ', loc = c('instate', 'outofstate')) {
  
  sub_df_visited <- pubhs_visited_df %>% group_by(get(var_name)) %>% 
    summarize(`Public HS, 1+ visit` = n()) %>% 
    pivot_longer(cols = 'Public HS, 1+ visit',
                 names_to = 'universe',
                 values_to = 'value')
  names(sub_df_visited) <- c('key', 'univ_name', 'value')
  
  sub_df_universe <- pubhs_universe_df %>% group_by(get(var_name)) %>% 
    summarize(`Public HS, universe` = n()) %>% 
    pivot_longer(cols = 'Public HS, universe',
                 names_to = 'universe',
                 values_to = 'value')
  names(sub_df_universe) <- c('key', 'univ_name', 'value')
  
  sub_df_universe_weighed <- pubhs_universe_df %>% group_by(get(var_name)) %>% 
    summarize(`Public HS, universe (wt)` = sum(g12, na.rm = T)) %>% 
    pivot_longer(cols = 'Public HS, universe (wt)',
                 names_to = 'universe',
                 values_to = 'value')
  names(sub_df_universe_weighed) <- c('key', 'univ_name', 'value')
  
  sub_ego_df <- ego_df %>% filter(control == !!control, type == !!type) %>% 
    left_join(unique_visited_schools %>% select(univ_id, pub_hs), by = 'univ_id') %>% 
    mutate(univ_name = str_c(univ_name, ' (', get(str_c('univ_', plot_type)), ') [N=', format(pub_hs, big.mark = ','), ']'))
  
  sub_df_univs <- events_df %>%
    filter(event_type == 'pub_hs', event_loc %in% loc) %>%
    right_join(sub_ego_df %>% select(univ_id, univ_name), by = 'univ_id') %>% 
    left_join(pubhs_universe_df, by = c('school_id' = 'ncessch')) %>%
    group_by(get(var_name), univ_name) %>%
    summarise(count = n_distinct(school_id))
  names(sub_df_univs) <- c('key', 'univ_name', 'value')
  
  sub_df <- bind_rows(sub_df_universe, sub_df_universe_weighed, sub_df_univs) %>% filter(!is.na(key))
  
  # sort univs first by characteristic if region or religion, then by ranking
  if (plot_type %in% c('region', 'religion')) {
    univ_order <- (sub_ego_df[order(match(sub_ego_df[[str_c('univ_', plot_type)]], characteristic), sub_ego_df$univ_rank), ])$univ_name
  } else {
    univ_order <- (sub_ego_df[order(sub_ego_df$univ_rank), ])$univ_name
  }
  
  sub_df$univ_name <- factor(sub_df$univ_name, levels = rev(c('Public HS, universe', 'Public HS, universe (wt)', univ_order)))
  sub_df$key <- factor(sub_df$key, levels = characteristic)
  
  ggplot(sub_df, aes(fill = key, y = value, x = univ_name)) + 
    geom_bar(stat='identity', position = position_fill(reverse = TRUE), width = 0.7) +
    xlab('') + ylab('') +
    labs(fill = label) +
    scale_fill_manual(labels = label_text, values = color_palette) +
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

plot_characteristic_privhs <- function(plot_type, var_name, label, characteristic, label_text, control = 'public', type = 'univ') {
  
  sub_df_visited <- privhs_visited_df %>% group_by(get(var_name)) %>% 
    summarize(`Private HS, 1+ visit` = n()) %>% 
    pivot_longer(cols = 'Private HS, 1+ visit',
                 names_to = 'universe',
                 values_to = 'value')
  names(sub_df_visited) <- c('key', 'univ_name', 'value')
  
  sub_df_universe <- privhs_universe_df %>% group_by(get(var_name)) %>% 
    summarize(`Private HS, universe` = n()) %>% 
    pivot_longer(cols = 'Private HS, universe',
                 names_to = 'universe',
                 values_to = 'value')
  names(sub_df_universe) <- c('key', 'univ_name', 'value')
  
  sub_df_universe_weighed <- privhs_universe_df %>% group_by(get(var_name)) %>% 
    summarize(`Private HS, universe (wt)` = sum(enroll, na.rm = T)) %>% 
    pivot_longer(cols = 'Private HS, universe (wt)',
                 names_to = 'universe',
                 values_to = 'value')
  names(sub_df_universe_weighed) <- c('key', 'univ_name', 'value')
  
  sub_ego_df <- ego_df %>% filter(control == !!control, type == !!type) %>% 
    left_join(unique_visited_schools %>% select(univ_id, priv_hs), by = 'univ_id') %>% 
    mutate(univ_name = str_c(univ_name, ' (', get(str_c('univ_', plot_type)), ') [N=', format(priv_hs, big.mark = ','), ']'))
  
  sub_df_univs <- sub_ego_df %>% 
    select(univ_name, all_of(characteristic)) %>%
    gather(key, value, characteristic) %>%
    mutate(value = parse_number(value)) %>% 
    select(key, univ_name, value)
  
  sub_df <- bind_rows(sub_df_universe, sub_df_universe_weighed, sub_df_univs) %>% filter(!is.na(key))
  
  # sort univs first by characteristic if region or religion, then by ranking
  if (plot_type %in% c('region', 'religion')) {
    univ_order <- (sub_ego_df[order(match(sub_ego_df[[str_c('univ_', plot_type)]], characteristic), sub_ego_df$univ_rank), ])$univ_name
  } else {
    univ_order <- (sub_ego_df[order(sub_ego_df$univ_rank), ])$univ_name
  }
  
  sub_df$univ_name <- factor(sub_df$univ_name, levels = rev(c('Private HS, universe', 'Private HS, universe (wt)', univ_order)))
  sub_df$key <- factor(sub_df$key, levels = characteristic)
  
  ggplot(sub_df, aes(fill = key, y = value, x = univ_name)) + 
    geom_bar(stat='identity', position = position_fill(reverse = TRUE), width = 0.7) +
    xlab('') + ylab('') +
    labs(fill = label) +
    scale_fill_manual(labels = label_text, values = color_palette) +
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
  a <- plot_characteristic_privhs(plot_types[[i]], var_names[[i]], var_labels[[i]], var_values[[i]], var_keys[[i]])
  b <- plot_characteristic_privhs(plot_types[[i]], var_names[[i]], var_labels[[i]], var_values[[i]], var_keys[[i]], control = 'private', type = 'univ')
  # c <- plot_characteristic_privhs(plot_types[[i]], var_names[[i]], var_labels[[i]], var_values[[i]], var_keys[[i]], control = 'private', type = 'lib arts')
  
  # pubu + privu + privc
  # pdf(file.path(figures_dir, str_c('ego_network_', plot_types[[i]], '_pubu_privu_privc.pdf')))
  # par(mar = c(0, 0, 0, 0) + 0.1, mai = c(0, 0, 0, 0))
  # grid.arrange(a, b, c, nrow = 3, ncol = 1)
  # dev.off()
  
  # pubu + privu
  pdf(file.path(figures_dir, str_c('ego_network_', plot_types[[i]], '_pubu_privu.pdf')))
  par(mar = c(0, 0, 0, 0) + 0.1, mai = c(0, 0, 0, 0))
  grid.arrange(a, b, nrow = 2, ncol = 1)
  dev.off()
}

# pubu to pubhs + privhs (enroll)
a <- plot_characteristic_privhs('enroll', enroll_var, enroll_title, enroll_values, enroll_keys)
b <- plot_characteristic_pubhs('enroll', enroll_var, enroll_title, enroll_values, enroll_keys)

pdf(file.path(figures_dir, str_c('ego_network_enroll_pubu_privhs_pubhs.pdf')))
par(mar = c(0, 0, 0, 0) + 0.1, mai = c(0, 0, 0, 0))
grid.arrange(a, b, nrow = 2, ncol = 1)
dev.off()

# pubu to pubhs (separated by instate/outofstate) + privhs (enroll)
# a <- plot_characteristic_privhs('enroll', enroll_var, enroll_title, enroll_values, enroll_keys)
# b <- plot_characteristic_pubhs('enroll', enroll_var, enroll_title, enroll_values, enroll_keys, loc = 'instate')
# c <- plot_characteristic_pubhs('enroll', enroll_var, enroll_title, enroll_values, enroll_keys, loc = 'outofstate')
# 
# pdf(file.path(figures_dir, str_c('ego_network_enroll_pubu_privhs_pubhs_by_loc.pdf')))
# par(mar = c(0, 0, 0, 0) + 0.1, mai = c(0, 0, 0, 0))
# grid.arrange(a, b, c, nrow = 3, ncol = 1)
# dev.off()

# pubu to pubhs + privhs (race)
a <- plot_characteristic_privhs('race', race_var, race_title, race_values, race_keys)
b <- plot_characteristic_pubhs('race', race_var, race_title, race_values, race_keys)

pdf(file.path(figures_dir, str_c('ego_network_race_pubu_privhs_pubhs.pdf')))
par(mar = c(0, 0, 0, 0) + 0.1, mai = c(0, 0, 0, 0))
grid.arrange(a, b, nrow = 2, ncol = 1)
dev.off()

# privu to pubhs + privhs (race)
a <- plot_characteristic_privhs('race', race_var, race_title, race_values, race_keys, control = 'private', type = 'univ')
b <- plot_characteristic_pubhs('race', race_var, race_title, race_values, race_keys, control = 'private', type = 'univ')

pdf(file.path(figures_dir, str_c('ego_network_race_privu_privhs_pubhs.pdf')))
par(mar = c(0, 0, 0, 0) + 0.1, mai = c(0, 0, 0, 0))
grid.arrange(a, b, nrow = 2, ncol = 1)
dev.off()


# pubu to pubhs (separated by instate/outofstate) + privhs (race)
# a <- plot_characteristic_privhs('race', race_var, race_title, race_values, race_keys)
# b <- plot_characteristic_pubhs('race', race_var, race_title, race_values, race_keys, loc = 'instate')
# c <- plot_characteristic_pubhs('race', race_var, race_title, race_values, race_keys, loc = 'outofstate')
# 
# pdf(file.path(figures_dir, str_c('ego_network_race_pubu_privhs_pubhs_by_loc.pdf')))
# par(mar = c(0, 0, 0, 0) + 0.1, mai = c(0, 0, 0, 0))
# grid.arrange(a, b, c, nrow = 3, ncol = 1)
# dev.off()
