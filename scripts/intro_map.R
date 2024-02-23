library(tidyverse)
library(leaflet)
library(tigris)
library(formattable)
library(htmlwidgets)
library(htmltools)


output_dir <- file.path('.', 'assets', 'maps')
imgs_dir <- file.path('.', 'assets', 'images')
data_dir <- file.path('.', 'data')

# Load school data
ipeds_1718 <- readRDS(file.path(data_dir, 'ipeds_1718.RDS'))
ccd_1718 <- readRDS(file.path(data_dir, 'ccd_1718.RDS'))
# pss_1718 <- readRDS(file.path(data_dir, 'pss_1718.RDS'))

pubhs_universe_df <- readRDS(file.path(data_dir, 'pubhs_universe.RDS'))
privhs_universe_df <- readRDS(file.path(data_dir, 'privhs_universe.RDS'))

univ_sample <- readRDS(file.path(data_dir, 'univ_sample.RDS'))
events_data <- readRDS(file.path(data_dir, 'events_data.RDS'))

# Harpeth Hall (01296378), est. 1951
# University School of Nashville (01296764), est. 1975
# Christ Presbyterian Academy (A9106269), est. 1985
intro_schools <- c('01296378', '01296764', 'A9106269')

visits_data <- events_data %>% 
  filter(event_date > '2017-08-01') %>% 
  distinct() %>% 
  arrange(event_date) %>% 
  mutate(
    visit = str_c(univ_abbrev, ' (', event_date, ')')
  ) %>% 
  group_by(school_id, classification) %>%
  summarise(
    visit = paste(visit, collapse = '<br>')
  ) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = school_id,
    names_from = classification,
    values_from = visit
  )

pubhs_data <- pubhs_universe_df %>% 
  left_join(ccd_1718 %>% select(ncessch, sch_name, latitude, longitude), by = 'ncessch') %>% 
  filter(state_code == 'TN') %>% 
  left_join(visits_data, by = c('ncessch' = 'school_id'))

privhs_data <- privhs_universe_df %>% 
  filter(state_code == 'TN') %>% 
  left_join(visits_data, by = c('ncessch' = 'school_id'))

intro_schools_data <- privhs_universe_df %>% 
  filter(ncessch %in% intro_schools) %>% 
  left_join(visits_data, by = c('ncessch' = 'school_id'))

# Create marker popups
pubhs_data$popup <- paste0(
  '<div style="max-height: 400px; overflow-y: scroll;font-size: 15px;"><b>', pubhs_data$sch_name, '</b><br>',
  '<br>Grade 12 Enrollment: ', pubhs_data$g12,
  '<br>% White: ', sprintf('%.1f', pubhs_data$pct_white),
  '<br>% Asian: ', sprintf('%.1f', pubhs_data$pct_asian),
  '<br>% Latinx: ', sprintf('%.1f', pubhs_data$pct_hispanic),
  '<br>% Black: ', sprintf('%.1f', pubhs_data$pct_black),
  '<br>% AIAN/NHPI: ', sprintf('%.1f', pubhs_data$pct_amerindian + pubhs_data$pct_nativehawaii),
  '<br>% 2+ races: ', sprintf('%.1f', pubhs_data$pct_tworaces),
  if_else(!is.na(pubhs_data$private_national), paste0('<br><br><b>Private national visits</b><br>', pubhs_data$private_national), ''),
  if_else(!is.na(pubhs_data$private_libarts), paste0('<br><br><b>Private liberal arts visits</b><br>', pubhs_data$private_libarts), ''),
  if_else(!is.na(pubhs_data$public_research), paste0('<br><br><b>Public research visits</b><br>', pubhs_data$public_research), ''),
  '</div>'
)
privhs_data$popup <- paste0(
  '<div style="max-height: 400px; overflow-y: scroll;font-size: 15px;"><b>', str_to_title(privhs_data$name), '</b><br>',
  '<br>Grade 12 Enrollment: ', privhs_data$total_12,
  '<br>Niche ranking: ', privhs_data$overall_niche_letter_grade, if_else(!is.na(privhs_data$rank_within_category), paste0(' (', privhs_data$rank_within_category, ')'), ''),
  '<br>Religious affiliation: ', str_to_title(privhs_data$religion),
  '<br>% White: ', sprintf('%.1f', privhs_data$pct_white),
  '<br>% Asian: ', sprintf('%.1f', privhs_data$pct_asian),
  '<br>% Latinx: ', sprintf('%.1f', privhs_data$pct_hispanic),
  '<br>% Black: ', sprintf('%.1f', privhs_data$pct_black),
  '<br>% AIAN/NHPI: ', sprintf('%.1f', privhs_data$pct_amerindian + privhs_data$pct_nativehawaii),
  '<br>% 2+ races: ', sprintf('%.1f', privhs_data$pct_tworaces),
  if_else(!is.na(privhs_data$private_national), paste0('<br><br><b>Private national visits</b><br>', privhs_data$private_national), ''),
  if_else(!is.na(privhs_data$private_libarts), paste0('<br><br><b>Private liberal arts visits</b><br>', privhs_data$private_libarts), ''),
  if_else(!is.na(privhs_data$public_research), paste0('<br><br><b>Public research visits</b><br>', privhs_data$public_research), ''),
  '</div>'
)
intro_schools_data$popup <- paste0(
  '<div style="max-height: 400px; overflow-y: scroll;font-size: 15px;"><b>', str_to_title(intro_schools_data$name), '</b> (est. ', c(1985, 1951, 1975), ')<br>',
  '<br>Grade 12 Enrollment: ', intro_schools_data$total_12,
  '<br>Niche ranking: ', intro_schools_data$overall_niche_letter_grade, if_else(!is.na(intro_schools_data$rank_within_category), paste0(' (', intro_schools_data$rank_within_category, ')'), ''),
  '<br>Religious affiliation: ', str_to_title(intro_schools_data$religion),
  '<br>% White: ', sprintf('%.1f', intro_schools_data$pct_white),
  '<br>% Asian: ', sprintf('%.1f', intro_schools_data$pct_asian),
  '<br>% Latinx: ', sprintf('%.1f', intro_schools_data$pct_hispanic),
  '<br>% Black: ', sprintf('%.1f', intro_schools_data$pct_black),
  '<br>% AIAN/NHPI: ', sprintf('%.1f', intro_schools_data$pct_amerindian + intro_schools_data$pct_nativehawaii),
  '<br>% 2+ races: ', sprintf('%.1f', intro_schools_data$pct_tworaces),
  if_else(!is.na(intro_schools_data$private_national), paste0('<br><br><b>Private national visits</b><br>', intro_schools_data$private_national), ''),
  if_else(!is.na(intro_schools_data$private_libarts), paste0('<br><br><b>Private liberal arts visits</b><br>', intro_schools_data$private_libarts), ''),
  if_else(!is.na(intro_schools_data$public_research), paste0('<br><br><b>Public research visits</b><br>', intro_schools_data$public_research), ''),
  '</div>'
)

# Load ACS data
acs_data <- read_csv(file.path(data_dir, 'acs_tracts_TN_2017.csv'), na = c('-666666666'), col_types = c('fips_state_code' = 'c')) %>% 
  mutate(
    county_tract = str_c(fips_county_code, tract),
    tract = str_split(tract_name, ', ', simplify = T)[,1],
    county = str_split(tract_name, ', ', simplify = T)[,2],
    median_inc_2564 = if_else(is.na(median_inc_2544) | is.na(median_inc_4564), coalesce(median_inc_2544, median_inc_4564), (median_inc_2544 + median_inc_4564) / 2),
    pop_white_1519 = `pop_white_m_15-17` + `pop_white_m_18-19` + `pop_white_f_15-17` + `pop_white_f_18-19`,
    pop_black_1519 = `pop_black_m_15-17` + `pop_black_m_18-19` + `pop_black_f_15-17` + `pop_black_f_18-19`,
    pop_amerindian_1519 = `pop_amerindian_m_15-17` + `pop_amerindian_m_18-19` + `pop_amerindian_f_15-17` + `pop_amerindian_f_18-19`,
    pop_asian_1519 = `pop_asian_m_15-17` + `pop_asian_m_18-19` + `pop_asian_f_15-17` + `pop_asian_f_18-19`,
    pop_nativehawaii_1519 = `pop_nativehawaii_m_15-17` + `pop_nativehawaii_m_18-19` + `pop_nativehawaii_f_15-17` + `pop_nativehawaii_f_18-19`,
    pop_otherrace_1519 = `pop_otherrace_m_15-17` + `pop_otherrace_m_18-19` + `pop_otherrace_f_15-17` + `pop_otherrace_f_18-19`,
    pop_tworaces_1519 = `pop_tworaces_m_15-17` + `pop_tworaces_m_18-19` + `pop_tworaces_f_15-17` + `pop_tworaces_f_18-19`,
    pop_hispanic_1519 = `pop_hispanic_m_15-17` + `pop_hispanic_m_18-19` + `pop_hispanic_f_15-17` + `pop_hispanic_f_18-19`,
    pop_total_1519 = pop_white_1519 + pop_black_1519 + pop_amerindian_1519 + pop_asian_1519 + pop_nativehawaii_1519 + pop_otherrace_1519 + pop_tworaces_1519 + pop_hispanic_1519,
    pct_white_1519 = pop_white_1519 / pop_total_1519 * 100,
    pct_black_1519 = pop_black_1519 / pop_total_1519 * 100,
    pct_amerindian_1519 = pop_amerindian_1519 / pop_total_1519 * 100,
    pct_asian_1519 = pop_asian_1519 / pop_total_1519 * 100,
    pct_nativehawaii_1519 = pop_nativehawaii_1519 / pop_total_1519 * 100,
    pct_otherrace_1519 = pop_otherrace_1519 / pop_total_1519 * 100,
    pct_tworaces_1519 = pop_tworaces_1519 / pop_total_1519 * 100,
    pct_hispanic_1519 = pop_hispanic_1519 / pop_total_1519 * 100,
    pct_poc_1519 = if_else(pop_total_1519 == 0, NA, pct_black_1519 + pct_hispanic_1519 + pct_amerindian_1519)
  ) %>% 
  select(county_tract, tract, county, pop_total, median_inc_2564, pct_poc_1519)

# Load shape data
cbsas <- core_based_statistical_areas(cb = T, year = 2017)
tn_tracts <- tracts('TN', cb = T, year = 2017)

tract_data <- tn_tracts %>%
  mutate(
    county_tract = str_c(COUNTYFP, TRACTCE)
  ) %>% 
  left_join(acs_data, by = 'county_tract')

# Create color legends
color_pop <- colorNumeric('YlGnBu', tract_data$pop_total, n = 5)

tract_data$race_brks <- cut(
  tract_data$pct_poc_1519, 
  breaks = c(-1, 20, 40, 60, 80, 90, 101), 
  labels = c('0-19%', '20-39%', '40-59%', '60-79%', '80-89%', '90-100%')
)
color_income <- colorFactor('YlGnBu', tract_data$inc_brks)

tract_data$inc_brks <- cut(
  tract_data$median_inc_2564, 
  breaks = c(-1, 50000, 75000, 100000, 150000, 200000, 10000000), 
  labels = c('<$50k', '$50k-74k', '$75k-99k', '$100k-149k', '$150k-199k', '$200k+')
)
color_race <- colorFactor('YlGnBu', tract_data$race_brks)

# Create overlay popups
tract_data$popup_pop <- paste0('<div style="font-size: 15px;"><b>', tract_data$tract, ' (', tract_data$county, ')</b><br>Total Population: ', trimws(format(tract_data$pop_total, big.mark = ',')), '</div>')
tract_data$popup_income <- paste0('<div style="font-size: 15px;"><b>', tract_data$tract, ' (', tract_data$county, ')</b><br>Median Household Income: ', currency(tract_data$median_inc_2564, digits = 0L), '</div>')
tract_data$popup_race <- paste0('<div style="font-size: 15px;"><b>', tract_data$tract, ' (', tract_data$county, ')</b><br>% Black, Latinx, and Native American: ', sprintf('%.1f', tract_data$pct_poc_1519), '</div>')

# Create Vanderbilt icon
vanderbilt <- ipeds_1718 %>% 
  filter(univ_name == 'Vanderbilt University') %>% 
  select(univ_id, univ_name, countycd, countynm, longitude, latitude)

vanderbilt_icon <- makeIcon(iconUrl = file.path(imgs_dir, 'vanderbilt.png'), iconWidth = 30)

# Generate map
m <- leaflet() %>%
  setView(lat = 36.0526, lng = -82.85674, zoom = 5.5) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = cbsas %>% filter(NAME == 'Nashville-Davidson--Murfreesboro--Franklin, TN'), stroke = T, weight = 3, color = 'black', fillOpacity = 0, group = 'MSA') %>% 
  addPolylines(data = cbsas %>% filter(NAME == 'Nashville-Davidson--Murfreesboro--Franklin, TN'), stroke = T, weight = 3, color = 'black', fillOpacity = 0) %>% 
  addPolygons(data = tract_data, stroke = T, weight = 1, color = 'lightgray', fillColor = ~color_pop(pop_total), fillOpacity = 0.8, smoothFactor = 0.2, popup = ~popup_pop, group = 'MSA by Population Total') %>% 
  addPolygons(data = tract_data, stroke = T, weight = 1, color = 'lightgray', fillColor = ~color_income(inc_brks), fillOpacity = 0.8, smoothFactor = 0.2, popup = ~popup_income, group = 'MSA by Median Household Income') %>% 
  addPolygons(data = tract_data, stroke = T, weight = 1, color = 'lightgray', fillColor = ~color_race(race_brks), fillOpacity = 0.8, smoothFactor = 0.2, popup = ~popup_race, group = 'MSA by Race/Ethnicity') %>% 
  addMarkers(data = vanderbilt, lng=~longitude, lat=~latitude, icon = vanderbilt_icon) %>% 
  addCircleMarkers(data = pubhs_data, lng = ~longitude, lat = ~latitude, radius = 3, weight = 1, color = 'white', fillOpacity = 1, fillColor = '#2267ff', popup = ~popup, group = 'Public High Schools') %>%
  addCircleMarkers(data = privhs_data, lng = ~longitude, lat = ~latitude, radius = 3, weight = 1, color = 'white', fillOpacity = 1, fillColor = '#d16822', popup = ~popup, group = 'Private High Schools') %>%
  addCircleMarkers(data = intro_schools_data, lng = ~longitude, lat = ~latitude, radius = 6, weight = 1, color = 'white', fillOpacity = 1, fillColor = '#d16822', popup = ~popup, group = 'Private High Schools') %>% 
  addLayersControl(
    position = 'bottomleft',
    baseGroups = c('MSA', 'MSA by Population Total', 'MSA by Median Household Income', 'MSA by Race/Ethnicity'),
    overlayGroups = c('Public High Schools', 'Private High Schools'),
    options = layersControlOptions(collapsed = F)
  ) %>%
  addLegend(
    data = tract_data, title = 'Population Total',
    position = 'topright', pal = color_pop, values = ~pop_total,
    className = 'info legend legend-pop',
    na.label = 'NA', opacity = 1
  ) %>% 
  addLegend(
    data = tract_data, title = 'Median Household Income',
    position = 'topright', pal = color_income, values = ~inc_brks,
    className = 'info legend legend-income',
    na.label = 'NA', opacity = 1
  ) %>%
  addLegend(
    data = tract_data, title = 'Black, Latinx, and <br>Native American Population',
    position = 'topright', pal = color_race, values = ~race_brks,
    className = 'info legend legend-race',
    na.label = 'NA', opacity = 1
  ) %>%
  htmlwidgets::onRender("
    function(el, x) {
      $('.legend').css('display', 'none');
      this.on('baselayerchange', function(e) {
        $('.legend').css('display', 'none');
        switch (e.name) {
          case 'MSA by Median Household Income':
            $('.legend-income').css('display', 'inherit');
            break;
          case 'MSA by Population Total':
            $('.legend-pop').css('display', 'inherit');
            break;
          case 'MSA by Race/Ethnicity':
            $('.legend-race').css('display', 'inherit');
            break;
        }
        e.layer.bringToBack();
      })
    }
  ")

saveWidget(m, file.path(output_dir, 'intro_map.html'), selfcontained = T, background = 'transparent')
