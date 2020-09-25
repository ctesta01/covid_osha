
#  load osha.analysis
devtools::load_all()

library(urbnmapr) # convenient US states mapping package


# get osha data
load_osha()

# split & clean addresses
osha %<>% clean_osha_addresses()

# infer addresses
osha %<>% infer_states()

# all the Added for Legacy Migration values are from Oregon

{ 
  added_for_legacy_migration_rids <- filter(osha, state == 'Added for legacy migration') %>% 
    select(rid) %>% unique() %>% unlist()

  filter(osha, rid %in% added_for_legacy_migration_rids) %>% select(state) %>% table()

  osha[osha$state == 'Added for legacy migration', 'state'] <- 'OREGON'
}


######################
# Map the US Regions #
######################

states <- urbnmapr::get_urbn_map(map='states', sf=TRUE)
us_census_regions <- get_us_census_regions()

states %<>% merge(us_census_regions, by.x = 'state_abbv', by.y = 'state_abb')
states %<>% mutate(region = tools::toTitleCase(region))

national_map <- 
  ggplot(states, aes()) + 
  geom_sf(fill='white', size=0.3) + 
  theme_void()

make_regional_map_inset <- function(region) {
  region <- enquo(region)
  ggplot(states %>% filter(region == !! region), aes()) + 
    geom_sf(fill='white', size=0.3) + 
    scale_x_continuous(breaks = seq(-125, -105, length.out = 3)) + 
    theme_void()
}

west_map <- make_regional_map_inset("West")
midwest_map <- make_regional_map_inset("Midwest")
south_map <- make_regional_map_inset("South")
northeast_map <- make_regional_map_inset("Northeast")


#######################
# Plot the COVID data #
#######################

covid <- readr::read_csv(system.file("covid/covidtracking_allstateshistory.csv", package='osha.analysis'))

covid %<>% merge(us_census_regions, by.x = 'state', by.y = 'state_abb')

covid %<>% mutate(date = lubridate::ymd(date))

load_state_popsizes()

state_popsizes$state[[1]] <- 'National'

state_popsizes %<>% select(state, `2019`) %>% 
  filter(state %in% c("National", 'West', 'Midwest', 'South', 'Northeast'))

state_popsizes %<>% rename(popsize = `2019`)

covid %<>% group_by(region, date) %>% 
  summarize(
    death = sum(deathIncrease),
    positive = sum(positiveIncrease),
    totalTestResults = sum(totalTestResultsIncrease))

covid$region <- tools::toTitleCase(covid$region)

covid %<>% merge(state_popsizes, by.x = 'region', by.y = 'state')

national_covid <- covid %>% group_by(date) %>% 
  summarize(death = sum(death), positive = sum(positive), totalTestResults = sum(totalTestResults),
    popsize = sum(popsize)) 

national_covid %<>% mutate(region = 'National')

covid %<>% bind_rows(national_covid)

covid %<>% mutate(
  death = death / popsize * 1e6,
  positive = positive / popsize * 1e6,
  totalTestResults = totalTestResults / popsize * 1e6)

make_regional_plt <- function(var, region_name) { 
  var <- enquo(var)

  varname <- c(death = 'Deaths',
    positive = 'Cases',
    totalTestResults = 'Tests')[quo_name(var)] %>% 
    tools::toTitleCase()

  regionname <- tools::toTitleCase(region_name)


  ggplot(covid %>% filter(region == tools::toTitleCase(region_name)), aes(x = date, y = !! var)) + 
    geom_line() + 
    ggtitle(paste0("COVID ", varname, " - ", regionname)) + 
    ylab(paste0(varname, ' per Million')) + 
    expand_limits(y = max(covid[, quo_name(var)])) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    scale_y_continuous(labels = scales::comma_format())
}

national_covid_deaths_plt <- make_regional_plt(death, 'national')
west_covid_deaths_plt <- make_regional_plt(death, 'west')
midwest_covid_deaths_plt <- make_regional_plt(death, 'midwest')
south_covid_deaths_plt <- make_regional_plt(death, 'south')
northeast_covid_deaths_plt <- make_regional_plt(death, 'northeast')



############################
# Plot the OSHA Complaints #
############################


# format dates
osha$date <- as.Date(osha$receipt_date, origin = "1899-12-30")

# insert top level naics into osha
load_naics()
osha$top_level_naics <- substr(osha$primary_site_naics, 1, 2)
naics_shortname <- sapply(naics, `[[`, 1)
osha$top_level_naics_str <- naics_shortname[osha$top_level_naics]

osha$top_level_naics_str <- gsub("\\s+", " ", osha$top_level_naics_str)

osha_max_vol <- osha %>% group_by(date) %>% 
  summarize(count = n()) %>%
  pull(count) %>%
  max()

osha %<>% mutate(
  top_level_naics_str = ifelse(top_level_naics_str %in% c('Health Care and Social Assistance', 'Retail Trade', 'Manufacturing'),
    top_level_naics_str, 'Other'))

osha$top_level_naics_str %<>% factor(levels = 
  rev(c('Health Care and Social Assistance', "Retail Trade", "Manufacturing", "Other")))


ma <- function(x, n = 7){stats::filter(x, rep(1 / n, n), sides = 1)}

national_osha_volume <- 
  osha %>% 
  ggplot(aes(x = date, fill = top_level_naics_str)) + 
    geom_bar() + 
    geom_line(
      data = national_covid,
      mapping = aes(x = date, y = ma(death / popsize * 1e6 * 90), fill=NULL, linetype='')) + 
    theme_bw() + 
    theme(legend.position='none') + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    ylab("Count of OSHA Complaints") + 
    labs(fill = guide_legend(title="OSHA Complaints\nby Industry")) + 
    scale_linetype_manual('', values = 'solid') + 
    scale_fill_viridis_d(option = 'A', begin=.25, end = 0.85, direction=-1) + 
    scale_y_continuous( sec.axis = sec_axis(~ (. / 90), name = "Deaths Per Million (7-Day Avg)")) + 
    guides(fill=guide_legend(nrow=2,byrow=TRUE), linetype=guide_legend(title='7-Day Avg of COVID-19\nDeaths per Million')) + 
    ggtitle("National OSHA Complaints and COVID-19 Deaths per Million (7-Day Avg)",
      "Complaints were most correlated with deaths 17 days later, \u03C1=0.845")

national_osha_volume_w_legend <- 
  national_osha_volume + 
  theme(legend.position='bottom')

national_plot_w_map <- 
  ggdraw() +
    draw_plot(national_osha_volume) + 
    draw_plot(national_map, x = 0.05, y = 0.55, width = 0.25, height = 0.25)

legend <- cowplot::get_legend(national_osha_volume_w_legend)

state_abb_hash <- setNames(state.abb, state.name)

osha$state_abb <- state_abb_hash[tools::toTitleCase(tolower(osha$state))]

osha %<>% merge(us_census_regions, by = "state_abb")

osha_regional_plot <- function(region_name) { 
  region_name <- enquo(region_name)

  mean_osha_count <- osha %>% filter(region == !! region_name) %>% 
    group_by(date) %>% 
    summarize(count = n()) %>% 
    pull(count) %>% 
    mean(na.rm=T)

  mean_covid_deaths <- 
    covid %>% filter(region == tools::toTitleCase(!! region_name)) %>% 
    mutate(ma = ma(death)) %>% 
    pull(ma) %>% 
    mean(na.rm=T)

  scaling_factor <- mean_osha_count / mean_covid_deaths

  days_later <- switch(quo_name(region_name),
    west = 24,
    south = 22,
    midwest = 11,
    northeast = 15)

  cor_rho = switch(quo_name(region_name),
    west = 0.706,
    south = 0.717,
    midwest = 0.907,
    northeast = 0.938)

  map_inset <- switch(quo_name(region_name),
    west = west_map,
    south = south_map,
    midwest = midwest_map,
    northeast = northeast_map)

  inset_shift <- switch(quo_name(region_name),
    midwest = .02, 0)

  main_plot <- osha %>% 
    filter(region == !! region_name ) %>% 
    ggplot(aes(x = date, fill = top_level_naics_str)) + 
      geom_bar() + 
      geom_line(
        data = covid %>% filter(region == tools::toTitleCase(!! region_name)),
        mapping = aes(x = date, y = ma(death * scaling_factor), fill=NULL, linetype='')) + 
      theme_bw() + 
      theme(legend.position='none') + 
      scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
      ylab("Count of OSHA Complaints") + 
      labs(fill = guide_legend(title="OSHA Complaints\nby Industry")) + 
      scale_linetype_manual('', values = 'solid') + 
      scale_fill_viridis_d(option = 'A', begin=.25, end = 0.85, direction=-1) + 
      scale_y_continuous( sec.axis = sec_axis(~ (. / scaling_factor), name = "Deaths Per Million (7-Day Avg)")) + 
      ggtitle(paste0("OSHA Complaints and COVID-19 Deaths per Million (7-Day Avg), ", tools::toTitleCase(quo_name(region_name))),
        paste0("Complaints were most correlated with deaths ", days_later, " days later, \u03C1=", 
        cor_rho))

  ggdraw() + 
    draw_plot(main_plot) + 
    draw_plot(map_inset, x = 0.07+inset_shift, y = 0.55, width = 0.25, height = 0.25)
}


west_osha_volume <- osha_regional_plot('west')
south_osha_volume <- osha_regional_plot('south')
midwest_osha_volume <- osha_regional_plot('midwest')
northeast_osha_volume <- osha_regional_plot('northeast')


four_panel <- cowplot::plot_grid(
  northeast_osha_volume, midwest_osha_volume, 
  south_osha_volume, west_osha_volume,
  ncol = 2)

ggsave("four_panel.png", width=14, height=10)


full_plot <- cowplot::plot_grid(
  national_plot_w_map,
  four_panel,
  legend,
  nrow=3,
  rel_heights = c(1, 2, .25),
  rel_widths = c(.5, 1, 1))

ggsave("full_plot.png", width=14, height=16)


