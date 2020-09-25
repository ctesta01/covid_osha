
#  load osha.analysis
devtools::load_all()
library(urbnmapr) # convenient US states mapping package

# get osha data
load_osha()

# load covid data
covid <- readr::read_csv(system.file("covid/covidtracking_allstateshistory.csv", package='osha.analysis'))

# get us census regions
us_census_regions <- get_us_census_regions()

# merge in regions to covid data 
covid %<>% merge(us_census_regions, by.x = 'state', by.y = 'state_abb')

# use date-types for covid dates
covid %<>% mutate(date = lubridate::ymd(date))

# group by region
covid %<>% group_by(region, date) %>% 
  summarize(
    death = sum(deathIncrease),
    positive = sum(positiveIncrease),
    totalTestResults = sum(totalTestResultsIncrease))

# add title casing to regions
covid$region <- tools::toTitleCase(covid$region)

# aggregate national data
national_covid <- covid %>% group_by(date) %>% 
  summarize(death = sum(death), positive = sum(positive), totalTestResults = sum(totalTestResults)) 

# indicate national region for national aggregate data
national_covid %<>% mutate(region = 'National')

# make osha dates date-type
osha$date <- as.Date(osha$receipt_date, origin = "1899-12-30")


# get osha daily counts
osha_daily_counts <- osha %>% group_by(date) %>% 
  summarize(count = n())

# add positivity
national_covid %<>% mutate(positivity = positive / totalTestResults)

# add smoothing
ma <- function(x, n = 7){stats::filter(x, rep(1 / n, n), sides = 1)}

osha_daily_counts$count <- ma(osha_daily_counts$count)
national_covid %<>% mutate_if(is.numeric, ma)

osha_daily_counts %<>% filter(!is.na(count))


df <- merge(osha_daily_counts, national_covid, by='date', all=T)
start_date_idx <- which(df$date == min(osha_daily_counts$date))
end_date_idx <- which(df$date == max(osha_daily_counts$date))

lagged_positivity_cor <- c()
lagged_cases_cor <- c()
lagged_death_cor <- c()


for (i in -28:28) {

  # effective start date
  start_date_eff <- max(1, start_date_idx + i)
  end_date_eff <- min(nrow(df), end_date_idx + i)
  date_vec <- start_date_eff:end_date_eff
  
  osha_series <- df$count[start_date_idx:(start_date_idx + length(date_vec) - 1)]
  positivity_series <- df$positivity[date_vec]
  cases_series <- df$positive[date_vec]
  death_series <- df$death[date_vec]

  valid_idxs <- which( (! is.na(osha_series)) & 
    (! is.na(positivity_series)) & 
    (! is.na(cases_series)) & 
    (! is.na(death_series)))

  osha_series <- osha_series[valid_idxs]
  positivity_series <- positivity_series[valid_idxs]
  cases_series <- cases_series[valid_idxs]
  death_series <- death_series[valid_idxs]

  lagged_positivity_cor[length(lagged_positivity_cor)+1] <- 
    cor(osha_series, positivity_series)

  lagged_cases_cor[length(lagged_cases_cor)+1] <- 
    cor(osha_series, cases_series)

  lagged_death_cor[length(lagged_death_cor)+1] <- 
    cor(osha_series, death_series)
}

cor_df <- data.frame(shift = -28:28, positivity = lagged_positivity_cor, 
  cases = lagged_cases_cor, death = lagged_death_cor)

cor_df %<>% tidyr::pivot_longer(cols = c(positivity, cases, death), 
  names_to = 'variable', values_to='rho')

# get positivity max cor and corresponding lag
max_positivity_cor <- cor_df %>% 
  filter(variable == 'positivity') %>% 
  filter(rho == max(rho))

max_positivity_cor_val <- max_positivity_cor %>% pull(rho)
max_positivity_cor_lag <- max_positivity_cor %>% pull(shift)

# get cases max cor and corresponding lag
max_cases_cor <- cor_df %>% 
  filter(variable == 'cases') %>% 
  filter(rho == max(rho))

max_cases_cor_val <- max_cases_cor %>% pull(rho)
max_cases_cor_lag <- max_cases_cor %>% pull(shift)

# get death max cor and corresponding lag
max_death_cor <- cor_df %>% 
  filter(variable == 'death') %>% 
  filter(rho == max(rho))

max_death_cor_val <- max_death_cor %>% pull(rho)
max_death_cor_lag <- max_death_cor %>% pull(shift)

cor_df$variable %<>% factor(levels = c("death", 'cases', 'positivity'))

ggplot(cor_df, aes(x = shift, y = variable, fill = rho)) + 
  geom_tile() + 
  scale_fill_viridis_c() + 
  ggtitle("Lagged Correlations with OSHA Complaint Volume at the National Level\n",
    paste0(
    "OSHA complaints were most correlated with COVID-19 test positivity ", 
    ifelse(max_positivity_cor_lag >= 0,
      paste0(max_positivity_cor_lag, " days later, \u03C1=", signif(max_positivity_cor_val, 3), ".\n\n"),
      paste0(abs(max_positivity_cor_lag), " days prior, \u03C1=", signif(max_positivity_cor_val, 3), ".\n\n")),
    "OSHA complaints were most correlated with COVID-19 cases ", 
    ifelse(max_cases_cor_lag >= 0,
      paste0(max_cases_cor_lag, " days later, \u03C1=", signif(max_cases_cor_val, 3), ".\n\n"),
      paste0(abs(max_cases_cor_lag), " days prior, \u03C1=", signif(max_cases_cor_val, 3), ".\n\n")),
    "OSHA complaints were most correlated with COVID-19 deaths ", 
    ifelse(max_death_cor_lag >= 0,
      paste0(max_death_cor_lag, " days later, \u03C1=", signif(max_death_cor_val, 3)), 
      paste0(abs(max_death_cor_lag), " days prior, \u03C1=", signif(max_death_cor_val, 3))),
      "."
    )) +
  labs(fill = guide_legend(title="Pearson's\ncorrelation\ncoefficient\n\u03C1"))

ggsave("lagged_correlation.png", width=13, height=7)
