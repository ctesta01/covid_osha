
#' Decumulate COVID19 Deaths by Race/Ethnicity
#'

decumulate_weekly_covid19_deaths <- function() {
  
  covid19_deaths <- load_covid19_deaths_by_race_ethnicity(export=F)

  covid19_deaths$race_and_hispanic_origin_group %<>%
    recode('Hispanic' = 'Hispanic or Latino',
      'All Race-Hisp' = 'Total Deaths',
      'Non-Hispanic Asian or Pacific Islander' = 'Non-Hispanic Asian')


  # convert date columns to actual dates
  covid19_deaths %<>% clean_covid19_deaths()


  # group by state, age group, and race, sort by 
  # observation date (data_as_of) and de-cumulate.
  # 
  # remove the first data_as_of since for any vector x, 
  # the x - lag(x) is NA in the first entry.
  
  covid19_deaths %<>% 
    group_by(state, age_group, race_and_hispanic_origin_group) %>% 
    arrange(end_week) %>% 
    mutate_at(
      c("covid-19_deaths", "total_deaths", "pneumonia_deaths",
      "pneumonia_and_covid-19_deaths", "influenza_deaths",
      "pneumonia_influenza_or_covid-19_deaths"),
      function(x) { x - lag(x) }) %>% 
    mutate(
      start_week = lag(end_week)) %>% 
    ungroup() %>% 
    filter(data_as_of > min(data_as_of)) 

  # adding a count of number of weeks represented
  covid19_deaths %<>% 
    mutate(
      num_weeks = (end_week - start_week)/7)

  return(covid19_deaths)
}


#' Decumulate Monthly COVID19 Deaths by Race/Ethnicity
#'

decumulate_monthly_covid19_deaths <- function() {
  
  covid19_deaths <- load_covid19_deaths_by_race_ethnicity(export=F)

  covid19_deaths$race_and_hispanic_origin_group %<>%
    recode('Hispanic' = 'Hispanic or Latino',
      'All Race-Hisp' = 'Total Deaths',
      'Non-Hispanic Asian or Pacific Islander' = 'Non-Hispanic Asian')

  covid19_deaths$age_group %<>% recode(
    'All ages' = 'All Ages')


  # convert date columns to actual dates
  covid19_deaths %<>% clean_covid19_deaths()


  # group by state, age group, and race, sort by 
  # observation date (data_as_of) and de-cumulate.
  # 
  # remove the first data_as_of since for any vector x, 
  # the x - lag(x) is NA in the first entry.

  jan_to_may <- covid19_deaths %>% ungroup() %>% 
    filter(end_week == min(end_week))

  last_data <- covid19_deaths %>% filter(end_week == max(end_week))

  covid19_deaths %<>% mutate(month = lubridate::month(lubridate::ymd(end_week)))

  covid19_deaths %<>% filter(end_week != '2020-08-29')

  covid19_deaths %<>% 
    group_by(state, age_group, race_and_hispanic_origin_group, month) %>% 
    filter(end_week == min(end_week))

  value_cols <- c("covid-19_deaths", "total_deaths", "pneumonia_deaths",
      "pneumonia_and_covid-19_deaths", "influenza_deaths",
      "pneumonia_influenza_or_covid-19_deaths")

  covid19_deaths %<>% bind_rows(last_data)
  
  covid19_deaths %<>% 
    group_by(state, age_group, race_and_hispanic_origin_group) %>% 
    arrange(end_week) %>% 
    mutate_at(
      value_cols,
      function(x) { x - lag(x) }) %>% 
    mutate(
      start_week = lag(end_week)) %>% 
    ungroup() %>% 
    filter(data_as_of > min(data_as_of)) 

  covid19_deaths <- bind_rows(jan_to_may, covid19_deaths)

  # adding a count of number of weeks represented
  covid19_deaths %<>% 
    mutate(
      num_days = (end_week - start_week))

  covid19_deaths %<>% rename(
    start_date = start_week,
    end_date = end_week)

  covid19_deaths %<>% select(-footnote)

  covid19_deaths %<>% arrange(data_as_of, state, age_group, race_and_hispanic_origin_group)
  
  covid19_deaths_us <- covid19_deaths %>% filter(state == 'United States')

  # starting aug29 they added the following new age_groups:
  # 
  #> [1] "0-17 years"  "18-29 years" "30-49 years" "50-64 years"
  # 
  orig_age_groups <- covid19_deaths %>% filter(end_date == min(end_date)) %>% pull(age_group) %>% unique()
  new_age_groups <- covid19_deaths %>% filter(end_date == max(end_date)) %>% pull(age_group) %>% unique()

  covid19_deaths %<>% filter(! age_group %in% setdiff(new_age_groups, orig_age_groups))
  covid19_deaths_us %<>% filter(! age_group %in% setdiff(new_age_groups, orig_age_groups))

  write.csv(covid19_deaths, 
    '~/Desktop/covid19_deaths.csv')

  write.csv(covid19_deaths_us, 
    '~/Desktop/covid19_deaths_us.csv')
  
  write.csv(covid19_deaths_us[(apply(covid19_deaths_us[,value_cols], 1, function(x) any(is.na(x)))) | 
    rowSums(covid19_deaths_us[,value_cols] < 0) > 0,], 
    '~/Desktop/negative_rows.csv')

  return(covid19_deaths)
}


#' Plot Decumulated COVID19 Deaths by Race/Ethnicity

plot_decumulated_covid19_deaths <- function() {
  covid19_deaths <- decumulate_covid19_deaths()

  # convert to weekly rates by dividing by num_weeks
  covid19_deaths %<>% mutate_at(
      c("covid-19_deaths", "total_deaths", "pneumonia_deaths",
      "pneumonia_and_covid-19_deaths", "influenza_deaths",
      "pneumonia_influenza_or_covid-19_deaths"),
      ~ . / as.integer(num_weeks)
  )

  covid19_deaths %>% 
  filter(state == 'United States',
    age_group == 'All Ages',
    ! race_and_hispanic_origin_group %in% c('Total', 'Unknown')
    ) %>% 
  janitor::clean_names() %>% 
  ggplot(aes(
    x = end_week, 
    y = covid_19_deaths,
    color = race_and_hispanic_origin_group)) + 
    geom_point() + 
    geom_line() +
    scale_y_log10() + 
    xlab("Week Ending") + 
    ylab("Weekly Deaths")

  ggsave("~/Desktop/race_ethnicity_weekly_deaths.png")

}
