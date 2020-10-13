

correlation_df_filenames <- 
  list.files("csv/")

correlation_df_filenames

variable <- gsub("lagged_correlation - ",
  "", correlation_df_filenames)

variable <- gsub(".csv", "", variable)

variable <- gsub("\\s+", " ", variable)

df_list <- 
  lapply(file.path("csv/", correlation_df_filenames),
    readr::read_csv)

names(df_list) <- variable


# helper function for getting max lag, rho 
# for each of positivity, cases, and deaths
get_max_lag_and_rho <- function(cor_df, var) {
  cor_df_filtered <- cor_df %>% 
    filter(variable == {{ var }}) %>% 
    filter(rho == max(rho)) 

  return(c(lag = cor_df_filtered[['shift']], 
      rho = cor_df_filtered[['rho']]))
}


df_data <- list()

for (i in 1:length(df_list)) {
  for (outcome in c('positivity', 'cases', 'death')) {
    lag_rho <- 
      get_max_lag_and_rho(df_list[[i]], outcome)

    df_data[[length(df_data)+1]] <-
      list(industry = variable[i],
        variable = outcome,
        lag = lag_rho[['lag']],
        rho = lag_rho[['rho']]
        )
  }
}

df <- do.call(rbind.data.frame, df_data)

df %<>% select(variable, industry, lag, rho) %>% 
  arrange(variable, industry)



# add in industry complaints N

osha <- load_osha()

osha$top_level_naics <- 
  stringr::str_extract(
    osha$primary_site_naics,
    "^[0-9][0-9]")

osha$industry <- 
  sapply(osha$top_level_naics, 
    function(x) {
      naics[[x]][[1]]})

osha$industry %<>% gsub("\\s+", " ", .)

osha %>% 
  group_by(industry) %>% 
  summarize(complaints = n()) ->
    industry_counts

df %<>% merge(industry_counts, all.x=T)

df %<>% arrange(variable, industry)

write.csv(df, 'industry_correlation_lags.csv', row.names=F)


plot <- ggplot(df, aes(x = lag, y = rho, size = complaints, color = industry)) + 
  geom_vline(xintercept = 0, alpha=0.6) + 
  geom_hline(yintercept = 0, alpha=0.6) + 
  geom_point(alpha=0.7) + 
  facet_wrap(~variable) + 
  guides(color = guide_legend(nrow = 8)) + 
  expand_limits(y = c(-1,1)) + 
  labs(size = "Number of OSHA Complaints") + 
  labs(color = "Industry") + 
  labs(x = 
    "Lag in days after which OSHA complaints had the highest correlation with each measure") + 
  labs(y = 'Correlation Coefficient \u03C1') + 
  theme(legend.position = 'bottom')

ggplotly(plot) %>%
  layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
