
national_plt <- readRDS('../national_osha_covid_correlations/national_lagged_correlations.rds')
west_plt <- readRDS('rds/lagged_correlation_West.rds')
midwest_plt <- readRDS('rds/lagged_correlation_Midwest.rds')
south_plt <- readRDS('rds/lagged_correlation_South.rds')
northeast_plt <- readRDS('rds/lagged_correlation_Northeast.rds')

national_plt <- national_plt + 
  theme(legend.position = 'bottom') + 
  scale_fill_viridis_c(limits = c(-1,1))
  labs(fill = "Pearson's Correlation Coefficient \u03C1")

legend <- get_legend(national_plt)

national_plt <- national_plt + theme(legend.position = 'none') + scale_fill_viridis_c(limits = c(-1,1)) + ylab("")
west_plt <- west_plt + theme(legend.position = 'none') + scale_fill_viridis_c(limits = c(-1,1)) + ylab("") 
midwest_plt <- midwest_plt + theme(legend.position = 'none') + scale_fill_viridis_c(limits = c(-1,1)) + ylab("")
south_plt <- south_plt + theme(legend.position = 'none') + scale_fill_viridis_c(limits = c(-1,1)) + ylab("")
northeast_plt <- northeast_plt + theme(legend.position = 'none') + scale_fill_viridis_c(limits = c(-1,1)) + ylab("")

four_panel <- plot_grid(
  northeast_plt, midwest_plt,
  south_plt, west_plt)

cowplot::plot_grid(
  national_plt,
  four_panel,
  legend,
  nrow=3,
  rel_heights = c(1, 2, .25),
  rel_widths = c(.5, 1, 1))

ggsave("five_panel_correlations.png", width=14, height=16)

