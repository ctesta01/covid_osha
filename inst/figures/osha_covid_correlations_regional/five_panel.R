
devtools::load_all()

national_plt <- readRDS('../national_osha_covid_correlations/national_lagged_correlations.rds')
west_plt <- readRDS('rds/lagged_correlation_West.rds')
midwest_plt <- readRDS('rds/lagged_correlation_Midwest.rds')
south_plt <- readRDS('rds/lagged_correlation_South.rds')
northeast_plt <- readRDS('rds/lagged_correlation_Northeast.rds')

# version 1 uses:
# title_size <- 15
# subtitle_size <- 11.5
# 
# and disables wrap_subtitle in adjust_plot

title_size <- 14
subtitle_size <- 14

national_plt <- national_plt + 
  theme(legend.position = 'bottom', 
    legend.title = element_text(size = subtitle_size),
    legend.text = element_text(size = 11.5)) + 
  scale_fill_viridis_c(limits = c(-1,1)) + 
  labs(fill = "Pearson's Correlation Coefficient \u03C1")

legend <- get_legend(national_plt)

adjust_plot <- function(plt, wrap_subtitle = T) {
  plt + 
    theme(
      legend.position = 'none',
      plot.title = element_text(size=title_size),
      plot.subtitle = element_text(size = subtitle_size),
      axis.text = element_text(size=subtitle_size),
      axis.title = element_text(size=subtitle_size)) + 
    scale_fill_viridis_c(limits = c(-1,1)) + ylab("") + 
    if (wrap_subtitle) {
      labs(
        subtitle = paste0(strwrap(plt$labels$subtitle, 58), collapse='\n'))
    } else NULL
}

national_plt %<>% adjust_plot(wrap_subtitle = F)
west_plt %<>% adjust_plot()
midwest_plt %<>% adjust_plot()
south_plt %<>% adjust_plot()
northeast_plt %<>% adjust_plot()

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

