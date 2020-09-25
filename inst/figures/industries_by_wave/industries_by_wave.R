
devtools::load_all()
library(magrittr)
library(janitor)

load_osha()

osha %<>% clean_names()

osha$top_level_naics <- substr(osha$primary_site_naics, 1, 2)

load_naics()

naics_shortname <- sapply(naics, `[[`, 1)

osha$top_level_naics_str <- naics_shortname[osha$top_level_naics]


osha$date <- as.Date(osha$receipt_date, origin = "1899-12-30")

osha_industry_pcts <- osha %>% 
  mutate(first_or_second = date > lubridate::ymd("2020-5-21")) %>% 
  group_by(top_level_naics_str, first_or_second) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  group_by(first_or_second) %>% 
  mutate(
    pct = n / sum(n) * 100,
    first_or_second = recode(as.factor(first_or_second), 
      `TRUE` = "> 2020-5-21",
      `FALSE` = "≤ 2020-5-21")
    ) %>%
  ungroup() %>% 
  mutate(top_level_naics_str = forcats::fct_rev(as.factor(top_level_naics_str)))

# osha_industry_pcts %>% 
#   filter(! is.na(top_level_naics_str)) %>% 
#   ggplot(aes(x = reorder(top_level_naics_str, pct), y = pct)) + 
#     geom_line(aes(group = top_level_naics_str), 
#       arrow = arrow(length=unit(0.3,"cm"), ends="last", type = "closed"), size=1.75) + 
#     theme(legend.position='bottom') + 
#     coord_flip() + 
#     ylab("Percent of Complaints.") + 
#     xlab("") + 
#     labs(caption = 
#     paste0("Start of arrow indicates the percent of complaints ",
#     "in the given industry before May 21st. \nEnd of the arrow indicates the percent of ",
#     "complaints in the given industry starting May 22nd through August 22nd")) + 
#     ggtitle("Percentage of OSHA Complaints by Industry -- Comparing First & Second Waves")

osha_industry_pcts2 <- osha_industry_pcts %>%
  rename(industry = top_level_naics_str) %>% 
  filter(!is.na(industry)) %>% 
  select(-n) %>% 
  tidyr::pivot_wider(id_cols=c(industry), names_from = first_or_second, values_from = pct) %>% 
  mutate(pct_change = `> 2020-5-21` - `≤ 2020-5-21`) %>% 
  arrange(pct_change) 

osha_industry_pcts2 %>% 
  mutate(inc_stable_or_dec = ifelse(pct_change >= .5, "increasing", ifelse(pct_change <= -.5, 'decreasing', '~stable'))) %>% 
  select(-pct_change) %>% 
  tidyr::pivot_longer(cols = colnames(.)[2:3], names_to = 'wave', values_to = 'pct') %>% 
  ggplot(aes(x = reorder(industry, pct), y = pct, color = inc_stable_or_dec)) + 
    geom_line(aes(group = industry), 
      arrow = arrow(length=unit(0.3,"cm"), ends="last", type = "closed"), size=1.75) + 
    theme(legend.position='bottom') + 
    coord_flip() + 
    ylab("Percent of Complaints.") + 
    xlab("") + 
    scale_color_manual(values = c("#377EB8", "#4DAF4A", "#E41A1C")) + 
    labs(caption = 
    paste0("Start of arrow indicates the percent of complaints ",
    "in the given industry before May 21st. \nEnd of the arrow indicates the percent of ",
    "complaints in the given industry starting May 22nd through August 22nd")) + 
    ggtitle("Percentage of OSHA Complaints by Industry -- Comparing First & Second Waves")

ggsave("industry_pcts_by_wave.png", width=12, height=6)


osha_industry_pcts2 %>% 
  mutate_if(is.numeric, ~ signif(., 3)) %>% 
  write.csv("industry_pcts_by_wave.csv", row.names=F)

