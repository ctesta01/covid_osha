# Code to replicate analysis of monthly disparities in COVID-19 mortality by race/ethnicity.
# Data from US Centers for Disease Control and Prevention, National Center for Health Statistics,
# Deaths involving coronavirus disease 2019 (COVID-19) by race and Hispanic origin group and age, by state
# https://data.cdc.gov/NCHS/Deaths-involving-coronavirus-disease-2019-COVID-19/ks3g-spdg
# Population denominator data are from CDC Wonder Bridged Race Estimates for 2019
# Available at: https://wonder.cdc.gov/Bridged-Race-v2019.HTML

library(dplyr)
library(tidyverse)
library(tidyr)
library(tidycensus)
library(Hmisc)
library(ggplot2)
library(lubridate)
library(colorBlindness)
library(gridExtra)
library(grid)


# Read decumulated CDC dataset - national
# Cut the data at 2020-09-19 because of delays in reporting for the most recent weeks
cdc.raw <- read.csv("covid19_deaths_us 093020.csv") %>%
  filter(!age_group=="All Ages") %>%
  rename(Race.and.Hispanic.Origin.Group=race_and_hispanic_origin_group) %>%
  mutate(month = case_when(
    end_date == "2020-05-02" ~ 5,
    end_date == "2020-06-06" ~ 6,
    end_date == "2020-07-04" ~ 7,
    end_date == "2020-08-01" ~ 8,
    end_date == "2020-09-05" ~ 9,
    end_date == "2020-09-19" ~ 10))

# Recode ages to be consistent with population denominators
cdc.us <- cdc.raw %>% 
  mutate(Age.group=case_when(
    age_group=="Under 1 year" ~ "0-4 years",
    age_group=="1-4 years" ~ "0-4 years",
    age_group=="85 years and over" ~ "85+ years",
    TRUE ~ as.character(age_group))) %>%
  mutate(Race.and.Hispanic.Origin.Group = ifelse(
    Race.and.Hispanic.Origin.Group %in% c("Non-Hispanic Native Hawaiian or Other Pacific Islander",
                                          "Non-Hispanic Asian"),
                                          "Non-Hispanic Asian or Pacific Islander",
    as.character(Race.and.Hispanic.Origin.Group))) %>%
  group_by(month, Race.and.Hispanic.Origin.Group, Age.group) %>%
  summarise(COVID.19.Deaths=sum(covid.19_deaths),
            Total.Deaths=sum(total_deaths),
            Pneumonia.Deaths = sum(pneumonia_deaths),
            Pneumonia.and.COVID.19.Deaths = sum(pneumonia_and_covid.19_deaths),
            Influenza.Deaths = sum(influenza_deaths),
            Pneumonia..Influenza..or.COVID.19.Deaths=sum(pneumonia_influenza_or_covid.19_deaths),
            start_date=first(start_date),
            end_date=first(end_date),
            num_days=first(num_days)) %>%
  filter(Race.and.Hispanic.Origin.Group %in%
           c("Non-Hispanic White",
             "Non-Hispanic Black",
             "Non-Hispanic American Indian or Alaska Native",
             "Non-Hispanic Asian or Pacific Islander",
             "Hispanic or Latino"))  %>%
  ungroup()


# Read in the CDC Wonder bridged race population estimates
# Create Race.and.Hispanic.Origin.Group categories to match cdc.raw data
bridged.raw <- read.delim(file="Bridged-Race Population Estimates 1990-2019_US.txt", nrows=153) %>%
  mutate(Race.and.Hispanic.Origin.Group=case_when(
    Race=="White" & Ethnicity=="Not Hispanic or Latino" ~ "Non-Hispanic White",
    Race=="Black or African American" & Ethnicity=="Not Hispanic or Latino" ~ "Non-Hispanic Black",
    Race=="American Indian or Alaska Native" & Ethnicity=="Not Hispanic or Latino" ~ "Non-Hispanic American Indian or Alaska Native",
    Race=="Asian or Pacific Islander" & Ethnicity=="Not Hispanic or Latino" ~ "Non-Hispanic Asian or Pacific Islander")) %>%
  filter(Race.and.Hispanic.Origin.Group %in%
           c("Non-Hispanic White","Non-Hispanic Black","Non-Hispanic American Indian or Alaska Native",
             "Non-Hispanic Asian or Pacific Islander"))

# Read in the CDC Wonder bridged race population estimates for all Hispanics
bridged.hisp <- read.delim(file="Bridged-Race Population Estimates 1990-2019_hisp_US.txt", nrows=39) %>%
  filter(Ethnicity=="Hispanic or Latino") %>%
  mutate(Race.and.Hispanic.Origin.Group="Hispanic or Latino")

# Concatenate the population estimate datasets and keep just the variables we need
bridged.all <- bind_rows(bridged.raw, bridged.hisp) %>%
  mutate(Age.group=case_when(
    Age.Group %in% c("< 1 year") ~ "0-4 years",
    Age.Group %in% c("1-4 years") ~ "0-4 years",
    Age.Group %in% c("5-9 years","10-14 years") ~ "5-14 years",
    Age.Group %in% c("15-19 years","20-24 years") ~ "15-24 years",
    Age.Group %in% c("25-29 years","30-34 years") ~ "25-34 years",
    Age.Group %in% c("35-39 years","40-44 years") ~ "35-44 years",
    Age.Group %in% c("45-49 years","50-54 years") ~ "45-54 years",
    Age.Group %in% c("55-59 years","60-64 years ") ~ "55-64 years",
    Age.Group %in% c("65-69 years","70-74 years") ~ "65-74 years",
    Age.Group %in% c("75-79 years","80-84 years") ~ "75-84 years",
    Age.Group %in% c("85+ years") ~ "85+ years")) %>%
  group_by(Race.and.Hispanic.Origin.Group, Age.group) %>%
  summarise(denom = sum(Population)) %>% 
  ungroup()



# Merge the death data with the population data
cdc.merge <- left_join(cdc.us, bridged.all, by=c("Age.group",
                                                 "Race.and.Hispanic.Origin.Group"))

# We have the option of analyzing multiple causes of death
outcome.lookup <- names(cdc.us)[4:9]
names(outcome.lookup) <- c("covid","total","pneu","pneu.covid","flu","pneu.flu.covid")


# This function will calculate age-specific mortality rates for specific cause of death
# Rates are true rates per person-time, where person-time is computed by
# multiplying population at risk by number of days.
# This is rescaled so that rates are per person-year.
f.cod.analyze <- function(cause){
  this.cause <- sym(outcome.lookup[cause])
  this.data <- cdc.merge %>%
    mutate(denom.time = as.numeric(0.1*num_days * denom / 36.525),
           deaths = !!this.cause,
           asr = !!this.cause/denom.time,
           var.asr = !!this.cause/denom.time^2,
           asr.lo95 = asr - 1.96*sqrt(var.asr),
           asr.up95 = asr + 1.96*sqrt(var.asr),
           cod = cause) %>%
    select(cod, Race.and.Hispanic.Origin.Group, Age.group, month, num_days, deaths, denom, denom.time, 
           asr, asr.lo95, asr.up95, var.asr)
  return(this.data)
}

# We are focusing on covid deaths here
asr.covid <- f.cod.analyze("covid")

asr.ref <- asr.covid %>% filter(Race.and.Hispanic.Origin.Group=="Non-Hispanic White") %>%
  rename(nhw.deaths = deaths,
         nhw.denom = denom,
         nhw.asr = asr,
         nhw.var = var.asr) %>%
  select(cod, month, Age.group, 
         nhw.deaths, nhw.denom, nhw.asr, nhw.var)

asr.disparities <- left_join(asr.covid, asr.ref, by=c("cod","month","Age.group")) %>%
  mutate(ird = asr - nhw.asr,
         var.ird = var.asr + nhw.var,
         ird.lo95 = ird - 1.96*sqrt(var.ird),
         ird.up95 = ird + 1.96*sqrt(var.ird),
         irr = asr/nhw.asr,
         var.log.irr = (var.asr/asr^2) + (nhw.var/nhw.asr^2),
         irr.lo95 = exp(log(irr) - 1.96*sqrt(var.log.irr)),
         irr.up95 = exp(log(irr) + 1.96*sqrt(var.log.irr)),
         irr.z = log(irr)/sqrt(var.log.irr),
         ird.z = ird/sqrt(var.ird),
         irr.pvalue = 2*pnorm(abs(log(irr)/sqrt(var.log.irr)), lower.tail=F),
         ird.pvalue = 2*pnorm(abs(ird/sqrt(var.ird)), lower.tail=F)) %>%
  mutate(old.asr.lo95 = asr.lo95,
         orig.asr = asr,
         asr.lo95 = ifelse(old.asr.lo95<0,
                           (var.asr/orig.asr)*qgamma(0.025,(orig.asr^2/var.asr)),
                           asr.lo95),
         asr.up95 = ifelse(old.asr.lo95<0,
                           (var.asr/orig.asr)*qgamma(0.975,(orig.asr^2/var.asr)),
                           asr.up95),
         ird = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White",
                      NA, ird),
         ird.lo95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White",
                           NA, ird.lo95),
         ird.up95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White",
                           NA, ird.up95),
         irr = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White",
                      NA, irr),
         irr.lo95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White",
                           NA, irr.lo95),
         irr.up95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White",
                           NA, irr.up95)) %>%
  select(cod, month, Race.and.Hispanic.Origin.Group, Age.group,
         deaths, denom, denom.time, asr, asr.lo95, asr.up95, 
         irr, irr.lo95, irr.up95, irr.pvalue, ird, ird.lo95, ird.up95, ird.pvalue) %>%
  mutate(Age.group=factor(Age.group, 
                          levels=c("0-4 years","5-14 years","15-24 years","25-34 years",
                                   "35-44 years","45-54 years","55-64 years","65-74 years",
                                   "75-84 years","85+ years")),
         Race.and.Hispanic.Origin.Group = factor(Race.and.Hispanic.Origin.Group,
                                                 levels=c("Non-Hispanic White",
                                                          "Non-Hispanic Black",
                                                          "Non-Hispanic American Indian or Alaska Native",
                                                          "Non-Hispanic Asian or Pacific Islander",
                                                          "Hispanic or Latino")),
         month = factor(case_when(month==5 ~ "Feb 1-May 2",
                                  month==6 ~ "May 3-June 6",
                                  month==7 ~ "June 7-July 4",
                                  month==8 ~ "July 5-Aug 1",
                                  month==9 ~ "Aug 2-Sept 5",
                                  month==10 ~ "Sept 6-Sept 19"),
                        levels=c("Feb 1-May 2","May 3-June 6","June 7-July 4",
                                 "July 5-Aug 1","Aug 2-Sept 5", "Sept 6-Sept 19"))) %>%
  arrange(cod, month, Race.and.Hispanic.Origin.Group, Age.group)



# graphing of age specific disparities by race/ethnicity
these.colors <- as.character(safeColors[c(2,4,6,7,8)])

# Remove the youngest age categories where rates are unstable
asr.disparities.covid <- asr.disparities %>% filter(!Age.group %in% c("0-4 years","5-14 years","15-24 years"))

age.labels <- c("25-\n34","35-\n44","45-\n54","55-\n64","65-\n74","75-\n84","85+")

# Plot age standardized rates on the log10 scale because rates differ so much by age
asr.plot.covid <- ggplot(asr.disparities.covid, aes(x=Age.group, y=100000*asr, 
                                                    group=Race.and.Hispanic.Origin.Group,
                                                    color=Race.and.Hispanic.Origin.Group,
                                                    shape=Race.and.Hispanic.Origin.Group,
                                                    fill=Race.and.Hispanic.Origin.Group)) +
  geom_point() +
  geom_line() +
  scale_shape_manual(values=21:25) +
  scale_fill_manual(values=these.colors) +
  scale_color_manual(name="Race/ethnicity", values=these.colors) +
  scale_x_discrete(name="Age",labels=age.labels) +
  facet_wrap(~month, nrow=1) +
  scale_y_log10() +
  labs(y="Rate per 100,000 person-years", 
       x="Age", color  = "Race/ethnicity", shape = "Race/ethnicity", fill="Race/ethnicity") +
  ggtitle("COVID-19: Age-specific mortality rates by race/ethnicity - log10 scale") +
  theme(axis.text.x=element_text(size=6),
        axis.text.y=element_text(size=8),
        axis.title.x=element_text(size=7),
        axis.title.y=element_text(size=8),
        plot.title = element_text(size = 9),
        legend.position="bottom") 

# Plot incidence rate differences
ird.plot.covid <- ggplot(asr.disparities.covid, aes(x=Age.group, y=100000*ird, 
                                                    group=Race.and.Hispanic.Origin.Group,
                                                    color=Race.and.Hispanic.Origin.Group,
                                                    shape=Race.and.Hispanic.Origin.Group,
                                                    fill=Race.and.Hispanic.Origin.Group)) +
  geom_point() +
  geom_line() +
  scale_shape_manual(values=21:25) +
  scale_fill_manual(values=these.colors) +
  scale_color_manual(name="Race/ethnicity", values=these.colors) +
  scale_x_discrete(name="Age",labels=age.labels) +
  facet_wrap(~month, nrow=1) +
  labs(y="Rate difference per 100,000 person-years", x="Age",
       color  = "Race/ethnicity", 
       shape = "Race/ethnicity", 
       fill="Race/ethnicity") +
  ggtitle("COVID-19: Age-specific mortality rate differences by race/ethnicity (ref: Non-Hispanic White)") +
  theme(axis.text.x=element_text(size=6),
        axis.text.y=element_text(size=8),
        axis.title.x=element_text(size=7),
        axis.title.y=element_text(size=8),
        plot.title = element_text(size = 9),
        legend.position="bottom") +
  geom_hline(yintercept=0, linetype="dotted") +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

# Plot incidence rate ratios
irr.plot.covid <- ggplot(asr.disparities.covid, aes(x=Age.group, y=irr, 
                                                    group=Race.and.Hispanic.Origin.Group,
                                                    color=Race.and.Hispanic.Origin.Group,
                                                    shape=Race.and.Hispanic.Origin.Group,
                                                    fill=Race.and.Hispanic.Origin.Group)) +
  geom_point() +
  geom_line() +
  scale_shape_manual(values=21:25) +
  scale_fill_manual(values=these.colors) +
  scale_color_manual(name="Race/ethnicity", values=these.colors) +
  scale_x_discrete(name="Age",labels=age.labels) +
  facet_wrap(~month, nrow=1) +
  labs(y="Rate ratio", x="Age", color  = "Race/ethnicity", shape = "Race/ethnicity", fill="Race/ethnicity") +
  ggtitle("COVID-19: Age-specific mortality rate ratios by race/ethnicity  (ref: Non-Hispanic White)") +
  theme(axis.text.x=element_text(size=6),
        axis.text.y=element_text(size=8),
        axis.title.x=element_text(size=7),
        axis.title.y=element_text(size=8),
        plot.title = element_text(size = 9),
        legend.position="bottom") +
  geom_hline(yintercept=1, linetype="dotted")+
  guides(color=guide_legend(nrow=2,byrow=TRUE))


# A function to facilitate plotting with shared legends
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position, legend.text=element_text(size=6)))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  return(combined)
  
}


cdc.asr.race.plot <- grid_arrange_shared_legend(asr.plot.covid+scale_y_continuous()+ggtitle("COVID-19: Age-specific mortality rates by race/ethnicity"), asr.plot.covid, irr.plot.covid, ird.plot.covid,
                                             position="bottom", ncol = 1, nrow = 4)

ggsave("cdc_asr_race_plot_091920.pdf", cdc.asr.race.plot, width=8, height=10, useDingbats=FALSE)









# Code for age standardization
# read SEER standard population data
seer.age19 <- read_fwf("stdpop.19ages.txt",
                       fwf_widths(c(3,3,8),
                                  c("standard","age","std_raw"))) %>%
  filter(standard=="201") %>%
  mutate(Age.group=recode(age,
                          '000'="0-4 years",                   
                          '001'="0-4 years",
                          '002'="5-14 years",
                          '003'="5-14 years",
                          '004'="15-24 years",
                          '005'="15-24 years",
                          '006'="25-34 years",
                          '007'="25-34 years",
                          '008'="35-44 years",
                          '009'="35-44 years",
                          '010'="45-54 years",
                          '011'="45-54 years",
                          '012'="55-64 years",
                          '013'="55-64 years",
                          '014'="65-74 years",
                          '015'="65-74 years",
                          '016'="75-84 years",
                          '017'="75-84 years",
                          '018'="85+ years"),
         std.pop=as.numeric(std_raw)) %>%
  group_by(Age.group) %>%
  summarise(std=sum(std.pop))


asr.std <- left_join(asr.disparities, seer.age19, by="Age.group") %>% 
  select(month, cod, Race.and.Hispanic.Origin.Group, Age.group, deaths, denom, denom.time, std) %>%
  mutate(ir.i = std*deaths/denom.time,
         var.ir.i = std^2*deaths/denom.time^2) %>%
  group_by(month, cod, Race.and.Hispanic.Origin.Group) %>%
  summarise(deaths = sum(deaths),
            pop = sum(denom),
            sumwt = sum(std),
            sumwt2 = sum(std^2),
            ir.std = sum(ir.i),
            var.ir.std = sum(var.ir.i)) %>%
  mutate(ir.crude = deaths/pop,
         var.ir.crude = deaths/pop^2,
         ir.std = ir.std/sumwt,
         var.ir.std = var.ir.std/sumwt2) %>%
  select(-sumwt, -sumwt2)

asr.std.ref <- filter(asr.std, Race.and.Hispanic.Origin.Group=="Non-Hispanic White") %>%
  rename(nhw.ir.crude = ir.crude,
         nhw.var.ir.crude = var.ir.crude,
         nhw.ir.std = ir.std,
         nhw.var.ir.std = var.ir.std) %>%
  select(month, cod, nhw.ir.std, nhw.var.ir.std, nhw.ir.crude, nhw.var.ir.crude)

asr.std.compare <- left_join(asr.std, asr.std.ref, by=c("month","cod")) %>%
  mutate(ir.crude.lo95 = ir.crude - 1.96*sqrt(var.ir.crude),
         ir.crude.up95 = ir.crude + 1.96*sqrt(var.ir.crude),
         IRD.crude = ir.crude - nhw.ir.crude,
         var.IRD.crude = var.ir.crude + nhw.var.ir.crude,
         IRD.crude.lo95 = IRD.crude - 1.96*sqrt(var.IRD.crude),
         IRD.crude.up95 = IRD.crude + 1.96*sqrt(var.IRD.crude),
         IRR.crude = ir.crude/nhw.ir.crude,
         var.IRR.crude = var.ir.crude/ir.crude^2 + nhw.var.ir.crude/nhw.ir.crude^2,
         IRR.crude.lo95 = exp(log(IRR.crude) - 1.96*sqrt(var.IRR.crude)),
         IRR.crude.up95 = exp(log(IRR.crude) + 1.96*sqrt(var.IRR.crude)),
         ir.std.lo95 = ir.std - 1.96*sqrt(var.ir.std),
         ir.std.up95 = ir.std + 1.96*sqrt(var.ir.std),
         IRD = ir.std - nhw.ir.std,
         var.ird = var.ir.std + nhw.var.ir.std,
         IRD.lo95 = IRD - 1.96*sqrt(var.ird),
         IRD.up95 = IRD + 1.96*sqrt(var.ird),
         IRR = ir.std / nhw.ir.std,
         var.log.irr = var.ir.std/ir.std^2 + nhw.var.ir.std/nhw.ir.std^2,
         IRR.lo95 = exp(log(IRR) - 1.96*sqrt(var.log.irr)),
         IRR.up95 = exp(log(IRR) + 1.96*sqrt(var.log.irr))) %>%
  mutate(ir.std = 100000*ir.std,
         ir.std.lo95 = 100000*ir.std.lo95,
         ir.std.up95 = 100000*ir.std.up95,
         IRD = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, 100000*IRD),
         IRD.lo95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, 100000*IRD.lo95),
         IRD.up95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, 100000*IRD.up95),
         IRR = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, IRR),
         IRR.lo95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, IRR.lo95),
         IRR.up95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, IRR.up95),
         ir.crude = 100000*ir.crude,
         ir.crude.lo95 = 100000*ir.crude.lo95,
         ir.crude.up95 = 100000*ir.crude.up95,
         IRD.crude = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, 100000*IRD.crude),
         IRD.crude.lo95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, 100000*IRD.crude.lo95),
         IRD.crude.up95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, 100000*IRD.crude.up95),
         IRR.crude = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, IRR.crude),
         IRR.crude.lo95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, IRR.crude.lo95),
         IRR.crude.up95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, IRR.crude.up95),
         irr.crude.pvalue = 2*pnorm(abs(log(IRR.crude)/sqrt(var.IRR.crude)), lower.tail=F),
         ird.crude.pvalue = 2*pnorm(abs(IRD.crude/sqrt(var.IRD.crude)), lower.tail=F),
         irr.std.pvalue = 2*pnorm(abs(log(IRR)/sqrt(var.log.irr)), lower.tail=F),
         ird.std.pvalue = 2*pnorm(abs(IRD/sqrt(var.ird)), lower.tail=F)
  ) %>%
  mutate(ir.std.format = sprintf("%.1f (%.1f, %.1f)",
                                 ir.std, ir.std.lo95, ir.std.up95),
         IRD.format = sprintf("%.1f (%.1f, %.1f)",
                              IRD, IRD.lo95, IRD.up95),
         IRR.format = sprintf("%.1f (%.1f, %.1f)",
                              IRR, IRR.lo95, IRR.up95),
         ir.crude.format = sprintf("%.1f (%.1f, %.1f)",
                                   ir.crude, ir.crude.lo95, ir.crude.up95),
         IRD.crude.format = sprintf("%.1f (%.1f, %.1f)",
                                    IRD.crude, IRD.crude.lo95, IRD.crude.up95),
         IRR.crude.format = sprintf("%.1f (%.1f, %.1f)",
                                    IRR.crude, IRR.crude.lo95, IRR.crude.up95),
         deaths.format = sprintf("%.0f", deaths),
         pop.format = sprintf("%.0f", pop),
         irr.crude.p.format = sprintf("%.3f", irr.crude.pvalue),
         irr.std.p.format = sprintf("%.3f", irr.std.pvalue),
         ird.crude.p.format = sprintf("%.3f", ird.crude.pvalue),
         ird.std.p.format = sprintf("%.3f", ird.std.pvalue)) %>%
  select(month, cod, Race.and.Hispanic.Origin.Group,
         deaths.format, pop.format, ir.crude.format, ir.std.format, IRR.crude.format, irr.crude.p.format, 
         IRR.format, irr.std.p.format,
         IRD.crude.format, ird.crude.p.format, IRD.format, ird.std.p.format,
  ) %>%
  filter(cod %in% c("covid")) %>%
  pivot_longer(cols=c(deaths.format, pop.format, ir.crude.format, ir.std.format,  
                      IRR.crude.format, irr.crude.p.format,
                      IRR.format, irr.std.p.format, IRD.crude.format, ird.crude.p.format, 
                      IRD.format, ird.std.p.format),
               names_to="param") %>%
  pivot_wider(names_from=month,
              values_from=value)

write.csv(asr.std.compare, "crudeVsStandardizedRates_byRace_month.csv")



# Plot the age-standardized rates, rate ratios, and rate differences
asr.std.plot <- left_join(asr.std, asr.std.ref, by=c("month","cod")) %>%
  mutate(ir.crude.lo95 = ir.crude - 1.96*sqrt(var.ir.crude),
         ir.crude.up95 = ir.crude + 1.96*sqrt(var.ir.crude),
         IRD.crude = ir.crude - nhw.ir.crude,
         var.IRD.crude = var.ir.crude + nhw.var.ir.crude,
         IRD.crude.lo95 = IRD.crude - 1.96*sqrt(var.IRD.crude),
         IRD.crude.up95 = IRD.crude + 1.96*sqrt(var.IRD.crude),
         IRR.crude = ir.crude/nhw.ir.crude,
         var.IRR.crude = var.ir.crude/ir.crude^2 + nhw.var.ir.crude/nhw.ir.crude^2,
         IRR.crude.lo95 = exp(log(IRR.crude) - 1.96*sqrt(var.IRR.crude)),
         IRR.crude.up95 = exp(log(IRR.crude) + 1.96*sqrt(var.IRR.crude)),
         ir.std.lo95 = ir.std - 1.96*sqrt(var.ir.std),
         ir.std.up95 = ir.std + 1.96*sqrt(var.ir.std),
         IRD = ir.std - nhw.ir.std,
         var.ird = var.ir.std + nhw.var.ir.std,
         IRD.lo95 = IRD - 1.96*sqrt(var.ird),
         IRD.up95 = IRD + 1.96*sqrt(var.ird),
         IRR = ir.std / nhw.ir.std,
         var.log.irr = var.ir.std/ir.std^2 + nhw.var.ir.std/nhw.ir.std^2,
         IRR.lo95 = exp(log(IRR) - 1.96*sqrt(var.log.irr)),
         IRR.up95 = exp(log(IRR) + 1.96*sqrt(var.log.irr))) %>%
  mutate(ir.std = 100000*ir.std,
         ir.std.lo95 = 100000*ir.std.lo95,
         ir.std.up95 = 100000*ir.std.up95,
         IRD = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, 100000*IRD),
         IRD.lo95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, 100000*IRD.lo95),
         IRD.up95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, 100000*IRD.up95),
         IRR = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, IRR),
         IRR.lo95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, IRR.lo95),
         IRR.up95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, IRR.up95),
         ir.crude = 100000*ir.crude,
         ir.crude.lo95 = 100000*ir.crude.lo95,
         ir.crude.up95 = 100000*ir.crude.up95,
         IRD.crude = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, 100000*IRD.crude),
         IRD.crude.lo95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, 100000*IRD.crude.lo95),
         IRD.crude.up95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, 100000*IRD.crude.up95),
         IRR.crude = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, IRR.crude),
         IRR.crude.lo95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, IRR.crude.lo95),
         IRR.crude.up95 = ifelse(Race.and.Hispanic.Origin.Group=="Non-Hispanic White", NA, IRR.crude.up95),
         irr.crude.pvalue = 2*pnorm(abs(log(IRR.crude)/sqrt(var.IRR.crude)), lower.tail=F),
         ird.crude.pvalue = 2*pnorm(abs(IRD.crude/sqrt(var.IRD.crude)), lower.tail=F),
         irr.std.pvalue = 2*pnorm(abs(log(IRR)/sqrt(var.log.irr)), lower.tail=F),
         ird.std.pvalue = 2*pnorm(abs(IRD/sqrt(var.ird)), lower.tail=F)
  )  %>%
  select(month, cod, Race.and.Hispanic.Origin.Group,
         ir.std, ir.std.lo95, ir.std.up95, 
         IRD, IRD.lo95, IRD.up95,
         IRR, IRR.lo95, IRR.up95)

# Plot age-standardized rates
std.rate.plot <- ggplot(asr.std.plot, aes(x=month, y=ir.std, 
                                          group=Race.and.Hispanic.Origin.Group,
                                          color=Race.and.Hispanic.Origin.Group,
                                          shape=Race.and.Hispanic.Origin.Group,
                                          fill=Race.and.Hispanic.Origin.Group)) +
  geom_point() +
  geom_line() +
  scale_shape_manual(values=21:25) +
  scale_fill_manual(values=these.colors) +
  scale_color_manual(name="Race/ethnicity", values=these.colors) +
  labs(y="Rate per 100,000 person-years", 
       x="Month", color  = "Race/ethnicity", shape = "Race/ethnicity", fill="Race/ethnicity") +
  ggtitle("COVID-19: Age-standardized mortality rates per 100,000 person-years by race/ethnicity") +
  theme(axis.text.x=element_text(size=9),
        axis.text.y=element_text(size=9),
        axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
        plot.title = element_text(size = 9),
        legend.position="bottom") 


# Plot age-standardized rate ratios
std.irr.plot <- ggplot(asr.std.plot, aes(x=month, y=IRR, 
                                          group=Race.and.Hispanic.Origin.Group,
                                          color=Race.and.Hispanic.Origin.Group,
                                          shape=Race.and.Hispanic.Origin.Group,
                                          fill=Race.and.Hispanic.Origin.Group)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=1, linetype="dotted") +
  scale_shape_manual(values=21:25) +
  scale_fill_manual(values=these.colors) +
  scale_color_manual(name="Race/ethnicity", values=these.colors) +
  labs(y="Mortality Rate Ratio", 
       x="Month", color  = "Race/ethnicity", shape = "Race/ethnicity", fill="Race/ethnicity") +
  ggtitle("COVID-19: Age-standardized mortality rate ratios by race/ethnicity") +
  theme(axis.text.x=element_text(size=9),
        axis.text.y=element_text(size=9),
        axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
        plot.title = element_text(size = 9),
        legend.position="bottom") 



# Plot age-standardized rate differences
std.ird.plot <- ggplot(asr.std.plot, aes(x=month, y=IRD, 
                                         group=Race.and.Hispanic.Origin.Group,
                                         color=Race.and.Hispanic.Origin.Group,
                                         shape=Race.and.Hispanic.Origin.Group,
                                         fill=Race.and.Hispanic.Origin.Group)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=0, linetype="dotted") +
  scale_shape_manual(values=21:25) +
  scale_fill_manual(values=these.colors) +
  scale_color_manual(name="Race/ethnicity", values=these.colors) +
  labs(y="Mortality Rate Difference per 100,000 person-years", 
       x="Month", color  = "Race/ethnicity", shape = "Race/ethnicity", fill="Race/ethnicity") +
  ggtitle("COVID-19: Age-standardized mortality rates differences by race/ethnicity") +
  theme(axis.text.x=element_text(size=9),
        axis.text.y=element_text(size=9),
        axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
        plot.title = element_text(size = 9),
        legend.position="bottom") 



cdc.std.race.plot <- grid_arrange_shared_legend(std.rate.plot,
                                                std.irr.plot,
                                                std.ird.plot,
                                             position="bottom", ncol = 1, nrow = 3)

ggsave("cdc_std_race_plot_091920.pdf", cdc.std.race.plot, width=8, height=10, useDingbats=FALSE)

