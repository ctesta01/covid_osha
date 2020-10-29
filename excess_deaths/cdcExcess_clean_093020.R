# Code to replicate excess death analysis
# Data on excess deaths from US Centers for Disease Control and Prevention,
# National Center for Health Statistics,
# Weekly counts of deaths by jurisdiction and age group
# https://data.cdc.gov/NCHS/Weekly-counts-of-deaths-by-jurisdiction-and-age-gr/y5bj-9g5w/
# Population denominator data are from CDC Wonder Bridged Race Estimates for 2019
# Available at: https://wonder.cdc.gov/Bridged-Race-v2019.HTML
# There are two "Types" of numbers in the CDC data files.
# We are using the Predicted (weighted) death counts which adjust for underreporting.

library(dplyr)
library(tidyverse)
library(tidyr)
library(tidycensus)
library(Hmisc)
library(ggplot2)
library(lubridate)

setwd("~/Dropbox/cdcExcess")


# Read in the CDC data.
cdc.raw <- read.csv("Weekly_counts_of_deaths_by_jurisdiction_and_age_group 093020.csv")

cdc.cooked <- cdc.raw %>%
  mutate(Week.Ending.Date = mdy(Week.Ending.Date),
         State = Jurisdiction) %>%
  filter(State=="United States")


# Read in CDC WONDER denominator data
bridged.raw.national <- read.delim(file="Bridged-Race Population Estimates 1990-2019 national.txt") %>%
  filter(!Age.Group=="") %>%
  mutate(new.age.group = case_when(
    Age.Group %in% c("< 1 year",
                     "1-4 years",
                     "5-9 years",
                     "10-14 years",
                     "15-19 years",
                     "20-24 years") ~ "Under 25 years",
    Age.Group %in% c("25-29 years",
                     "30-34 years",
                     "35-39 years",
                     "40-44 years") ~ "25-44 years",
    Age.Group %in% c("45-49 years",
                     "50-54 years",
                     "55-59 years",
                     "60-64 years ") ~ "45-64 years",
    Age.Group %in% c("65-69 years",
                     "70-74 years") ~ "65-74 years",
    Age.Group %in% c("75-79 years",
                     "80-84 years") ~ "75-84 years",
    Age.Group == "85+ years" ~ "85 years and older")) %>%
  group_by(Yearly.July.1st.Estimates, new.age.group) %>%
  summarise(pop=sum(Population, na.rm=T)) %>%
  rename(Year=Yearly.July.1st.Estimates,
         Age.Group = new.age.group) %>%
  ungroup()

# Use 2019 denominators for 2020
denom.2020 <- bridged.raw.national %>%
  filter(Year==2019) %>%
  mutate(Year=2020)

# Concatenate the denominator datasets
denom.all <- bind_rows(bridged.raw.national, denom.2020) %>%
  arrange(Year, Age.Group)


# Merge the excess death dataset with the denominator dataset
cdc.merge <- left_join(cdc.cooked, denom.all, by=c("Year","Age.Group")) %>%
  mutate(Age.Group=relevel(factor(Age.Group), ref="Under 25 years")) 


# Tabulate excess death rates
cdc.analyze <- cdc.merge %>%
  mutate(y2020 = ifelse(Year==2020, 1, 0)) %>%
  group_by(Type, y2020, Week, Age.Group) %>%
  summarise(pop=sum(pop),
            deaths = sum(Number.of.Deaths)) 


# read SEER standard population data
seer.age19 <- read_fwf("stdpop.19ages.txt",
                       fwf_widths(c(3,3,8),
                                  c("standard","age","std_raw"))) %>%
  filter(standard=="201") %>%
  mutate(Age.Group=recode(age,
                          '000'="Under 25 years",                   
                          '001'="Under 25 years",
                          '002'="Under 25 years",
                          '003'="Under 25 years",
                          '004'="Under 25 years",
                          '005'="Under 25 years",
                          '006'="25-44 years",
                          '007'="25-44 years",
                          '008'="25-44 years",
                          '009'="25-44 years",
                          '010'="45-64 years",
                          '011'="45-64 years",
                          '012'="45-64 years",
                          '013'="45-64 years",
                          '014'="65-74 years",
                          '015'="65-74 years",
                          '016'="75-84 years",
                          '017'="75-84 years",
                          '018'="85 years and older"),
         std.pop=as.numeric(std_raw)) %>%
  group_by(Age.Group) %>%
  summarise(std=sum(std.pop)) 

# Need the number of days to calculate person-time denominator for rates
num.days <- as.numeric(ymd("2020-09-12")-ymd("2020-01-01"))

# First way to compute the excess mortality rate: 
# sum up the total deaths for 2020 and the average deaths for 2015-2019
# over the weeks. Cut off at week 37 to avoid some of the delayed reporting issues.
# Compute age-standardized rates for 2020 and average annual rates for 2015-2019.
# Compare using rate differences and rate ratios.

cdc.std <- cdc.analyze %>%
  filter(Week<=37) %>%
  group_by(Type, y2020, Age.Group) %>%
  summarise(deaths=sum(deaths),
            pop=first(pop)) %>%
  left_join(seer.age19, by="Age.Group") %>%
  mutate(poptime = pop*num.days/365.25,
         y.i = std*deaths,
         var.yi = std^2*deaths,
         cum.i = std*deaths/pop,
         ir.i = std*deaths/poptime,
         var.cumi = std^2*deaths/pop^2,
         var.iri = std^2*deaths/poptime^2)  %>%
  group_by(Type, y2020) %>%
  summarise(y.std = sum(y.i),
            var.y.std = sum(var.yi),
            ir.std = sum(ir.i),
            var.ir.std = sum(var.iri),
            cum.std = sum(cum.i),
            var.cum.std = sum(var.cumi),
            sum.wt = sum(std),
            sum.wtsq = sum(std^2)) %>%
  mutate(y.std = y.std/sum.wt,
         var.y.std = var.y.std/sum.wtsq,
         y.std.lo95 = y.std - 1.96*sqrt(var.y.std),
         y.std.up95 = y.std + 1.96*sqrt(var.y.std),
         ir.std = ir.std/sum.wt,
         var.ir.std = var.ir.std/sum.wtsq,
         ir.std.lo95 = ir.std - 1.96 * sqrt(var.ir.std),
         ir.std.up95 = ir.std + 1.96 * sqrt(var.ir.std),
         cum.std = cum.std/sum.wt,
         var.cum.std = var.cum.std/sum.wtsq,
         cum.std.lo95 = cum.std - 1.96*sqrt(var.cum.std),
         cum.std.up95 = cum.std + 1.96*sqrt(var.cum.std)) %>% 
  ungroup()


cdc.ref <- cdc.std %>%
  filter(y2020==0) %>%
  mutate(y.std.ref = y.std,
         var.y.std.ref = var.y.std,
         ir.std.ref = ir.std,
         var.ir.std.ref = var.ir.std,
         cum.std.ref = cum.std,
         var.cum.std.ref = var.cum.std) %>%
  select(Type, y.std.ref, var.y.std.ref,ir.std.ref, var.ir.std.ref, cum.std.ref, var.cum.std.ref) 

cdc.std.compare <- cdc.std %>%
  filter(y2020==1) %>%
  left_join(cdc.ref, by="Type") %>%
  mutate(excess.y = y.std - y.std.ref,
         excess.ird = ir.std - ir.std.ref,
         var.excess.ird = var.ir.std + var.ir.std.ref,
         excess.irr = ir.std/ir.std.ref,
         var.log.excess.irr = (var.ir.std/(ir.std^2) + var.ir.std.ref/(ir.std.ref^2)),
         excess.cum.irr = cum.std / cum.std.ref,
         var.log.excess.cum.irr = (var.cum.std/(cum.std^2) + var.cum.std.ref/(cum.std.ref^2)),
         excess.ird.lo95 = excess.ird - 1.96*sqrt(var.excess.ird),
         excess.ird.up95 = excess.ird + 1.96*sqrt(var.excess.ird),
         excess.irr.lo95 = exp(log(excess.irr) - 1.96*sqrt(var.log.excess.irr)),
         excess.irr.up95 = exp(log(excess.irr) + 1.96*sqrt(var.log.excess.irr)),
         excess.cum.irr.lo95 = exp(log(excess.cum.irr) - 1.96*sqrt(var.log.excess.cum.irr)),
         excess.cum.irr.up95 = exp(log(excess.cum.irr) + 1.96*sqrt(var.log.excess.cum.irr))) %>%
  mutate(excess.ird = 100000*excess.ird,
         excess.ird.lo95 = 100000*excess.ird.lo95,
         excess.ird.up95 = 100000*excess.ird.up95) 

cdc.std %>% mutate(ir.std=100000*ir.std, ir.std.lo95=100000*ir.std.lo95, ir.std.up95=100000*ir.std.up95, 
                          cum.std=100000*cum.std, cum.std.lo95=100000*cum.std.lo95, cum.std.up95=100000*cum.std.up95) %>%
         select(Type, y2020, ir.std, ir.std.lo95, ir.std.up95, cum.std, cum.std.lo95, cum.std.up95)

cdc.std.compare %>% select(Type, excess.ird, excess.ird.lo95, excess.ird.up95, 
       excess.irr, excess.irr.lo95, excess.irr.up95, excess.cum.irr, excess.cum.irr.lo95, excess.cum.irr.up95)


# Second way of computing excess deaths
# Four comparisons:
# 1 = sum up the number of deaths in 2020 vs. the average number in 2015-2019
# 2 = treat negative excess as 0
# 3 = sum up the number of deaths in 2020 vs. the 95% upper limit of the average number in 2015-2019
# 4 = treat negative excess as 0

cdc.std.method2 <- cdc.analyze %>% 
  ungroup() %>%
  select(Type, y2020, Week, Age.Group, deaths, pop) %>%
  filter(Week<=37) %>%
  mutate(deaths=ifelse(y2020==0, deaths/5, deaths),
         pop=ifelse(y2020==0, pop/5, pop)) %>%
  pivot_wider(names_from=y2020, values_from=c(deaths, pop)) %>%
  mutate(excess1=deaths_1 - deaths_0,
         excess2=ifelse(excess1<0, 0, excess1),
         excess3=deaths_1-(deaths_0+1.96*sqrt(deaths_0)),
         excess4=ifelse(excess3<0, 0, excess3)) %>%
  group_by(Type, Age.Group) %>%
  summarise(excess1=sum(excess1),
            excess2=sum(excess2),
            excess3=sum(excess3),
            excess4=sum(excess4)) %>%
  left_join(denom.2020, by="Age.Group") %>%
  left_join(seer.age19, by="Age.Group") %>%
  pivot_longer(cols=c(excess1, excess2, excess3, excess4),
               names_to="method",
               names_prefix="excess",
               values_to="excess") %>%
  mutate(poptime=pop*224/365.25,
         y.i = std*excess,
         var.yi = std^2*excess,
         ir.i = std*excess/poptime,
         var.iri = std^2*excess/poptime^2,
         cum.i = std*excess/pop,
         var.cumi = std^2*excess/pop^2)  %>%
  group_by(Type, method) %>%
  summarise(y.crude = sum(excess),
            var.y.crude = sum(excess),
            y.std = sum(y.i),
            var.y.std = sum(var.yi),
            ir.std = sum(ir.i),
            var.ir.std = sum(var.iri),
            cum.std = sum(cum.i),
            var.cum.std = sum(var.cumi),
            sum.wt = sum(std),
            sum.wtsq = sum(std^2)) %>%
  mutate(y.crude.lo95 = y.crude - 1.96*sqrt(var.y.crude),
         y.crude.up95 = y.crude + 1.96*sqrt(var.y.crude),
         y.std = y.std/sum.wt,
         var.y.std = var.y.std/sum.wtsq,
         y.std.lo95 = y.std - 1.96*sqrt(var.y.std),
         y.std.up95 = y.std + 1.96*sqrt(var.y.std),
         ir.std = ir.std/sum.wt,
         var.ir.std = var.ir.std/sum.wtsq,
         ir.std.lo95 = ir.std - 1.96 * sqrt(var.ir.std),
         ir.std.up95 = ir.std + 1.96 * sqrt(var.ir.std),
         cum.std = cum.std/sum.wt,
         var.cum.std = var.cum.std/sum.wtsq,
         cum.std.lo95 = cum.std - 1.96 * sqrt(var.cum.std),
         cum.std.up95 = cum.std + 1.96 * sqrt(var.cum.std)) %>% 
  ungroup() %>% filter(Type=="Predicted (weighted)") %>%
  select(method, y.crude, y.crude.lo95, y.crude.up95, ir.std, ir.std.lo95, ir.std.up95, cum.std, cum.std.lo95, cum.std.up95) %>%
  mutate(ir.std = 100000*ir.std,
         ir.std.lo95 = 100000*ir.std.lo95,
         ir.std.up95 = 100000*ir.std.up95,
         cum.std = 100000*cum.std,
         cum.std.lo95 = 100000*cum.std.lo95,
         cum.std.up95 = 100000*cum.std.up95)

write.csv(cdc.std.method2, "excess_rates_methods1-4_093020.csv")

