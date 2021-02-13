# Author: Andrew Nalundasan
# For: OMSBA 5300, Seattle University
# Date: 2/12/2021
# Week 6 class materials (Fixed Variables)

library(wooldridge)
library(tidyverse)
library(vtable)
library(jtools)

data(crime4)
help(crime4)
View(crime4)

# use group_by() to get means-within-groups and subtract them out
data(crime4, package = 'wooldridge')
crime4 <- crime4 %>%
  # Filter to the data points from our graph
  filter(county %in% c(1,3,7, 23),
         prbarr < .5) %>%
  group_by(county) %>%
  mutate(mean_crime = mean(crmrte),
         mean_prob = mean(prbarr)) %>%
  mutate(demeaned_crime = crmrte - mean_crime,
         demeaned_prob = prbarr - mean_prob)

# Regress as usual:
orig_data <- lm(crmrte ~ prbarr, data = crime4)
de_mean <- lm(demeaned_crime ~ demeaned_prob, data = crime4)

# Fixed Effects Regression: 
library(tidyverse)
library(jtools)

# Read in Data:
gm <- read_csv('gapminder.csv')
export_summs(orig_data, de_mean)

# Least Squares Dummy ('Binary')
lsdv <- lm(crmrte ~ prbarr + factor(county), data = crime4)
export_summs(orig_data, de_mean, lsdv, coefs = c('prbarr', 'demeaned_prob'))

# using lm_robust from estimatr
library(estimatr)
pro <- lm_robust(crmrte ~ prbarr, data = crime4, fixed_effects = ~county)
export_summs(de_mean, pro, statistics = c(N = 'nobs', R2 = 'r.squared'))
