library(rstan)
library(data.table)
library(dplyr)
library(reshape2)

D <- fread('democracies.csv')

melted <- melt(D, c('country', 'sftgcode', 'year')) %>%
  filter(!is.na(value)) %>%
  mutate(sftgcode = factor(sftgcode),
         year = factor(year),
         expert = factor(variable))

expert_dat <- with(melted, list(
    country = as.integer(sftgcode),
    year = as.integer(year),
    expert = as.integer(expert),
    labels = as.integer(value),
    num_obs = nrow(melted)
))

expert_dat[['num_countries']] <- max(expert_dat[['country']])
expert_dat[['num_years']] <- max(expert_dat[['year']])
expert_dat[['num_experts']] <- max(expert_dat[['expert']])


fit <- stan('democracy_model.stan',
            data = expert_dat, 
            iter = 100,
            chains = 1)
