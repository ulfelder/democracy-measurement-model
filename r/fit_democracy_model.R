library(rstan)
library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)
library(optparse)

option_list <- list(
    ## data
    make_option(c("--infile"), type="character", action="store",
        default='democracies.csv',
        dest="infile")
    ## posteriors
    ,make_option(c("--outfile"), type="character", action="store",
        default='democracies_output.csv',
        dest="outfile")
    ## expert posteriors
    ,make_option(c("--hyperparams"), type="character", action="store",
        default='experts_output.csv',
        dest="hyperparams")
    ,make_option(c("--model"), type="character", action="store",
        dest="model")
    ,make_option(c("--draws"), type="integer", default=100,
        dest="draws")
    )
opt <- parse_args(OptionParser(option_list=option_list))

D <- fread(opt$infile)

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


fn <- paste('stan/', opt$model, '.stan', sep='')
fit <- stan(fn,
            data = expert_dat, 
            iter = opt$draws,
            chains = 1)

## hyper parameters
## country.var <- extract(fit, 'country_var')[['country_var']]
## time.var <- extract(fit, 'time_var')[['time_var']]

save(fit, file = paste('cache/', opt$model, 'fit.RData', sep=''))

## expert parameters
expert.biases <- melt(extract(fit, 'expert_bias')[['expert_bias']]) %>%
  select(rep = iterations, expert = Var2, value) %>%
  mutate(expert = factor(expert, labels = levels(melted$expert))) %>%
  group_by(expert) %>%
  summarise(mean = mean(value), se = sd(value)) %>%
  mutate(value = 'bias')

expert.vars <- melt(extract(fit, 'expert_var')[['expert_var']]) %>%
  select(rep = iterations, expert = Var2, value) %>%
  mutate(expert = factor(expert, labels = levels(melted$expert))) %>%
  group_by(expert) %>%
  summarise(mean = mean(value), se = sd(value)) %>%
  mutate(value = 'variance')

write.csv(rbind(expert.biases, expert.vars), opt$hyperparams)

## country democracy time series
democracy <- melt(extract(fit, 'democracy')[['democracy']]) %>%
  select(rep = iterations, year = Var3, country = Var2, value) %>%
  mutate(year = as.numeric(as.character(factor(year, labels = levels(melted$year)))),
         country = factor(country, labels = levels(melted$sftgcode))) %>%
  group_by(year, country) %>%
  summarise(demo = 1 / (1 + exp(-mean(value))),
            lcl = 1 / (1 + exp(-(mean(value) - 1.96*sd(value)))),
            ucl = 1 / (1 + exp(-(mean(value) + 1.96*sd(value)))))

write.csv(democracy, opt$outfile)

