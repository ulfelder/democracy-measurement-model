
library(rstan)
library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)
library(optparse)

option_list <- list(
    ## data
    make_option(c("--model"), type="character", action="store",
        dest="model")
    ,make_option(c("--outfile"), type="character", action="store",
        dest="outfile")
    )
opt <- parse_args(OptionParser(option_list=option_list))

load(file = paste('cache/', opt$model, 'fit.RData', sep=''))

s <- summary(fit)

df <- data.frame(s$summary) %>%
  mutate(variable = rownames(s$summary),
         name = str_extract(variable, '\\w+'),
         row = as.numeric(str_match(variable, '\\[(\\d+)')[,2]),
         col = as.numeric(str_match(variable, ',(\\d+)')[,2]))

df %>%
  group_by(name) %>%
  summarise(n = n(),
            p00 = min(Rhat),
            p25 = quantile(Rhat, .25),
            p50 = quantile(Rhat, .5),
            p75 = quantile(Rhat, .75),
            p100 = max(Rhat))
           
