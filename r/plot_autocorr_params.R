library(rstan)
library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)
library(optparse)

option_list <- list(
    make_option(c("--outfile"), type="character", action="store",
        dest="outfile")
    )

opt <- parse_args(OptionParser(option_list=option_list))

## Creates the "fit" objects from which we extract these params
load('cache/autocorrfit.RData')

df <- rbind(melt(extract(fit, 'country_var')[['country_var']]) %>%
            mutate(variable = 'Country'),
            melt(extract(fit, 'time_var')[['time_var']]) %>%
            mutate(variable = 'Time')) %>%
  group_by(variable) %>%
  summarise(mean = mean(value),
            se = sd(value))

p <- ggplot(df, aes(x = variable, y = mean, ymin = mean - 1.96*se, ymax = mean + 1.96*se)) +
  geom_pointrange() +
  ylab('Posterior Distribution') +
  xlab('Variance Parameter') +
  theme_bw()

ggsave(opt$outfile, plot = p, height = 3, width = 3)
