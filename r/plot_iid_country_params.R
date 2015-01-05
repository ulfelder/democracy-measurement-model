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
    ,make_option(c("--outfile"), type="character", action="store",
        dest="outfile")
    ## countries to plot
    ,make_option(c("--countries"), type="character",
        default = 'UKR,GRG,RUS,KYR,ARM,BLR',
        dest="countries")
    )

opt <- parse_args(OptionParser(option_list=option_list))
D <- fread(opt$infile)
countries <- levels(factor(D$sftgcode))
toplot <- unlist(strsplit(opt$countries, ","))

## Creates the "fit" objects from which we extract these params
load('cache/iid_countryfit.RData')

country.mean <- melt(extract(fit, 'country_mean')[['country_mean']]) %>%
  select(rep = iterations,
         country = Var2,
         value) %>%
  mutate(country = factor(country, labels = countries),
         statistic = 'mean')

country.var <- melt(extract(fit, 'country_var')[['country_var']]) %>%
  select(rep = iterations,
         country = Var2,
         value) %>%
  mutate(country = factor(country, labels = countries),
         statistic = 'variance')

p <- rbind(country.mean, country.var) %>%
  group_by(country, statistic) %>%
  summarise(mean = pmin(pmax(mean(value), -10), 10),
            se = sd(value)) %>%
  filter(country %in% toplot) %>%
  ggplot(aes(x = reorder(country, mean), y = mean, ymin = mean - 1.96*se, ymax = mean + 1.96*se)) +
  geom_pointrange() +
  geom_hline(aes(yintercept = z), linetype = 'dashed', data.frame(z = c(0), statistic = c('mean'))) +
  xlab('Country') +
  ylab('Posterior Distribution') +
  facet_grid(statistic ~ ., scales = 'free_y') +
  theme_bw()

ggsave(opt$outfile, plot = p, width = 6, height = 4)
