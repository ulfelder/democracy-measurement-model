library(ggplot2)
library(optparse)
library(dplyr)

option_list <- list(
    make_option(c("--infile"), type="character", action="store",
        default='democracies_output.csv',
        dest="infile")
    ,make_option(c("--outfile"), type="character", action="store",
        dest="outfile")
    ## countries to plot
    ,make_option(c("--countries"), type="character",
        default = 'UKR,GRG,RUS,KYR,ARM,BLR',
        dest="countries")
    ,make_option(c("--start-year"), type="integer",
        default = 1990,
        dest="startyear")
    )
opt <- parse_args(OptionParser(option_list=option_list))
countries <- unlist(strsplit(opt$countries, ","))

democracy <- read.csv(opt$infile)

p <- democracy %>%
  filter(country %in% countries, year >= opt$startyear) %>%
  ggplot(aes(x = year, y = demo, ymin = lcl, ymax = ucl)) +
  geom_smooth(stat = 'identity') +
  facet_grid(country ~ .) +
  theme_bw()

ggsave(opt$outfile, plot = p)
