library(ggplot2)
library(optparse)
library(plyr)
library(dplyr)

option_list <- list(
    make_option(c("--infile"), type="character", action="store",
        dest="infile")
    ,make_option(c("--outfile"), type="character", action="store",
        dest="outfile")
    )
opt <- parse_args(OptionParser(option_list=option_list))


D <- read.csv(opt$infile)

p <- D %>%
  filter(year >= 1955, year <= 2008) %>%
  ggplot(aes(x = demo)) +
  geom_histogram(binwidth = 0.1) +
  theme_bw() +
  xlab('Probability of Democracy') +
  ylab('Frequency')
ggsave(opt$outfile, plot = p)
