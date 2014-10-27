library(ggplot2)
library(optparse)
library(dplyr)

option_list <- list(
    make_option(c("--infile"), type="character", action="store",
        default='data.out/iid_expert_scores.csv',
        dest="infile")
    ,make_option(c("--outfile"), type="character", action="store",
        dest="outfile")
    )
opt <- parse_args(OptionParser(option_list=option_list))

experts <- read.csv(opt$infile)

p <- ggplot(experts, aes(x = expert, y = mean, ymin = mean - 1.96*se, max = mean + 1.96*se)) +
  geom_pointrange() +
  facet_grid(value ~ ., scales = 'free_y') +
  theme_bw()

ggsave(opt$outfile, plot = p)
