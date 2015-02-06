library(ggplot2)
library(optparse)
library(plyr)
library(dplyr)

option_list <- list(
    make_option(c("--models"), type="character", action="store",
        default='iid,iid_country,autocorr',
        dest="models")
    ,make_option(c("--outfile"), type="character", action="store",
        dest="outfile")
    )
opt <- parse_args(OptionParser(option_list=option_list))

Data <- ldply(unlist(strsplit(opt$models, ",")), function(model) {
  fn <- paste('data.out/', model, '_expert_scores.csv', sep='')
  cat('filename:', fn)
  df <- read.csv(fn)
  df$model <- model
  df
}) %>%
  mutate(model = ordered(model, levels = c('iid', 'iid_country', 'autocorr'), labels = c('No model', 'Country-level', 'Autocorrelation')),
         expert = factor(expert, levels = c('d.polity', 'd.jay', 'd.fh', 'd.mbr', 'd.ddr'), labels = c('PITF', 'DA', 'FH', 'BMR', 'DD')))

p <- ggplot(Data, aes(x = expert, y = mean, ymin = mean - 1.96*se, max = mean + 1.96*se)) +
  geom_pointrange() +
  geom_hline(aes(yintercept = z), linetype = 'dashed', data = data.frame(z = c(0), value = c('bias'))) +
  facet_grid(value ~ model, scales = 'free_y') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab('Parameter Value') +
  xlab('Data Source')

ggsave(opt$outfile, plot = p, height = 5, width = 8)
