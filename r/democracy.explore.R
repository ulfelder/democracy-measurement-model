# DEMOCRACY SCORES EXPLORING
# 2015-02-04

# Clear the workspace
rm(list=ls(all=TRUE))

# Set working directory
setwd("c:/users/jay/documents/democracy-measurement-model/")

# Load required packages and functions
library(reshape)
library(DataCombine)
library(plyr)
library(scales)
wd <- getwd()
source(paste0(wd, "/r/countryyear.utilities.r"))

# Ingest the two versions and prep them for merging with country-year rack
auto <- read.csv(paste0(wd, "/data.out/autocorr_democracy_scores.csv"))
auto$country <- as.character(auto$country)
auto$X <- NULL
names(auto) <- c("year", "sftgcode", "auto.p", "auto.lcl", "auto.ucl")
nomod <- read.csv(paste0(wd, "/data.out/nomodel_democracy_scores.csv"))
nomod$country <- as.character(nomod$country)
nomod$X <- NULL
names(nomod) <- c("year", "sftgcode", "nomod.p", "nomod.lcl", "nomod.ucl")

# Create country-year rack limited to valid years with PITF codes for post-WWII
rack <- pitfcodeit(countryyearrackit(1945, 2013), "country")

# Merge all the scores with the rack
scores <- merge(rack, auto, all.x = TRUE)
scores <- merge(scores, nomod, all.x = TRUE)
scores <- MoveFront(scores, c("country", "sftgcode", "year"))
rm(rack)

# Load and add source data
votes <- read.csv(paste0(wd, "/data.in/democracies.csv"))
votes$country <- NULL
votes$sftgcode <- as.character(votes$sftgcode)
names(votes) <- c("sftgcode", "year", "d.pitf", "d.da", "d.fh", "d.bmr", "d.dd")
scores <- merge(scores, votes, all.x = TRUE)

# Create markers for years with votes from 5, 4, and 3 sources for easier subsetting
scores$all <- ifelse(is.na(scores$d.pitf)==FALSE & is.na(scores$d.da)==FALSE & is.na(scores$d.fh)==FALSE &
  is.na(scores$d.bmr)==FALSE & is.na(scores$d.dd)==FALSE, 1, 0)
scores$four <- ifelse(is.na(scores$d.pitf) + is.na(scores$d.da) + is.na(scores$d.fh) + is.na(scores$d.bmr) +
  is.na(scores$d.dd) <= 1, 1, 0)
scores$three <- ifelse(is.na(scores$d.pitf) + is.na(scores$d.da) + is.na(scores$d.fh) + is.na(scores$d.bmr) +
  is.na(scores$d.dd) <= 2, 1, 0)

# Add the Unified Democracy Scores
uds <- read.csv(paste0(wd, "/data.in/uds_summary.csv"))
uds$country <- as.character(uds$country)
uds <- pitfcodeit(uds, "country")
uds <- subset(uds, select = c(sftgcode, year, mean, sd, median, pct025, pct975))
names(uds) <- c(names(uds)[1:2], paste0("uds.", names(uds)[3:7]))
scores <- merge(scores, uds, all.x = TRUE)

# Write out that .csv
write.csv(scores, file = paste0(wd, "/data.out/democracy.scores.merged.csv"), row.names = FALSE)

###################
# PLOTS
###################

# SCATTER PLOTS TO COMPARE SERIES
png(paste0(wd, "/figs/demscores.version.compare.png"), width = 2.5, height = 8, bg = "white", units = "in", res = 150)
par(mfcol=c(3,1))
plot(scores$iid.p, scores$auto.p)
plot(scores$nomod.p, scores$auto.p)
plot(scores$iid.p, scores$nomod.p)
dev.off()

# HISTOGRAMS OF SERIES
pdf(paste0(wd, "/figs/demscores.histogram.autocorr.pdf"), width=4, height=4, bg='white')
par(mai=c(1,1,0.25,0.25))
par(cex.axis = 0.7, cex.main = 0.8)
hist(scores$auto.p[scores$four==1], breaks = seq(0,1,0.1),
  col = "gray50", border = "white", main = "", xlab = "Probability of Democracy", axes = FALSE)
axis(2, las = 2, tick = FALSE, line = -0.75)
axis(1, at = c(0, 0.5, 1), tick = FALSE, line = -0.75)
dev.off()

pdf(paste0(wd, "/figs/demscores.histogram.nomod.pdf"), width=4, height=4, bg='white')
par(mai=c(1,1,0.25,0.25))
par(cex.axis = 0.7, cex.main = 0.8)
hist(scores$nomod.p[scores$four==1], breaks = seq(0,1,0.1),
  col = "gray50", border = "white", main = "", xlab = "Probability of Democracy", axes = FALSE)
axis(2, las = 2, tick = FALSE, line = -0.75)
axis(1, at = c(0, 0.5, 1), tick = FALSE, line = -0.75)
dev.off()

pdf(paste0(wd, "/figs/demscores.histogram.iid.pdf"), width=4, height=4, bg='white')
par(mai=c(1,1,0.25,0.25))
par(cex.axis = 0.7, cex.main = 0.8)
hist(scores$iid.p[scores$four==1], breaks = seq(0,1,0.1),
  col = "gray50", border = "white", main = "", xlab = "Probability of Democracy", axes = FALSE)
axis(2, las = 2, tick = FALSE, line = -0.75)
axis(1, at = c(0, 0.5, 1), tick = FALSE, line = -0.75)
dev.off()

# DENSITY PLOT
d.auto <- with(scores, density(auto.p, from=0, to=1))
d.iid <- with(scores, density(iid.p))
d.nomod <- with(scores, density(nomod.p))
pdf(paste0(wd, "/figs/demscores.densities.pdf"), width=4, height=4, bg='white')
par(mai=c(1,1,0.25,0.25))
par(cex.axis = 0.7, cex.main = 0.8)
plot(d.auto, col="black", lwd=1, main="", xlab="Probability of Democracy", axes=FALSE)
axis(2, las = 2, tick = FALSE, line = -0.75)
axis(1, at = c(0, 0.5, 1), tick = FALSE, line = -0.75)
dev.off()

# GROUPED LINE PLOT FOR SOVIET SUCCESSOR STATES (4+ sources, no-model version)
# Get subset of data for Soviet successor states
scores.fsu <- subset(scores, (sftgcode == "RUS" | sftgcode == "LAT" | sftgcode == "LIT" | sftgcode == "EST" |
  sftgcode == "BLR" | sftgcode == "MLD" | sftgcode == "UKR" | sftgcode == "GRG" | sftgcode == "ARM" |
  sftgcode == "AZE" | sftgcode == "KZK" | sftgcode == "UZB" | sftgcode == "TKM" | sftgcode == "KYR" |
  sftgcode == "TAJ") & year >= 1991 & four == 1)
# Line plot with country labels for selected cases
pdf(paste0(wd, "/figs/demscores.lineplot.fsu.nomod.four.pdf"), width=6, height=4, bg='white')
par(mar = c(3,2,1,5))
par(cex.axis = 0.75)
par(xpd = NA) # To let text spill over the boundaries of the plot area
plot(scores.fsu$nomod.p[scores.fsu$sftgcode=="RUS"], type = "n", ylim = c(0,1), xlab = "", ylab = "", axes = FALSE)
segments(x0 = 1, y0 = 0, x1 = 18, y1 = 0, lwd = 0.5, col = "gray")
segments(x0 = 1, y0 = 0.5, x1 = 18, y1 = 0.5, lwd = 0.5, col = "gray")
segments(x0 = 1, y0 = 1, x1 = 18, y1 = 1, lwd = 0.5, col = "gray")
axis(2, at = c(0,0.5,1), labels = c("0", "0.50", "1"), las = 2, tick = FALSE, line = -1)
axis(1, at = c(1,5,10,15,18), labels = c(1991,1995,2000,2005,2008), las = 1, tick = FALSE, line = -1)
lines(scores.fsu$nomod.p[scores.fsu$sftgcode=="RUS"], lwd = 1.5, col = "red")
text(x = 18, y = scores.fsu$nomod.p[scores.fsu$sftgcode=="RUS" & scores.fsu$year==2008], "Russia", col = "red", pos = 4, cex = 0.75)
lines(scores.fsu$nomod.p[scores.fsu$sftgcode=="GRG"], lwd = 1.5, col = "forestgreen")
text(x = 18, y = scores.fsu$nomod.p[scores.fsu$sftgcode=="GRG" & scores.fsu$year==2008] + 0.06, "Georgia", col = "forestgreen", pos = 4, cex = 0.75)
lines(scores.fsu$nomod.p[scores.fsu$sftgcode=="ARM"], lwd = 1.5, col = "bisque4")
text(x = 18, y = scores.fsu$nomod.p[scores.fsu$sftgcode=="ARM" & scores.fsu$year==2008], "Armenia", col = "bisque4", pos = 4, cex = 0.75)
lines(scores.fsu$nomod.p[scores.fsu$sftgcode=="UKR"], lwd = 1.5, col = "dodgerblue")
text(x = 18, y = scores.fsu$nomod.p[scores.fsu$sftgcode=="UKR" & scores.fsu$year==2008] - 0.04, "Ukraine", col = "dodgerblue", pos = 4, cex = 0.75)
lines(scores.fsu$nomod.p[scores.fsu$sftgcode=="KYR"], lwd = 1.5, col = "purple4")
text(x = 18, y = scores.fsu$nomod.p[scores.fsu$sftgcode=="KYR" & scores.fsu$year==2008] - 0.03, "Kyrgyzstan", col = "purple4", pos = 4, cex = 0.75)
lines(scores.fsu$nomod.p[scores.fsu$sftgcode=="BLR"], lwd = 1.5, col = "deeppink3")
text(x = 18, y = scores.fsu$nomod.p[scores.fsu$sftgcode=="BLR" & scores.fsu$year==2008], "Belarus", col = "deeppink3", pos = 4, cex = 0.75)
lines(scores.fsu$nomod.p[scores.fsu$sftgcode=="MLD"], lwd = 1.5, col = "darkorange")
text(x = 18, y = scores.fsu$nomod.p[scores.fsu$sftgcode=="MLD" & scores.fsu$year==2008], "Moldova", col = "darkorange", pos = 4, cex = 0.75)
dev.off()

# MEAN PROBABILITY OF DEMOCRACY BY REGION AND YEAR, AUTOCORRELATION MODEL
# Create data frame with annual means by region
regionmean.auto <- ddply(subset(scores, year >= 1960 & year <= 2008), .(dosreg, year),
  summarize, mean = mean(auto.p, na.rm = TRUE))
# Plot the results
pdf(paste0(wd, "/figs/demscores.lineplot.mean.by.region.autocorr.pdf"), width=6, height=4, bg='white')
par(mar = c(3,2,1,5))
par(cex.axis = 0.5)
par(xpd = NA) # To let text spill over the boundaries of the plot area
plot(regionmean.auto$mean[regionmean.auto$dosreg=="Africa"], type = "n", ylim = c(0,1), xlab = "", ylab = "", axes = FALSE)
segments(x0 = 1, y0 = 0, x1 = length(unique(regionmean.auto$year)), y1 = 0, lwd = 0.5, col = "gray")
segments(x0 = 1, y0 = 0.5, x1 = length(unique(regionmean.auto$year)), y1 = 0.5, lwd = 0.5, col = "gray")
segments(x0 = 1, y0 = 1, x1 = length(unique(regionmean.auto$year)), y1 = 1, lwd = 0.5, col = "gray")
axis(2, at = c(0,0.5,1), labels = c("0", "0.50", "1"), las = 2, tick = FALSE, line = -1)
axis(1, at = c(1,6,11,16,21,26,31,36,41,46), labels = c(1960,1965,1970,1975,1980,1985,1990,1995,2000,2005), las = 1, tick = FALSE, line = -1)
lines(regionmean.auto$mean[regionmean.auto$dosreg=="Africa"], lwd = 1.5, col = "forestgreen")
text(x = length(unique(regionmean.auto$year)), y = regionmean.auto$mean[regionmean.auto$dosreg=="Africa" & regionmean.auto$year==max(regionmean.auto$year)],
  "Sub-Saharan Africa", col = "forestgreen", pos = 4, cex = 0.5)
lines(regionmean.auto$mean[regionmean.auto$dosreg=="Americas"], lwd = 1.5, col = "red")
text(x = length(unique(regionmean.auto$year)), y = regionmean.auto$mean[regionmean.auto$dosreg=="Americas" & regionmean.auto$year==max(regionmean.auto$year)],
  "Americas", col = "red", pos = 4, cex = 0.5)
lines(regionmean.auto$mean[regionmean.auto$dosreg=="East Asia & Pacific"], lwd = 1.5, col = "cornflowerblue")
text(x = length(unique(regionmean.auto$year)), y = regionmean.auto$mean[regionmean.auto$dosreg=="East Asia & Pacific" & regionmean.auto$year==max(regionmean.auto$year)],
  "East Asia & Pacific", col = "cornflowerblue", pos = 4, cex = 0.5)
lines(regionmean.auto$mean[regionmean.auto$dosreg=="Europe & Eurasia"], lwd = 1.5, col = "gray50")
text(x = length(unique(regionmean.auto$year)), y = regionmean.auto$mean[regionmean.auto$dosreg=="Europe & Eurasia" & regionmean.auto$year==max(regionmean.auto$year)],
  "Europe & Eurasia", col = "gray50", pos = 4, cex = 0.5)
lines(regionmean.auto$mean[regionmean.auto$dosreg=="Middle East & North Africa"], lwd = 1.5, col = "bisque4")
text(x = length(unique(regionmean.auto$year)), y = regionmean.auto$mean[regionmean.auto$dosreg=="Middle East & North Africa" & regionmean.auto$year==max(regionmean.auto$year)],
  "Middle East & N. Africa", col = "bisque4", pos = 4, cex = 0.5)
lines(regionmean.auto$mean[regionmean.auto$dosreg=="South & Central Asia"], lwd = 1.5, col = "darkorange")
text(x = length(unique(regionmean.auto$year)), y = regionmean.auto$mean[regionmean.auto$dosreg=="South & Central Asia" & regionmean.auto$year==max(regionmean.auto$year)],
  "South & Central Asia", col = "darkorange", pos = 4, cex = 0.5)
dev.off()

# COMPARISON TO UNIFIED DEMOCRACY SCORES (4+ sources, autocorrelation version)
pdf(paste0(wd, "/figs/demscores.auto.uds.compare.scatter.pdf"), width=4, height=4, bg='white')
par(mar = c(3,3,1,1))
par(cex.axis = 0.75, cex.lab = 0.8)
plot(x = scores$uds.mean[scores$four==1], y = scores$auto.p[scores$four==1],
  xlab="", ylab="", pch = 20, col = alpha("black", 0.25), axes = FALSE )
axis(1, at = seq(-2, 2, 1), las = 1, tick = FALSE, line = -1)
axis(2, at = seq(0, 1, 0.25), labels = c("0", ".25", ".5", ".75", "1"), las = 2, tick = FALSE, line = -1) 
mtext("Unified Democracy Score mean", side=1, line=1.5, cex=0.8)
mtext("Probability of democracy", side=2, line=1.5, cex=0.8) 
dev.off()
