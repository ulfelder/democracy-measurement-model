# Housekeeping
rm(list=ls(all=TRUE))
setwd("c:/users/jay/documents/democracy-measurement-model/")

# Load required packages and functions
library(XLConnect)
library(reshape2)
library(DataCombine)
library(plyr)
library(caret)
library(verification)
library(mgcv)
library(survival)
source("r/countryyear.utilities.r")

# Set seed to make sampling replicable
set.seed(709)

# Ingest democracy status data
Dem <- read.csv("data.out/democracy.scores.merged.csv", stringsAsFactors=FALSE)

# Make some categorical variables from autocorrelation scores
Dem$dem.probably <- ifelse(Dem$auto.p >= 0.5, 1, 0) # Dem if p > 0.5
Dem$dem.surely <- ifelse(Dem$auto.lcl >= 0.5, 1, 0) # Dem if lower bound of 95% CI > 0.5
Dem$autocracy <- ifelse(Dem$auto.ucl < 0.5, 1, 0)  # Next 3 together: autocracy if upper CI bound < 0.5
Dem$unclear <- ifelse(Dem$auto.lcl < 0.5 & Dem$auto.ucl > 0.5, 1, 0)  # Unclear if CI straddles 0.5
Dem$democracy <- ifelse(Dem$auto.lcl > 0.5, 1, 0)  # democracy is lower CI bound > 0.5

# Get coup event data
Coups <- read.csv("data.in/cou.csv", stringsAsFactors=FALSE)
# Make 1-yr lead of indicator for any attempts to use as DV
for (i in 1:dim(Coups)[1]) Coups$cou.a.d_1[i] <- Coups$cou.a.d[i+1]
Coups$cou.a.d_1 <- replace(Coups$cou.a.d_1, which(Coups$year==max(Coups$year) | Coups$year==Coups$yrdied), NA)

# Get and process Polity data
Polity <- readWorksheetFromFile("data.in/p4v2013.xls", sheet=1)
Polity <- Polity[,3:21]  # Lop off columns I won't use
Polity <- rename(Polity, c(scode = "sftgcode"))
Polity <- subset(Polity, year >= 1945, select=c("sftgcode", "year", "polity", "durable", "exrec", "parcomp", "xconst"))
# Change a few country codes to match PITF standard for merging
Polity$sftgcode[Polity$sftgcode=="SER"] <- "SRB"
Polity$sftgcode[Polity$sftgcode=="MNT"] <- "MNE"
Polity$sftgcode[Polity$sftgcode=="GMY"] <- "GER"
Polity$sftgcode[Polity$sftgcode=="SSU"] <- "SSD"
Polity$sftgcode[Polity$sftgcode=="SDN"] <- "SUD"
Polity$sftgcode[Polity$sftgcode=="USR"] <- "USS"
# Create country-year rack with proper year range and merge data into it
Polity <- merge(pitfcodeit(countryyearrackit(1945, 2013), "country"), Polity, all.x = TRUE)
# Fearon & Laitin regime type (autocracy, anocracy, democracy)
Polity$polcat[Polity$polity >= -10 & Polity$polity < -5] <- 1 
Polity$polcat[Polity$polity >= -5 & Polity$polity <= 5] <- 2
Polity$polcat[Polity$polity > 5] <- 3
Polity$polcat[Polity$polity == -66 | Polity$polity == -77 | Polity$polity == -88 ] <- 7
# Make dummy variables for those categories
for(level in unique(Polity[,"polcat"])[1:length(unique(Polity[,"polcat"]))-1]){
  Polity[paste("polcat", level, sep = ".")] <- ifelse(is.na(Polity[,"polcat"])==TRUE, NA, ifelse(Polity[,"polcat"]==level, 1, 0))
}
# Subset for cleaner merging
Polity <- subset(Polity, select=c(sftgcode, year, polity, durable, polcat.1, polcat.2, polcat.3, polcat.7))
# Treat Polity special codes as missing for modeling

# Get infant mortality data
IM <- read.csv("data.in/imr.csv", stringsAsFactors=FALSE)

# Merge them
AF <- merge(Dem, Coups, all.x=TRUE)
AF <- merge(AF, Polity, all.x=TRUE)
AF <- merge(AF, IM, all.x=TRUE)

# Make post-Cold War dummy
AF$postcw <- ifelse(AF$year >= 1991, 1, 0)

### CROSS-VALIDATION ###

# Trim data set to required variables with no missing values
varlist <- c("sftgcode", "year",  # IDs
  "auto.p", "auto.lcl", "auto.ucl", "dem.probably", "dem.surely", "autocracy", "unclear", "democracy",  # Dem status
  "all", "four", "three",  # Number of votes
  "xxxcimrln", "postcw",  # Infant mortality rate (normed to annual global median, logged)
  "cou.a.d_1", "cou.tries5d",  # Coups
  "polity", "durable", "polcat.1", "polcat.2", "polcat.3", "polcat.7")  # Polity
# Subset to years with at least three votes on democracy status
Valdat <- subset(AF, four==1, select = varlist)
# Listwise deletion now, so comparison is always apples to apples
Valdat <- na.omit(Valdat)
# Treat Polity special codes as missing for modeling
Valdat$polity[Valdat$polity < -10] <- NA

# Create folds for 10 iterations of k-fold CV
dv <- "cou.a.d_1"
Valdat$ik1 <- createFolds(y = as.factor(Valdat[,dv]), k = 5, list = FALSE)
Valdat$ik2 <- createFolds(y = as.factor(Valdat[,dv]), k = 5, list = FALSE)
Valdat$ik3 <- createFolds(y = as.factor(Valdat[,dv]), k = 5, list = FALSE)
Valdat$ik4 <- createFolds(y = as.factor(Valdat[,dv]), k = 5, list = FALSE)
Valdat$ik5 <- createFolds(y = as.factor(Valdat[,dv]), k = 5, list = FALSE)
Valdat$ik6 <- createFolds(y = as.factor(Valdat[,dv]), k = 5, list = FALSE)
Valdat$ik7 <- createFolds(y = as.factor(Valdat[,dv]), k = 5, list = FALSE)
Valdat$ik8 <- createFolds(y = as.factor(Valdat[,dv]), k = 5, list = FALSE)
Valdat$ik9 <- createFolds(y = as.factor(Valdat[,dv]), k = 5, list = FALSE)
Valdat$ik10 <- createFolds(y = as.factor(Valdat[,dv]), k = 5, list = FALSE)

# Create function to generate predicted probabilities by iteration and fold of k-fold CV
predit <- function(i, x) {
  var <- paste0("ik", as.character(i))
  train <- Valdat[ -which(Valdat[var] == x),]
  test <- Valdat[ which(Valdat[var] == x),]
  test$polcat.p <- predict(glm(cou.a.d_1 ~ xxxcimrln + cou.tries5d + postcw + polcat.2 + polcat.3 + polcat.7,
    family=binomial, data=train, na.action=na.exclude), newdata=test, type="response")
  test$polity.p <- predict(glm(cou.a.d_1 ~ xxxcimrln + cou.tries5d + postcw + polity + I(polity^2),
    family=binomial, data=train, na.action=na.exclude), newdata=test, type="response")
  test$demcat.p <- predict(glm(cou.a.d_1 ~ xxxcimrln + cou.tries5d + postcw + dem.probably,
    family=binomial, data=train, na.action=na.exclude), newdata=test, type="response")
  test$demtype.p <- predict(glm(cou.a.d_1 ~ xxxcimrln + cou.tries5d + postcw + autocracy + unclear,
    family= binomial, data=train, na.action=na.exclude), newdata=test, type="response")
  test$demscore.p <- predict(glm(cou.a.d_1 ~ xxxcimrln + cou.tries5d + postcw + auto.p + I(auto.p^2),
    family=binomial, data=train, na.action=na.exclude), newdata=test, type="response")
  test$iteration <- i
  test$k <- x
  out <- subset(test, select = c(sftgcode, year, cou.a.d_1, polcat.p, polity.p, demcat.p, demtype.p, demscore.p, iteration, k ))
  return(out)
}

out <- rbind(predit(1,1), predit(1,2), predit(1,3), predit(1,4), predit(1,5),
  predit(2,1), predit(2,2), predit(2,3), predit(2,4), predit(2,5),
  predit(3,1), predit(3,2), predit(3,3), predit(3,4), predit(3,5),
  predit(4,1), predit(4,2), predit(4,3), predit(4,4), predit(4,5),
  predit(5,1), predit(5,2), predit(5,3), predit(5,4), predit(5,5),
  predit(6,1), predit(6,2), predit(6,3), predit(6,4), predit(6,5),
  predit(7,1), predit(7,2), predit(7,3), predit(7,4), predit(7,5),
  predit(8,1), predit(8,2), predit(8,3), predit(8,4), predit(8,5),
  predit(9,1), predit(9,2), predit(9,3), predit(9,4), predit(9,5),
  predit(10,1), predit(10,2), predit(10,3), predit(10,4), predit(10,5))

# Melt results to make data tidy
out.melt <- melt(out, id = c("sftgcode", "year", "cou.a.d_1", "k", "iteration"), na.rm = FALSE)

# Get tables of accuracy stats by model
acc.by.iter <- ddply(out.melt, .(iteration, variable), summarise,
  brier = mean((cou.a.d_1 - value)^2),
  log = mean((cou.a.d_1 * log(value)) + ((1 - cou.a.d_1) * log(1 - value))),
  auc = roc.area(cou.a.d_1, value)$A )
acc.overall <- ddply(acc.by.iter, .(variable), summarise,
  brier = mean(brier),
  log = mean(log),
  auc = mean(auc))
acc.overall

# Inspect models estimated from whole data set
mod.polcat <- glm(cou.a.d_1 ~ xxxcimrln + cou.tries5d + postcw + polcat.1 + polcat.2 + polcat.7,
  family = binomial, data = AF, na.action = na.exclude)
mod.demtype <- glm(cou.a.d_1 ~ xxxcimrln + cou.tries5d + postcw + autocracy + unclear,
  family = binomial, data = AF, na.action = na.exclude)
mod.demprob <- glm(cou.a.d_1 ~ xxxcimrln + cou.tries5d + postcw + auto.p + I(auto.p^2),
  family = binomial, data = AF, na.action = na.exclude)
summary(mod.polcat)
summary(mod.demtype)
summary(mod.demprob)
