###############################
# Data Exploration
# The Trade Desk Data Presentation, 2019
# Author: Rachel Weber
# Created: 01-17-19
###############################

library(ggplot2)
library(tidyverse)
library(anytime)
library(lubridate)
library(magrittr)

dat <- read.table(file = "C:/Users/weberra/Documents/Thesis/DataForCandidate.txt", 
                  header = T, fill = T, row.names = NULL, na.strings = c("", "NA"))
############################ data cleaning #################################
# space exists between the data info, splitting it into 2 columns
# collapse columns into 1 and move colnames to appropriate locations
cols <- c("Organization", "LogEntryTime")
dat$entry_date_time <- apply(dat[,cols], 1, paste, collapse = " ")
dat$entry_date_time <- anytime(dat$entry_date_time)

dat <- dat[,-c(8:9)]
colnames(dat)[1:7] <- c("AdFormat", "BidFeedbackID", "Browser", "CreativeId",
                        "DeviceType", "Frequency","Organization")

# some IDs ended up in the price column, lets move them back
# first we need to move websites into the TDID column
dat$site <- case_when(grepl(".com$", dat$TDID) ~ dat$TDID,
                      grepl(".com$", dat$TDID) ~ dat$site)
# now move IDs from price to TDID
dat$TDID <- case_when(dat$WinningPriceCPMInBucks >= 1000 ~ factor(dat$WinningPriceCPMInBucks),
                      dat$WinningPriceCPMInBucks < 1000 ~ dat$TDID)
# finally remove unrealistic prices from winning price column
dat[dat$WinningPriceCPMInBucks >= 1000,]$WinningPriceCPMInBucks <- NA

dat$day <- day(dat$entry_date_time)
# temperature
dat[dat$TemperatureInCelsius == "(null)",]$TemperatureInCelsius <- NA
dat$TemperatureInCelsius <- as.numeric(as.character(dat$TemperatureInCelsius))

write.csv(dat, file = "C:/Users/weberra/Documents/Thesis/clean_data.csv")
dat <- read.csv(file = "C:/Users/weberra/Documents/Thesis/clean_data.csv")

############################## Looking for Interesting Patterns ###########################
ggplot(dat, aes(x = TemperatureInCelsius, fill = factor(Organization))) + geom_histogram(alpha = .5) + xlim(0,50)
  # that's an interesting trend...

quantile(dat[dat$Organization == 3,]$TemperatureInCelsius, na.rm = T)
# middle 50% exist between 19.5 and 23 degrees
quantile(dat[dat$Organization == 2,]$TemperatureInCelsius, na.rm = T)
# middle 50% between 0 and 5 degrees
quantile(dat[dat$Organization == 1,]$TemperatureInCelsius, na.rm = T)
# middle 50% between .4 and 3 degrees

# did org 3 pay different amounts between temps?
# I.E. in higher temps where they were shown more, did they pay less?
t.test(dat[dat$TemperatureInCelsius < 19.5 & dat$Organization == 3,]$WinningPriceCPMInBucks,
       dat[dat$TemperatureInCelsius >= 19.5 & dat$Organization == 3,]$WinningPriceCPMInBucks)
  # mean <19.5: 3.34
  # mean >=19.5: 3.28
  # insignificant p-value. paid roughly the same but was more successful in warmer weather

# what about the other 2 orgs?
t.test(dat[dat$TemperatureInCelsius < 5 & dat$Organization == 2,]$WinningPriceCPMInBucks,
       dat[dat$TemperatureInCelsius >= 5 & dat$Organization == 2,]$WinningPriceCPMInBucks)
  # why are these prices so much higher?

t.test(dat[dat$Organization == 3,]$TemperatureInCelsius,
       dat[dat$Organization != 3,]$TemperatureInCelsius)
  # there's clearly a difference here...
  # if it's warmer, org 3 should definitely bid


# unique people
length(unique(dat$TDID))
  # 24365 individuals
  # an average of 34 events per person

table(dat$Browser, dat$Organization)
table(dat$DeviceType, dat$Organization)
table(dat$site, dat$Organization)

######################## How to Predict Price? ##########################
m1 <- lm(WinningPriceCPMInBucks ~ Browser + TemperatureInCelsius + RenderingContext + OSFamily + DeviceType, data = dat)
  # this is a big data problem. There are so many observations, everything is significant

# but temperature remains really interesting
# let's see if temperature alone affects prices
m2 <- lm(WinningPriceCPMInBucks ~ TemperatureInCelsius, data = dat)
  # it doesn't

dat$Organization <- factor(dat$Organization)
# what about each organization?
m3 <- lm(WinningPriceCPMInBucks ~ Organization, data = dat)
  # organization alone doesn't affect cost...

# but makes Org 2 win over Org 1 at the same temperature?
# if the prices are the same, other factors are at play

o3_winner <- subset(dat, dat$Organization == 3)
o3_winner$site <- droplevels(o3_winner$site)
  # Organization 3 never has site documented when it wins an auction
