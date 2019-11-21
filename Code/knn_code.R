#####################################
# K Nearest Neighbors Model
# The Trade Desk Data Presentation
# Author: Rachel Weber
# Created: 1-20-19
####################################

library(tidyverse)
library(magrittr)
library(caret)
library(ggplot2)
library(wesanderson)

dat <- read.csv(file = "C:/Users/weberra/Documents/Thesis/clean_data.csv")

# condense browser levels
dat[dat$Browser == "(null)",]$Browser <- NA
dat$Browser <- droplevels(dat$Browser)
dat$Browser <- factor(dat$Browser,
                      levels = c("Chrome", "Edge", "Firefox", "InternetExplorer10", "InternetExplorer11", "InternetExplorer8",
                                 "InternetExplorer9", "Opera", "Other", "Safari", "Yandex"),
                      labels = c("Chrome", "Edge", "Firefox", "IE", "IE", "IE",
                                 "IE", "Opera", "Other", "Safari", "Other"))
dat$Browser <- droplevels(dat$Browser)
dat$Browser <- factor(dat$Browser)

# keep only variables I want to use for KNN
dat <- dat[,c("Browser", "DeviceType", "Organization", "OSFamily", "TemperatureInCelsius", "WinningPriceCPMInBucks")]


normalize <- function(x) {
  return ((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))) }

dat$temp_n <- normalize(dat$TemperatureInCelsius)

# need to make dummary variables for categorical outcomes
dat %<>% 
  mutate(chrome = ifelse(Browser == "Chrome", 1, 0)) %>% 
  mutate(edge = ifelse(Browser == "Edge", 1, 0)) %>% 
  mutate(firefox = ifelse(Browser == "Firefox", 1, 0)) %>%
  mutate(IE = ifelse(Browser == "IE", 1, 0)) %>%
  mutate(opera = ifelse(Browser == "Opera", 1, 0)) %>%
  mutate(other_browser = ifelse(Browser == "Other", 1, 0)) %>%
  mutate(safari = ifelse(Browser == "Safari", 1, 0))

dat %<>%
  mutate(tv = ifelse(DeviceType == "ConnectedTV", 1, 0)) %>% 
  mutate(mobile = ifelse(DeviceType == "Mobile", 1, 0)) %>% 
  mutate(other_device = ifelse(DeviceType == "Other", 1, 0)) %>% 
  mutate(pc = ifelse(DeviceType == "PC", 1, 0)) %>%
  mutate(tablet = ifelse(DeviceType == "Tablet", 1, 0))

dat %<>%
  mutate(windows = ifelse(OSFamily == "Windows", 1, 0)) %>% 
  mutate(osx = ifelse(OSFamily == "OSX", 1, 0)) %>% 
  mutate(ios = ifelse(OSFamily == "iOS", 1, 0)) %>% 
  mutate(android = ifelse(OSFamily == "Android", 1, 0)) %>% 
  mutate(linux = ifelse(OSFamily == "Linux", 1, 0)) %>% 
  mutate(windowsphone = ifelse(OSFamily == "WindowsPhone", 1, 0)) %>% 
  mutate(other_os = ifelse(OSFamily == "(Other)", 1, 0))

# make training and testing. I'll split 70 30
# in running the knn code 'other_os' and windows phone had no variation, so we're removing them
dat_c <- dat[-c(1,2,4,5,6,25:26)]
dat_c <- dat_c[complete.cases(dat_c),]
dat_c <- subset(dat_c, dat_c$Organization != "???")
dat_c$Organization <- droplevels(dat_c$Organization)

set.seed(3033)
intrain <- createDataPartition(y = dat_c$Organization, p= 0.7, list = FALSE)
training <- dat_c[intrain,]
testing <- dat_c[-intrain,]


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- train(Organization ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
  # settled on k = 5
plot(knn_fit)

test_pred <- predict(knn_fit, newdata = testing)
test_pred

test_outcome <- cbind(testing, test_pred)

# Visualization
confusionMatrix(test_pred, testing$Organization)


ggplot(test_outcome, aes(x = test_pred, y = Organization, color = Organization)) + 
  geom_point() + geom_jitter() + theme_minimal() +
  labs(x = "Model Prediction", y = "Organization", title = "Model Prediction Agreement with Organization Identity") +
  scale_color_manual(values = wes_palette(n = 3, name = "FantasticFox1"))
