install.packages("tidyverse")
install.packages("glmnet")
install.packages("corrplot")
install.packages("moments")
install.packages("ggpubr")
install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
install.packages("dplyr") 
install.packages("caret")
install.packages("reshapes")
install.packages("grid")
install.packages("car")
install.packages("gridExtra")
install.packages("forecast")
install.packages("xgboost")
install.packages("modeltime")
install.packages("libridate")
install.packages("timetk")
install.packages("fpp3")
install.packages("Metrics")

library(dplyr) 
library(knitr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(glmnet)
library(caret)
library(car)
library(grid)
library(gridExtra)
library(corrplot
library(moments)
library(readr)
library(tseries)
library(xgboost)
library(modeltime)
library(lubridate)
library(timetk)
library(fpp3)
library(Metrics)

# importing dataset as csv
garments_worker_productivity_updated <- read_csv("C:/Users/nsika/Desktop/garments_worker_productivity updated.csv")
View(garments_worker_productivity_updated)

set.seed(2)
# Make test and train sets, dividing and rearranging them in the ratio 80:20
sample_size <- floor(0.8*nrow(garments_worker_productivity_updated))
train_index <- sample(seq_len(nrow(garments_worker_productivity_updated)),size = sample_size) #https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
train <- garments_worker_productivity_updated[train_index,]
test <- garments_worker_productivity_updated[-train_index,]

# Determining train test records
dim(train) # 957 records
dim(test) # 240 records

# Measurement information
cat("The garment productivity dataset has", dim(train)[1], "records, each with", dim(train)[2],
    "attributes. The structure is:\n\n")

# Display the structure of the test
str(garments_worker_productivity_updated)
head(garments_worker_productivity_updated)


cat("\nThe first few and last few records in the dataset are:")
# Below are attributes of the initial records
head(train)
# Below are the attributes of the last records
tail(train)

cat("\Below Are The Stathe Basic statistics for each attribute are:")
# Statistical summary 
summary(train)

cat("For every attribute, the number of distinct values is as shown below:")
apply(train, 2, function(x) length(unique(x)))

# Generating the box plots of focused variables excluding date, quarter, department, day and team
ggplot(gather(train[,-c(1:5)]), aes(key,value)) +
  geom_boxplot(bins=5,color='blue', fill='green') +
  facet_wrap(~key, scales="free") +
  labs(title = "Box plots of focused variables", subtitle ="Showing similarities, contrast of observed anomalies")

#visualization of data using boxplot and histogram
str(garments_worker_productivity_updated)
boxplot(garments_worker_productivity_updated$no_of_workers)
hist(garments_worker_productivity_updated$no_of_workers)

tseries_testing <- split_tseries$test

train %>% ggplot(aes(x=wip,y=actual_productivity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Box plots of train test of wip against actual productivity")

train %>% filter(wip < 1700 & wip != 0) %>% 
  ggplot(aes(x=wip,y=actual_productivity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of wip and actual_productivity without zeros and outliers")
a <- train %>% filter(wip < 17000 & wip != 0) %>% select(wip, actual_productivity)
cor(a)

train %>% ggplot(aes(x=targeted_productivity,y=actual_productivity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Box plots of train test of targeted_productivity against actual productivity")

train %>% filter(targeted_productivity < 0.8 & targeted_productivity != 0) %>% 
  ggplot(aes(x=targeted_productivity,y=actual_productivity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of targeted_productivity and actual_productivity without zeros and outliers")
a <- train %>% filter(targeted_productivity < 0.8 & targeted_productivity != 0) %>% select(targeted_productivity, actual_productivity)
cor(a)

# Determine the RSME of the targeted_productivity and actual_productivity
sqrt(garments_worker_productivity_updated$targeted_productivity)
sqrt(garments_worker_productivity_updated$actual_productivity)

# Visualizing RMSE - Plot targeted_productivity vs actual_productivity
plot(garments_worker_productivity_updated$actual_productivity, garments_worker_productivity_updated$targeted_productivity, xlab = "actual_productivity", ylab = "targeted_productivity",
     main = "actual_productivity vs targeted_productivity")
abline(0, 1, col = "red") # Add a reference line










