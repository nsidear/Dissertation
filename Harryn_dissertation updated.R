install.packages("tidyverse")
install.packages("glmnet")
install.packages("corrplot")
install.packages("moments")
install.packages("ggpubr")
install.packages("ggplot2")
install.packages("ggplot")
install.packages("dplyr") 
install.packages("caret")
install.packages("reshapes")
install.packages("grid")
install.packages("car")
install.packages("gridExtra")
install.packages("randomForest")
library(datasets
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
library("ggplot")
garments_worker_productivity_updated <- read_csv("C:/Users/nsika/Desktop/garments_worker_productivity updated.csv")
View(garments_worker_productivity_updated)

set.seed(2)
# Make test and train sets, dividing and rearranging them in the ratio 80:20
sample_size <- floor(0.8*nrow(garments_worker_productivity_updated))
train_index <- sample(seq_len(nrow(garments_worker_productivity_updated)),size = sample_size) #https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
train <- garments_worker_productivity_updated[train_index,]



# ============ Data Preparation process ================

# Determine the number of records running train test
dim(train) # 957 records
dim(test) # 240 records

# Data Discovery & Merging - Performing Train Test and Data Exploration to Understanding overall attributes of the data
# Show the measurements. The  total number of records and the attributes
cat("This productivity dataset includes", dim(train)[1], "records, that includes", dim(train)[2],
    "attributes. The structure is:\n\n")
# Population and sample - Data is based on existing data. Display the test structure and characteristics of the variables in the dataset
str(garments_worker_productivity_updated)

# Data Cleansing showing missing data - Breakdown Of dataset productivity structure
head(garments_worker_productivity_updated) # Breakdown Of dataset productivity structure with missing data


#================ Data Cleansing ===================

cat("\nThe initial and final entries in the dataset are as follows:")
# Missing data shown as NA
head(train)
# Below are the attributes of the last records with missing data shown as NA
tail(train)

# Data Cleansing (Removing NA) Total # of NA's is show below
cat("\nFundermental of the data is shown below:")
# Statistical summary 
summary(train)



#=========== Data Exploration Process =============


# ==== Data Clustering / Structuring (data modelling into tables)======

cat("For every attribute, the number of distinct values is as shown below:")
apply(train, 2, function(x) length(unique(x)))

# Generating the box plots of focused variables excluding date, quarter, department, day and team
ggplot(gather(train[,-c(1:5)]), aes(key,value)) +
  geom_boxplot(bins=5,color='blue', fill='green') +
  facet_wrap(~key, scales="free") +
  labs(title = "Box plots of focused variables", subtitle ="Showing similarities, contrast of observed anomalies")


# ======== visualization of data using boxplot and histogram ========


str(garments_worker_productivity_updated)

#=======Data Exploration using boxplot of total productivity =======
boxplot(garments_worker_productivity_updated$no_of_workers)

#=======Data Mining using Histogram of total productivity =======
hist(garments_worker_productivity_updated$no_of_workers)

# ==== Histogram of focused group distribution and anomalies ============
# Plot a histogram or bar chart of each variable except the first 5 categorical ones
ggplot(gather(train[,-c(1:5)]), aes(value)) +
  geom_histogram(bins=20,color='blue', fill='green') +
  facet_wrap(~key, scales = 'free') +
  labs(title = "Histograms of Focused Variables", subtitle ="showing similarities, contrast of observed anomalies")


# ========= Data Modeling Evaluation =================
train %>% ggplot(aes(x=wip,y=actual_productivity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Box plots of train test of wip against actual productivity")

# ================= Scatter Plot of total Productivity =========
train %>% filter(wip < 1700 & wip != 0) %>% 
  ggplot(aes(x=wip,y=actual_productivity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of wip and actual_productivity without zeros and outliers")
a <- train %>% filter(wip < 17000 & wip != 0) %>% select(wip, actual_productivity)
cor(a)

# ======== Train Test of productivity comparison =================
train %>% ggplot(aes(x=targeted_productivity,y=actual_productivity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Box plots of train test of targeted_productivity against actual productivity")

# ==== Scattered plot of productivity without zeros and outliners =======
train %>% filter(targeted_productivity < 0.8 & targeted_productivity != 0) %>% 
  ggplot(aes(x=targeted_productivity,y=actual_productivity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of targeted_productivity and actual_productivity without zeros and outliers")
a <- train %>% filter(targeted_productivity < 0.8 & targeted_productivity != 0) %>% select(targeted_productivity, actual_productivity)
cor(a)


# ======== Root Mean Square Error =========================


# == Determine the RSME of the targeted_productivity and actual_productivity ===
sqrt(garments_worker_productivity_updated$targeted_productivity)
sqrt(garments_worker_productivity_updated$actual_productivity)

# === Visualizing RMSE - Plot targeted_productivity vs actual_productivity ===
plot(garments_worker_productivity_updated$actual_productivity, garments_worker_productivity_updated$targeted_productivity, xlab = "actual_productivity", ylab = "targeted_productivity",
     main = "actual_productivity vs targeted_productivity")
abline(0, 1, col = "red") # Add a reference line

# Data Analysis and transformation (data modelling into graphical representations)
# Recalling claustered data
str(garments_worker_productivity_updated)

# Generating the box plots of focused variables excluding date, quarter, department, day and team
boxplot(garments_worker_productivity_updated$no_of_workers)

# Generating the histogram of focused variables excluding date, quarter, department, day and team
hist(garments_worker_productivity_updated$no_of_workers)


# ====== Determining the Variable Relationships ==========
cor(train[,-c(1:5)])


























