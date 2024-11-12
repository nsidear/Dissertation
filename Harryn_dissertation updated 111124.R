install.packages("tidyverse")
install.packages("glmnet")
install.packages("corrplot")
install.packages("moments")
install.packages("ggpubr")
install.packages("ggplot2")
install.packages("dplyr") 
install.packages("caret")
install.packages("reshapes")
install.packages("grid")
install.packages("car")
install.packages("gridExtra")
install.packages("randomForest")
library(datasets)
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

df<- read_csv("C:/Users/nsika/Desktop/garments_worker_productivity updated.csv")
View(garments_worker_productivity_updated)



#================ Data Cleansing ===================

cat("\nThe initial and final entries in the dataset are as follows:")
# Missing data shown as NA
head(train)
# Below are the attributes of the last records with missing data shown as NA
tail(train)

# Data Cleansing (Removing NA) Total # of NA's is show below
cat("\nFundermental of the data is shown below:")

head(df)
str(df)
str(df$wip)
df_1<-as.numeric(df$wip)
mean(df$wip)
var(df$wip)
sd(df$wip)
is.na(df$wip)    
is.na(df$targeted_productivity)
is.na(df$actual_productivity)
is.na(df$over_time)
str(df_1)
print(df_1)
is.na(df_1)
na.omit(df_1)
df_1<-mean(df$df$wip, na.rm=TRUE
           

set.seed(2)
# Make test and train sets, dividing and rearranging them in the ratio 80:20
sample_size <- floor(0.8*nrow(garments_worker_productivity_updated))
train_index <- sample(seq_len(nrow(garments_worker_productivity_updated)),size = sample_size) #https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
train <- garments_worker_productivity_updated[train_index,]



# ============ Data preprocessing process ================

# Determine the number of records running train test
dim(train) # 957 records
dim(test) # 240 records


# =========== Data Data Integration ======================

cat("This productivity dataset includes", dim(train)[1], "records, that includes", dim(train)[2],
    "attributes. The structure is:\n\n")
# Population and sample - Data is based on existing data. Display the test structure and characteristics of the variables in the dataset
str(garments_worker_productivity_updated)

# Data Cleansing showing missing data - Breakdown Of dataset productivity structure
head(garments_worker_productivity_updated) # Breakdown Of dataset productivity structure with missing data



# ===============Feature Engineering =======================

sapply(df_1,class)
sapply(df, class)
head(df)

actual_productivity_cat <- df[, 2]
df <- cbind(df, actual_productivity_cat)
print(df$actual_productivity_cat)
print(df$actual_productivity)
na.omit(df$actual_productivity)
na.omit(df$actual_productivity_cat)
head(df)

targeted_productivity_cat <- df[, 2]
df <- cbind(df, targeted_productivity_cat)
print(df$targeted_productivity_cat)
print(df$targeted_productivity)
na.omit(df$targeted_productivity)
na.omit(df$targeted_productivity_cat)
head(df)

wip_cat <- df[, 2]
df <- cbind(df, wip_cat)
print(df$wip_cat)
print(df$wip)
na.omit(df$wip)
na.omit(df$wip_cat)
head(df)

smv_cat <- df[, 2]
df <- cbind(df, smv_cat)
print(df$smv_cat)
print(df$smv)
na.omit(df$smv)
na.omit(df$smv_cat)
head(df)

day_cat <- df[, 4]
df <- cbind(df, day_cat)
print(df$day)
head(df)




# ==== Data Clustering / Structuring (data modelling into tables)======


# Generating the box plots of focused variables excluding date, quarter, department, day and team
ggplot(gather(train[,-c(1:5)]), aes(key,value)) +
  geom_boxplot(bins=5,color='blue', fill='green') +
  facet_wrap(~key, scales="free") +
  labs(title = "Box plots of focused variables", subtitle ="Showing similarities, contrast of observed anomalies")


# ======== visualization of data using boxplot and histogram ========

str(garments_worker_productivity_updated)



#=======Data Exploration and Mining using boxplot and Hist of total productivity =======

boxplot(garments_worker_productivity_updated$no_of_workers)

# ==== Histogram of focused group distribution and anomalies ============
# Plot a histogram or bar chart of each variable except the first 5 categorical ones
ggplot(gather(train[,-c(1:5)]), aes(value)) +
  geom_histogram(bins=20,color='blue', fill='green') +
  facet_wrap(~key, scales = 'free') +
  labs(title = "Histograms of Focused Variables", subtitle ="showing similarities, contrast of observed anomalies")

# Generating the histogram of focused variables excluding date, quarter, department, day and team
hist(garments_worker_productivity_updated$no_of_workers)



# ========= Data Model Validation and Hyperparameter Tuning ==============

# =============== Root Mean Square Error =========================

# === Visualizing RMSE - Plot targeted_productivity vs actual_productivity ===
plot(garments_worker_productivity_updated$actual_productivity, garments_worker_productivity_updated$targeted_productivity, xlab = "actual_productivity", ylab = "targeted_productivity",
     main = "actual_productivity vs targeted_productivity")
abline(0, 1, col = "red") # Add a reference line

# == Determine the RSME of the targeted_productivity and actual_productivity ===
sqrt(garments_worker_productivity_updated$targeted_productivity)
sqrt(garments_worker_productivity_updated$actual_productivity)



# =========   Result of Train Test ===========================
cat("For every attribute, the number of distinct values is as shown below:")
apply(train, 2, function(x) length(unique(x)))

# ====== Determining the Variable Relationships with uncleaned data ==========
cor(train[,-c(1:5)])

# ======== trains test mean, median variables ====================
cat("\nBasic test result of cleaned variable attribute are:")
# The grouped result of the distributed variables are listed below 
summary(train)




# ============= Data Modeling Evaluation  ===================


# ====== Box plots of train test of wip against actual productivity ========
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


