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
install.packages("glimpse")
install.packages("matrix")
install.packages("caTools")       
install.packages("tree")
install_github("reprtree")
install.packages("rpart")
install.packages("carseats")
install.packages("igraph")
install.packages("MASS")

library(tidyverse)
library(glmnet)
library(corrplot
library(moments)
library(ggpubr)
library(ggplot2)
library(dplyr) 
library(caret)
library(reshape2)
library(grid)
library(car)
library(gridExtra)
library(randomForest)
library(glimpse)
library(Matrix)
library(caTools)
library(tree)
library(reprtree)
library(rpart)
liibrary(carseats)
library(igraph)
library(MASS)

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
median(df$wip)
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
df_1<-median(df$df$wip, na.rm=TRUE

print("Position of missing values ")       
which(is.na(df))
print("Count of total missing values  ")
sum(is.na(df))
           
set.seed(2)
# Make test and train sets, dividing and rearranging them in the ratio 80:20
sample_size <- floor(0.8*nrow(garments_worker_productivity_updated))
train_index <- sample(seq_len(nrow(garments_worker_productivity_updated)),size = sample_size) #https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
train <- garments_worker_productivity_updated[train_index,]


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

# - Actual_productivity -
actual_productivity_cat <- df[, 2]
df <- cbind(df, actual_productivity_cat)
print(df$actual_productivity_cat)
print(df$actual_productivity)
na.omit(df$actual_productivity)
na.omit(df$actual_productivity_cat)
head(df)


zero_index = which(df$actual_productivity == 0) 
for(i in zero_index){ 
  item = df$actual_productivity[i] 
  df$actual_productivity[i] = median( 
    df$actual_productivity[df$actual_productivity == item],  
    na.rm = T 
  )  
} 

# - Target_productivity - 
targeted_productivity_cat <- df[, 2]
df <- cbind(df, targeted_productivity_cat)
print(df$targeted_productivity_cat)
print(df$targeted_productivity)
as.factor("targeted_productivity")
na.omit(df$targeted_productivity)
na.omit(df$targeted_productivity_cat)
head(df)

zero_index = which(df$targeted_productivity == 0) 
for(i in zero_index){ 
  item = df$targeted_productivity[i] 
  df$targeted_productivity[i] = median( 
    df$targeted_productivity[df$targeted_productivity == item],  
    na.rm = T 
  )  
} 

# - wip - 
wip_cat <- df[, 2]
df <- cbind(df, wip_cat)
print(df$wip_cat)
print(df$wip)
na.omit(df$wip)
na.omit(df$wip_cat)
head(df)

zero_index = which(df$wip == 0) 
for(i in zero_index){ 
  item = df$wip[i] 
  df$wip[i] = median( 
    df$wip[df$wip == item],  
    na.rm = T 
  )  
} 

# - smv - 
smv_cat <- df[, 2]
df <- cbind(df, smv_cat)
print(df$smv_cat)
print(df$smv)
na.omit(df$smv)
na.omit(df$smv_cat)
head(df)

zero_index = which(df$smv_cat == 0) 
for(i in zero_index){ 
  item = df$smv_cat[i] 
  df$smv_cat[i] = median( 
    df$smv_cat[df$smv_cat == item],  
    na.rm = T 
  )  
} 

# - day -
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


# ===== Lasso regression ============


# Determine the number of records running train test
dim(train) # 957 records
dim(test) # 240 records
train <- train[,1:(length(train)-3)]
str(train)
plot(train)
plot(tree$targeted_productivity)

# plot the tree
rpart.plot(iris_model2)
tree <-("tree.targeted_productivity")
function(formula, data, weights, subset,
         na.action = na.pass, control = tree.control(nobs, ...),
         method = "recursive.partition",
         split = c("deviance", "gini"),
         model = FALSE, x = FALSE, y = TRUE, wts = TRUE, ...)

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


lambdas <- 10^seq(2, -3, by = -.1)
# Lasso regression

x <- df.matrix(TrainingSet[, c("quarter", "department", "day", "team", "targeted_productivity", "standard_minute_value", 
                                 "work_in_progress", "over_time", "incentive", "idle_men", "no_of_style_change", 
                                 "no_of_workers")])
y <- TrainingSet$actual_productivity

lasso_mod = glmnet(x, y, alpha = 1, lambda = lambdas)

x <- data.matrix(TestingSet[, c("quarter", "department", "day", "team", "targeted_productivity", "standard_minute_value", 
                                "work_in_progress", "over_time", "incentive", "idle_men", "no_of_style_change", 
                                "no_of_workers")])
lasso_MAE <- mae(TestingSet$actual_productivity, predict(lasso_mod, newx = x))


rf <- randomForest(Species~., df=train, proximity=TRUE) print(rf)
Call:
  randomForest(formula = Species ~ ., data = train)
Type of random forest: classification
Number of trees: 500
No. of variables tried at each split: 2
OOB estimate of  error rate: 2.83%

model <- randomForest(Species ~ ., data=iris, importance=TRUE, ntree=500, mtry = 2, do.trace=100)

reprtree:::plot.getTree(model)

plot (tree.targeted_productivity)
text( tree.targeted_productivity , pretty =0)

tree <- read_csv("C:/Users/nsika/Desktop/garments_worker_productivity updated.csv")
View(garments_worker_productivity_updated)


# plot the tree
rpart.plot(iris_model2)
tree <-("tree.targeted_productivity")
  function(formula, data, weights, subset,
           na.action = na.pass, control = tree.control(nobs, ...),
           method = "recursive.partition",
           split = c("deviance", "gini"),
           model = FALSE, x = FALSE, y = TRUE, wts = TRUE, ...)
   
  
# - splitting dataset into training and test sets  - 
Test using mock-up data
head(df)
head(train)
head(train_index)
#training set dimension
dim.data.frame(train)

set.seed(1234) 
data <- iris
#training test dimension
dim.data.frame(test)

# We will eliminate the date attribute since it will not contribute to the construction of our models
train <- train[,-1]

# Change character attributes to factor before building our models
train[,1:3] <- as.data.frame(sapply(train[,1:3],as.factor))

# Order the day attribute 
train$day <- factor(train$day, levels = c("Monday", "Tuesday", "Wednesday","Thursday","Saturday","Sunday"))

str(train)

indexSet <- sample(2, nrow(df), replace = T, prob = c(0.8, 0.2))
train <- df[indexSet=1]
test <- df[indexSet=2]
indexSet


#make this example reproducible
set.seed(1)

#create ID column
df$id <- 1:nrow(df)

#use 80% of dataset as training set and 20% as test set 
train <- df %>% dplyr::sample_frac(0.80)
test  <- dplyr::anti_join(df, train, by = 'id')

help("glm")
mymodel <- glm(wip +date+quarter+department, data = train, team = 'binomial')
summary(mymodel)
    



