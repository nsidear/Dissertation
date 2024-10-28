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
garments_worker_productivity_updated <- read_csv("C:/Users/nsika/Desktop/garments_worker_productivity updated.csv")
View(garments_worker_productivity_updated)
set.seed(2)
# Make test and train sets, dividing and rearranging them in the ratio 80:20
sample_size <- floor(0.8*nrow(garments_worker_productivity_updated))
train_index <- sample(seq_len(nrow(garments_worker_productivity_updated)),size = sample_size) #https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
train <- garments_worker_productivity_updated[train_index,]
test <- garments_worker_productivity_updated[-train_index,]

dim(train) # 957 records
dim(test) # 240 records

# Show the measurements
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

str(garments_worker_productivity_updated)
boxplot(garments_worker_productivity_updated$no_of_workers)
hist(garments_worker_productivity_updated$no_of_workers)

# Generating a histogram / bar chart of focused variables excluding date, quarter, department, day and team
ggplot(gather(train[,-c(1:5)]), aes(value)) +
  geom_histogram(bins=5,color='blue', fill='green') +
  facet_wrap(~key, scales = 'free') +
  labs(title = "Histograms of focused variables", subtitle ="Showing similarities, contrast of observed anomalies")

ggplots(train, aes(x=targeted_productivity,y=actual_productivity, color=department)) +
  geom_point() +   
  geom_smooth(method=lm,se=FALSE) +
  labs(title = "Scatterplot showing the relationship between actual productivity and targeted_productivity")









