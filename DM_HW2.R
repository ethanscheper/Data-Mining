# Import libraries
library(datasets)
library(arules)
library(arulesViz)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(TH.data)
library(ISLR2)
library(lattice)
library(stats)
library(rattle)
library(RColorBrewer)
library(caret)
library(ROCR)
library(tidyverse)  
library(cluster)  
library(factoextra) 
library(gridExtra)
library(NbClust)
library(dendextend)
library(class)
library(ClustOfVar)
library(MASS)
library(kableExtra)
library(partykit)
library(dbscan)

# Read in the data
df <- read.csv("https://raw.githubusercontent.com/sjsimmo2/DataMining-Fall/master/TelcoChurn.csv")

# Split into training and testing
set.seed(123)
perm <- sample(1:nrow(df))
df_random <- df[perm,]
train <- df_random[1:floor(.8*nrow(df_random)),]
test <- df_random[(floor(.8*nrow(df_random)) + 1):nrow(df_random),]
# 5634 in train, 1409 in test

# Remove the customerID variable (we won't want to split on that)
train <- train %>%
  dplyr::select(-customerID)
test <- test %>%
  dplyr::select(-customerID)

# Create a tree using gini
churn_tree = rpart(Churn ~ ., data=train, method='class', 
                   parms = list(split='gini')) ## or 'information'
summary(churn_tree)
print(churn_tree)
rpart.plot(churn_tree)

# Make predictions
tscores = predict(churn_tree,type='class')
scores = predict(churn_tree, test, type='class')

# Training misclassification rate
sum(tscores!=train$Churn)/nrow(train)

# Test misclassification rate
sum(scores!=test$Churn)/nrow(test)





