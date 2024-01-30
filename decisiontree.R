library(FSelector)
library(rpart)
library(rpart.plot)
library(caret)
library(tidyverse)
library(data.tree)
library(caTools)
library(ElemStatLearn)

#Load the data ------------------------------------------------------------
data <- read.csv("ship.csv")

#See column names of the data frame -------------------------------------
colnames(data)
#-------------------------------------------------------------------------

#Selecting columns of interest ---------------------------------------------

data |> mutate(Survived = as.factor(X2urvived), Pclass = as.numeric(Pclass), Age = as.numeric(Age)) |> select(Survived, Pclass, Sex, Age, sibsp) -> df
str(df)

#Split data into train and test --------------------------------------------
set.seed(123)
sample <- sample.split(df$Survived, SplitRatio = 0.7)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

#Train the decision tree --------------------------------------------------

tree <- rpart(Survived ~ ., data = train)

tree.survived.predict <- predict(tree, test, type = 'class')

#confusion matrix ---------------------------------------------------------
confusionMatrix(tree.survived.predict,test$Survived)


#Plot the tree ------------------------------------------------------------

prp(tree)
