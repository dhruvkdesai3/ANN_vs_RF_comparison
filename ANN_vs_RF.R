#Neural Net Project
setwd('C:/Users/Dhruv Desai/Documents/Data')
df <- read.csv('bank_note_data.csv')
head(df)

str(df)

#Train Test Split
#Use the caTools library to split the data into training (80%) and testing (20%) sets.

library(caTools)
set.seed(101)
split = sample.split(df$Class, SplitRatio = 0.80)

train = subset(df, split == TRUE)
test = subset(df, split == FALSE)


str(train)

#Building the Neural Net
#Call the neuralnet library


library(neuralnet)

#Training the Model
#Use the neuralnet function to train a neural net, set linear.output=FALSe and choose 10 hidden neurons (hidden=10)

nn <- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train,hidden=10,linear.output=FALSE)

#Predictions
#Use compute() to grab predictions useing your nn model on the test set

predicted.nn.values <- compute(nn,test[,1:4])
#Check the head of the predicted values. They are still probabilities.

head(predicted.nn.values$net.result)
#Apply the round function to the predicted values so you only 0s and 1s as your predicted classes.

predictions <- sapply(predicted.nn.values$net.result,round)
head(predictions)

#Use table() to create a confusion matrix of your predictions versus the real values

table(predictions,test$Class)


#Comparing ANN with RF


library(randomForest)
#randomForest needs it to be a factor, not an int like neural nets did. 
#Then re-do the train/test split

df$Class <- factor(df$Class)
library(caTools)
set.seed(101)
split = sample.split(df$Class, SplitRatio = 0.80)

train = subset(df, split == TRUE)
test = subset(df, split == FALSE)
#Create a randomForest model with the new adjusted training data.

model <- randomForest(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train)
#Use predict() to get the predicted values from your rf model.

rf.pred <- predict(model,test)


table(rf.pred,test$Class)
