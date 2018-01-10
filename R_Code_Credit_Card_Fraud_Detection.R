#Importing libraries
library(caTools)
library(e1071)
library(rpart)
library(rpart.plot)
library(caret)
library(ROSE)

##Importing Data Set
Data <- read.csv("creditcard.csv",header=TRUE)

##Exploring Data
summary(Data)
#check for types of columns
str(Data)
##Checking Dependent Variable
table(Data$Class)
# This shows that the data is highly imbalanced, which is usual in case of fraud detection
# So to deal with imbalanced nature of dependent varibale we can use over-sampling or under sampling or others as mentioned in this research paper present on this URL
#https://www.researchgate.net/profile/Vaishali_Ganganwar/publication/292018027_An_overview_of_classification_algorithms_for_imbalanced_datasets/links/58c7707a458515478dc4c68b/An-overview-of-classification-algorithms-for-imbalanced-datasets.pdf

# IN case of imbalanced nature of dependent variable we will look for precision and recall, accuracy wont matter much, but balanced accuracy will be a parameter to look for in Confusion Matrix stats

#before performing under sampling lets convert the dependent variable to Factor
Data$Class = as.factor(Data$Class)
#We will use "Random Over Sampling Package(ROSE) package in R"
#The data generated from oversampling has repeated observations and data generated from undersampling looses important information from the original data. 
#Which leads to inaccuracies in the accuracy. To overcome these problems, ROSE can generate data synthetically as well. 
Data_balanced <- ROSE(Class ~ ., data = Data, seed = 1)$data
table(Data_balanced$Class)

# Now the data is balanced
set.seed(2)
split <- sample.split(Data_balanced, SplitRatio = 0.7)
train <- subset(Data_balanced, split == T)
test <- subset(Data_balanced, split == F)

model_GLM = glm(formula = Class ~ ., family = binomial, data = train)
test$predictions_GLM = predict(model_GLM, type = 'response', newdata = test[-31])
test$predictions_GLM = ifelse(test$predictions_GLM >0.5, 1, 0)
confusionMatrix(test$Class,test$predictions_GLM)
##We get 93.5% accuracy
