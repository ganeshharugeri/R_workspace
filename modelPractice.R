# data_train <- iris[ trainIndex,]
# data_test <- iris[-trainIndex,]
# # train a naive bayes model
# model <- NaiveBayes(Species~., data=data_train)
# # make predictions
# x_test <- data_test[,1:4]
# y_test <- data_test[,5]
# predictions <- predict(model, x_test)
# # summarize results
# confusionMatrix(predictions$class, y_test) 


trainData=complete_data_formatted1[1:3700,]
testData = complete_data_formatted1[3701:3900,]
x_test=testData[, 1:19]
y_test=testData[,20]


rfmodel <- randomForest(classlabel ~.,data=trainData,importance=TRUE,ntree= 35)
#Check variable importance
?varImpPlot
varImpPlot(rfmodel,sort=T,n.var=10,main = "Variable importance")


#Check for the error rate based on the no of ntrees
plot(rfmodel)

#predTest<- predict(rfmodel,x_test)

confusionMatrix(data=x_test$pt,reference=testData$classlabel,positive='yes.')

table(trainData$classlabel,x_test$pt)

unique(trainData$classlabel)
unique(x_test$pt)

typeof(x_test$pt)
str(trainData$classlabel)

length(x_test$pt)
length(trainData$classlabel)

length(pred)
length(complete_data_vaild$classlabel)


?table

summary(x_test$predTest)
summary(trainData$classlabel)


summary(x_test)
summary(trainData)
str(x_test$pt)
str(trainData)


table(trainData$classlabel)
table(x_test$predTest)

which(is.na(x_test$predTest))
which(is.na(trainData$classlabel))
which(trainData$classlabel=='')
which(x_test$predTest=='')




print(seq(1,2, by=0.01))

v1 <- c(3,8,4,5,0,11)
v2 <- c(4,11,0,8,1,2)

# Vector addition.
add.result <- v1+v2
print(add.result)


df_Example= data.frame(
  sno=c(1:3),
  year=c(2015:2017),
  sem=c("year1","year2","year3"),
  impSubs = c("DataScience","Project","Thesis"),
  stringsAsFactors = 0
)


## Subset test 
subsetTest <- subset(test,Pclass==3)
