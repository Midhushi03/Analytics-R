#Decision Tree- 
#Regression: Predicitng Continuous Dependent Var Values.
#Classification: Predicting Categorical Dependent Var Values.
#Pruning:It is a technique used in determining the size of the tree.

install.packages("rpart")
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")
#Read dataset----
data("iris")
data_iris <- iris
View(data_iris)

#explore iris data set structure by str() command.
str(iris)

#Create training dataset and testing dataset----
indexes = sample(150, 110)
iris_train = iris[indexes,]
iris_test = iris[-indexes,]
View(iris_train)
View(iris_test)

#Build and plot model----
#Classification Tree-
ctree = rpart(Species ~., data = iris_train, method = "class")
rpart.plot(ctree)
View(ctree)

#Regression Tree-
rtree = rpart(Sepal.Length ~., data = iris_train, method = "anova")
rpart.plot(rtree)
print(ctree)

#Predicitng:----
#Classification Tree: Predicting whether the species is "Setosa" or not,
iris_test$Prediction_Species = predict(ctree, iris_test)
iris_test$Species <- ifelse(iris_test$Species == "setosa", 1, 0)
View(iris_test)

#Tried to check accuracy by using confusion matrix
length(iris_test$Species)
length(iris_test$Prediction_Species[,2])
table_mat<-table(iris_test$Species,iris_test$Prediction_Species[,2])
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#Regression Tree: Predicting the lenth of Sepal,
iris_test$Prediction_Sepal.Length = predict(rtree, iris_test)
View(iris_test)
library(Metrics)
#checking accuracy using mape
mape(iris_test$Prediction_Sepal.Length,iris_test$Sepal.Length)

#Pruning----
tree_ms3 = rpart(Species~., iris_train,control = rpart.control(minsplit = 3))
tree_ms50 = rpart(Species~., iris_train, control = rpart.control(minsplit = 50))

par(mfcol = c(1, 2))
rpart.plot(tree_ms3, main = "minsplit=3")
rpart.plot(tree_ms50, main = "minsplit=50")