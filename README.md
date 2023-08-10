# Week_6_Assignment
SVM + K Means + C50

SVM Summary: Classifies iris flower species based on their measurements using SVM. Basically, it depicts the data and the SVM approach, predicts labels, and estimates the misclassification rate of the iris flowers. 

K Means Summary: It shows how efficiently the algorithm grouped comparable data points based on their feature values. The main purpose was to identify the three distinct species of iris flowers.

C50 Summary: It shows how a decision tree algorithm may be used to create decision trees from a range of inputs. The main purpose was to identify the three distinct species of iris flowers.

SVM accuracy: The misclassification rate was 2.66%, meaning that the accuracy rate was high at 97.3%

K Means accuracy: The misclassification rate was 11.4%, meaning that the accuracy rate was high at 88.6%

C50 accuracy: The misclassification rate was 4.44%, meaning that the accuracy rate was high at 95.56%

#The following are the actual codes:
SVM(line 12-84):
#e1071 will be used for Support Vector Classification.
install.packages("e1071")
install.packages("GGally")
install.packages("ggplot2")

library(e1071)
library(GGally)
library(ggplot2)

#get the dataset
data(iris)

#explore the data
str(iris)
head(iris,3)

#Create the SVM model
svm_model <- svm(Species ~ ., data=iris,
                 kernel="radial") #linear/polynomial/sigmoid

#Lets have a closer look at the parameters 
#and judge before hand if a good model can be created or not.
ggpairs(iris, ggplot2::aes(colour = Species, alpha = 0.4))

#We can clearly see from the Histograms of Petal.length 
#and Petal.width that we can clearly seperate out Setosa species with very high confidence.

#However, Versicolor and Virginica Species are overlapped. 
#If we look at the scatterplot of Sepal.Length vs Petal.Length 
#and Petal.Width vs Petal.Length, 
#we can distintly see a seperator that can be draw between the groups of Species.

#Looks like we can just use Petal.Width and Petal.Length as parameters 
#and come with a good model. SVM seems to be a very good model for this type of data.

plot(svm_model, data=iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4) 
)

#from the graph you can see data, support vector(represented by cross sign) 
#and decision boundry, belong to 3 types of species

#White color represented predicted class for second species(versicolor)

#Pink color represented predicted class for third species(virginica)

#Also we have 52 Support vector, 
#8 of them belongs to first species
#(You can see 8 cross in first class), 
#22 of them belongs to second species, 
#21 of them belongs to third species.

#Predict each Species
#Confusion matrix and missclassification error
pred = predict(svm_model,iris)
tab = table(Predicted=pred, Actual = iris$Species)
tab

#Get missclassification rate
1-sum(diag(tab)/sum(tab))

#How did the model do?
#(AT comment) The model preformed exceptionally well given that it was able
#to correctly predict the species of flowers 97.34% of the time. It did 
#have errors though. For the versicolor section, it correctly predicted 48  
#versicolor, but incorrectly predicted 2 virginica. For the virginica section, 
#it correctly predicted 48  virginica, but incorrectly predicted 2 versicolor.

#What is the accuracy rate?
#(AT comment)The missclassification rate gave 2.66%, meaning that the accuracy 
#rate was high at 97.3%
(50+48+48)/150


K Means (line 87 - 141)
#K-Means Clustering

require("datasets")
data("iris") # load Iris Dataset
str(iris) #view structure of dataset

summary(iris) #view statistical summary of dataset

head(iris, 3) #view top  rows of dataset

#Preprocess the dataset
#Since clustering is a type of Unsupervised Learning, 
#we would not require Class Label(output) during execution of our algorithm. 
#We will, therefore, remove Class Attribute “Species” and store it in another variable. 
#We would then normalize the attributes between 0 and 1 using our own function.

iris.new<- iris[,c(1,2,3,4)]
iris.class<- iris[,"Species"]
head(iris.new, 3)

head(iris.class, 3)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

iris.new$Sepal.Length<- normalize(iris.new$Sepal.Length)
iris.new$Sepal.Width<- normalize(iris.new$Sepal.Width)
iris.new$Petal.Length<- normalize(iris.new$Petal.Length)
iris.new$Petal.Width<- normalize(iris.new$Petal.Width)
head(iris.new)

#Apply the K-means clustering algorithm
result<- kmeans(iris.new,3) #aplly k-means algorithm with no. of centroids(k)=3
result$size # gives no. of records in each cluster

result$centers # gives value of cluster center datapoint value(3 centers for k=3)

result$cluster #gives cluster vector showing the custer where each record falls

#Verify results of clustering
par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(iris.new[c(1,2)], col=result$cluster)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(iris.new[c(1,2)], col=iris.class)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed originally as per "class" attribute in dataset
plot(iris.new[c(3,4)], col=result$cluster)# Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(iris.new[c(3,4)], col=iris.class)

table(result$cluster,iris.class)

#Results of the table show that Cluster 1 corresponds to Virginica, 
#Cluster 2 corresponds to Versicolor 
#and Cluster 3 to Setosa.

#Total number of correctly classified instances are: 36 + 47 + 50= 133
#Total number of incorrectly classified instances are: 3 + 14= 17

#How did the model do?

#TASK: Accuracy = number of correctly classified/(total classified) = ?
#i.e our model has achieved ?% accuracy!
#(AT Comment)The missclassification rate gave 11.4%, meaning that the accuracy 
#rate was high at 88.6%
133 / (133 + 17)


C5.0 (line 154 - 224)
#The C5.0 algorithm
#First install these packages
install.packages("C50")
install.packages("dplyr")
library(dplyr)

#get the iris dataset (for more info https://en.wikipedia.org/wiki/Iris_flower_data_set)
iris

head(iris,4)
dim(iris)

#we need to divide our data into training data and test data. 
#C5.0 is a classifier, so you’ll be teaching it how to classify the different species of irises using the training data.
library(C50)

#Splitting data set into traing and testing.
#Splitting data based on the species
iris_setosa <- iris[iris$Species == "setosa", ]
iris_versicolor <- iris[iris$Species == "versicolor",]
iris_virginica <- iris[iris$Species == "virginica",]

#splitting data sequentially *optional
iris_train <- rbind(iris_setosa[1:35,], iris_versicolor[1:35,], iris_virginica[1:35,])
iris_test <- rbind(iris_setosa[36:50,], iris_versicolor[36:50,], iris_virginica[36:50,])

#spliting randomly
#install caret lib which is used to split the dataset
install.packages("caret")
library(caret)

install.packages("lattice")
install.packages("ggplot2")

library(lattice)
library(ggplot2)

attach(iris)
inTrainingData<-createDataPartition(y=Species,p=0.70,list=FALSE)
trainData <- iris[inTrainingData,]
testData <- iris[-inTrainingData,]

#Build the model on the training data
dtModel <- C5.0(trainData[,-5], trainData$Species)
plot(dtModel)

#Checking accuracy of the training data model
#The predict() function takes your model, the test data 
#and one parameter that tells it to guess the class 
#(in this case, the model indicates species).
predict(dtModel, testData)

summary(testData)

(testData$Species == predict(dtModel, testData))

mean(testData$Species == predict(dtModel, testData))

#cross table
install.packages("gmodels")
library(gmodels)
CrossTable(testData$Species,predict(dtModel, testData))

CrossTable(testData$Species == predict(dtModel, testData))

#How did the model do? 
#TASK: What percent of cases were correctly classified?
#(AT Comment)The missclassification rate gave 4.44%, meaning that the accuracy 
#rate was high at 95.56%
2/45
43/45
