#Analyze the historical data and determine the key drivers for admission.
getwd()
setwd(choose.dir())
getwd()
library(ggplot2)
library(dplyr)
library(plyr)
collegedata <- read.csv('College_admission.csv')
View(collegedata)
summary(collegedata)

#finding missing data
apply(is.na(collegedata),2,sum)

#Detecting and handling outliers
#outliers can be detected via the use of box plot if they exist in our datframe
par(mfrow = c(1,2))             
boxplot(collegedata$gpa, main = 'Grade Point Average', col = 'orange',sub=paste("Outlier rows: ", boxplot.stats(collegedata$gpa)$out))
boxplot(collegedata$gre, main = 'Graduate Record Exam Scores', col = 'green', sub=paste("Outlier rows: ", boxplot.stats(collegedata$gre)$out))
#To perform outlier treatment, we simply assign values closer to the median of each variable to the outlier fields.
#replacing the outlier in GPA
collegedata$gpa[collegedata['gpa'] == 2.26] <- 3.39
#checking to see if the outlier in index 290 in GPA has being replaced
collegedata$gpa[290]
#replacing the outlier in GRE
collegedata$gre[collegedata['gre'] == c(220, 300)] <- 560
#checking to see if the outlier values (220 and 300) has being replaced
collegedata$gre[collegedata['gre'] == c(220, 300)]

#retrieving the structure of the dataframe and changing datatypes
str(collegedata)
#changing certain numeric or integer data type to factor
collegedata$admit <- sapply(collegedata$admit, factor)
collegedata$ses <- sapply(collegedata$ses, factor)
collegedata$Gender_Male <- sapply(collegedata$Gender_Male, factor)
collegedata$Race <- sapply(collegedata$Race, factor)
collegedata$rank <- sapply(collegedata$rank, factor)
str(collegedata) # checking if the datatype changed accordingly


#checking if the data is normally distributed
#one way to check for normality is to examine if the mean, median and mode are the same.
summary(collegedata)
#another way to check for normality is to plot the normal distribution curve
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(collegedata$gpa), main="Density Plot: GPA", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(collegedata$gpa), 2)))  # density plot for 'GPA'
polygon(density(collegedata$gpa), col="orange")
plot(density(collegedata$gre), main="Density Plot: GRE", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(collegedata$gre), 2)))  # density plot for 'GRE'
polygon(density(collegedata$gre), col="green")

#normalizing the dataset using the normailze function from the BBmisc package
install.packages('BBmisc')
library(BBmisc)
normalized_data <- normalize(collegedata, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
View(normalized_data)

#applying variable reduction technique to identify significant variables
install.packages('FactoMineR') #installing package for PCA dimensionality reduction
library(FactoMineR)
pca_nncd <- PCA(normalized_data[c(2:3)], scale = TRUE)
summary(pca_nncd)

#applying logistic regression to determine factors influencing admission
install.packages('blorr')
library(blorr)
sample_split <- floor(.7 * nrow(normalized_data)) #splitting the dataset by 70:30 ratio, i.e., extracting 70% of the data for training...
set.seed(1) # ensuring randomized variabeles are fixed. In order words we want to obtain the same variable results anytime we run our model
training_data <- sample(seq_len(nrow(normalized_data)), size = sample_split) # creating the randomized sample training dataset of which it is made uo of 70% of the entire dataset
admit_train <- normalized_data[training_data, ] #inserting the taining_data variable as an argument in the overall dataset(Customer_data)
admit_test <- normalized_data[-training_data, ] #here we are assigning the rest of the test dataset to the churn test variable as can be seen using '-' symbol in front of the training_data
#using the glm method
admit_model <- glm(formula = admit ~ ., data = admit_train, family = binomial(link = 'logit'))
summary(admit_model)
print(admit_model)

#accessing the accuracy of our logistics model and running validation checks with Confusion matrix
library(caret)
#validation check using k-fold cross validation
a <- train(admit ~ ., data = normalized_data, method = 'glm', trControl = trainControl(method = 'cv', number = 10, verboseIter = TRUE ))
print(a)

#using the decision tree algorithm
library(rpart) #load the library rpart for tree classifier
#create a tree_model
admit_tree_model <- rpart(formula = admit ~ ., data = admit_train, method = 'class')
admit_tree_model
summary(admit_tree_model)
plotcp(admit_tree_model)
plot(admit_tree_model)
#to enhance the decision tree plot we use rattle
install.packages('rattle')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(admit_tree_model)
#to validate the tree model we use the printcp 
printcp(admit_tree_model)

#create an SVM model
library(e1071)
svm_admit <- svm(admit ~ ., admit_train) #creating the train set model to predict the factors affecting 'churn' of mobile subscribers and assigning it to the variable svm_churn
summary(svm_admit)
confusionMatrix(admit_train$admit, predict(svm_admit), positive = '1')


#assigning categories to the gpa scores
View(collegedata)
max(collegedata$gpa)
min(collegedata$gpa)
New_collegedata <- transform(collegedata, gpa_categories = ifelse(gpa <= 2.99, 'Low', ifelse(gpa <= 3.49,'Medium', 'High' )))
View(New_collegedata)
New_collegedata1 <- transform(New_collegedata, Probabilty_of_being_admitted = round(New_collegedata$gpa/max(New_collegedata$gpa), 2))
View(New_collegedata1)
#plot the admission probability curve with the new college data1
plot(New_collegedata1$gpa, New_collegedata1$Probabilty_of_being_admitted, main = 'Graph of Grade point against Admission ratio', type = 'p', xlab = 'Grade Point Average', ylab = '% probability of being admitted', ylim = c(.5,1))
