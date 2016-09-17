#author: Alankar D. Singh
#author: Paridhi Srivastava
#author: Prajesh Jhumkhawala


library(car)
library(caret)
library(nnet)
library(devtools)
library(e1071)
library(rpart)
library(foreign)
library(ggplot2)
library(corrplot)
library(tree)
library(stringr)
library(party)
library(plotrix)
library(kernlab)

se#read the crime and offense dataset
crimedataset <-  read.csv("crime.csv", header = TRUE)
offensedataset <-  read.csv("offensedataset.csv", header = TRUE)

#remove the extra columns
dataset <- crimedataset[,3:11]

#factorize the target attribute
dataset$DISTRICT_ID <- as.factor(crimedataset$DISTRICT_ID)
dataset$NEIGHBORHOOD_ID <- as.factor(crimedataset$NEIGHBORHOOD_ID)

#plot the correlation matrix
corrmatrix <- cor(crimedataset[sapply(crimedataset, is.numeric)], method = 'spearman')
datasubsets <- findCorrelation(corrmatrix, cutoff = .20, verbose = FALSE,
                               names = FALSE)
dataset <- crimedataset[c(datasubsets)]
cols <- names(dataset)
rmatrix <- cor(dataset[sapply(dataset, is.numeric)], method = 'spearman')
corrplot(rmatrix)



#split the data into testing and training data using year
testset <- subset(dataset, FIRST_OCCURRENCE__year== "2016")
trainset <- subset(dataset, FIRST_OCCURRENCE__year!= "2016")


#normalize the data
normdata <- preProcess(trainset, method = c("center", "scale"))
normtrainset <-  predict(normdata, trainset)
normtestset <-  predict(normdata, testset)


#draw the neural network model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
fitgrid <- expand.grid(.decay = c(1.0, 0.5), .size = c(1, 2, 3, 4, 5, 6, 7))
modelfit <- train(DISTRICT_ID ~ ., data = normtrainset,
                  method = "nnet", maxit = 1000, tuneGrid = fitgrid, trControl= control, trace = F, linout = 1)    


#predict the target variable in testset
pred <- predict(modelfit, newdata = normtestset[,1:11],type=("class"))


#draw the confusion matrix of the model
cmatrix <- confusionMatrix(as.factor(pred), as.factor(normtestset$DISTRICT_ID))


#plot the neural network
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
plot.nnet(model, circle.col = list('lightgray', 'white'), bord.col = 'black')
dev.off()




#support vector machine
svm.model <- svm(DISTRICT_ID~., crimedataset)
svm.pred <- predict(svm.model, subset(testset, select=-c(DISTRICT_ID)))
confusionMatrix(as.factor(svm.pred), as.factor(testset$DISTRICT_ID))




#Creating a decision tree
crimedataset <- crimedataset[complete.cases(crimedataset),]
iri<-ctree(as.factor(DISTRICT_ID)~OFFENSE_CODE__details+NEIGHBORHOOD_ID, data=trainset )
print(iri )
plot(iri )
prediction <- predict(iri, subset(testset, select=-c(DISTRICT_ID)))
confusionMatrix(as.factor(prediction), as.factor(testset$DISTRICT_ID))




#Naive Bayesian
naivebayes <- naiveBayes(DISTRICT_ID~.,data = trainset)
summary(naivebayes)
nb_prediction<- predict(naivebayes,subset(testset, select=-c(DISTRICT_ID)))
#confusion matrix
table(pred=nb_prediction, true = testset$DISTRICT_ID)
confusionMatrix(as.factor(nb_prediction), as.factor(testset$DISTRICT_ID))

