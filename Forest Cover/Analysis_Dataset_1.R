#author: Prajesh Jhumkhawala
#author: Alankar D. Singh
#author: Paridhi Srivastava

#program to analyse the given dataset
library(ggplot2)
library(corrplot)
library(tree)
library(stringr)
library(party)
library(plotrix)
setwd('C:/RIT/Intro to Big data/Finals')
forestdataset <-  read.csv("dataset.csv", header = TRUE)

#represent the structure of the dataset 
str(forestdataset)

#Creating a pie chart to for the WIld Area
mytable <- table(forestdataset$Final_Wild_Area)
pct <- round(mytable/sum(mytable)*100)
cols=c("red","green","cyan","violet");
lbls <- paste(pct,sep=":") # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(mytable, labels = lbls, col=cols,main="Pie Chart of Wild Area")
legend("bottomright",c("Rawah","Neota","Comanche Peak","Cache la Poudre "), cex=1.4, fill=cols)

#Creating a Pie chart for the Cover Type
mytable=table(forestdataset$Cover_Type);
cols= c("red", "blue", "green", "pink", "orange", "yellow", "violet")
lab=c("Spruce/Fir","Lodgepole Pine","Ponderosa Pine","Cottonwood/Willow","Aspen","Douglas-fir","Krummholz")
pie3D(mytable, theta= 1, main = "Various Cover Type",col=cols,labels = lab)

#Creating a pie chart for the binned distance
mytable=table(forestdataset$Binned_Distance);
cols= c("red", "blue", "green", "pink", "orange", "yellow", "violet")
lab=c("","","","","","")
pie(mytable, main = "Average Binned Distance",labels = lab,col = cols)
legend("bottomright",c("0-200","200-400","400-600","600-800","800-1000","1000-1200","1200-1400"), cex=1.4, fill=cols)


#Plotting a bar graph fot the SOil_Type
counts <- table(forestdataset$Soil_Type)
barplot(counts, main="Soil Type Distribution",xlab="Soil Type",beside=TRUE, col ="darkgreen")


#relation between Soil Type and Wild Area
ggplot(forestdataset, aes(x = forestdataset$Soil_Type,fill = factor(forestdataset$Final_Wild_Area))) +
  stat_count(width = 0.5) + xlab("Soil Type") + ylab("Trees") + labs(fill ="Final WIld Area")

#splitting the dataset randomly into 70% training and 30% test dataset
set.seed(11111)

train_index <- sample(1:8144, nrow(forestdataset)*0.7)
trainset <- forestdataset[train_index,]
testset <- forestdataset[-train_index,]


#Creating and plotting the correlation matrix
corrmatrix <- cor(trainset[sapply(trainset, is.numeric)],method = "pearson")
corrplot(corrmatrix, method = "circle")


#Creating a decision tree
forestdataset <- forestdataset[complete.cases(forestdataset),]
iri<-ctree(as.factor(Occupancy)~., data=trainset )
print(iri )
plot(iri )

#Prediction using the test data
prediction <- predict(iri, subset(testset, select=-c(Cover_Type)))
#create the confusion matrix of the predicted data
confusionMatrix(as.factor(prediction), as.factor(testset$Cover_Type))
