#author: Alankar D. Singh
#author: Paridhi Srivastava
#author: Prajesh Jhumkhawala

library(corrplot)
library(tree)
library(stringr)
library(party)
library(plotrix)
library(ggplot2)
library(foreign)
library(stringr)
library(lubridate)
library(plyr)
library(xtable)
library(scales)
library(RColorBrewer)
library(ggmap)
library(maptools)
library(sp)
library(rgdal)
library(spatstat)


#read the dataset
crimedataset <-  read.csv("crime.csv", header = TRUE)


#factorize the target variable
crimedataset$DISTRICT_ID <- as.factor(crimedataset$DISTRICT_ID)


#plot the map highlighting offense locations in year 2011
testset <- subset(crimedataset, FIRST_OCCURRENCE__year== "2011")
testset$color <- factor(testset$OFFENSE_CATEGORY_ID)

denvermap <- qmap(location = "Denver", zoom =12, extent = "device", legend="bottomright")
crimemap <- denvermap + 
  geom_point(aes(x=GEO_LON, y=GEO_LAT, label=factor(testset$OFFENSE_CATEGORY_ID)), data=testset, color = (testset$color), na.rm = TRUE) +
  scale_colour_continuous(low = "red", high = "blue", space = "Lab", guide = "colorbar") +
  scale_size_continuous(range=range(testset$OFFENSE_CODE__details)) +
  theme(legend.key = element_rect(fill = testset$OFFENSE_CATEGORY_ID, color = testset$color, size = 5)) +
  ggtitle("Offenses that occured in 2011")

crimemap

#offenses that occurred in Denver according to Offense ID
offenses <- table(testset$OFFENSE_CATEGORY_ID)
barplot(offenses, main="OFFENSES in DENVER",xlab="Offense ID",beside=TRUE, col =testset$color, cex.names = 0.6, las = 2)



#plot the map of Denver highlighting offense locations in year 2012
testset <- subset(crimedataset, FIRST_OCCURRENCE__year== "2012")
testset$color <- factor(testset$OFFENSE_CATEGORY_ID)

denvermap <- qmap(location = "Denver", zoom =12, extent = "device", legend="bottomright")
crimemap <- denvermap + 
  geom_point(aes(x=GEO_LON, y=GEO_LAT, label=factor(testset$OFFENSE_CATEGORY_ID)), data=testset, color = (testset$color), na.rm = TRUE) +
  scale_colour_continuous(low = "red", high = "blue", space = "Lab", guide = "colorbar") +
  scale_size_continuous(range=range(testset$OFFENSE_CODE__details)) +
  theme(legend.key = element_rect(fill = testset$OFFENSE_CATEGORY_ID, color = testset$color, size = 5)) +
  ggtitle("Offenses that occured in 2012")

crimemap

#offenses that occurred in Denver according to Offense ID
offenses <- table(testset$OFFENSE_CATEGORY_ID)
barplot(offenses, main="OFFENSES in DENVER",xlab="Offense ID",beside=TRUE, col =testset$color, cex.names = 0.6, las = 2)



#plot the map of Denver highlighting offense locations in year 2013
testset <- subset(crimedataset, FIRST_OCCURRENCE__year== "2013")
testset$color <- factor(testset$OFFENSE_CATEGORY_ID)

denvermap <- qmap(location = "Denver", zoom =12, extent = "device", legend="bottomright")
crimemap <- denvermap + 
  geom_point(aes(x=GEO_LON, y=GEO_LAT, label=factor(testset$OFFENSE_CATEGORY_ID)), data=testset, color = (testset$color), na.rm = TRUE) +
  scale_colour_continuous(low = "red", high = "blue", space = "Lab", guide = "colorbar") +
  scale_size_continuous(range=range(testset$OFFENSE_CODE__details)) +
  theme(legend.key = element_rect(fill = testset$OFFENSE_CATEGORY_ID, color = testset$color, size = 5)) +
  ggtitle("Offenses that occured in 2016")


crimemap

#offenses that occurred in Denver according to Offense ID
offenses <- table(testset$OFFENSE_CATEGORY_ID)
barplot(offenses, main="OFFENSES in DENVER",xlab="Offense ID",beside=TRUE, col =testset$color, cex.names = 0.6, las = 2)



#plot the map of Denver highlighting offense locations in year 2014
testset <- subset(crimedataset, FIRST_OCCURRENCE__year== "2014")
testset$color <- factor(testset$OFFENSE_CATEGORY_ID)

denvermap <- qmap(location = "Denver", zoom =12, extent = "device", legend="bottomright")
crimemap <- denvermap + 
  geom_point(aes(x=GEO_LON, y=GEO_LAT, label=factor(testset$OFFENSE_CATEGORY_ID)), data=testset, color = (testset$color), na.rm = TRUE) +
  scale_colour_continuous(low = "red", high = "blue", space = "Lab", guide = "colorbar") +
  scale_size_continuous(range=range(testset$OFFENSE_CODE__details)) +
  theme(legend.key = element_rect(fill = testset$OFFENSE_CATEGORY_ID, color = testset$color, size = 5)) +
  ggtitle("Offenses that occured in 2014")

crimemap

#offenses that occurred in Denver according to Offense ID
offenses <- table(testset$OFFENSE_CATEGORY_ID)
barplot(offenses, main="OFFENSES in DENVER",xlab="Offense ID",beside=TRUE, col =testset$color, cex.names = 0.6, las = 2)



#plot the map of Denver highlighting offense locations in year 2015
testset <- subset(crimedataset, FIRST_OCCURRENCE__year== "2015")
testset$color <- factor(testset$OFFENSE_CATEGORY_ID)

denvermap <- qmap(location = "Denver", zoom =12, extent = "device", legend="bottomright")
crimemap <- denvermap + 
  geom_point(aes(x=GEO_LON, y=GEO_LAT, label=factor(testset$OFFENSE_CATEGORY_ID)), data=testset, color = (testset$color), na.rm = TRUE) +
  scale_colour_continuous(low = "red", high = "blue", space = "Lab", guide = "colorbar") +
  scale_size_continuous(range=range(testset$OFFENSE_CODE__details)) +
  theme(legend.key = element_rect(fill = testset$OFFENSE_CATEGORY_ID, color = testset$color, size = 5)) +
  ggtitle("Offenses that occured in 2015")

crimemap

#offenses that occurred in Denver according to Offense ID
offenses <- table(testset$OFFENSE_CATEGORY_ID)
barplot(offenses, main="OFFENSES in DENVER",xlab="Offense ID",beside=TRUE, col =testset$color, cex.names = 0.6, las = 2)



#plot the map of Denver highlighting offense locations in year 2016
testset <- subset(crimedataset, FIRST_OCCURRENCE__year== "2016")
testset$color <- factor(testset$OFFENSE_CATEGORY_ID)

denvermap <- qmap(location = "Denver", zoom =12, extent = "device", legend="bottomright")
crimemap <- denvermap + 
  geom_point(aes(x=GEO_LON, y=GEO_LAT, label=factor(testset$OFFENSE_CATEGORY_ID)), data=testset, color = (testset$color), na.rm = TRUE) +
  scale_colour_continuous(low = "red", high = "blue", space = "Lab", guide = "colorbar") +
  scale_size_continuous(range=range(testset$OFFENSE_CODE__details)) +
  theme(legend.key = element_rect(fill = testset$OFFENSE_CATEGORY_ID, color = testset$color, size = 5)) +
  ggtitle("Offenses that occured in 2016")

crimemap

#offenses that occurred in Denver according to Offense ID
offenses <- table(testset$OFFENSE_CATEGORY_ID)
barplot(offenses, main="OFFENSES in DENVER",xlab="Offense ID",beside=TRUE, col =testset$color, cex.names = 0.6, las = 2)

#Creating a pie chart to for the District_Id
mytable <- table(forestdataset$DISTRICT_ID)
pct <- round(mytable/sum(mytable)*100)
cols=c("red","green","cyan","violet","black","yellow","grey");
lbls <- paste(pct,sep=":") # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(mytable, labels = lbls, col=cols,main="Pie Chart of DISTRICT_ID")
legend("bottomright",c("1","2","3","4","5","6","7"), cex=1.6, fill=cols)


#Plotting a bar graph for the NEIGHBORHOOD_ID
counts <- table(forestdataset$NEIGHBORHOOD_ID)
barplot(counts, main="NEIGHBORHOOD_ID",xlab="NEIGHBOR_HOOD NAME",beside=TRUE, col ="darkgreen")


#relation between Soil Type and Wild Area
ggplot(forestdataset, aes(x = forestdataset$NEIGHBORHOOD_ID,fill = factor(forestdataset$ FIRST_OCCURRENCE__year))) +
  stat_count(width = 0.5) + xlab("Neighborhood_ID") + ylab("Total Crime") + labs(fill ="Year")



