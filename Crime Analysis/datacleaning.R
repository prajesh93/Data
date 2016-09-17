#author: Alankar D. Singh
#author: Paridhi Srivastava
#author: Prajesh Jhumkhawala

require(reshape)
library("tidyr")

#read the crime and offense dataset
crimedataset <-  read.csv("crime.csv", header = TRUE)
offensedataset <-  read.csv("offense_codes.csv", header = TRUE)

#remove the missing values in the crime dataset
cleaned <- na.omit(crimedataset[complete.cases(crimedataset),])

#remove the duplicated values in the INCIDENT_ID field of the crime dataset
cleaned <- cleaned[!duplicated(cleaned[,c('INCIDENT_ID')]),]

#split the FIRST_OCCURRENCE_DATE into the date and time fields
cleaned <- separate(data = cleaned, col = FIRST_OCCURENCE_DATE, into = c("FIRST_OCCURRENCE__date", "FIRST_OCCURRENCE__time"), sep = " ")

#split the REPORTED_DATE into the date and time fields
cleaned <- separate(data = cleaned, col = REPORTED_DATE, into = c("REPORTED_DATE__date", "REPORTED_DATE__time"), sep = " ")

#combine the OFFENSE_CODE and the OFFENSE_CODE_EXTENSION into one field
offensedataset$OFFENSE_CODE__details<- with(offensedataset, paste0(OFFENSE_CODE, OFFENSE_CODE_EXTENSION))

#arrange the columns
cleaned <- cleaned[ c(1:5, 7:15) ] 
offensedataset <- offensedataset[ c(9,1:8)]


#remove the unneeded columns
offensedataset <- offensedataset[ , -which(names(offensedataset) %in% c('OFFENSE_CODE', 'OFFENSE_CODE_EXTENSION'))]

#split the REPORTED_DATE__date into the year, month and day fields
cleaned <- separate(data = cleaned, col = REPORTED_DATE__date, into = c("REPORTED_DATE__month", "REPORTED_DATE__day", "REPORTED_DATE__year"), sep = "/")

#split the FIRST_OCCURRENCE__date into the year, month and day fields
new_cleaned <- separate(data = cleaned, col = FIRST_OCCURRENCE__date, into = c("FIRST_OCCURRENCE__month", "FIRST_OCCURRENCE__day", "FIRST_OCCURRENCE__year"), sep = "/")

#round the time of FIRST_OCCURRENCE time to the nearest hour
time <- new_cleaned$FIRST_OCCURENCE_TIME
time <- strftime(cut(as.POSIXct(time, format="%H:%M:%S"),'hours'),
           format='%H:%M:%S')
new_cleaned$FIRST_OCCURENCE_TIME <- time

#round the time to the nearest quarter of the day having labels 1 to 4
new_cleaned <- separate(data = new_cleaned, col = FIRST_OCCURENCE_TIME, into = c("FIRST_OCCURRENCE__quartertime"), sep = ":")
time<- as.numeric(new_cleaned$FIRST_OCCURRENCE__quartertime) %% 4 +1
new_cleaned$FIRST_OCCURRENCE__quartertime <- time

#round the time of FIRST_OCCURRENCE time to the nearest hour
time <- new_cleaned$REPORTED_DATE__time
time <- strftime(cut(as.POSIXct(time, format="%H:%M:%S"),'hours'),
                 format='%H:%M:%S', na.rm  = TRUE)
new_cleaned$REPORTED_DATE__time <- time

#round the time to the nearest quarter of the day having labels 1 to 4
new_cleaned <- separate(data = new_cleaned, col = REPORTED_DATE__time, into = c("REPORTED_DATE__quartertime"), sep = ":")
time<- as.numeric(new_cleaned$REPORTED_DATE__quartertime) %% 4 +1
new_cleaned$REPORTED_DATE__quartertime <- time

#write the cleaned data to csv file
write.csv(new_cleaned, file = "crimedataset.csv", row.names = FALSE)
write.csv(offensedataset, file = "offensedataset.csv", row.names = FALSE)

