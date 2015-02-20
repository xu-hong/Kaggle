setwd("~/Documents/Duke/MOOC/Kaggle/Titanic")




# define a readData function for the administrative ease
library(RCurl)
readData <- function(path.name, file.name, column.types, missing.types) {
  x <- getURL(paste(path.name, file.name, sep=""))
  read.csv( text = x, 
            colClasses=column.types,
            na.strings=missing.types )
# update: go with Rcurl since read.csv has problem with https
# Reference: stackoverflow.com/questions/14441729/read-a-csv-from-github-into-r
  
}

Titanic.path <- "https://raw.githubusercontent.com/xu-hong/Kaggle/master/Titanic/"
train.file <- "train.csv"
test.file <- "test.csv"
missing.types <- c("", "NA", "N/A")
train.column.types <- c('integer',  #PassengerId
                        'factor',   #Survived
                        'factor',   #Pclass
                        'character',#Name
                        'factor',   #Sex
                        'numeric',  #Age
                        'integer',  #SibSp
                        'integer',  #Parch
                        'character', #Ticket
                        'numeric',     #Fare
                        'character',    #Cabin
                        'factor'     #Embarked
                        )
test.column.types <- train.column.types[-2]

# read data 
train.raw <- readData(Titanic.path, train.file, train.column.types, missing.types)
df.train <- train.raw
test.raw <- readData(Titanic.path, test.file, test.column.types, missing.types)
df.test <- test.raw


###
### Part I: Data Munging
###



# inspect missing data
library(Amelia)
missmap(df.train, col=c("yellow", "black"), 
        main="Missingness Map of Train Data",
        legend=T)


# put data into visualization to build intuition
library(ggplot2)
ggplot(data=df.train, aes(x=Survived)) + 
  geom_bar() +
  ggtitle("Survived") + 
  scale_x_discrete(breaks=c(0, 1),
                  labels=c("Perished", "Survived"))

ggplot(data=df.train, aes(x=Pclass)) + 
  geom_bar(aes(fill=Survived), position="fill") +
  ggtitle("Class")

ggplot(data=df.train, aes(x=Sex)) + 
  geom_bar(aes(fill=Sex)) +
  ggtitle("Sex") +
  facet_wrap(~Survived)

ggplot(data=df.train, aes(x=Age)) + 
  geom_histogram(aes(y=..density..), fill="blue", binwidth=1) +
  geom_density() +
  ggtitle("Age") +
  facet_wrap(~Survived)

ggplot(data=df.train, aes(x=Fare, y=..count../sum(..count..))) + 
  geom_freqpoly(aes(color=Survived), binwidth=1)+
  ggtitle("Fare") +
  xlim(0, 100) 

ggplot(data=df.train, aes(x=factor(SibSp))) + 
  geom_histogram(aes(y=..count..), fill="blue", binwidth=1) +
  ggtitle("Sibling/Spouses Number") +
  facet_wrap(~Survived)

ggplot(data=df.train, aes(x=factor(Parch))) + 
  geom_histogram(aes(y=..count..), fill="blue", binwidth=1) +
  ggtitle("Parents Number") +
  facet_wrap(~Survived)
  
ggplot(data=df.train, aes(x=Embarked)) + 
  geom_bar(aes(fill=Survived), position="fill") +
  ggtitle("Embarked Site") 

# plot correlation
library(plyr)
source("pair.fun.R")
corr.data <- df.train
## change features of factor type to numeric type for inclusion on pairs
corr.data$Survived <- as.numeric(corr.data$Survived)
corr.data$Pclass <- as.numeric(corr.data$Pclass)
corr.data$Embarked <- revalue(corr.data$Embarked, 
                                  c("C" = 1, "Q" = 2, "S" = 3))
## generate correlogram
corr.data <- subset(corr.data, select = c("Survived", "Pclass", "Sex", "Age", 
                   "SibSp", "Parch", "Fare", "Embarked"))
pairs(corr.data, lower.panel=panel.smooth, upper.panel=panel.cor,
      diag.panel=panel.hist)


# 
# Now we tackle the missing Age
# 
summary(df.train$Age)
> summary(df.train$Age)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.42   20.12   28.00   29.70   38.00   80.00     177 

ggplot(data=df.train, aes(x=Pclass, y=Age)) +
  geom_boxplot()
# Disparity of Age among classes

# Name can give us a hint
## function for extracting honorific (i.e. title) from the Name feature
## Escape with \\, see:
# http://stackoverflow.com/questions/6638072/escaped-periods-in-r-regular-expressions

getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, ignore.case=TRUE)
  title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
  data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
  return (data$Title)
}   

df.train$Title <- getTitle(df.train)
unique(df.train$Title)

### get stats for each Title group
library(dplyr)

# the title group that has NA values are: 
# c("Dr", "Master", "Mr", "Mrs", "Miss")



bytitle.age.data <- df.train %>%
    group_by(Title) %>%
    summarise(
      mean.Age = mean(Age, na.rm=T),
      median.Age = median(Age, na.rm=T),
      n = n()) %>%
   arrange(Title)

bytitle.age.data
title.na.groups <- bytitle.age.data$Title[which(bytitle.age.data$n.na > 0)]

'''
# outdated version
assignMedian <- function(record, df.median){
  if (is.na(record["Age"])) {
    record["Age"] <- with(df.median, median.Age[which(Title == record["Title"])])
  }
  record
}
m <- apply(df.train, 1, assignMedian, df.median=bytitle.age.data)
df.train <- data.frame(t(m))
'''

assignMedian <- function(assign.to.var, filter.var, filter.levels, df.median){
  for (v in filter.levels) {
    assign.to.var[(filter.var == v) & is.na(assign.to.var)] <- 
      df.median$median.Age[which(df.median$Title == v)]
  }
  assign.to.var
}

df.train$Age <- assignMedian(df.train$Age, df.train$Title, title.na.groups, bytitle.age.data)
summary(df.train$Age)

# 
# How about the missing Embarked?
#
summary(df.train$Embarked)
#C    Q    S NA's 
# 168   77  644    2 
df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'


#
# While there are no missing Fare values, a summary does show at least one Fare=0...
summary(df.train$Fare)
# As big as 512.30: could be group purchase, will address it later
# At least one fare at Zero
subset(df.train, Fare < 7)[order(subset(df.train, Fare < 7)$Fare, 
                                 subset(df.train, Fare < 7)$Pclass), 
                           c("Age", "Title", "Pclass", "Fare")]
## We might suspect the zero is error and want to change the zero fare. 

#
# Now look at Title
# 
df.train$Title <- factor(df.train$Title,
                         c("Capt","Col","Major","Sir","Lady","Rev",
                           "Dr","Don","Jonkheer","the Countess","Mrs",
                           "Ms","Mr","Mme","Mlle","Miss","Master"))

ggplot(data=df.train, aes(x=Title, y=Age)) + geom_boxplot()


###
### Now wrap up feature engineering
###

# load stringr to use sub_str
library(stringr)

isEven <- function(x) x %in% c("0", "2", "4", "6", "8")
isOdd <- function(x) x %in% c("1", "3", "5", "7", "9")


featureEng <- function(data) {
  # Using Fate since it's shorter and straighforward
  data$Fate <- data$Survived
  # Revaluing Fate factor
  data$Fate <- revalue(data$Fate, c("1"="Survived", "0"="Perished"))
  # Boat.dibs attempts to capture the "woman and children first" policy
  # doubt: shouldn't our algorithm catch this?
  data$Boat.dibs <- "No"
  data$Boat.dibs[which(data$Sex=="female" | data$Age < 15)] <- "Yes"
  data$Boat.dibs <- as.factor(data$Boat.dibs)
  # Family consolidates siblings and spouses plus 
  # parents and children into one feature
  data$Family <- data$SibSp + data$Parch
  # Fare.pp attempts to adjust group purchases by size of family
  data$Fare.pp <- data$Fare/(data$Family + 1)
  # Giving the traveling class feature a new look, not necessary though
  data$Class <- data$Pclass
  data$Class <- revalue(data$Class, 
                        c("1"="First", "2"="Second", "3"="Third"))
  # First character in Cabin means something..
  data$Deck <- substring(data$Cabin, 1, 1)
  data$Deck[which(is.na(data$Deck))] <- "UNK"
  data$Deck <- as.factor(data$Deck)
  # Odd-numbered cabins were reportedly on the port side of the ship
  # Even-numbered cabins assigned Side="starboard"
  data$cabin.last.digit <- str_sub(data$Cabin, -1)
  data$Side <- "UNK"
  data$Side[which(isEven(data$cabin.last.digit))] <- "starboard"
  data$Side[which(isOdd(data$cabin.last.digit))] <- "port"
  data$Side <- as.factor(data$Side)
  data$cabin.last.digit <- NULL
  
  return(data)
  
}

df.train.fe <- featureEng(df.train)
train.keeps <- c("Fate", "Sex", "Boat.dibs", "Age", "Title", 
                "Class", "Deck", "Side", "Fare", "Fare.pp", 
                "Embarked", "Family")
df.train.munged <- df.train.fe[, train.keeps]



###
### Part II: Fitting a Model
###
library(caret)
set.seed(1001)
train.rows <- createDataPartition(df.train.munged$Fate, p=0.8, list=F)
df.train.batch <- df.train.munged[train.rows]
df.test.batch <- df.train.munged[-train.rows]
