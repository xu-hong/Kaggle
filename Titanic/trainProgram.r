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
                  labels=c("Perished", "Survived")) +
  facet_grid(Pclass~Sex)

ggplot(data=df.train, aes(x=Pclass)) + 
  geom_bar(aes(fill=Survived), position="fill") +
  ggtitle("Class")

ggplot(data=df.train, aes(x=Sex)) + 
  geom_bar(aes(fill=Sex)) +
  ggtitle("Sex") +
  facet_grid(~Survived)

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

# plot it
ggplot(data=df.train, aes(x=Title)) + 
  geom_bar(aes(fill=Survived)) +
  ggtitle("Title") 

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
# inspect the new feature a little bit
ggplot(data=df.train.fe, aes(x=Deck)) + 
  geom_bar(aes(fill=Fate), position="fill") +
  ggtitle("Deck") 

ggplot(data=df.train.fe, aes(x=Side)) + 
  geom_bar(aes(fill=Fate), position="fill") +
  ggtitle("Side") 

# keep the features that I deem informative
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
df.train.batch <- df.train.munged[train.rows, ]
df.test.batch <- df.train.munged[-train.rows, ]

##
## First try: logistic regression 
##

## By setting family to binomial with a logit link
titanic.logit.1 <- glm(formula = Fate ~ Sex + Age + Class + Deck + Fare + Embarked + Family, 
                       family = binomial(link = "logit"), data = df.train.batch)
summary(titanic.logit.1)
### result

'
Call:  glm(formula = Fate ~ Sex + Age + Class + Deck + Fare + Embarked + 
    Family, family = binomial(link = "logit"), data = df.train.batch)

Coefficients:
(Intercept)      Sexmale          Age  ClassSecond   ClassThird        DeckB        DeckC  
   4.773892    -2.616986    -0.043456    -0.436696    -1.595093    -0.231965    -0.925133  
      DeckD        DeckE        DeckF        DeckG        DeckT      DeckUNK         Fare  
  -0.356817     0.460285     0.142760    -1.699976   -13.378889    -1.170828     0.004527  
  EmbarkedQ    EmbarkedS       Family  
  -0.263531    -0.549262    -0.199972  

Degrees of Freedom: 713 Total (i.e. Null);  697 Residual
Null Deviance:      950.9 
Residual Deviance: 620 	AIC: 654
'
# reduction in deviance: 950.9 - 620 = 330.9, df = 713 - 697 = 16
1 - pchisq(330.9, df=16)
# 0 
# In other words, the model put forth is significantly different from the null model. 

anova(titanic.logit.1, test="Chisq")
'
Analysis of Deviance Table

Model: binomial, link: logit

Response: Fate

Terms added sequentially (first to last)


         Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
NULL                       713     950.86              
Sex       1  203.105       712     747.76 < 2.2e-16 ***
Age       1    1.168       711     746.59  0.279746    
Class     2  100.266       709     646.33 < 2.2e-16 ***
Deck      8   11.614       701     634.71  0.169276    
Fare      1    0.642       700     634.07  0.422983    
Embarked  2    6.105       698     627.96  0.047231 *  
Family    1    8.004       697     619.96  0.004668 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


'

### Sex and Class features account for a large share of reduction in deviance
# providing some support to our hypotheses about life boat access and location on ship. 
# Since Fare isn't doing much for us, let's see if the Fare.pp we created fares any better (pun intended).
titanic.logit.2 <- glm(formula = Fate ~ Sex + Age + Class + Deck + Fare.pp + Embarked + Family, 
                       family = binomial(link = "logit"), data = df.train.batch)

anova(titanic.logit.2, test="Chisq")
# not too much help

## drop Fare, Fare.pp, Age, Family
titanic.logit.3 <- glm(formula = Fate ~ Sex + Class + Deck + Embarked, 
                    family = binomial(link = "logit"), data = df.train.batch)
anova(titanic.logit.3, test="Chisq")


#######
# I'm going to use a form of resampling known as 10-fold cross-validation (CV), 
# repeated 3 times.
# Later, I plan to compare the fitted logit model to other model types using 
# the receiver operating characteristic (ROC) curve. 
# The twoClassSummary function in caret can calculate the figures 
# I'll need for that if I give it class probabilities predicted
# by the logistic regression model.

## All of these things I want 
# -- 
# 3x 10-fold CV, 
# estimation of class probabilities, 
# metrics from twoClassSummary 
# -- 
# can be passed through the trainControl function.
library(pROC) # for ROC metric

cv.ctrl <- trainControl(method="repeatedcv", repeats=3, 
                        summaryFunction = twoClassSummary,
                        classProbs = T)

set.seed(1234)
titanic.tune.1 <- train(Fate ~ Sex + Class + Deck + Embarked,
                        data = df.train.batch,
                        trControl = cv.ctrl,
                        method = "glm",
                        metric = "ROC")

titanic.tune.1
summary(titanic.tune.1)

## 
# About 70 percent of the Titanic's passengers boarded the ship at Southampton. 
# I'm going to use Embarked and the I() function, 
# which inhibits interpretation & conversion of R objects, 
# to create a new 2-level factor within the model formula. 
set.seed(1234)
titanic.tune.2 <- train(Fate ~ Sex + Class + Deck + I(Embarked == "S"),
                        data = df.train.batch,
                        trControl = cv.ctrl,
                        method = "glm",
                        metric = "ROC")
titanic.tune.2
summary(titanic.tune.2)

# plug Title in. See if it improve the model
set.seed(1234)
titanic.tune.3 <- train(Fate ~ Sex + Class + Deck + I(Embarked == "S") + Title,
                        data = df.train.batch,
                        trControl = cv.ctrl,
                        method = "glm",
                        metric = "ROC")
titanic.tune.3
summary(titanic.tune.3)
# yeah, improved!

# try Family
set.seed(1234)
titanic.tune.4 <- train(Fate ~ Sex + Class + Deck + I(Embarked == "S") + Title + Family,
                        data = df.train.batch,
                        trControl = cv.ctrl,
                        method = "glm",
                        metric = "ROC")
titanic.tune.4
summary(titanic.tune.4)
# nice

# try Age
set.seed(1234)
titanic.tune.5 <- train(Fate ~ Sex + Class + Deck + I(Embarked == "S") + Title + Family + Age,
                        data = df.train.batch,
                        trControl = cv.ctrl,
                        method = "glm",
                        metric = "ROC")
titanic.tune.5
summary(titanic.tune.5)
# shave a little.. 

# play with Title a little more
# I will collapse the titles “Miss” and “Mrs” and leave a duo of Title-related
# factors which should represent the “women and children first” theme well.

set.seed(1234)
titanic.tune.6 <- train(Fate ~ Sex + Class + Deck + 
                          I(Embarked == "S") + 
                          I(Title %in% c("the Countess", "Ms")) + # the Mrs
                          I(Title %in% c("Mlle", "Mme")) +  # the Miss
                          Family + Age,
                        data = df.train.batch,
                        trControl = cv.ctrl,
                        method = "glm",
                        metric = "ROC")
titanic.tune.6
summary(titanic.tune.6)
# well.. no improvement
# try another way
set.seed(1234)
titanic.tune.6.1 <- train(Fate ~ Sex + Class + Deck + 
                          I(Embarked == "S") + 
                          # the female and young
                          I(Title %in% c("the Countess", "Ms", "Master", "Miss", "Mme", "Mlle", "Mrs", "Lady")) + 
                          Family + Age,
                        data = df.train.batch,
                        trControl = cv.ctrl,
                        method = "glm",
                        metric = "ROC")
titanic.tune.6.1
summary(titanic.tune.6.1)
# better than first, but still no improvement




# add Fare
set.seed(1234)
titanic.tune.7 <- train(Fate ~ Sex + Class + Deck + I(Embarked == "S") + 
                          Title + Family + Age + 
                          Fare.pp,
                        data = df.train.batch,
                        trControl = cv.ctrl,
                        method = "glm",
                        metric = "ROC")
titanic.tune.7
summary(titanic.tune.7)
# just a tiny little. drop it

# add Side
set.seed(1234)
titanic.tune.8 <- train(Fate ~ Sex + Class + Deck + I(Embarked == "S") + 
                          Title + Family + Age + 
                          Side,
                        data = df.train.batch,
                        trControl = cv.ctrl,
                        method = "glm",
                        metric = "ROC")
titanic.tune.8
summary(titanic.tune.8)
# just a tiny little. drop it

# How about the Boat.dibs?
set.seed(1234)
titanic.tune.9 <- train(Fate ~ Sex + Class + Deck + I(Embarked == "S") + 
                          Title + Family + Age + 
                          Boat.dibs,
                        data = df.train.batch,
                        trControl = cv.ctrl,
                        method = "glm",
                        metric = "ROC")
titanic.tune.9
summary(titanic.tune.9)
# just a tiny little. drop it



# final (maybe not) model 
set.seed(1234)
titanic.tune.f <- train(Fate ~ Sex + Class + Deck + I(Embarked == "S") + 
                          Title + Family + Age,
                        data = df.train.batch,
                        trControl = cv.ctrl,
                        method = "glm",
                        metric = "ROC")
titanic.tune.f
summary(titanic.tune.f)


# can we do better?
# so far the 5th is best. improve on that.
set.seed(1234)
titanic.tune.5.1 <- train(Fate ~ Sex + Class + Deck +
                            I(Embarked == "S") + Family + Age + Title +
                            I(Sex == "male" & Class=="First") +
                            I(Sex == "female" & Class=="Third"),    
                        data = df.train.batch,
                        trControl = cv.ctrl,
                        method = "glm",
                        metric = "ROC")
titanic.tune.5.1
summary(titanic.tune.5.1)
# aha!


##
## let give the popular Random Forest a try
## 
#  The number of randomly pre-selected predictor variables for each node, 
#  designated mtry, is the sole parameter available for tuning an RF with train.
library(randomForest)
rf.grid <- data.frame(.mtry = c(2, 3))
set.seed(1234)
rf.tune <- train(Fate ~ Sex + Class + Deck +
                   I(Embarked == "S") + Family + Age + Title +
                   I(Sex == "male" & Class=="First") +
                   I(Sex == "female" & Class=="Third"),   
                 data = df.train.batch,
                 method = "rf",
                 metric = "ROC",
                 tuneGrid = rf.grid,
                 trControl = cv.ctrl)

rf.tune
# Strobl et al suggested setting mtry at the square root of the number of variables. 
# In this case, that would be mtry = 3, which did produce the better RF model.


##
## let give the popular SVM a try
## 

# The default value for one of the tunable functions -– sigest –- 
# produces good results on most occasions. 
# The default grid of cost parameter C is 0.25, 0.5, and 1.
# If we set train argument tuneLength = 9, 
# the grid expands to c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64).
set.seed(1234)
svm.tune <- train(Fate ~ Sex + Class + Deck +
                    I(Embarked == "S") + Family + Age + Title +
                    I(Sex == "male" & Class=="First") +
                    I(Sex == "female" & Class=="Third"), 
                  data = df.train.batch,
                  method = "svmRadial",
                  tuneLength = 9,
                  preProcess = c("center", "scale"), # normalize data within resampling loops
                  metric = "ROC",
                  trControl = cv.ctrl)
svm.tune
# C=0.25 has the biggest ROC



###
### Part III: Model Evaluation
###

# logistic model
glm.pred <- predict(titanic.tune.5.1, df.test.batch)
confusionMatrix(glm.pred, df.test.batch$Fate)

# RandomForest model
rf.pred <- predict(rf.tune, df.test.batch)
confusionMatrix(rf.pred, df.test.batch$Fate)

# SVM model
svm.pred <- predict(svm.tune, df.test.batch)
confusionMatrix(svm.pred, df.test.batch$Fate)

# the logistic regression model we put together earlier appears to do the best job of selecting 
# the survivors among the passengers in the df.test.batch. (Specificity i.e. FP)
# The Random Forest model, on the other hand, seems to have a slight edge 
# on predicting those who perished (Sensitivity i.e. TP)



# I also calculate, using each of the three fitted models, 
# the predicted probabilities for the df.test.batch, 
# and use those probabilities to plot the ROC curves.


## Logistic regression model (BLACK curve)
glm.probs <- predict(titanic.tune.5.1, df.test.batch, type = "prob")
glm.ROC <- roc(response = df.test.batch$Fate,
               predictor = glm.probs$Survived,
               levels = levels(df.test.batch$Fate))
plot(glm.ROC, type="S")   
## Area under the curve: 0.8799

## Random Forest model (RED curve)
rf.probs <- predict(rf.tune, df.test.batch, type = "prob")
rf.ROC <- roc(response = df.test.batch$Fate,
              predictor = rf.probs$Survived,
              levels = levels(df.test.batch$Fate))
plot(rf.ROC, add=TRUE, col="red") 
## Area under the curve: 0.8767

## SVM model (GREEN curve)
svm.probs <- predict(svm.tune, df.test.batch, type = "prob")
svm.ROC <- roc(response = df.test.batch$Fate,
              predictor = svm.probs$Survived,
              levels = levels(df.test.batch$Fate))
plot(svm.ROC, add=TRUE, col="green") 
## Area under the curve: 0.8385


### If you have been looking for that one graph which sums up the performance
### this is it
cv.values <- resamples(list(Logit = titanic.tune.5.1, 
                            RF = rf.tune, SVM = svm.tune))
dotplot(cv.values, metric = "ROC")


### oh, maybe this is it..
### it compares the four models on the basis of ROC, sensitivity, and specificity





