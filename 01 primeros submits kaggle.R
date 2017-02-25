----#librerias----
library(aplpack)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(mboost)


---#carga de datos----  
train <- read.table("train.csv",header = TRUE,sep = ",")
train <- read.table("train.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)

table(train$Survived)
prop.table(train$Survived)
prop.table(table(train$Survived))

----#entrega 1----

test <- read.table("test.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)
test$Survived <- rep(0,418)

submit <- data.frame(PassengerId= test$PassengerId,Survived=test$Survived)


write.csv(submit,file = "diedtitanic.csv",row.names = FALSE)

table(train$Sex)

----#entrega 2----

prop.table(table(train$Sex,train$Survived))

prop.table(table(train$Sex,train$Survived),1)

test$Survived <- 0

test$Survived[test$Sex=='female'] <- 1

write.csv(submit,file = "diedtitanic_v2.csv",row.names = FALSE)

submit_v2 <- data.frame(PassengerId= test$PassengerId,Survived=test$Survived)

write.csv(submit_v2,file = "diedtitanic_v2.csv",row.names = FALSE)

----#entrega 3---------

summary(train$Age)

train$Child <- 0
train$Child[train$Age<18] <- 1
aggregate(Survived ~ Child+Sex,data = train,FUN = sum)
aggregate(Survived ~ Child+Sex,data = train,FUN = length)
aggregate(Survived ~ Child+Sex,data = train,FUN = count)
aggregate(Survived ~ Child+Sex,data = train,FUN = count.fields)
aggregate(Survived ~ Child+Sex,data = train,FUN = function(x){sum(x)/length(x)})



head(train$Fare)

summary(train$Fare)

boxplot(train$Fare)

stem.leaf(train$Fare)

train$Fare2 <- '30+'
train$Fare2[train$Fare<30 & train$Fare>=20] <- '20-30'
train$Fare2[train$Fare<20 & train$Fare>=10] <- '10-30'
train$Fare2[train$Fare<20 & train$Fare>=10] <- '10-20'
train$Fare2[train$Fare<10] <- '<10'

aggregate(Survived ~ Fare+Pclass+Sex,data = train,FUN = function(x){sum(x)/length(x)})
aggregate(Survived ~ Fare2+Pclass+Sex,data = train,FUN = function(x){sum(x)/length(x)})

test$Survived <- 0
test$Survived[test$Sex=='female'] <- 1
test$Survived[test$Sex=='female' & test$Pclass==3 & test$Fare >=20] <- 0


submit_v3 <- data.frame(PassengerId= test$PassengerId,Survived=test$Survived)

write.csv(submit_v3,file = "diedtitanic_v3.csv",row.names = FALSE)

----#entrega 4----

fit <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train)
fit <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train,method = "class")
plot(fit)
text(fit)


fancyRpartPlot(fit)
Prediction <- predict(fit,test,type = "class")
submit_v4 <- data.frame(PassenngerId = test$PassengerId,Survived = Prediction)
write.csv(submit_v4,file = "diedtitanic_rpart_v4.csv",row.names = FALSE)
submit_v4 <- data.frame(PassengerId = test$PassengerId,Survived = Prediction)
write.csv(submit_v4,file = "diedtitanic_rpart_v4.csv",row.names = FALSE)

----#entrega 5--------

fit <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train,method = "class",control=rpart.control(minsplit = 2,cp=0))
fancyRpartPlot(fit)
fit <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train,method = "class",control=rpart.control(minsplit = 2,maxdepth = 5))
fancyRpartPlot(fit)
fit <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train,method = "class",control=rpart.control(minsplit = 2,maxdepth = 7))
fancyRpartPlot(fit)
fit <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train,method = "class",control=rpart.control(minsplit = 2,maxdepth = 9))
fancyRpartPlot(fit)
fit <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train,method = "class",control=rpart.control(minsplit = 2,maxdepth = 12))
new.fit <- prp(fit,snip=TRUE)$obj
train$Name[1]
test$Survived
test$Survived <- NA
combi <- rbind(train,test)
remove(train$Child)

fit_rf <- randomForest(as.factor(Survived) ~ Pclass+Sex+Age+Sibsp+Parch+Fare+Embarked,data = train,importance=TRUE,ntree=2000)
fit_rf <- randomForest(as.factor(Survived) ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train,importance=TRUE,ntree=2000)
fit_rf <- randomForest(Survived ~ Pclass+Sex+Age+Sibsp+Parch+Fare+Embarked,data = train,importance=TRUE,ntree=2000)
fit_rf <- randomForest(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train,importance=TRUE,ntree=2000)
test$Survived <- 0
fit_rf <- randomForest(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train,importance=TRUE,ntree=2000)
fit_rf <- randomForest(as.factor(Survived) ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train,importance=TRUE,ntree=2000)
is.na(test)
which(is.na(test$Fare))
summary(test$Fare)
test$Fare[153] <- median(train$Fare,na.rm = TRUE)
summary(test$Fare)
fit_rf <- randomForest(as.factor(Survived) ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train,importance=TRUE,ntree=2000)
combi <- rbind(train, test)
train <- read.table("train.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)
combi <- rbind(train, test)
unique(combi$Embarked)
unique(combi$Survived)
combi$Name[1]
strsplit(combi$Name[1],split='[,.]')
strsplit(combi$Name[1],split='[,.]')
strsplit(combi$Name[1],split='[,.]')[1]
strsplit(combi$Name[1],split='[,.]')[[1]]
strsplit(combi$Name[1],split='[,.]')[[1]][2]
strsplit(combi$Name[1],split='[,.]')[2]
strsplit(combi$Name[1],split='[,.]')[,2]
strsplit(combi$Name[1],split='[,.]')[1][2]
strsplit(combi$Name[1],split='[,.]')[[1]]
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
head(combi$Title)
combi$Title <- sub(' ','',combi$Title)
head(combi$Title)
table(combi$Title)
combi$Title[combi$Title %in% c('Mme','Mlle')] <- 'Mlle'
table(combi$Title)
combi$Title[combi$Title %in% c('Mrs','Ms')] <- 'Mrs'
combi$Title[combi$Title %in% c('Mrs','Miss')] <- 'Mrs'
table(combi$Title)
combi$Title[combi$Title %in% c('Capt','Don','Major','Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona','Lady','the Countess','Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
strsplit(combi$Name[1],split='[,.]')[[1]][3]
strsplit(combi$Name[1],split='[,.]')[[1]][1]
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize,combi$Surname,sep=""))
head(combi$FamilyID)
combi$FamilyID <- paste(as.character(combi$FamilySize),combi$Surname,sep="")
head(combi$FamilyID)
combi$FamilyID[combi$FamilySize<=2] <- 'Small'
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))
View(famIDs)
famIDs <- famIDs[famIDs$Freq<=2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
train <- combi[1:891,]
test <- combi[892:1309,]
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
data=train, method="class")

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
data=train, method="class")
fancyRpartPlot(fit)
Prediction <- predict(fit,test,type = "class")
submit_v5 <- data.frame(PassengerId=test$PassengerId,Survived=Prediction)
write.csv(submit_v5,file = "diedtitanic_rpart_v5.csv",row.names = FALSE)

-----#entrega 6------

summary(combi$Age)
summary(combi)
summary(combi$Embarked)
summary(as.factor(combi$Embarked)
)
which(combi$Embarked=='')
combi$Embarked <- factor(combi$Embarked[which(combi$Embarked=='')])
combi$Embarked[which(combi$Embarked=='')]="S"
combi$Embarked <- factor(combi$Embarked)

summary(combi$Embarked)
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
summary(combi$FamilyID)
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
FamilyID2, data=train, importance=TRUE, ntree=2000)
train <- combi[1:891,]
test <- combi[892:1309,]
varImpPlot(fit)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
FamilyID2, data=train, importance=TRUE, ntree=2000)
summary(train$Survived)
summary(train)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
FamilyID2, data=train, importance=TRUE, ntree=2000)
train <- combi[1:891,]
test <- combi[892:1309,]
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
FamilyID2, data=train, importance=TRUE, ntree=2000)
summary(train)
summary(factor(train$Pclass))
summary(factor(train$Sex))
summary(factor(train$Fare))
summary(factor(train$SibSp))
summary(factor(train$Age))
combi$Age <- round(combi$Age,digits = 0)
summary(factor(train$Age))
combi$Age <- round(combi$Age)
summary(factor(train$Age))
round(70.5)
round(combi$Age)
summary(factor(combi$Age))
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
FamilyID2, data=train, importance=TRUE, ntree=2000)
train <- combi[1:891,]
test <- combi[892:1309,]
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
FamilyID2, data=train, importance=TRUE, ntree=2000)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare2 + Embarked + Title + FamilySize +
FamilyID2, data=train, importance=TRUE, ntree=2000)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
FamilyID2, data=train, importance=TRUE, ntree=2000)
summary(factor(combi$Fare))
which(is.na(combi$Fare))
summary(combi$Fare)
summary(combi$SibSp)
summary(combi$Age)
summary(combi$Pclass)
summary(combi$Parch)
summary(combi$Title)
summary(combi$FamilySize)
summary(combi$FamilyID2)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2, data=train, importance=TRUE, ntree=2000)
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Join together the test and train sets for easier feature engineering
test$Survived <- NA
combi <- rbind(train, test)
# Convert to a string
combi$Name <- as.character(combi$Name)
# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)
# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1
# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)
# Fill in Age NAs
summary(combi$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
# Check what else might be missing
summary(combi)
# Fill in Embarked blanks
summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
# Fill in Fare NAs
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
# New factor for Random Forests, only allowed <32 levels, so reduce number
combi$FamilyID2 <- combi$FamilyID
# Convert back to string
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
# And convert back to factor
combi$FamilyID2 <- factor(combi$FamilyID2)
# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
data=train, importance=TRUE, ntree=2000)
varImpPlot(fit)
Prediction <- predict(fit,test)
submit_v5_rf <- data.frame(PassengerId=test$PassengerId,Survived=Prediction)
write.csv(submit_v5_rf,file = "diedtitanic_rf1.csv",row.names = FALSE)
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")

submit_v6_rf <- data.frame(PassengerId=test$PassengerId,Survived=Prediction)

write.csv(submit_v6_rf,file = "diedtitanic_rf2.csv",row.names = FALSE)


----#entrega 7------
set.seed(415)
fit <- cforest(as.factor(Survived) ~ .,
data = train, controls=cforest_unbiased(ntree=200, mtry=3))
plot(fit)
varImpPlot(fit)
summary(fit)

m.boost <- glmboost(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
family=Binomial(), # needed for classification
data=train)
m.boost <- glmboost(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
family=Binomial(), # needed for classification
data=train)
m.boost <- glmboost(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,family=Binomial(),data=train)
m.boost <- glmboost(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,family=Binomial(),data=train)
coef(m.boost)
plot(m.boost)
Prediction <- predict(m.boost,test,type = "class")
Prediction_m.boost <- predict(m.boost,test)
Prediction_m.boost <- predict(m.boost,type = "response",newdata = test)
train <- combi[1:891,]
test <- combi[892:1309,]
Prediction_m.boost <- predict(m.boost,type = "response",newdata = test)
m.boost <- glmboost(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilySize + FamilyID,family=Binomial(),data=train)
Prediction_m.boost <- predict(m.boost,type = "response",newdata = test)
Prediction_m.boost <- predict(m.boost,newdata = test)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilySize + FamilyID,
data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit_v7_rf <- data.frame(PassengerId=test$PassengerId,Survived=Prediction)
write.csv(submit_v7_rf,file = "diedtitanic_rf3.csv",row.names = FALSE)

----#entrega 8------

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilySize + FamilyID2+Title,
data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit_v8_rf <- data.frame(PassengerId=test$PassengerId,Survived=Prediction)
write.csv(submit_v8_rf,file = "diedtitanic_rf4.csv",row.names = FALSE)

----#entrega 9------

head(combi)
unique(embarked)
unique(combi$Embarked)
combi$ZFare <- scale(combi$Fare,2,sd)
combi$ZFare <- scale(combi$Fare)
combi$ZAge <- scale(combi$Age)
summary(combi$Cabin)
table(combi$Cabin,combi$Sex)
head(strsplit(combi$Cabin))
head(strsplit(combi$Cabin,1,2))
combi$cabin_letter <- substr(combi$Cabin,1,1)
unique(combi$Cabin)
unique(combi$cabin_letter)
subset(combi,combi$cabin_letter="")
subset(combi,combi$cabin_letter == "")
head(subset(combi,combi$cabin_letter == ""))
train <- combi[1:891,]
test <- combi[892:1309,]
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + ZAge + SibSp + Parch + ZFare + Embarked + FamilySize +Title+cabin_letter,data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + ZAge + SibSp + Parch + ZFare + Embarked + FamilySize +Title+cabin_letter,data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + ZAge + SibSp + Parch + ZFare + Embarked + FamilySize +Title+cabin_letter,data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + ZAge + SibSp + Parch + ZFare + Embarked + FamilySize +Title+cabin_letter,data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
combi$cabin_letter <- factor(substr(combi$Cabin,1,1))
train <- combi[1:891,]
test <- combi[892:1309,]
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + ZAge + SibSp + Parch + ZFare + Embarked + FamilySize +Title+cabin_letter,data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit_v9_rf <- data.frame(PassengerId=test$PassengerId,Survived=Prediction)
write.csv(submit_v9_rf,file = "diedtitanic_rf9.csv",row.names = FALSE)

-----#entrega 10------

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + ZAge + SibSp + Parch + ZFare + Embarked +Title,data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit_v10_rf <- data.frame(PassengerId=test$PassengerId,Survived=Prediction)
write.csv(submit_v10_rf,file = "diedtitanic_rf10.csv",row.names = FALSE)

