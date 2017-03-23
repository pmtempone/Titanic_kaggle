-----#library-----

library(rpart)
library(xgboost)
library(ranger)
library(randomForest)
library(caret)
library(pROC)
library(dplyr)
library(reshape)
library(ggplot2)
library(FactoMineR)
library(funModeling)
library(aplpack)
library(partykit)				# Convert rpart object to BinaryTree
library(party)	
library(rpart.plot)
library(rattle)					# Fancy tree plot

---#revision de datos----



my_data_status=df_status(combi)

#pasamos a character factores con más de 25 categorias

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ','',combi$Title)

table(combi$Title)

combi$Title[combi$Title %in% c('Mme','Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Mrs','Ms')] <- 'Mrs'
combi$Title[combi$Title %in% c('Mrs','Miss')] <- 'Mrs'
combi$Title[combi$Title %in% c('Capt','Don','Major','Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona','Lady','the Countess','Jonkheer')] <- 'Lady'

combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize),combi$Surname,sep="")
combi$FamilyID[combi$FamilySize<=2] <- 'Small'
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

combi$Ticket <- as.character(combi$Ticket)
combi$Cabin <- as.character(combi$Cabin)
combi$Ticket <- as.character(combi$Ticket)
combi$FamilyID <- as.character(combi$FamilyID)
combi$Survived <- factor(combi$Survived)
combi$Pclass <- factor(combi$Pclass)
combi$SibSp <- factor(combi$SibSp)



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


my_data_status=df_status(combi)

----#visualizacion de datos-----

table(combi$Survived)
prop.table(train$Survived)
prop.table(table(train$Survived))

summary(train$Age)

combi$Child <- 0
combi$Child[combi$Age<18] <- 1
aggregate(Survived ~ Child+Sex,data = combi[!is.na(combi$Survived),],FUN = sum)
aggregate(Survived ~ Child+Sex,data = combi[!is.na(combi$Survived),],FUN = length)
aggregate(Survived ~ Child+Sex,data = combi[!is.na(combi$Survived),],FUN = count)
aggregate(Survived ~ Child+Sex,data = combi[!is.na(combi$Survived),],FUN = count.fields)
aggregate(Survived ~ Child+Sex,data = combi[!is.na(combi$Survived),],FUN = function(x){sum(x)/length(x)})


summary(train$Fare)

boxplot(train$Fare)

stem.leaf(train$Fare)

combi$Fare2 <- '30+'
combi$Fare2[combi$Fare<30 & combi$Fare>=20] <- '20-30'
combi$Fare2[combi$Fare<20 & combi$Fare>=10] <- '10-30'
combi$Fare2[combi$Fare<20 & combi$Fare>=10] <- '10-20'
combi$Fare2[combi$Fare<10] <- '<10'

combi$Fare2 <- factor(combi$Fare2)



----#reglas de asociacion----
  
  library(arules)
library(tidyr)

df.reglas <- combi %>% select(Sex,Survived,Pclass,SibSp,Embarked,Title,FamilyID2,cabin_letter)

apriori_df <- apriori(df.reglas,parameter = list(support=0.001))
summary(apriori_df)

inspect(apriori_df)
inspect(head(apriori_df, n = 25, by = "lift"))

#Coerce into data frame
rules_df_surv <- as(apriori_df, "data.frame");
rules_df_surv$rules <- as.character(rules_df_surv$rules)
rules_df_surv <- rules_df_surv %>% separate(rules,c("lhs","rhs"),"=>") %>% filter(grepl('Survived', rhs))
rules_subset <- subset(apriori_df,n=5, by = "lift",rhs %in% 'Survived=0');



write.table(rules_subset,"reglas_no_surv.txt")

----#train-----

train <- combi[1:891,-4]
test <- combi[892:1309,-4]

fit.rpart <- rpart(Survived ~ .,data = train,control = rpart.control(cp = 0.05,minsplit = 50))


summary(fit.rpart)

caret::varImp(fit.rpart)

plot(fit.rpart)
text(fit.rpart)


fancyRpartPlot(fit.rpart)				# A fancy plot from rattle
