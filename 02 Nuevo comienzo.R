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


---#revision de datos----

my_data_status=df_status(combi)

#pasamos a character factores con más de 25 categorias

combi$Ticket <- as.character(combi$Ticket)
combi$Cabin <- as.character(combi$Cabin)
combi$Ticket <- as.character(combi$Ticket)
combi$FamilyID <- as.character(combi$FamilyID)
combi$Ticket <- as.character(combi$Ticket)
combi$Survived <- factor(combi$Survived)
combi$Pclass <- factor(combi$Pclass)
combi$SibSp <- factor(combi$SibSp)


my_data_status=df_status(combi)

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

fit.rpart <- rpart(Survived ~ .,data = train,control = rpart.control(cp = 0.05),type = "prob")


summary(fit.rpart)

caret::varImp(fit.rpart)

plot(fit.rpart)
text(fit.rpart)
