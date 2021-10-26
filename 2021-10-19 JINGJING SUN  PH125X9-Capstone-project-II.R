
# Title: PH125X9 Capston Project II: Prediction of liver disease  Submission
## by JING JING SUN
## OCT. 19, 2021



## loading needed packages

library(tidyverse)
library(dslabs)
library(matrixStats)
library(caret)
library(lubridate)
library(dplyr)

## data clean and organizenation
## load dataset

liver_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv", 
                       header = FALSE)
head(liver_data)

## give each column a name

colnames(liver_data) <- c("Age", "Sex", "Tot_Bil", "Dir_Bil", "Alkphos", "Alamine", 
                          "Aspartate", "Tot_Prot", "Albumin", "A_G_Ratio", "Disease")


## convert value in column disease into 0 (no disease) and 1 ( liver disease)

liver_data$Disease <- as.numeric(ifelse(liver_data$Disease == 2, 0, 1))  
head(liver_data)

## add column "liverdisease" as.factor "0" represent "nodisease", "1" represent "liverdisease"

liver_data<- liver_data %>% mutate ( liver_data, liverdisease= as.factor (Disease))
head(liver_data)

## data set exploration

str(liver_data)
dim(liver_data)

## remove those rows that have missing data

liver_data<-na.omit(liver_data)
dim(liver_data)

## know our cleaned data

summary(liver_data)

## how many kind of liver disease in this data set

n_distinct(liver_data$Disease)
histogram(liver_data$Disease)

## how many liver disease case

count(liver_data$Disease == "1") 

## how many no disease case

sum(liver_data$Disease == "0")

## distribution of Age

liver_data%>% ggplot(aes(Age)) +
  geom_histogram( binwidth = 5,color = "black") +
  scale_x_continuous(breaks=seq(0, 100, by= 20)) +
  labs(x="Age", y="Frenquence") +
  ggtitle("Age Distribution")

## Is liver disease related to Age

liver_data %>% 
  ggplot(aes(liverdisease, Age))+
  geom_boxplot()+ geom_point()+
  ggtitle("Disease vs Age") +
  xlab("liverdisease") + ylab("Age")

## Sex distribution in this data set

liver_data %>% group_by(Sex)%>%
  summarize(n=n())

## liver disease summary by Sex

liver_data%>% ggplot(aes( liverdisease, fill= Sex)) +
  geom_bar()

liver_data%>% ggplot(aes( liverdisease, fill= Sex)) +
  geom_bar(position = position_dodge())

## Male with liver disease

liver_dataM<-liver_data %>% filter(Sex %in% 'Male')
Male_liverdiseace<-count(liver_dataM$Disease == "1") 
Male_liverdiseace

## Female with liver disease

liver_dataF<-liver_data %>% filter(Sex %in% 'Female')
Female_liverdiseace <-count(liver_dataF$Disease == "1") 
Female_liverdiseace 

## ratio of Male/Female in liver disease

Male_liverdiseace/Female_liverdiseace 

## Male with no disease

Male_nodiseace<-count(liver_dataM$Disease == "0") 
Male_nodiseace

## Female with liver disease

Female_nodiseace <-count(liver_dataF$Disease == "0") 
Female_nodiseace 

## Male liver disease incident 

Male_incident <- Male_liverdiseace/(Male_liverdiseace+Male_nodiseace)
Male_incident

## Female liver incident  

Female_incident <- Female_liverdiseace/(Male_liverdiseace+Male_nodiseace)
Female_incident

## liver disease incident ratio based on sex

Male_incident/Female_incident

## Tot_Bil express based on liverdisease

liver_data %>% 
  ggplot(aes(liverdisease, Tot_Bil))+
  geom_boxplot()+geom_point()+
  ggtitle("Disease vs Tot_Bil") +
  xlab("liverdisease") + ylab("Tot_Bil")

## Tot_Bil expression based on liver disease (mean+/-2se)

liver_data %>% group_by(liverdisease) %>% 
  summarize(n=n(), avg=mean(Tot_Bil),sd=sd(Tot_Bil),se=sd(Tot_Bil)/sqrt(n()))%>%
  ggplot(aes(x=liverdisease, y=avg, ymin=avg-2*se, ymax=avg+2*se))+
  geom_point()+
  geom_errorbar(width=0.75, colour="black", alpha=0.75, size=0.25)+
  ggtitle("Tot_Bil")+
  xlab("liverdisease") + ylab("Tot_Bil")

## Dir_Bil expression based on liver disease---wrong 

liver_data %>% 
  ggplot(aes(liverdisease, Dir_Bil))+
  geom_boxplot()+geom_point()+
  ggtitle("Disease vs Dir_Bil") +
  xlab("liverdisease") + ylab("Dir_Bil")

## Dir_Bil expression based on liver disease (mean+/-2se) 

liver_data %>% group_by(liverdisease) %>% 
  summarize(n=n(), avg=mean(Dir_Bil),sd=sd(Dir_Bil),se=sd(Dir_Bil)/sqrt(n()))%>%
  ggplot(aes(x=liverdisease, y=avg, ymin=avg-2*se, ymax=avg+2*se))+
  geom_point()+
  geom_errorbar(width=0.75, colour="black", alpha=0.75, size=0.25)+
  ggtitle("Dir_Bil")+
  xlab("liverdisease") + ylab("Dir_Bil")

## Alkphos expression based on liver disease

liver_data %>% 
  ggplot(aes(liverdisease,  Alkphos))+
  geom_boxplot()+geom_point()+
  ggtitle("Disease vs  Alkphos ") +
  xlab("liverdisease") + ylab(" Alkphos ")

## Alkphos  expression based on liver disease (mean+/-2se) 

liver_data %>% group_by(liverdisease) %>% 
  summarize(n=n(), avg=mean( Alkphos),sd=sd( Alkphos ),se=sd( Alkphos )/sqrt(n()))%>%
  ggplot(aes(x=liverdisease, y=avg, ymin=avg-2*se, ymax=avg+2*se))+
  geom_point()+
  geom_errorbar(width=0.75, colour="black", alpha=0.75, size=0.25)+
  ggtitle(" Alkphos ")+
  xlab("liverdisease") + ylab(" Alkphos ")

## Alamine expression based on liver disease

liver_data %>% 
  ggplot(aes(liverdisease, Alamine ))+
  geom_boxplot()+geom_point()+
  ggtitle("Disease vs Alamine ") +
  xlab("liverdisease") + ylab(" Alamine ")

## Alamine expression based on liver disease (mean+/-2se)

liver_data %>% group_by(liverdisease) %>% 
  summarize(n=n(), avg=mean(Alamine ),sd=sd(Alamine),se=sd(Alamine)/sqrt(n()))%>%
  ggplot(aes(x=liverdisease, y=avg, ymin=avg-2*se, ymax=avg+2*se))+
  geom_point()+
  geom_errorbar(width=0.75, colour="black", alpha=0.75, size=0.25)+
  ggtitle("Alamine")+
  xlab("liverdisease") + ylab(" Alamine ")

## Aspartate expression based on liver disease

liver_data %>% 
  ggplot(aes(liverdisease, Aspartate))+
  geom_boxplot()+geom_point()+
  ggtitle("Disease vs Aspartate") +
  xlab("liverdisease") + ylab(" Aspartate")

## Aspartate expression based on liver disease (mean+/-2se)

liver_data %>% group_by(liverdisease) %>% 
  summarize(n=n(), avg=mean(Aspartate),sd=sd(Aspartate),se=sd(Aspartate)/sqrt(n()))%>%
  ggplot(aes(x=liverdisease, y=avg, ymin=avg-2*se, ymax=avg+2*se))+
  geom_point()+
  geom_errorbar(width=0.75, colour="black", alpha=0.75, size=0.25)+
  ggtitle("Aspartate")+
  xlab("liverdisease") + ylab("Aspartate")

## Tot_Prot expression based on liver disease

liver_data %>% 
  ggplot(aes(liverdisease, Tot_Prot))+
  geom_boxplot()+geom_point()+
  ggtitle("Disease vs Tot_Prot") +
  xlab("liverdisease") + ylab("Tot_Prot")

## Tot_Prot expression based on liver disease (mean+/-2se)

liver_data %>% group_by(liverdisease) %>% 
  summarize(n=n(), avg=mean(Tot_Prot ),sd=sd(Tot_Prot),se=sd(Tot_Prot)/sqrt(n()))%>%
  ggplot(aes(x=liverdisease, y=avg, ymin=avg-2*se, ymax=avg+2*se))+
  geom_point()+
  geom_errorbar(width=0.75, colour="black", alpha=0.75, size=0.25)+
  ggtitle("Aspartate")+
  xlab("liverdisease") + ylab("Tot_Prot")

## Albumin expression based on liver disease

liver_data %>% 
  ggplot(aes(liverdisease, Albumin))+
  geom_boxplot()+geom_point()+
  ggtitle("Disease vs Albumin") +
  xlab("liverdisease") + ylab("Albumin")

## Albumin expression based on liver disease (mean+/-2se)

liver_data %>% group_by(liverdisease) %>% 
  summarize(n=n(), avg=mean(Albumin),sd=sd(Albumin),se=sd(Albumin)/sqrt(n()))%>%
  ggplot(aes(x=liverdisease, y=avg, ymin=avg-2*se, ymax=avg+2*se))+
  geom_point()+
  geom_errorbar(width=0.75, colour="black", alpha=0.75, size=0.25)+
  ggtitle("Albumin")+
  xlab("liverdisease") + ylab("Albumin")

## A_G_Ratio expression based on liver disease

liver_data %>% 
  ggplot(aes(liverdisease, A_G_Ratio))+
  geom_boxplot()+geom_point()+
  ggtitle("Disease vs A_G_Ratio") +
  xlab("liverdisease") + ylab("A_G_Ratio")

## A_G_Ratio expression based on liver disease (mean+/-2se)

liver_data %>% group_by(liverdisease) %>% 
  summarize(n=n(), avg=mean(A_G_Ratio),sd=sd(Albumin),se=sd(A_G_Ratio)/sqrt(n()))%>%
  ggplot(aes(x=liverdisease, y=avg, ymin=avg-2*se, ymax=avg+2*se))+
  geom_point()+
  geom_errorbar(width=0.75, colour="black", alpha=0.75, size=0.25)+
  ggtitle("A_G_Ratio")+
  xlab("liverdisease") + ylab("A_G_Ratio")

# Build the prediction system

## separete data set to train_set(70%) and test_set(30%)

set.seed(1, sample.kind="Rounding") 
test_index1 <- createDataPartition(y = liver_data$Disease, times = 1, p = 0.3, list = FALSE)
train_set <- liver_data[-test_index1,]
test_set<- liver_data[test_index1,]
dim(train_set)
dim(test_set)

# build models
# model 1. liner Regression Model
## create liner Regression Model

fit_lm <- lm(liverdisease ~ Age + Sex + Tot_Bil + Dir_Bil + Alkphos + Alamine + Aspartate + 
               Tot_Prot + Albumin + A_G_Ratio, data = train_set)
fit_lm$coef

## get a prediction based on liner Regression Model

prediction_lm<- predict(fit_lm, test_set, type = "response")

Disease_lm <- ifelse(prediction_lm > 0.5, "1", "0") %>% factor

## The Accuracy of liner Regression Model

confusionMatrix(Disease_lm, test_set$liverdisease)$table
Accuracy_lm <- confusionMatrix(Disease_lm, test_set$liverdisease)$overall[["Accuracy"]]
Accuracy_lm

## rmse of  liner Regression Model

model_lm_rmse <- RMSE(prediction_lm, test_set$Disease)
model_lm_rmse

# model 2. Logistic Regression Model

## create Logistic Regression Model

fit_glm <- glm(liverdisease ~ Age + Sex + Tot_Bil + Dir_Bil + Alkphos + Alamine + Aspartate + 
                 Tot_Prot + Albumin + A_G_Ratio, data = train_set, family = binomial)
fit_glm$coef

## get a prediction based on Logistic Regression Model

prediction_glm<- predict(fit_glm, test_set, type = "response")

Disease_glm <- ifelse(prediction_glm > 0.5, "1", "0") %>% factor

## The Accuracy of Logistic Regression Model

confusionMatrix(Disease_glm, test_set$liverdisease)$table
Accuracy_glm <-confusionMatrix(Disease_glm, test_set$liverdisease)$overall[["Accuracy"]]
Accuracy_glm

## rmse of  Logistic Regression Model

model_glm_rmse <- RMSE(Accuracy_glm, test_set$Disease)
model_glm_rmse 

# model 3. knn Model

## find the best K for KNN model

fit_knnK <- train(liverdisease ~ Age + Sex + Tot_Bil + Dir_Bil + Alkphos + Alamine + Aspartate + 
                    Tot_Prot + Albumin + A_G_Ratio,   method = "knn", 
                  tuneGrid = data.frame(k = seq(1, 15, 2)), 
                  data = train_set)
ggplot(fit_knnK)

fit_knnK

## create Logistic Regression Model use k=13

fit_knn <- knn3(liverdisease ~ Age + Sex + Tot_Bil + Dir_Bil + Alkphos + Alamine + Aspartate + 
                  Tot_Prot + Albumin + A_G_Ratio, data = train_set, k=13)

## get a prediction based on knn Model

prediction_knn <- predict(fit_knn, test_set, type = "class")
prediction_knn 

## The Accuracy of knn Model

confusionMatrix(data = prediction_knn, reference = test_set$liverdisease)$table
Accuracy_knn <- confusionMatrix(data = prediction_knn, reference = test_set$liverdisease)$overall["Accuracy"]
Accuracy_knn

## rmse of knn Model

model_knn_rmse <- RMSE(Accuracy_knn, test_set$Disease)
model_knn_rmse 

# model 4. qda Model

## create qda Model

fit_qda <- train(liverdisease ~ Age + Sex + Tot_Bil + Dir_Bil + Alkphos + Alamine + Aspartate + 
                   Tot_Prot + Albumin + A_G_Ratio, data = train_set)

## get a prediction based on qda Model

prediction_qda <- predict(fit_qda, test_set)
prediction_qda

## compute accuracy

confusionMatrix(predict(fit_qda, test_set), test_set$liverdisease)$table
Accuracy_qda<-confusionMatrix(predict(fit_qda, test_set), test_set$liverdisease)$overall["Accuracy"]
Accuracy_qda

## rmse of qda Model

model_qda_rmse <- RMSE(Accuracy_qda, test_set$Disease)
model_qda_rmse 

# model 5. lda Model

## create lda Model

fit_lda <- train(liverdisease ~ Age + Sex + Tot_Bil + Dir_Bil + Alkphos + Alamine + Aspartate + 
                   Tot_Prot + Albumin + A_G_Ratio, data = train_set)

## get a prediction based on lda Model

prediction_lda <- predict(fit_lda, test_set)
prediction_lda

## compute accuracy

confusionMatrix(predict(fit_qda, test_set), test_set$liverdisease)$table
Accuracy_lda <- confusionMatrix(predict(fit_lda, test_set), test_set$liverdisease)$overall["Accuracy"]
Accuracy_lda

## rmse of lda Model

model_lda_rmse <- RMSE(Accuracy_lda, test_set$Disease)
model_lda_rmse 

# model 6. Classification (Decision) Trees

## fit a classification tree and plot it

fit_tree <- train(liverdisease ~ Age + Sex + Tot_Bil + Dir_Bil + Alkphos + Alamine + Aspartate + 
                    Tot_Prot + Albumin + A_G_Ratio,
                  method = "rpart",
                  tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                  data = train_set)
plot(fit_tree)

## get a prediction based on Decision Trees Model

prediction_Decision_Trees <- predict(fit_tree, test_set)
prediction_Decision_Trees

## compute accuracy

confusionMatrix(predict(fit_tree, test_set), test_set$liverdisease)$table
Accuracy_Decision_Trees <- confusionMatrix(predict(fit_tree, test_set), test_set$liverdisease)$overall["Accuracy"]
Accuracy_Decision_Trees

## rmse of classification tree Model

model_Decision_Trees_rmse <- RMSE(Accuracy_Decision_Trees, test_set$Disease)
model_Decision_Trees_rmse 

# model 7. randomForest Trees

library(randomForest)

## create a randomForest tree model

fit_randomForest <- randomForest(liverdisease ~ Age + Sex + Tot_Bil + Dir_Bil + Alkphos + Alamine + Aspartate + 
                                   Tot_Prot + Albumin + A_G_Ratio, data = train_set) 

## get a prediction based on randomForest Trees

prediction_randomForest <- predict(fit_randomForest, test_set)
prediction_randomForest

## compute accuracy

confusionMatrix(predict(fit_randomForest, test_set), test_set$liverdisease)$table
Accuracy_randomForest<- confusionMatrix(predict(fit_randomForest, test_set), test_set$liverdisease)$overall["Accuracy"]
Accuracy_randomForest

## rmse of randomForest Model

model_randomForest_rmse <- RMSE(Accuracy_randomForest, test_set$Disease)
model_randomForest_rmse 

# model 8.Regularization model
## create a test set that exclude "NA"

test_set1 <- test_set %>% 
  semi_join(train_set, by = "Age") %>%
  semi_join(train_set, by = "Sex") %>%
  semi_join(train_set, by = "Tot_Bil")%>%
  semi_join(train_set, by = "Dir_Bil")%>%
  semi_join(train_set, by = "Alkphos")%>%
  semi_join(train_set, by = "Alamine")%>%
  semi_join(train_set, by = "Tot_Prot")%>%
  semi_join(train_set, by = "Aspartate")%>%
  semi_join(train_set, by = "Albumin")%>%
  semi_join(train_set, by = "A_G_Ratio")

dim(test_set1)

## compute the average based on each predictor

mu_Disease<- mean(train_set$Disease)
mu_Disease

### average by Age

Age_avgs <- train_set %>% 
  group_by(Age) %>% 
  summarize(b_Age = mean(Disease - mu_Disease))

Age_avgs

### average by Sex

Sex_avgs <- train_set %>% 
  left_join(Age_avgs, by='Age') %>%
  group_by(Sex) %>%
  summarize(b_Sex = mean(Disease - mu_Disease -b_Age ))

Sex_avgs

### average by Tot_Bil 

Tot_Bil_avgs <- train_set %>% 
  left_join(Age_avgs, by='Age') %>%
  left_join(Sex_avgs, by='Sex') %>%
  group_by(Tot_Bil) %>%
  summarize(b_Tot_Bil = mean(Disease - mu_Disease -b_Age-b_Sex))

Tot_Bil_avgs

### average by Dir_Bil

Dir_Bil_avgs <- train_set %>% 
  left_join(Age_avgs, by='Age') %>%
  left_join(Sex_avgs, by='Sex') %>%
  left_join(Tot_Bil_avgs, by='Tot_Bil') %>%
  group_by(Dir_Bil) %>%
  summarize(b_Dir_Bil = mean(Disease - mu_Disease - b_Age - b_Sex -b_Tot_Bil ))

Dir_Bil_avgs

### average by Alkphos 

Alkphos_avgs <- train_set %>% 
  left_join(Age_avgs, by='Age') %>%
  left_join(Sex_avgs, by='Sex') %>%
  left_join(Tot_Bil_avgs, by='Tot_Bil') %>%
  left_join(Dir_Bil_avgs, by='Dir_Bil') %>%
  group_by(Alkphos) %>%
  summarize(b_Alkphos = mean(Disease - mu_Disease - b_Age - b_Sex -b_Tot_Bil - b_Dir_Bil ))

Alkphos_avgs

### average by Alamine 

Alamine_avgs <- train_set %>% 
  left_join(Age_avgs, by='Age') %>%
  left_join(Sex_avgs, by='Sex') %>%
  left_join(Tot_Bil_avgs, by='Tot_Bil') %>%
  left_join(Dir_Bil_avgs, by='Dir_Bil') %>%
  left_join(Alkphos_avgs, by='Alkphos') %>%
  group_by(Alamine) %>%
  summarize(b_Alamine = mean(Disease - mu_Disease - b_Age - b_Sex -b_Tot_Bil - b_Dir_Bil - b_Alkphos))

Alamine_avgs

### average by Aspartate

Aspartate_avgs <- train_set %>% 
  left_join(Age_avgs, by='Age') %>%
  left_join(Sex_avgs, by='Sex') %>%
  left_join(Tot_Bil_avgs, by='Tot_Bil') %>%
  left_join(Dir_Bil_avgs, by='Dir_Bil') %>%
  left_join(Alkphos_avgs, by='Alkphos') %>%
  left_join(Alamine_avgs, by='Alamine') %>%
  group_by(Aspartate) %>%
  summarize(b_Aspartate = mean(Disease - mu_Disease - b_Age - b_Sex -b_Tot_Bil - b_Dir_Bil - b_Alkphos - b_Alamine))

Aspartate_avgs

### average by Tot_Prot

Tot_Prot_avgs <- train_set %>% 
  left_join(Age_avgs, by='Age') %>%
  left_join(Sex_avgs, by='Sex') %>%
  left_join(Tot_Bil_avgs, by='Tot_Bil') %>%
  left_join(Dir_Bil_avgs, by='Dir_Bil') %>%
  left_join(Alkphos_avgs, by='Alkphos') %>%
  left_join(Alamine_avgs, by='Alamine') %>%
  left_join(Aspartate_avgs, by='Aspartate') %>%
  group_by(Tot_Prot) %>%
  summarize(b_Tot_Prot = mean(Disease - mu_Disease - b_Age - b_Sex -b_Tot_Bil - b_Dir_Bil - b_Alkphos - b_Alamine -b_Aspartate))

Tot_Prot_avgs

### average by Albumin

Albumin_avgs <- train_set %>% 
  left_join(Age_avgs, by='Age') %>%
  left_join(Sex_avgs, by='Sex') %>%
  left_join(Tot_Bil_avgs, by='Tot_Bil') %>%
  left_join(Dir_Bil_avgs, by='Dir_Bil') %>%
  left_join(Alkphos_avgs, by='Alkphos') %>%
  left_join(Alamine_avgs, by='Alamine') %>%
  left_join(Aspartate_avgs, by='Aspartate') %>%
  left_join(Tot_Prot_avgs, by='Tot_Prot') %>%
  group_by(Albumin) %>%
  summarize(b_Albumin = mean(Disease - mu_Disease - b_Age - b_Sex -b_Tot_Bil - b_Dir_Bil - b_Alkphos - b_Alamine - b_Aspartate - b_Tot_Prot))

Albumin_avgs

### average by A_G_Ratio

A_G_Ratio_avgs <- train_set %>% 
  left_join(Age_avgs, by='Age') %>%
  left_join(Sex_avgs, by='Sex') %>%
  left_join(Tot_Bil_avgs, by='Tot_Bil') %>%
  left_join(Dir_Bil_avgs, by='Dir_Bil') %>%
  left_join(Alkphos_avgs, by='Alkphos') %>%
  left_join(Alamine_avgs, by='Alamine') %>%
  left_join(Aspartate_avgs, by='Aspartate') %>%
  left_join(Tot_Prot_avgs, by='Tot_Prot') %>%
  left_join(Albumin_avgs, by='Albumin') %>%
  group_by(A_G_Ratio) %>%
  summarize(b_A_G_Ratio = mean(Disease - mu_Disease - b_Age - b_Sex -b_Tot_Bil - b_Dir_Bil - b_Alkphos - b_Alamine - b_Aspartate - b_Tot_Prot - b_Albumin))

A_G_Ratio_avgs

## Data set for Regularization model

Regularization_predicted_Disease <- test_set1 %>% 
  left_join(Age_avgs, by='Age') %>%
  left_join(Sex_avgs, by='Sex') %>%
  left_join(Tot_Bil_avgs, by='Tot_Bil') %>%
  left_join(Dir_Bil_avgs, by='Dir_Bil') %>%
  left_join(Alkphos_avgs, by='Alkphos') %>%
  left_join(Alamine_avgs, by='Alamine') %>%
  left_join(Aspartate_avgs, by='Aspartate') %>%
  left_join(Tot_Prot_avgs, by='Tot_Prot') %>%
  left_join(Albumin_avgs, by='Albumin') %>%
  left_join(A_G_Ratio_avgs, by='A_G_Ratio') %>%
  mutate(pred = mu_Disease + b_Age + b_Sex + b_Tot_Bil + b_Dir_Bil + b_Alkphos + b_Alamine + b_Aspartate + b_Tot_Prot + b_Albumin + b_A_G_Ratio) %>%
  .$pred

Regularization_predicted_Disease

## transfer numeric to "0"--no disease, "1"--liver disease

Regularization_predicted_Disease1 <- ifelse(Regularization_predicted_Disease> 0.5, "1", "0") %>% factor
Regularization_predicted_Disease1

## compute accuracy

confusionMatrix(Regularization_predicted_Disease1, test_set1$liverdisease)$table
Accuracy_Regularization <-confusionMatrix(Regularization_predicted_Disease1, test_set1$liverdisease)$overall[["Accuracy"]]
Accuracy_Regularization

## rmse of Regularization_model

Regularization_model_rmse <- RMSE(Regularization_predicted_Disease, test_set1$Disease)
Regularization_model_rmse


# model 9. Tuning regularization model

## add Penalized least squares with different tuning

lambdas_dl <- seq(0, 40, 0.5)

all_reg_lambda_dl_rmses <- sapply(lambdas_dl, function(l){
  
  mu_Disease<- mean(train_set$Disease)
  
  b_iAge <- train_set %>% 
    group_by(Age) %>% 
    summarize(b_iAge = sum(Disease - mu_Disease)/(n()+l))
  
  b_iSex <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    group_by(Sex) %>%
    summarize(b_iSex = mean(Disease - mu_Disease - b_iAge)/(n()+l))
  
  b_iTot_Bil <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    group_by(Tot_Bil) %>%
    summarize(b_iTot_Bil = mean(Disease - mu_Disease - b_iAge - b_iSex)/(n()+l))
  
  b_iDir_Bil <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    left_join(b_iTot_Bil, by='Tot_Bil') %>%
    group_by(Dir_Bil) %>%
    summarize(b_iDir_Bil = mean(Disease - mu_Disease - b_iAge - b_iSex - b_iTot_Bil)/(n()+l))
  
  b_iAlkphos <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    left_join(b_iTot_Bil, by='Tot_Bil') %>%
    left_join(b_iDir_Bil, by='Dir_Bil') %>%
    group_by(Alkphos) %>%
    summarize(b_iAlkphos = mean(Disease - mu_Disease - b_iAge - b_iSex - b_iTot_Bil- b_iDir_Bil)/(n()+l))
  
  b_iAlamine <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    left_join(b_iTot_Bil, by='Tot_Bil') %>%
    left_join(b_iDir_Bil, by='Dir_Bil') %>%
    left_join(b_iAlkphos, by='Alkphos') %>%
    group_by(Alamine) %>%
    summarize(b_iAlamine = mean(Disease - mu_Disease - b_iAge - b_iSex - b_iTot_Bil- b_iDir_Bil - b_iAlkphos)/(n()+l))
  
  b_iAspartate <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    left_join(b_iTot_Bil, by='Tot_Bil') %>%
    left_join(b_iDir_Bil, by='Dir_Bil') %>%
    left_join(b_iAlkphos, by='Alkphos') %>%
    left_join(b_iAlamine, by='Alamine') %>%
    group_by(Aspartate) %>%
    summarize(b_iAspartate = mean(Disease - mu_Disease - b_iAge - b_iSex - b_iTot_Bil- b_iDir_Bil - b_iAlkphos - b_iAlamine)/(n()+l))
  
  
  b_iTot_Prot <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    left_join(b_iTot_Bil, by='Tot_Bil') %>%
    left_join(b_iDir_Bil, by='Dir_Bil') %>%
    left_join(b_iAlkphos, by='Alkphos') %>%
    left_join(b_iAlamine, by='Alamine') %>%
    left_join(b_iAspartate, by='Aspartate') %>%
    group_by(Tot_Prot) %>%
    summarize(b_iTot_Prot = mean(Disease - mu_Disease - b_iAge - b_iSex - b_iTot_Bil- b_iDir_Bil - b_iAlkphos - b_iAlamine - b_iAspartate)/(n()+l))
  
  
  b_iAlbumin <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    left_join(b_iTot_Bil, by='Tot_Bil') %>%
    left_join(b_iDir_Bil, by='Dir_Bil') %>%
    left_join(b_iAlkphos, by='Alkphos') %>%
    left_join(b_iAlamine, by='Alamine') %>%
    left_join(b_iAspartate, by='Aspartate') %>%
    left_join(b_iTot_Prot, by='Tot_Prot') %>%
    group_by(Albumin) %>%
    summarize(b_iAlbumin = mean(Disease - mu_Disease - b_iAge - b_iSex - b_iTot_Bil- b_iDir_Bil - b_iAlkphos - b_iAlamine - b_iAspartate - b_iTot_Prot)/(n()+l))
  
  
  b_iA_G_Ratio <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    left_join(b_iTot_Bil, by='Tot_Bil') %>%
    left_join(b_iDir_Bil, by='Dir_Bil') %>%
    left_join(b_iAlkphos, by='Alkphos') %>%
    left_join(b_iAlamine, by='Alamine') %>%
    left_join(b_iAspartate, by='Aspartate') %>%
    left_join(b_iTot_Prot, by='Tot_Prot') %>%
    left_join(b_iAlbumin, by='Albumin') %>%
    group_by(A_G_Ratio) %>%
    summarize(b_iA_G_Ratio = mean(Disease - mu_Disease - b_iAge - b_iSex - b_iTot_Bil- b_iDir_Bil - b_iAlkphos - b_iAlamine - b_iAspartate - b_iTot_Prot - b_iAlbumin)/(n()+l))
  
  Regularization_d_predicted_Disease <- test_set1 %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    left_join(b_iTot_Bil, by='Tot_Bil') %>%
    left_join(b_iDir_Bil, by='Dir_Bil') %>%
    left_join(b_iAlkphos, by='Alkphos') %>%
    left_join(b_iAlamine, by='Alamine') %>%
    left_join(b_iAspartate, by='Aspartate') %>%
    left_join(b_iTot_Prot, by='Tot_Prot') %>%
    left_join(b_iAlbumin, by='Albumin') %>%
    left_join(b_iA_G_Ratio, by='A_G_Ratio') %>%
    mutate(pred = mu_Disease + b_iAge + b_iSex + b_iTot_Bil+ b_iDir_Bil + b_iAlkphos + b_iAlamine + b_iAspartate + b_iTot_Prot + b_iAlbumin + b_iA_G_Ratio) %>%
    pull(pred)
  
  Regularization_d_predicted_Disease1 <- ifelse(Regularization_d_predicted_Disease> 0.5, "1", "0") %>% factor
  
  
  return(RMSE(Regularization_d_predicted_Disease, test_set1$Disease))
})

all_reg_lambda_dl_rmses


## **summary λ with corresponding rmse and find the lambda that get the smallest rmse**

qplot(lambdas_dl, all_reg_lambda_dl_rmses) 

lambda_rmse <- lambdas_dl [which.min(all_reg_lambda_dl_rmses)]
lambda_rmse

all_reg_lambda_dl_rmses[which.min(all_reg_lambda_dl_rmses)]


## prediction Accuracy by add Penalized least squares with different tuning

lambdas_dl <- seq(0, 40, 0.5)

all_reg_lambda_dl_Accuracy <- sapply(lambdas_dl, function(k){
  
  mu_Disease<- mean(train_set$Disease)
  
  b_iAge <- train_set %>% 
    group_by(Age) %>% 
    summarize(b_iAge = sum(Disease - mu_Disease)/(n()+k))
  
  b_iSex <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    group_by(Sex) %>%
    summarize(b_iSex = mean(Disease - mu_Disease - b_iAge)/(n()+k))
  
  b_iTot_Bil <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    group_by(Tot_Bil) %>%
    summarize(b_iTot_Bil = mean(Disease - mu_Disease - b_iAge - b_iSex)/(n()+k))
  
  b_iDir_Bil <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    left_join(b_iTot_Bil, by='Tot_Bil') %>%
    group_by(Dir_Bil) %>%
    summarize(b_iDir_Bil = mean(Disease - mu_Disease - b_iAge - b_iSex - b_iTot_Bil)/(n()+k))
  
  b_iAlkphos <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    left_join(b_iTot_Bil, by='Tot_Bil') %>%
    left_join(b_iDir_Bil, by='Dir_Bil') %>%
    group_by(Alkphos) %>%
    summarize(b_iAlkphos = mean(Disease - mu_Disease - b_iAge - b_iSex - b_iTot_Bil- b_iDir_Bil)/(n()+k))
  
  b_iAlamine <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    left_join(b_iTot_Bil, by='Tot_Bil') %>%
    left_join(b_iDir_Bil, by='Dir_Bil') %>%
    left_join(b_iAlkphos, by='Alkphos') %>%
    group_by(Alamine) %>%
    summarize(b_iAlamine = mean(Disease - mu_Disease - b_iAge - b_iSex - b_iTot_Bil- b_iDir_Bil - b_iAlkphos)/(n()+k))
  
  b_iAspartate <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    left_join(b_iTot_Bil, by='Tot_Bil') %>%
    left_join(b_iDir_Bil, by='Dir_Bil') %>%
    left_join(b_iAlkphos, by='Alkphos') %>%
    left_join(b_iAlamine, by='Alamine') %>%
    group_by(Aspartate) %>%
    summarize(b_iAspartate = mean(Disease - mu_Disease - b_iAge - b_iSex - b_iTot_Bil- b_iDir_Bil - b_iAlkphos - b_iAlamine)/(n()+k))
  
  
  b_iTot_Prot <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    left_join(b_iTot_Bil, by='Tot_Bil') %>%
    left_join(b_iDir_Bil, by='Dir_Bil') %>%
    left_join(b_iAlkphos, by='Alkphos') %>%
    left_join(b_iAlamine, by='Alamine') %>%
    left_join(b_iAspartate, by='Aspartate') %>%
    group_by(Tot_Prot) %>%
    summarize(b_iTot_Prot = mean(Disease - mu_Disease - b_iAge - b_iSex - b_iTot_Bil- b_iDir_Bil - b_iAlkphos - b_iAlamine - b_iAspartate)/(n()+k))
  
  
  b_iAlbumin <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    left_join(b_iTot_Bil, by='Tot_Bil') %>%
    left_join(b_iDir_Bil, by='Dir_Bil') %>%
    left_join(b_iAlkphos, by='Alkphos') %>%
    left_join(b_iAlamine, by='Alamine') %>%
    left_join(b_iAspartate, by='Aspartate') %>%
    left_join(b_iTot_Prot, by='Tot_Prot') %>%
    group_by(Albumin) %>%
    summarize(b_iAlbumin = mean(Disease - mu_Disease - b_iAge - b_iSex - b_iTot_Bil- b_iDir_Bil - b_iAlkphos - b_iAlamine - b_iAspartate - b_iTot_Prot)/(n()+k))
  
  
  b_iA_G_Ratio <- train_set %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    left_join(b_iTot_Bil, by='Tot_Bil') %>%
    left_join(b_iDir_Bil, by='Dir_Bil') %>%
    left_join(b_iAlkphos, by='Alkphos') %>%
    left_join(b_iAlamine, by='Alamine') %>%
    left_join(b_iAspartate, by='Aspartate') %>%
    left_join(b_iTot_Prot, by='Tot_Prot') %>%
    left_join(b_iAlbumin, by='Albumin') %>%
    group_by(A_G_Ratio) %>%
    summarize(b_iA_G_Ratio = mean(Disease - mu_Disease - b_iAge - b_iSex - b_iTot_Bil- b_iDir_Bil - b_iAlkphos - b_iAlamine - b_iAspartate - b_iTot_Prot - b_iAlbumin)/(n()+k))
  
  Regularization_d_predicted_Disease <- test_set1 %>% 
    left_join(b_iAge, by='Age') %>%
    left_join(b_iSex, by='Sex') %>%
    left_join(b_iTot_Bil, by='Tot_Bil') %>%
    left_join(b_iDir_Bil, by='Dir_Bil') %>%
    left_join(b_iAlkphos, by='Alkphos') %>%
    left_join(b_iAlamine, by='Alamine') %>%
    left_join(b_iAspartate, by='Aspartate') %>%
    left_join(b_iTot_Prot, by='Tot_Prot') %>%
    left_join(b_iAlbumin, by='Albumin') %>%
    left_join(b_iA_G_Ratio, by='A_G_Ratio') %>%
    mutate(pred = mu_Disease + b_iAge + b_iSex + b_iTot_Bil+ b_iDir_Bil + b_iAlkphos + b_iAlamine + b_iAspartate + b_iTot_Prot + b_iAlbumin + b_iA_G_Ratio) %>%
    pull(pred)
  
  Regularization_d_predicted_Disease1 <- ifelse(Regularization_d_predicted_Disease> 0.5, "1", "0") %>% factor
  
  
  return(confusionMatrix(Regularization_d_predicted_Disease1, test_set1$liverdisease)$overall[["Accuracy"]])
  
})
all_reg_lambda_dl_Accuracy 

## the Accuracy of Tuning_regularization_model

lambdas_dl [which.min(all_reg_lambda_dl_rmses)]
all_reg_lambda_dl_Accuracy[which.min(all_reg_lambda_dl_rmses)]

## the rmse of Tuning_regularization_model

lambdas_dl [which.max(all_reg_lambda_dl_Accuracy)]
all_reg_lambda_dl_rmses[which.max(all_reg_lambda_dl_Accuracy)]

## the highest accuracy of Tuning_regularization_model

the_highest_acccuracy_of_Tuning_regularization_model<-  all_reg_lambda_dl_Accuracy [which.max(all_reg_lambda_dl_Accuracy)]
the_highest_acccuracy_of_Tuning_regularization_model

## the smallest rmse of Tuning_regularization_model

the_smallest_rmse_of_all_Tuning_regularization_model <- all_reg_lambda_dl_rmses[which.min(all_reg_lambda_dl_rmses)]
the_smallest_rmse_of_all_Tuning_regularization_model


## summary of accuracy and rmse 

asscracy_rmse_summary<-data_frame ( Method = c("liner Regression Model", "Logistic Regression Model","knn Model", "qda Model","lda Model", "Decision Trees", "random Forest Trees", "Regularization model", "Tuning regularization model"), Accuracy = c(Accuracy_lm, Accuracy_glm, Accuracy_knn, Accuracy_qda, Accuracy_lda, Accuracy_Decision_Trees, Accuracy_randomForest, Accuracy_Regularization, the_highest_acccuracy_of_Tuning_regularization_model), rmse = c( model_lm_rmse, model_glm_rmse, model_knn_rmse, model_qda_rmse, model_lda_rmse, model_Decision_Trees_rmse, model_randomForest_rmse, Regularization_model_rmse, the_smallest_rmse_of_all_Tuning_regularization_model))

asscracy_rmse_summary

