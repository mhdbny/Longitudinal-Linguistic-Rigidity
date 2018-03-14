# Single Document Analysis

# -------------------- Load Data -----------------------------
setwd('path to dir')
df_clean = read.csv('SingleDocSignals.csv') 
#Drop Unneeded Variables:
df_clean<- df_clean[,-c(1)]
df_clean$perPosDoc<-as.factor(df_clean$perPosDoc)  

# -------------------- Data Info & Plotting -----------------------------

# obtain per-class distribution
dis<-vector()
for(k in 1:8)
  dis[k]=length(which(df_clean$rank==k))/dim(df_clean)[1]
dis

summary(df_clean)
# Per-Group Info
groupVar <- list()
groupAvg <- list()
groupCount <- list()
groupText<- list()
Groups=levels(df_clean$groupName) 
for( grp in Groups)
{
  groupVar[grp]=var(df_clean[which(df_clean$groupName==grp),]$rank)
  groupAvg[grp]=mean(df_clean[which(df_clean$groupName==grp),]$rank)
  groupCount[grp]=length(df_clean[which(df_clean$groupName==grp),]$rank)
  groupText[grp]=paste(grp,groupCount[grp],round(as.numeric(groupAvg[grp]), digits = 2),round(as.numeric(groupVar[grp]), digits = 2),sep=" ")
}

# Inter-Rater Reliability 
distance.to.label<-vector()
distance.to.label.add.one<- vector()
distance.to.label.minus.one<-vector()
for(k in 1:8){
  distance.to.label[k]<-0
  distance.to.label.add.one[k]<- 0
  distance.to.label.minus.one[k]<-0
}
 
for(k in 1:dim(df_clean)[1]){
  model <- lm(rank~.-rank-perNegDoc, data=df_clean[-c(k),c(2:20)])
  pred<-predict(model, df_clean[c(k),c(2:20)])
  testing.rank=df_clean[c(k),]$rank
  abs.difference=abs(testing.rank- pred)
  abs.add.one.difference=ifelse(testing.rank<8, abs((testing.rank+1) - pred), 10) # assign large value (10) so it will not be included in the min calculation 
  abs.minus.one.difference=ifelse(testing.rank>1,abs((testing.rank-1) - pred),10) # assign large value (10) so it will not be included in the min calculation
  min.difference=min(abs.difference,abs.add.one.difference,abs.minus.one.difference)
  if(min.difference==abs.difference) # original rank has the lowest absolute error
    distance.to.label[testing.rank]=distance.to.label[testing.rank]+1
  else if(min.difference==abs.add.one.difference) # rank+1 has the lowest absolute error
    distance.to.label.add.one[testing.rank]=distance.to.label.add.one[testing.rank]+1
  else
    distance.to.label.minus.one[testing.rank]=distance.to.label.minus.one[testing.rank]+1
}

# distribution
barplot(table(df_clean$rank)/length(df_clean$rank),ylim=c(0,0.3),xlab="Linguistic Rigidity",ylab="Percentage",border="blue",  col="lightblue",cex.lab=1.50, cex.axis=1.55, cex.main=1.75, cex.sub=1.75, cex=1.75)
lines(density(df_clean$rank),lwd=2)

library(ggplot2)
## Pronouns
ggplot(df_clean, aes(x=as.factor(rank), y = nous)) + geom_boxplot() + ggtitle('"We" Pronouns') + xlab("Linguistic Rigidity") + ylab('fraction of words that are "We" pronoun')
ggplot(df_clean, aes(x=as.factor(rank), y = ils)) + geom_boxplot() + ggtitle('"They" Pronouns') + xlab("Linguistic Rigidity") + ylab('fraction of words that are "They" pronoun')
ggplot(df_clean, aes(x=as.factor(rank), y = je)) + geom_boxplot() + ggtitle('"Me" Pronouns') + xlab("Linguistic Rigidity") + ylab('fraction of words that are "They" pronoun')
ggplot(df_clean, aes(x=as.factor(rank), y = le)) + geom_boxplot() + ggtitle('"It" Pronouns') + xlab("Linguistic Rigidity") + ylab('fraction of words that are "They" pronoun')

## Judgments
ggplot(df_clean, aes(x=as.factor(rank), y = judgementFrac)) + geom_boxplot() + ggtitle('Judgments') + xlab("Linguistic Rigidity") + ylab('fraction of sentences counted at judgments')

## MLK Plot
mlk_god<-c(49,20,9,7,7,6,4,21,20,12,42,7,26,12,13)
mlk_title<-c('','','','','',"I have a dream's Speech",'','','','','','','','','')
mlk_sentiment_pos<-c(0.0405703422053232,0.0403041825095057,0.0554752851711027,0.0488212927756654,0.0420342205323194,0.0593346007604563,0.0340494296577947,0.0320532319391635,0.0257984790874525,0.0368441064638783,0.0388403041825095,0.0404372623574144,0.0295247148288973,0.0263307984790875,0.0265969581749049)
mlk_sentiment_pos_perc<-mlk_sentiment_pos*100
mlk_year<-c(1954.25,1956.85,1957.33,1957.89,1963.47,1963.73,1965.24,1965.536,1966.46,1967.256,1967.256,1967.439,1967.658,1968.097,1968.268)
df = data.frame(mlk_god, mlk_year,mlk_sentiment_pos,mlk_sentiment_pos_perc,mlk_title)
events <- data.frame(time = c(1963.382,1965.183,1963.66),
                     text = c('Birmingham, Alabama protest',
                              'Selma, Alabama',"I have a dream's Speech"))
events <- data.frame(time = c(1963.382,1965.183 ),
                     text = c('Birmingham, Alabama protest',
                              'Selma, Alabama' ))
plot(mlk_year,mlk_god)
axis(side=1, at=seq(1954, 1968, by=2))
box()
abline(lm(mlk_god~.,data=df), col = 'red')
#install.packages("ggrepel")
library(ggrepel)
#frac pos sentiment vs date 3.54x11 
set.seed(10)
ggplot(df, aes(x=mlk_year, y = mlk_sentiment_pos_perc)) + geom_point(color = 'red') + geom_smooth(method = "lm")+
theme(axis.text.x = element_text(colour="black",size=12, face="plain"),
      axis.text.y = element_text(colour="black",size=12, ,face="plain"),  
      axis.title.x = element_text(colour="black",size=12, face="plain"),
      axis.title.y = element_text(colour="black",size=12,face="plain"))+
  scale_x_continuous("Document Date",breaks = seq(1954, 1968, by=2)) +
  scale_y_continuous("% Positive",breaks = seq(2.5, 6.0, by=0.5)) +
  geom_vline(data = events, aes(xintercept = as.numeric(time)),linetype = 2,size=1)+ 
  geom_label_repel(
    aes( label = mlk_title),
    fontface = 'bold', color = 'black',
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"),
    segment.color = 'grey50'
  )   
#  freq god vs date
ggplot(df, aes(x=mlk_year, y = mlk_god)) + geom_point(color = 'red') + geom_smooth(method = "lm")+
  theme(axis.text.x = element_text(colour="black",size=12, face="plain"),
        axis.text.y = element_text(colour="black",size=12, ,face="plain"),  
        axis.title.x = element_text(colour="black",size=12, face="plain"),
        axis.title.y = element_text(colour="black",size=12,face="plain"))+
  scale_x_continuous("Document Date",breaks = seq(1954, 1968, by=2)) +
  scale_y_continuous("Frequency of 'God'") +
  geom_vline(data = events, aes(xintercept = as.numeric(time)),linetype = 2,size=1)
#+geom_text(data = events, mapping = aes(label = text, x = time, y = -Inf), angle = 60, hjust = 0)
#  -------------------- Correlation Analysis and Partial F-test  -----------------------------
model <- lm(rank~.-rank-perNegDoc, data=df_clean[,c(2:20)])
model.nogroup <- lm(rank~.-rank-perNegDoc-groupName, data=df_clean[,c(2:20)])
#library(stargazer)
#stargazer(model)
summary(model)
anova(model,model.nogroup)
# -------------------- Function to evaluate regression predictions on data -----------------------------
eval_training <- function(model,data){
  pred<-predict(model, data)
  pred_rounded<-round(pred)
  data_len=dim(df_clean)[1]
  return(c(sum(abs(df_clean$rank - pred))/data_len,sum(df_clean$rank == pred_rounded)/data_len))
}
# -------------------- Training Error  -----------------------------
# Linear regression
model <- lm(rank~., data=df_clean[,c(2:20)])
perf=eval_training(model,df_clean[,c(2:20)])
perf
# Weighted Least squares with weights equals to the inverse of group-variance
train.len=dim(df_clean)[1]
doc_group_var<- rep(0, times = train.len)
Groups=levels(df_clean$groupName) 
for( grp in Groups)
{
  doc_group_var[which(df_clean$groupName==grp)]=as.numeric(var(df_clean[which(df_clean$groupName==grp),]$rank))
}
model <- lm(rank~., data=df_clean[,c(2:20)],weights=1/(doc_group_var+0.00001))
perf=eval_training(model,df_clean[,c(2:20)])
perf
# regression random forest
library(caret)
library(randomForest)
model <- randomForest(rank ~., data = df_clean[,c(2:20)])
perf=eval_training(model,df_clean[,c(2:20)])
perf
# boosted trees
require(xgboost)
library(caret)
library(dplyr)
param <- list("objective" = "reg:linear",   
              'max_depth' = 4,
              'eval_metric' = 'mae',
              'seed'='123')
train_X<-data.matrix(df_clean[,c(2:20)])
train_Y<- data.matrix(df_clean$rank)
model <- xgboost(param=param, data=train_X, label=train_Y, nrounds=5,verbose=0)
perf=eval_training(model,data.matrix(df_clean[,c(2:20)]))
perf
# regression support vector machine
library(kernlab)
model <- ksvm(rank ~., data = df_clean[,c(2:20)], type="eps-svr", kernel="vanilladot", C=100)
perf=eval_training(model,df_clean[,c(2:20)])
perf

#  -------------------- 10-CV Testing Error  -----------------------------
set.seed(123)
#Create Folds
folds<- createFolds(df_clean$rank, k=10, list = TRUE, returnTrain = FALSE)
# ------------------ Linear regression---------------------
#initialzie empty vector to store accuracy, mean absolute error, and per-class accuracy
exact_accuracy<- vector()
mae<-vector()
per_class_accuracy<- matrix(0,nrow=10, ncol=8)
i=0
j=0
for (i in 1:10) {
  difference<- vector()
  per_class_count<-rep(0, 8) 
  test.indices<- folds[[i]]
  #Create training and testing sets
  train = df_clean[-test.indices,]
  test = df_clean[test.indices,]
  train.len=dim(train)[1]
  #train Model
  model <- lm(rank~., data=train[,c(2:20)])
  #Make predictions based on model for testing set
  predictions<- predict(model, test[,c(2:20)])
  pred_labels <-round(predictions)
  #Find the difference between the predicted value and the actual value
  test_count<-dim(test)[1]
  for (j in 1: test_count) {
    diff<- abs(test$rank[j] - pred_labels[j])
    difference[j]<- diff
    # update per class count
    per_class_count[test$rank[j]]=per_class_count[test$rank[j]]+1
    per_class_accuracy[i,test$rank[j]]=per_class_accuracy[i,test$rank[j]]+(test$rank[j] == pred_labels[j])
  }
  #Accuracy
  exact_accuracy[i]= sum(difference ==0)/test_count
  # MEA
  mae[i]=mean(abs(predictions - test$rank))
  # calculate per class accuracy
  for(j in 1:8){
    if(per_class_count[j]>0){
      per_class_accuracy[i,j]=per_class_accuracy[i,j]/per_class_count[j]
    }
  }
}
Avg_exact_Accuracy = mean(exact_accuracy)
Avg_exact_Accuracy
Avg_MAE=mean(mae)
Avg_MAE 
per_class_accuracy_avg=round(mean(per_class_accuracy[,1]),2)
for(j in 2:8)
  per_class_accuracy_avg=paste(per_class_accuracy_avg,round(mean(per_class_accuracy[,j]),2),sep=",")
per_class_accuracy_avg

# ------------------ Weighted Least squares ---------------------
# Weighted Least squares with weights equals to the inverse of group-variance
#initialzie empty vector to store accuracy, mean absolute error, and per-class accuracy
Groups=levels(df_clean$groupName)
exact_accuracy<- vector()
mae<-vector()
per_class_accuracy<- matrix(0,nrow=10, ncol=8)
i=0
j=0
for (i in 1:10) {
  difference<- vector()
  per_class_count<-rep(0, 8) 
  test.indices<- folds[[i]]
  #Create training and testing sets
  train = df_clean[-test.indices,]
  test = df_clean[test.indices,]
  train.len=dim(train)[1]
  #train Model
  doc_group_var<- rep(0, times = train.len)
  for( grp in Groups)
  {
    doc_group_var[which(train$groupName==grp)]=as.numeric(var(train[which(train$groupName==grp),]$rank))
  }
  model <- lm(rank~., data=train[,c(2:20)],weights=1/(doc_group_var+0.00001))
  #Make predictions based on model for testing set
  predictions<- predict(model, test[,c(2:20)])
  pred_labels <-round(predictions)
  #Find the difference between the predicted value and the actual value
  test_count<-dim(test)[1]
  for (j in 1: test_count) {
    diff<- abs(test$rank[j] - pred_labels[j])
    difference[j]<- diff
    # update per class count
    per_class_count[test$rank[j]]=per_class_count[test$rank[j]]+1
    per_class_accuracy[i,test$rank[j]]=per_class_accuracy[i,test$rank[j]]+(test$rank[j] == pred_labels[j])
  }
  #Accuracy
  exact_accuracy[i]= sum(difference ==0)/test_count
  # MEA
  mae[i]=mean(abs(predictions - test$rank))
  # calculate per class accuracy
  for(j in 1:8){
    if(per_class_count[j]>0){
      per_class_accuracy[i,j]=per_class_accuracy[i,j]/per_class_count[j]
    }
  }
}
Avg_exact_Accuracy = mean(exact_accuracy)
Avg_exact_Accuracy
Avg_MAE=mean(mae)
Avg_MAE
per_class_accuracy_avg=round(mean(per_class_accuracy[,1]),2)
for(j in 2:8)
  per_class_accuracy_avg=paste(per_class_accuracy_avg,round(mean(per_class_accuracy[,j]),2),sep=",")
per_class_accuracy_avg
# ------------------ regression random forest ---------------------
library(caret)
library(randomForest)
#initialzie empty vector to store accuracy, mean absolute error, and per-class accuracy
exact_accuracy<- vector()
mae<-vector()
per_class_accuracy<- matrix(0,nrow=10, ncol=8)
i=0
j=0
for (i in 1:10) {
  difference<- vector()
  per_class_count<-rep(0, 8) 
  test.indices<- folds[[i]]
  #Create training and testing sets
  train = df_clean[-test.indices,]
  test = df_clean[test.indices,]
  train.len=dim(train)[1]
  #train Model
  model <- randomForest(rank ~. -groupId, data = train)
  #Make predictions based on model for testing set
  predictions<- predict(model, test)
  pred_labels <-round(predictions)
  #Find the difference between the predicted value and the actual value
  test_count<-dim(test)[1]
  for (j in 1: test_count) {
    diff<- abs(test$rank[j] - pred_labels[j])
    difference[j]<- diff
    # update per class count
    per_class_count[test$rank[j]]=per_class_count[test$rank[j]]+1
    per_class_accuracy[i,test$rank[j]]=per_class_accuracy[i,test$rank[j]]+(test$rank[j] == pred_labels[j])
  }
  #Accuracy
  exact_accuracy[i]= sum(difference ==0)/test_count
  # MEA
  mae[i]=mean(abs(predictions - test$rank))
  # calculate per class accuracy
  for(j in 1:8){
    if(per_class_count[j]>0){
      per_class_accuracy[i,j]=per_class_accuracy[i,j]/per_class_count[j]
    }
  }
}
Avg_exact_Accuracy = mean(exact_accuracy)
Avg_exact_Accuracy
Avg_MAE=mean(mae)
Avg_MAE
per_class_accuracy_avg=round(mean(per_class_accuracy[,1]),2)
for(j in 2:8)
  per_class_accuracy_avg=paste(per_class_accuracy_avg,round(mean(per_class_accuracy[,j]),2),sep=",")
per_class_accuracy_avg
# ------------------ boosted trees ---------------------
require(xgboost)
library(caret)
library(dplyr)
param <- list("objective" = "reg:linear",   
              'max_depth' = 4,
              'eval_metric' = 'mae',
              'seed'='123')
exact_accuracy<- vector()
mae<-vector()
per_class_accuracy<- matrix(0,nrow=10, ncol=8)
i=0
j=0
for (i in 1:10) {
  difference<- vector()
  per_class_count<-rep(0, 8) 
  test.indices<- folds[[i]]
  #Create training and testing sets
  train = df_clean[-test.indices,]
  test = df_clean[test.indices,]
  #Convert to Matrix
  train_X<-data.matrix(train[,c(2, 20)])
  train_Y<- data.matrix(train$rank)
  test_X<- data.matrix(test[,c(2,20)])
  test_Y = data.matrix(test$rank)

  train.len=dim(train)[1]
  #train Model
  model <- xgboost(param=param, data=train_X, label=train_Y, nrounds=5,verbose=0)
  #Make predictions based on model for testing set
  predictions<- predict(model, test_X)
  pred_labels <-round(predictions)
  #Find the difference between the predicted value and the actual value
  test_count<-dim(test)[1]
  for (j in 1: test_count) {
    diff<- abs(test$rank[j] - pred_labels[j])
    difference[j]<- diff
    # update per class count
    per_class_count[test$rank[j]]=per_class_count[test$rank[j]]+1
    per_class_accuracy[i,test$rank[j]]=per_class_accuracy[i,test$rank[j]]+(test$rank[j] == pred_labels[j])
  }
  #Accuracy
  exact_accuracy[i]= sum(difference ==0)/test_count
  # MEA
  mae[i]=mean(abs(predictions - test$rank))
  # calculate per class accuracy
  for(j in 1:8){
    if(per_class_count[j]>0){
      per_class_accuracy[i,j]=per_class_accuracy[i,j]/per_class_count[j]
    }
  }
}
Avg_exact_Accuracy = mean(exact_accuracy)
Avg_exact_Accuracy
Avg_MAE=mean(mae)
Avg_MAE
per_class_accuracy_avg=round(mean(per_class_accuracy[,1]),2)
for(j in 2:8)
  per_class_accuracy_avg=paste(per_class_accuracy_avg,round(mean(per_class_accuracy[,j]),2),sep=",")
per_class_accuracy_avg
# ------------------ regression support vector machine ---------------------
library(kernlab)
#initialzie empty vector to store accuracy, mean absolute error, and per-class accuracy
exact_accuracy<- vector()
mae<-vector()
per_class_accuracy<- matrix(0,nrow=10, ncol=8)
i=0
j=0
for (i in 1:10) {
  difference<- vector()
  per_class_count<-rep(0, 8) 
  test.indices<- folds[[i]]
  #Create training and testing sets
  train = df_clean[-test.indices,]
  test = df_clean[test.indices,]
  train.len=dim(train)[1]
  #train Model
  model <- ksvm(rank ~. -groupId, data = train, type="eps-svr", kernel="vanilladot", C=100)
  #Make predictions based on model for testing set
  predictions<- predict(model, test)
  pred_labels <-round(predictions)
  #Find the difference between the predicted value and the actual value
  test_count<-dim(test)[1]
  for (j in 1: test_count) {
    diff<- abs(test$rank[j] - pred_labels[j])
    difference[j]<- diff
    # update per class count
    per_class_count[test$rank[j]]=per_class_count[test$rank[j]]+1
    per_class_accuracy[i,test$rank[j]]=per_class_accuracy[i,test$rank[j]]+(test$rank[j] == pred_labels[j])
  }
  #Accuracy
  exact_accuracy[i]= sum(difference ==0)/test_count
  # MEA
  mae[i]=mean(abs(predictions - test$rank))
  # calculate per class accuracy
  for(j in 1:8){
    if(per_class_count[j]>0){
      per_class_accuracy[i,j]=per_class_accuracy[i,j]/per_class_count[j]
    }
  }
}
Avg_exact_Accuracy = mean(exact_accuracy)
Avg_exact_Accuracy
Avg_MAE=mean(mae)
Avg_MAE
per_class_accuracy_avg=round(mean(per_class_accuracy[,1]),2)
for(j in 2:8)
  per_class_accuracy_avg=paste(per_class_accuracy_avg,round(mean(per_class_accuracy[,j]),2),sep=",")
per_class_accuracy_avg


# ------------------------ Confusion Matrix from LS and WLS ------------------------
# -------------- LR ---------------------
confusionMatrix<- array(0, c(8,8,10))
i=0
j=0
for (i in 1:10) {
  per_class_count<-rep(0, 8) 
  test.indices<- folds[[i]]
  #Create training and testing sets
  train = df_clean[-test.indices,]
  test = df_clean[test.indices,]
  #train Model
  model <- lm(rank~., data=train[,c(2:20)])
  #Make predictions based on model for testing set
  predictions<- predict(model, test[,c(2:20)])
  pred_labels <-round(predictions)
  #Find the difference between the predicted value and the actual value
  test_count<-dim(test)[1]
  for (j in 1: test_count) {
    # update per class count
    per_class_count[test$rank[j]]=per_class_count[test$rank[j]]+1
    confusionMatrix[test$rank[j],pred_labels[j],i]=confusionMatrix[test$rank[j],pred_labels[j],i]+1
  }
  
  # calculate per class accuracy
  for(j in 1:8){
    if(per_class_count[j]>0){
      confusionMatrix[j,,i]=confusionMatrix[j,,i]/per_class_count[j]
    }
  }
}

Actual<-vector()
Predicted<-vector()
Accuracy<-vector()
for(i in 1:8)
  for(j in 1:8){
    Actual<-c(Actual,i)
    Predicted<-c(Predicted,j)
    Accuracy<-c(Accuracy,round(mean(confusionMatrix[i,j,]),2))
  }

confusion=data.frame(Actual,Predicted,Accuracy)

#render plot
# we use three different layers
# first we draw tiles and fill color based on percentage of test cases
tile <- ggplot() +
  geom_tile(aes(x=Actual, y=Predicted,fill=Accuracy),data=confusion, color="black",size=0.1) +
  scale_y_continuous(breaks = seq(1, 8, by = 1)) + 
  scale_x_continuous(breaks = seq(1, 8, by = 1)) + 
  ggtitle("Least Squares") + 
  labs(x="Actual",y="Predicted")+
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),text= element_text(size=14),plot.title = element_text(hjust = 0.5),legend.position="none")
tile = tile + 
  geom_text(aes(x=Actual,y=Predicted, label=sprintf("%.2f", Accuracy)),data=confusion, size=3, colour="black") +
  scale_fill_gradient(limits = c(0,1),low="white",high="red")+
  labs(fill="Accuracy\n")
# lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
tile = tile + 
  geom_tile(aes(x=Actual,y=Predicted),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=1, fill="black", alpha=0) 

#render
tile

# ------------------ WLS -------------------------
Groups=levels(df_clean$groupName)
confusionMatrix<- array(0, c(8,8,10))
i=0
j=0
for (i in 1:10) {
  per_class_count<-rep(0, 8) 
  test.indices<- folds[[i]]
  #Create training and testing sets
  train = df_clean[-test.indices,]
  test = df_clean[test.indices,]
  train.len=dim(train)[1]
  #train Model
  doc_group_var<- rep(0, times = train.len)
  for( grp in Groups)
  {
    doc_group_var[which(train$groupName==grp)]=as.numeric(var(train[which(train$groupName==grp),]$rank))
  }
  model <- lm(rank~., data=train[,c(2:20)],weights=1/(doc_group_var+0.00001))
  #Make predictions based on model for testing set
  predictions<- predict(model, test[,c(2:20)])
  pred_labels <-round(predictions)
  #Find the difference between the predicted value and the actual value
  test_count<-dim(test)[1]
  for (j in 1: test_count) {
    # update per class count
    per_class_count[test$rank[j]]=per_class_count[test$rank[j]]+1
    confusionMatrix[test$rank[j],pred_labels[j],i]=confusionMatrix[test$rank[j],pred_labels[j],i]+1
  }
  # calculate per class accuracy
  for(j in 1:8){
    if(per_class_count[j]>0){
      confusionMatrix[j,,i]=confusionMatrix[j,,i]/per_class_count[j]
    }
  }
}
Actual<-vector()
Predicted<-vector()
Accuracy<-vector()
for(i in 1:8)
  for(j in 1:8){
    Actual<-c(Actual,i)
    Predicted<-c(Predicted,j)
    Accuracy<-c(Accuracy,round(mean(confusionMatrix[i,j,]),2))
  }

confusion=data.frame(Actual,Predicted,Accuracy) 
#render plot
# we use three different layers
# first we draw tiles and fill color based on percentage of test cases
tile <- ggplot() +
  geom_tile(aes(x=Actual, y=Predicted,fill=Accuracy),data=confusion, color="black",size=0.1) +
  scale_y_continuous(breaks = seq(1, 8, by = 1)) + 
  scale_x_continuous(breaks = seq(1, 8, by = 1)) + 
  ggtitle("Weighted Least Squares") + 
  labs(x="Actual",y="Predicted")+
  theme(axis.text.x = element_text(size=10),
      axis.text.y = element_text(size=10),text= element_text(size=14),plot.title = element_text(hjust = 0.5),legend.position="none")
tile = tile + 
  geom_text(aes(x=Actual,y=Predicted, label=sprintf("%.2f", Accuracy)),data=confusion, size=3, colour="black") +
  scale_fill_gradient(limits = c(0,1),low="white",high="red")+
  labs(fill="Accuracy\n")
# lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
tile = tile + 
  geom_tile(aes(x=Actual,y=Predicted),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=1, fill="black", alpha=0) 

#render
tile


# -------------------- Scaling data -----------------------------
# load data
setwd('path to dir')
df_clean = read.csv('SingleDocSignals.csv') 
#Drop Unneeded Variables:
df_clean<- df_clean[,-c(1)]
df_clean$perPosDoc<-as.factor(df_clean$perPosDoc)
# compute inter-group variance
doc_group_var<- rep(0, times = train.len)
Groups=levels(df_clean$groupName) 
for( grp in Groups)
{
  doc_group_var[which(df_clean$groupName==grp)]=1/(as.numeric(var(df_clean[which(df_clean$groupName==grp),]$rank))+0.00001)
}
# Scale
names(df_clean)
df_clean$perPos<-df_clean$perPos*doc_group_var
df_clean$perNeg<-df_clean$perNeg*doc_group_var
df_clean$PSJudge<-df_clean$PSJudge*doc_group_var
df_clean$judgementCount<-df_clean$judgementCount*doc_group_var
df_clean$judgementFrac<-df_clean$judgementFrac*doc_group_var
df_clean$nous<-df_clean$nous*doc_group_var
df_clean$vous<-df_clean$vous*doc_group_var
df_clean$je<-df_clean$je*doc_group_var
df_clean$ils<-df_clean$ils*doc_group_var
df_clean$il<-df_clean$il*doc_group_var
df_clean$elle<-df_clean$elle*doc_group_var
df_clean$le<-df_clean$le*doc_group_var
df_clean$UniqueWordCount<-df_clean$UniqueWordCount*doc_group_var
df_clean$avgSD<-df_clean$avgSD*doc_group_var
df_clean$avgEVC<-df_clean$avgEVC*doc_group_var

# Linear regression
model <- lm(rank~., data=df_clean[,c(2:20)])
perf=eval_training(model,df_clean[,c(2:20)])
perf
# regression random forest
library(caret)
library(randomForest)
model <- randomForest(rank ~., data = df_clean[,c(2:20)])
perf=eval_training(model,df_clean[,c(2:20)])
perf
# boosted trees
require(xgboost)
library(caret)
library(dplyr)
param <- list("objective" = "reg:linear",   
              'max_depth' = 4,
              'eval_metric' = 'mae',
              'seed'='123')
train_X<-data.matrix(df_clean[,c(2:20)])
train_Y<- data.matrix(df_clean$rank)
model <- xgboost(param=param, data=train_X, label=train_Y, nrounds=5,verbose=0)
perf=eval_training(model,data.matrix(df_clean[,c(2:20)]))
perf
# regression support vector machine
library(kernlab)
model <- ksvm(rank ~., data = df_clean[,c(2:20)], type="eps-svr", kernel="vanilladot", C=100)
perf=eval_training(model,df_clean[,c(2:20)])
perf


# -------------------- Boko haram Prediction -----------------------------

# Load training data
setwd('path to dir')
df_clean = read.csv('signalDocs_Ranked.csv') 
#Drop Unneeded Variables:
df_clean<- df_clean[,-c(1)]
df_clean$perPosDoc<-as.factor(df_clean$perPosDoc) 
summary(df_clean)


# train model
# Linear regression
model <- lm(rank~., data=df_clean[,c(2:19)])
perf=eval_training(model,df_clean[,c(2:19)])
perf

# load testing data
df_clean_test = read.csv('signalDocs_all.csv')
#Drop Unneeded Variables:
df_clean_test<- df_clean_test[,-c(1)]
df_clean_test$perPosDoc<-as.factor(df_clean_test$perPosDoc) 
summary(df_clean_test)

# predict
pred<-predict(model, df_clean_test)
pred_rounded<-round(pred)
pred_rounded<-pmax(pred_rounded,1)
pred_rounded<-pmin(pred_rounded,8)

pred_docs<-vector()
for(i in 1:length(df_clean_test$groupId))
  pred_docs[i]=paste(df_clean_test$groupId[i],pred_rounded[i],sep=' ')

write.table(pred_docs, "mydata.txt", sep="\t",col.names = FALSE, row.names = FALSE) 
