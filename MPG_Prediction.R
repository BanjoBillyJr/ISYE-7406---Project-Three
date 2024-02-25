library(lattice)
library(ggplot2)
library(dplyr)
library(melt)
library(reshape2)
library(naivebayes)
library(psych)
library(MASS)
library(e1071)



df1 <- read.table("Auto.csv", head=T, sep=",")
dim(df1)
head(df1)
summary(df1)

#MPG median is 22.75

boxplot(df1$mpg, main='MPG Box Plot', ylab='MPG')

#No outliers - no need to remove points to reassess median. 

#assign new Variables (b) 
df1['mpg01'] <- ifelse((df1$mpg > 22.75),1,0)
dim(df1[df1$mpg01 ==1,])[1]
dim(df1[df1$mpg01 ==0,])[1]

df <- df1[,-1]
dim(df[df$mpg01 ==1,])[1]
dim(df[df$mpg01 ==0,])[1]

#Visually analyze variables (c) 
plot(df1$cylinders,df1$mpg, main='MPG & Cylinders')
plot(df1$displacement,df1$mpg,main='MPG & Displacement')
plot(df1$horsepower,df1$mpg,main='MPG & Horsepower')
plot(df1$weight,df1$mpg,main='MPG & Weight')
plot(df1$acceleration,df1$mpg,main='MPG & Acceleration')
plot(df1$year,df1$mpg,main='MPG & Year')
plot(df1$origin,df1$mpg,main='MPG & Origin')

#Correlation Heatmap
corr_mat <- round(cor(df1[-1]),2)  

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
dist <- as.dist((1-corr_mat)/2)
# hierarchical clustering the dist matrix
hc <- hclust(dist)
corr_mat <-corr_mat[hc$order, hc$order]
# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
#head(melted_corr_mat)
ggplot(data = melted_corr_mat,aes(x=Var1, y=Var2, fill=value)) + geom_tile() +geom_text(aes(Var2, Var1, label = value), color = "orange", 
                                                                         size = 4)

pairs.panels(df)


#Variable selection
glm1 <- glm(mpg01~.,df,family= 'binomial')
glm2 <- step(glm1, direction ='backward')
summary(glm2)
summary(glm1)




#Split the data into training and test sets
smp_size <- floor(0.75 * nrow(df))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

dim(train)
dim(test)







#modeling
TrainErr <- NULL;
TestErr  <- NULL; 
#formula <- mpg01 ~ cylinders + displacement + horsepower + weight +acceleration + year + origin
formula <- mpg01 ~ horsepower + weight + year + origin
#formula <- mpg01 ~ cylinders + displacement + horsepower+weight+year+origin 
#model 1: LDA
model_1 <- lda(train[,1:7], train$mpg01); 
## training error 
## we provide a detailed code here 
pred1 <- predict(model_1,train[,1:7])$class
TrainErr <- c(TrainErr, mean( pred1  != train$mpg01)); 
TrainErr; 
## 0.08163265 for miss.class.train.error
## testing error 
pred1test <- predict(model_1,test[,1:7])$class; 
TestErr <- c(TestErr,mean(pred1test != test$mpg01));  
TestErr;
## 0.1122449 for miss.class.test.error


## You can also see the details of Testing Error
##     by the confusion table, which shows how the errors occur
cm1 <- table(pred1test,  test$mpg01) 
cm1

#model 2: QDA 
model_2 <- qda(train[,1:7], train$mpg01)
model_2$means
plot(pred2)
## Training Error 
pred2 <- predict(model_2,train[,1:7])$class
TrainErr <- c(TrainErr, mean( pred2!= train$mpg01))
TrainErr
## 0.01136364 for miss.class.train.error of QDA,
##  which is much smaller than LDA
##  Testing Error 
pred2test <- predict(model_2,test[,1:7])$class

TestErr <- c(TestErr, mean(pred2test != test$mpg01))
TestErr
cm2 <- table(pred2test,  test$mpg01) 
cm2

#model 3:Naive Bayes 
model_3<-naiveBayes(formula, data = train)

pred3 <- predict(model_3, train[,1:7]);
TrainErr <- c(TrainErr, mean( pred3 != train$mpg01))
TrainErr 
## Testing Error 
pred3test <- predict(model_3,test[,1:7])
TestErr <- c(TestErr,  mean( predict(model_3,test[,1:7]) != test$mpg01))
TestErr

cm3 <- table(test$mpg01, pred3test)



#model 4: Logistic Regression
model_4 <- glm(formula,train, family =binomial(link="logit"))

summary(model_4)


plot(train$weight, train$mpg01)
lines(train$weight,fitted.values(model_4), col="red")

confint(model_4, level=0.95)
coefficients(model_4)

y_pred <- predict(model_4,test[,1:7], type = 'response')


## Training Error  of (multinomial) logisitic regression
#TrainErr <- c(TrainErr, mean( round(predict(model_4, train[,1:7]))  != train$mpg01))
#TrainErr
##  0.08843537 for miss.class.train.error
## Testing Error of (multinomial) logisitic regression
#TestErr <- c(TestErr, mean( round(predict(model_4,test[,1:7])) != test$mpg01) )
#TestErr


#cm <- table(data = test$mpg01, round(y_pred))
#cm


library(nnet)
mod4 <- multinom(formula, data=train) 
summary(mod4);
## Training Error  of (multinomial) logisitic regression
TrainErr <- c(TrainErr, mean( predict(mod4, train[,1:7])  != train$mpg01))
TrainErr

##  0.08843537 for miss.class.train.error
## Testing Error of (multinomial) logisitic regression
TestErr <- c(TestErr, mean( predict(mod4,test[,1:7]) != test$mpg01) )
TestErr

pred4test <- predict(mod4,test[,1:7])


cm4 <- table(test$mpg01, pred4test)
cm4



#model 5: KNN
library(class);
kkk <- c(1,3,5,7, 9, 11, 13, 15);
xnew <- train[,1:7];
res1train <- NULL;
for (i in 1: 8){
  kk <- kkk[i];
  ypred2.train <- knn(train[,1:7], xnew, train[,8], k=kk);
  res1train <- rbind(res1train, cbind(kk, mean(ypred2.train != train[,8])));

}


min(res1train)

TrainErr <- c(TrainErr, min(res1train))
TrainErr

testerror = NULL
## 4B: 8 KNN methods
xnew2 <- test[,-8];
for (i in 1: 8){
  kk <- kkk[i];
  ypred2.test <- knn(train[,1:7], xnew2, train[,8], k=kk);
  testerror <- cbind( testerror, mean( ypred2.test != test[,8]));
}
colnames(testerror) <- c("KNN1", "KNN3", "KNN5", "KNN7", "KNN9", "KNN11", "KNN13", "KNN15")

round(testerror,6)

min(round(testerror,6))

TestErr <- c(TestErr, min(round(testerror,6)) )
TestErr

ypred2.test <- knn(train[,1:7], xnew2, train[,8], k=3)
ypred2.test
cm5 <- table(test$mpg01,ypred2.test)
cm5
#model 6: SVM
model_6 = svm(formula = formula, data = train, type = 'C-classification', kernel = 'linear') 

svm_pred_train = predict(model_6, newdata = train[,1:7]) 

TrainErr <- c(TrainErr, mean( svm_pred_train  != train$mpg01))
TrainErr

svm_pred_test = predict(model_6, newdata = test[,1:7]) 
TestErr <- c(TestErr, mean(svm_pred_test != test$mpg01) )
TestErr
svm_pred_test
cm6 = table(test[, 8],svm_pred_test) 


cm1
cm2
cm3
cm4
cm5
cm6


name <- c("LDA","QDA","Bayes","Log", 'KNN','SVM')
Results <- data.frame(name,TrainErr,TestErr)
Results

n= dim(df)[1];
n1= round(n/10);

#Cross Validation
B = 100;
TEALL=NULL;
for (b in 1:B){
  flag = sort(sample(1:n, n1));
  train = df[-flag,];
  test = df[flag, ];
  
  #model 1: LDA
  model_1 <- lda(train[,1:7], train$mpg01); 

  pred1test <- predict(model_1,test[,1:7])$class; 
  TestErr1 <- c(TestErr,mean(pred1test != test$mpg01));  

  #model 2: QDA 
  model_2 <- qda(train[,1:7], train$mpg01)
  pred2test <- predict(model_2,test[,1:7])$class
  TestErr2<- c(TestErr, mean(pred2test != test$mpg01))
  
  #model 3:Naive Bayes 
  model_3<-naiveBayes(formula, data = train)
  
  pred3test <- predict(model_3,test[,1:7])
  TestErr3 <- c(TestErr,  mean( predict(model_3,test[,1:7]) != test$mpg01))

  #model 4: Logistic Regression

  library(nnet)
  model4 <- multinom(formula, data=train) 

  TestErr4 <- c(TestErr, mean( predict(mod4,test[,1:7]) != test$mpg01) )

  #model 5: KNN
  library(class);
  kkk <- c(1,3,5,7, 9, 11, 13, 15);
  xnew <- train[,1:7];
  testerror = NULL
  ## 4B: 8 KNN methods
  xnew2 <- test[,-8];
  #for (i in 1: 8){
   # kk <- kkk[i];
    #ypred2.test <- knn(train[,1:7], xnew2, train[,8], k=kk);
  #  testerror <- cbind( testerror, mean( ypred2.test != test[,8]));
  #}
  #colnames(testerror) <- c("KNN1", "KNN3", "KNN5", "KNN7", "KNN9", "KNN11", "KNN13", "KNN15")
  
  #round(testerror,6)
  
  #min(round(testerror,6))
  
  #TestErr5 <- c(TestErr, min(round(testerror,6)) )
  ypred2.test <- knn(train[,1:7], xnew2, train[,8], k=3);
  TestErr5 <- c(TestErr, mean( ypred2.test != test[,8]))
  
  
  #model 6: SVM
  model_6 = svm(formula = formula, data = train, type = 'C-classification', kernel = 'linear') 

  svm_pred_test = predict(model_6, newdata = test[,1:7]) 
  TestErr6 <- c(TestErr, mean(svm_pred_test != test$mpg01) )

  #name <- c("LDA","QDA","Bayes","Log", 'KNN','SVM')
  #Results <- data.frame(name,TrainErr,TestErr)
  #Results
  
  TEALL = rbind(TEALL, cbind(TestErr1,TestErr2,TestErr3,TestErr4,TestErr5,TestErr6))

}
dim(TEALL);
round(apply(TEALL, 2, mean),6);


round(apply(TEALL, 2, var),6);
name <- c("LDA","QDA","Bayes","Log", 'KNN','SVM')
Results2 <- data.frame(name,round(apply(TEALL, 2, mean),6),round(apply(TEALL, 2, var),6))
Results2


