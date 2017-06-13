########################
#  HOWEWORK 3
#  GT Account: kchen360
#  Name: Kai-yu Chen
########################

##
#### 0. Data Processing ####
##
library(graphics)
library(ggplot2)
#load data
mytrain <-read.csv("~/mnist_train.csv",header = FALSE)
mytest <-read.csv("~/mnist_test.csv",header = FALSE)
#training set for 0,1 and 3,5
train_0_1 <- mytrain[,which(mytrain[785,]==0 | mytrain[785,]==1)]
train_3_5 <- mytrain[,which(mytrain[785,]==3 | mytrain[785,]==5)]
#testing set for 0,1 and 3,5
test_0_1 <- mytest[,which(mytest[785,]==0 | mytest[785,]==1)]
test_3_5 <- mytest[,which(mytest[785,]==3 | mytest[785,]==5)]
#get class label vector in training set
true_label_train_0_1 <- train_0_1[785,]
true_label_train_3_5 <- train_3_5[785,]
#get class label vector in testing set
true_label_test_0_1 <- test_0_1[785,]
true_label_test_3_5 <- test_3_5[785,]
#get class 
class0 <- test_0_1[,which(test_0_1[785,]==0)]
class1 <- test_0_1[,which(test_0_1[785,]==1)]
class3 <- test_3_5[,which(test_3_5[785,]==3)]
class5 <- test_3_5[,which(test_3_5[785,]==5)]
#convert dataset to matrix and plotimage
#class 0
class0_matrix <- as.matrix(class0[-785,1],nrow=28, ncol=28)
image(class0_matrix,col=gray.colors(256))
title(main="class 0 from test_0_1")
dev.off()
#class 1
class1_matrix <- as.matrix(class1[-785,1],nrow=28, ncol=28)
image(class1_matrix,col=gray.colors(256))
title(main="class 1 from test_0_1")
dev.off()
#class 3
class3_matrix <- as.matrix(class3[-785,1],nrow=28, ncol=28)
image(class3_matrix,col=gray.colors(256))
title(main="class 3 from test_3_5")
dev.off()
#class 5
class5_matrix <- as.matrix(class5[-785,1],nrow=28, ncol=28)
image(class5_matrix,col=gray.colors(256))
title(main="class 5 from test_3_5")
dev.off()
#remove 785 row
train_0_1 <- train_0_1[-785,]
train_3_5 <- train_3_5[-785,]
test_0_1 <- test_0_1[-785,]
test_3_5 <- test_3_5[-785,]

##
#### 2. Implementation ####
##

# Function to fit logistic regression based on gradient descent
# x = training x
# y = training y
# w0 = initial weight
# max_it = maximum iteration
# threshold = converge criteria (stopping criteria)
logReg = function(x, y, w0, alpha, max_it, threshold){
  if(is.matrix(x)&is.numeric(x)){
    if(any(!is.na(x))){
      # Implement Sigmoid function
      sigmoid = function(z){
        s = 1/(1+exp(-z))
        return(s)
      }
      # Implement lost function 
      lost = function(w){
        n = nrow(x)
        g = sigmoid(x %*% c(w))
        L = -(1/n)*(sum((y*log(g)) + ((1-y)*log(1-g))))
        return(L)
      }
      # Define the gradient function
      gradient = function(w){
        n = nrow(x)
        g = sigmoid(x %*% c(w))
        grad = (1/n)*(t(x) %*% (g-y))
        return(t(grad))
      }
      gradient_descent = function(w0, alpha, max_it, threshold){
        # Initial w
        w = w0
        l = lost(w)
        l_initial = lost(w)
        l_update = NULL
        # Update w in maxit times
        for(i in 1:max_it){
          if((l_initial - l)<threshold){
            w = w - alpha * gradient(w)
            l = lost(w)
            l_update[[i]] = lost(w)
          }
        }
        return(list(w=w, l_update=l_update, iter=length(l_update)))
      }
      # Derive w and c using gradient descent
      gd = gradient_descent(w0, alpha, max_it, threshold)
      w = gd$w
      l_update = gd$l_update
      iter = gd$iter
      
      return(list(w_opt = w, l_update = l_update, iter = iter))
    } else{
      print("x contains missing values")
    }
  } else{
    print("x as to be a numeric matrix")
  }
}

# Function to predict probability based on fitted logreg_gd
logreg_predict = function(new_x, w_opt){
  sigmoid = function(z){
    g = 1/(1+exp(-z))
    return(g)
  }
  prob = sigmoid(new_x %*% c(w_opt))
  return(prob)
}

##
#### 3. Trainging ####
##

train_0_1 <- as.matrix(sapply(train_0_1, as.numeric)) 
test_0_1 <- as.matrix(sapply(test_0_1, as.numeric)) 
train_3_5 <- as.matrix(sapply(train_3_5, as.numeric)) 
test_3_5 <- as.matrix(sapply(test_3_5, as.numeric)) 
true_label_train_0_1 <-as.numeric(true_label_train_0_1)
true_label_train_3_5 <-as.numeric(true_label_train_3_5)
true_label_test_0_1 <-as.numeric(true_label_test_0_1)
true_label_test_3_5 <-as.numeric(true_label_test_3_5)
#transpose
train_0_1 <- t(train_0_1)
train_3_5 <- t(train_3_5)
test_0_1 <-t(test_0_1)
test_3_5 <-t(test_3_5)

# Function to calculate accuracy
accuracy = function(y_pred, y){
  e = y - y_pred
  a = length(e[e==0])/length(y)
  return(a)
}

# Funtion to calculate accuracy and logistic negative log likelihood 
# from repeated training and testing 
logreg_gd_split_repeat = function(w0, alpha, threshold,ratio,trainX,trainY,testX,testY){
  #assign vector 
  lr_l_update = NULL
  accuracy_train = NULL
  accuracy_test = NULL
  train_neg_loglikelihood = NULL
  test_neg_loglikelihood = NULL
  
  #combine train and test data for random sampling
  train <- cbind(trainX,trainY)
  test <-cbind(testX,testY)
  data <- rbind(train,test) 
  
  #measure accuracy and loss function repeated 10 times
  for (i in 1:10){
    #sampling train and test data
    size <- floor(ratio*nrow(data)) 
    train.ind <- sample(seq_len(nrow(data)), size)
    train<- data[train.ind, ]
    test<- data[-train.ind, ]
    
    train.x = train[,-785]
    train.y = ifelse(train[,785]==1|train[,785]==5,1,0)
    
    test.x = test[,-785]
    test.y = ifelse(test[,785]==1|test[,785]==5,1,0)
    
    # Fit Logistic Regression using gradient descent
    myfit = logReg(train.x, train.y, w0, alpha, max_it=3000, threshold)
    lr_w = myfit$w_opt
    lr_l_update[[i]] = myfit$l_update
    train_prob = log_predict(train.x, lr_w)
    train_event = ifelse(train_prob > 0.5,1,0)
    test_prob = log_predict(test.x, lr_w)
    test_event = ifelse(test_prob>0.5,1,0)
    accuracy_train[[i]] = round(accuracy(train_event, train.y),4)
    accuracy_test[[i]] = round(accuracy(test_event, test.y),4)
    # Train Loss 
    train_neg_loglikelihood[[i]] = -mean(train.y * log(train_prob) + (1-train.y) * log(1-train_prob))
    # Test Loss
    test_neg_loglikelihood[[i]] = -mean(test.y * log(test_prob) + (1-test.y) * log(1-test_prob))
    
  }
  #print train accuracy
  print(accuracy_train)
  #print test accuracy
  print(accuracy_test)
  avg_train_accuracy = mean(accuracy_train)
  avg_test_accuracy = mean(accuracy_test)
  avg_train_neg_log_lost = mean(train_neg_loglikelihood)
  avg_test_neg_log_lost = mean(test_neg_loglikelihood)
  return(list(avg_train_accuracy=avg_train_accuracy,
              avg_test_accuracy=avg_test_accuracy,
              avg_train_neg_log_lost=avg_train_neg_log_lost,
              avg_test_neg_log_lost=avg_test_neg_log_lost,
              l_update=lr_l_update))
}

#3a
#initialization parameter
w0 = rep(0, ncol(train_0_1))
alpha = 0.05
threshold = 0.1

#class 0,1
trainX = train_0_1
trainY = true_label_train_0_1
trainY = ifelse(trainY==1,1,0)

testX = test_0_1
testY = true_label_test_0_1
testY = ifelse(testY==1,1,0)

#run model
fit = logReg(trainX, trainY, w0, alpha, max_it=500, threshold)
lr_w = fit$w_opt
lr_l_update = fit$l_update
train_prob = log_predict(trainX, lr_w)
train_event = ifelse(train_prob > 0.5,1,0)
test_prob = log_predict(testX, lr_w)
test_event = ifelse(test_prob>0.5,1,0)
accuracy_train = round(accuracy(train_event, trainY),4)
accuracy_test = round(accuracy(test_event, testY),4)
sprintf("The training accuracy for class 0_1 is: %s",accuracy_train)
sprintf("The testing accuracy for class 0_1 is: %s",accuracy_test)

#class 3,5
trainX = train_3_5
trainY = true_label_train_3_5
trainY = ifelse(trainY==5,1,0)

testX = test_3_5
testY = true_label_test_3_5
testY = ifelse(testY==5,1,0)

#run model
fit2 = logReg(trainX, trainY, w0, alpha, max_it=500, threshold)
lr_w2 = fit2$w_opt
lr_l_update2 = fit2$l_update
train_prob2 = log_predict(trainX, lr_w2)
train_event2 = ifelse(train_prob2>0.5,1,0)
test_prob2 = log_predict(testX, lr_w2)
test_event2 = ifelse(test_prob2>0.5,1,0)
accuracy_train2 = round(accuracy(train_event2, trainY),4)
accuracy_test2 = round(accuracy(test_event2, testY),4)
sprintf("The training accuracy for class 3_5 is: %s",accuracy_train2)
sprintf("The testing accuracy for class 3_5 is: %s",accuracy_test2)

# 3b
#class 0, 1
w0 = rep(0, ncol(train_0_1))
alpha = 0.05
threshold = 0.1
#training dataset ratio
ratio = 0.7
fit = logreg_gd_split_repeat(w0, alpha, threshold, ratio, train_0_1,true_label_train_0_1,test_0_1,true_label_test_0_1)
paste("Average Training Accuracy of 10 runs:", round(fit$avg_train_accuracy,4))
paste("Average Testing Accuracy of 10 runs:", round(fit$avg_test_accuracy,4))

#class 3,5
w0 = rep(0, ncol(train_3_5))
alpha = 0.05
threshold = 0.1
ratio = 0.7
fit = logreg_gd_split_repeat(w0, alpha, threshold, ratio, train_3_5,true_label_train_3_5,test_3_5,true_label_test_3_5)
paste("Average Training Accuracy of 10 runs:", round(fit$avg_train_accuracy,4))
paste("Average Testing Accuracy of 10 runs:", round(fit$avg_test_accuracy,4))


##
#### 4. Evaluation ####
##

# 4a, there will be test in alpha = 0.001, 0.01, 0.05, 0.1, 0.5 
#initialization 1: alpha = 0.01
w0 = rep(0,ncol(train_3_5))
alpha = 0.01
threshold = 0.2
ratio = 0.7
fit = logreg_gd_split_repeat(w0, alpha, threshold, ratio, train_3_5,true_label_train_3_5,test_3_5,true_label_test_3_5)
paste("Average Training Accuracy for alpha = 0.01 of 10 runs:", round(fit$avg_train_accuracy,4))
paste("Average Testing Accuracy for alpha = 0.01 of 10 runs:", round(fit$avg_test_accuracy,4))

#initialization 2: alpha = 0.05
w0 = rep(0,ncol(train_3_5))
alpha = 0.05
threshold = 0.5
ratio = 0.7
fit = logreg_gd_split_repeat(w0, alpha, threshold, ratio, train_3_5,true_label_train_3_5,test_3_5,true_label_test_3_5)
paste("Average Training Accuracy for alpha = 0.05 of 10 runs:", round(fit$avg_train_accuracy,4))
paste("Average Testing Accuracy for alpha = 0.05 of 10 runs:", round(fit$avg_test_accuracy,4))

#initialization 3: alpha = 0.1
w0 = rep(0,ncol(train_3_5))
alpha = 0.1
threshold = 0.2
ratio = 0.7
fit = logreg_gd_split_repeat(w0, alpha, threshold, ratio, train_3_5,true_label_train_3_5,test_3_5,true_label_test_3_5)
paste("Average Training Accuracy for alpha = 0.1 of 10 runs:", round(fit$avg_train_accuracy,4))
paste("Average Testing Accuracy for alpha = 0.1 of 10 runs:", round(fit$avg_test_accuracy,4))

#initialization 4: alpha = 0.5
w0 = rep(0,ncol(train_3_5))
alpha = 0.5
threshold = 0.2
ratio = 0.7
fit = logreg_gd_split_repeat(w0, alpha, threshold, ratio, train_3_5,true_label_train_3_5,test_3_5,true_label_test_3_5)
paste("Average Training Accuracy for alpha = 0.5 of 10 runs:", round(fit$avg_train_accuracy,4))
paste("Average Testing Accuracy for alpha = 0.5 of 10 runs:", round(fit$avg_test_accuracy,4))

#initialization 4: alpha = 0.001
w0 = rep(0,ncol(train_3_5))
alpha = 0.001
threshold = 0.2
ratio = 0.7
fit = logreg_gd_split_repeat(w0, alpha, threshold, ratio, train_3_5,true_label_train_3_5,test_3_5,true_label_test_3_5)
paste("Average Training Accuracy for alpha = 0.001 of 10 runs:", round(fit$avg_train_accuracy,4))
paste("Average Testing Accuracy for alpha = 0.001 of 10 runs:", round(fit$avg_test_accuracy,4))

#4b: converge criteria is doen by 0.05, 0.1, 0.5

# 1.converge criteria = 0.05
w0 = rep(0,ncol(train_3_5))
alpha = 0.05
threshold = 0.05
ratio = 0.7
fit = logreg_gd_split_repeat(w0, alpha, threshold, ratio, train_3_5,true_label_train_3_5,test_3_5,true_label_test_3_5)
paste("Average Training Accuracy for threshold = 0.05 of 10 runs:", round(fit$avg_train_accuracy,4))
paste("Average Testing Accuracy for alpha = 0.05 of 10 runs:", round(fit$avg_test_accuracy,4))

# 2.converge criteria = 0.1
w0 = rep(0,ncol(train_3_5))
alpha = 0.05
threshold = 0.1
ratio = 0.7
fit = logreg_gd_split_repeat(w0, alpha, threshold, ratio, train_3_5,true_label_train_3_5,test_3_5,true_label_test_3_5)
paste("Average Training Accuracy for threshold = 0.1 of 10 runs:", round(fit$avg_train_accuracy,4))
paste("Average Testing Accuracy for alpha = 0.1 of 10 runs:", round(fit$avg_test_accuracy,4))

# 3.converge criteria = 0.5
w0 = rep(0,ncol(train_3_5))
alpha = 0.05
threshold = 0.5
ratio = 0.7
fit = logreg_gd_split_repeat(w0, alpha, threshold, ratio, train_3_5,true_label_train_3_5,test_3_5,true_label_test_3_5)
paste("Average Training Accuracy for threshold = 0.5 of 10 runs:", round(fit$avg_train_accuracy,4))
paste("Average Testing Accuracy for alpha = 0.5 of 10 runs:", round(fit$avg_test_accuracy,4))

##
## 5. Learning Curve
##

#5a
#class 0,1
train_size <- seq(0.05, 1.00, 0.05)
w0 <- rep(0,ncol(train_0_1))
alpha <- 0.05
threshold <- 0.5
# get learning curve
learning_cur <- sapply(train_size, function(x) logreg_gd_split_repeat(w0,alpha,threshold,x,train_0_1,true_label_train_0_1,test_0_1,true_label_test_0_1))
train_accuracy <- unlist(learning_cur["avg_train_accuracy",])
test_accuracy <- unlist(learning_cur["avg_test_accuracy",])
row <- cbind(train_size,train_accuracy,test_accuracy)
row <- as.data.frame(row)
colnames(split) <- c("train_size", "train_accuracy", "test_accuracy")
ggplot(split) + 
  geom_line(aes(x=train_size, y=train_accuracy, colour="train"), size=1) + 
  geom_line(aes(x=train_size, y=test_accuracy, colour="test"), size=1) +
  scale_color_manual(values=c("train"="#00abc5", "test"="#c45c57")) +
  labs(x="Training Set Proportion",
       y="Average Accuracy across 10 runs",
       title="Average Training and Testing Accuracy for different Training size in class 0/1")

#class3,5
#get learning curve
learning_cur2 <- sapply(train_size, function(x) logreg_gd_split_repeat(w0,alpha,threshold,x,train_3_5,true_label_train_3_5,test_3_5,true_label_test_3_5))
train_accuracy2 <- unlist(learning_cur2["avg_train_accuracy",])
test_accuracy2 <- unlist(learning_cur2["avg_test_accuracy",])
row2 <- cbind(train_size,train_accuracy2,test_accuracy2)
split2 <- as.data.frame(row2)
colnames(split2) <- c("train_size", "train_accuracy", "test_accuracy")
ggplot(split2) + 
  geom_line(aes(x=train_size, y=train_accuracy2, colour="train"), size=1) + 
  geom_line(aes(x=train_size, y=test_accuracy2, colour="test"), size=1) +
  scale_color_manual(values=c("train"="#00abc5", "test"="#c45c57")) +
  labs(x="Training Set Proportion",
       y="Average Accuracy across 10 runs",
       title="Average Training and Testing Accuracy for different Training size in class 3/5")

#5b
#class 0/1
#get loss function
train_log_lost <- unlist(learning_cur["avg_train_neg_log_lost",])
test_log_lost <-unlist(learning_cur["avg_test_neg_log_lost",])
row3 <- cbind(train_size,train_log_lost,test_log_lost)
split3 <- as.data.frame(row3)
colnames(split3) <- c("train_size", "train_neg_loglikelihood", "test_train_neg_loglikelihood")
ggplot(split3) + 
  geom_line(aes(x=train_size, y=train_log_lost, colour="train"), size=1) + 
  geom_line(aes(x=train_size, y=test_log_lost, colour="test"), size=1) +
  scale_color_manual(values=c("train"="#00abc5", "test"="#c45c57")) +
  labs(x="Training Set Proportion",
       y="Average Logistic Loss Loglikelihood across 10 runs",
       title="Average Training and Testing Logistic Loss Log Likelihood for different Training size in class 0/1")

#class3/5
#get loss function
train_log_lost2 <- unlist(learning_cur2["avg_train_neg_log_lost",])
test_log_lost2 <-unlist(learning_cur2["avg_test_neg_log_lost",])
row4 <- cbind(train_size,train_log_lost2,test_log_lost2)
split4 <- as.data.frame(row4)
colnames(split4) <- c("train_size", "train_neg_loglikelihood", "test_train_neg_loglikelihood")
ggplot(split4) + 
  geom_line(aes(x=train_size, y=train_log_lost2, colour="train"), size=1) + 
  geom_line(aes(x=train_size, y=test_log_lost2, colour="test"), size=1) +
  scale_color_manual(values=c("train"="#00abc5", "test"="#c45c57")) +
  labs(x="Training Set Proportion",
       y="Average Logistic Loss Loglikelihood across 10 runs",
       title="Average Training and Testing Logistic Loss Log Likelihood for different Training size in class 3/5")

