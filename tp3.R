library(RCurl)
library (Matrix)
library(readr)
library(dplyr)
library(plyr)
library(ggplot2)
u.data <- read.csv(text = getURL('http://www.groupes.polymtl.ca/log6308/Tp/20173/u.data.csv', userpwd = '20113:20113'), sep='|', header=T)
R <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(R) <- paste('u', 1:nrow(R), sep='')
colnames(R) <- paste('i', 1:ncol(R), sep='')
R<-as.matrix(R)
###############Q1#############
#Determine a point of comparison for the forecast of votes (a minimum performance)
#方法1
R[R==0]<-NA
Rmeans<-rowMeans(R, na.rm = TRUE)
Cmeans<-colMeans(R, na.rm = TRUE)
nc<-length(R[1,])
nr<-length(R[,1])
a <- matrix(Rmeans, nrow=nr, ncol=nc)
b <- t(matrix(Cmeans, nrow=nc, ncol=nr))
c <-(a+b)/2
mae<- function(m1, m2) mean(abs(m1-m2), na.rm=T)
mae(c,R)

#方法2
m.baseline <- outer(rowMeans(R, na.rm=T),colMeans(R, na.rm=T),'+') / 2
mean(abs(m.baseline - R), na.rm=T)


###############Q2###########
#Apply decomposition SVD (taking care to standardize beforehand)
R[R==0]<-NA
Rnorm<-R-rowMeans(R, na.rm=TRUE)
Rnorm[is.na(Rnorm)]<-0
Rsvd<-svd(Rnorm)
str(Rsvd)
S<-Rsvd$d
U<-Rsvd$u
V<-Rsvd$v


m.imp <- R #start with an imputation and we will choose to take the expected values.
m.imp[is.na(R)] <- m.baseline[is.na(R)]
m.imp[1:10,1:10]
m.imp.rowm <- rowMeans(m.imp)
sum(rowSums(m.imp - m.imp.rowm)) #verification
m.svd <- svd(m.imp - m.imp.rowm)
#############Q3###############
# Estimate votes based on SVD with 10 dimensions
#方法1
dim<-10
Sprime<-diag(c(S[1:10],rep(0, length(S)-dim)))
Rsvd.10dimens<-rowMeans(R, na.rm=TRUE)+ U %*% Sprime %*% t(V)

#方法2
m.svd10 <- m.svd$u[,1:10] %*% diag(m.svd$d[1:10]) %*% t(m.svd$v[,1:10]) + m.imp.rowm
#############Q4################
#Calculate the mean absolute error and the mean squared error
#方法1
mae<- function(m1, m2) mean(abs(m1-m2), na.rm=T)
mae(Rsvd.10dimens ,R)
sqrt(mean((Rsvd.10dimens - R)^2, na.rm=T))

#方法2
mean(abs(m.svd10 - R), na.rm=T)
sqrt(sum((m.svd10 - R)^2, na.rm=T)/sum(!is.na(R)))

#############Q5################
#Determine the optimal number of dimensions (without applying cross-validation). 
#A graph should indicate performance by number of dimensions 

###Without cross-validation, the number of optimal dimensions is the maximum number of dimensions,
#since SVD reproduces the original matrix.

calculate_error <- function(dim){
  Sprime <- diag(c(S[1:dim],rep(0,length(S)-dim)))
  Rsvd.10dimens <- rowMeans(R,na.rm = T)+U %*% Sprime %*% t(V)
  return(mae(Rsvd.10dimens,R))
}

dimensions <- seq(10, 943,50)
results <- sapply(dimensions,calculate_error)
qplot(dimensions,results) 


######################Q6###################
#Repeat the forecast based on the optimal number of dimensions, but this time using cross-validation
#方法1
err<-c(1:5)
errors <- seq(8, 14, 1)
index=0

observed <- which(R > 0) # Non-zero values of R
hasard <- sample(observed, length(observed))# sampling indices of non-zero values of R
fold.size <- round(length(hasard) / 5)  # 5 folds ==> test on 20% of items and train on 90%
i.false <- rep(FALSE, length(R))

for(di in seq(8,14,1)){ #only test the dimensions 8-14
  
  index=index+1
  
  for(k in 1:5){ #iteration of folds
    
    fold.number <- k # The index of the test set
    
    ## Boolean Indices for Test and Training Cells
    test.b <- i.false
    ##The corresponding indexed cells of the corresponding fold are set to TRUE for the test ...
    test.b[ hasard[((fold.number-1) * fold.size):((fold.number) * fold.size)] ] <- TRUE
    ## ... and FALSE for training
    train.b <-  !test.b
    R.train <- R
    R.train[R.train==0]<-NA
    R.train[test.b] <- NA    # we remove the test data for training
    Rnorm.train <- R.train-rowMeans(R.train,na.rm = T)
    Rnorm.train[is.na(Rnorm.train)] <- 0
    
    ### SVD on training data ###
    
    
    Rsvd.train <- svd(Rnorm.train)
    str(Rsvd.train)
    S.train<-Rsvd.train$d
    U.train<-Rsvd.train$u
    V.train<-Rsvd.train$v
    
    Sprime.train <- diag(c(S.train[1:di],rep(0,length(S.train)-di)))
    R.10dimens.train <- rowMeans(R.train,na.rm = T)+U.train %*% Sprime.train %*% t(V.train)
    R[is.na(R)] <- 0
    err[k]<-mae(R.10dimens.train[test.b], R[test.b])# the average absolute error for the folds in question
    
  }
  errors[index]<-min(err) # the minimum error between all the folds for the dimension in question
  
}

qplot(seq(8, 14, 1),errors) # error diagram = f (dimension)


#方法2
#We will cross-validate 10 folds for 10-20 dimensions.
val.i <- which(!is.na(R))
length(val.i) 
replis <- matrix(sample(length(val.i)),length(val.i)/10)
sim.replis <- function(m.v, ndim, test.i) {
  m.train <- m.v
  m.train[test.i] <- NA
  m.train.baseline <- outer(rowMeans(m.train, na.rm=T),colMeans(m.train, na.rm=T),'+') / 2
  m.train.imp <- m.train
  m.train.imp[is.na(m.train)] <- m.train.baseline[is.na(m.train)]
  m.train.imp.rowm <- rowMeans(m.train.imp)
  m.train.svd <- svd(m.train.imp - m.train.imp.rowm)
  ## iterations on the dimensions provided
  res.est <- sapply(ndim, function(i) {
    m.est <- m.train.svd$u[,1:i] %*% diag(m.train.svd$d[1:i]) %*% t(m.train.svd$v[,1:i]) + m.train.imp.rowm
    mean(abs(m.est[test.i] - m.v[test.i]), na.rm=T)
  })
  return(res.est)
}
## This calculation takes a few tens of minutes
sim.replis(R, 10:20, replis[,1])

result <- apply(replis, 2, function(j) sim.replis(R, 2:20, j))
plot(2:20,rowMeans(result))


######################Q7########################
#Compare the performance of this approach with that of a collaborative approach of your choice
#(with squared error and absolute error averages). Use cross validation.

cosinus.vm <- function (v, m) {n <- sqrt (colSums (m ^ 2)); (v %*% m)/(n * sqrt (sum (v ^ 2)))} 

## index of test data

i.observed <- which(R > 0) 
i.hasard <- sample(i.observed, length(i.observed)) 
fold.size <- round(length(i.hasard) / 10)  # 10 folds ==> test on 10% of items and train on 90%
i.false <- rep(FALSE, length(R))
fold.number <- 1 # The index of the test set

i.test.b <- i.false
## The indexed cells of the corresponding fold are set to TRUE for the test ...
i.test.b[ i.hasard[((fold.number-1) * fold.size):((fold.number) * fold.size)] ] <- TRUE
## ... and FALSE for training
i.train.b <-  !i.test.b
R[is.na(R)] <- 0
m.na.train <- R
m.na.train[i.test.b] <- 0   # we remove the test data for training



cosinematrix = matrix(nrow=943,ncol=943);
for( i in 1:943){
  cosinematrix[i,]<-cosinus.vm(t(m.na.train)[,i],t(m.na.train))  #calculus of the cosine between the lines of the matrix m.na.train
}
cosinematrix[is.na(cosinematrix)]<-0  #eliminate the negative values in the cosine matrix

m.na.train[m.na.train==0]<-NA
Rmeans<-rowMeans(m.na.train, na.rm = TRUE)
nc<-length(R[1,])
nr<-length(R[,1])
a <- matrix(Rmeans, nrow=nr, ncol=nc)
m.na.train[is.na(m.na.train)]<-0


#calculation of the matrix of predicted values according to the Item-Item approach with cosines as weights

m.prediction <- a+(cosinematrix%*% (m.na.train-a))/rowSums(abs(cosinematrix))
m.prediction[m.prediction < 0]<-0  # eliminate the negative values of the prediction matrix

#MAE

mae(m.prediction[i.test.b], R[i.test.b])
mean(abs(m.prediction[i.test.b] - R[i.test.b]), na.rm=T)


#RMSE
sqrt(mean((m.prediction[i.test.b] - R[i.test.b])^2, na.rm=T))


############ collaborative method without cross validation###########

cosinematrix = matrix(nrow=943,ncol=943);
for( i in 1:943){
  cosinematrix[i,]<-cosinus.vm(t(R)[,i],t(R))  #calculus of the cosine between the rows of the matrix m
}

R[R==0]<-NA
Rmeans<-rowMeans(R, na.rm = TRUE)
nc<-length(R[1,])
nr<-length(R[,1])
a <- matrix(Rmeans, nrow=nr, ncol=nc)
R[is.na(R)] <- 0
m.prediction <- a+(cosinematrix%*% (R-a))/rowSums(abs(cosinematrix))
mae(m.prediction, R)
sqrt(mean((m.prediction - R)^2, na.rm=T))