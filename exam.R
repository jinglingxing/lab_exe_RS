library(curl)
load(url('http://www.groupes.polymtl.ca/log6308/Public/20173/1915481.Rsave'))

##########Q1
#import some useful fuctions 

cosinus.vm <- function (v, m) {n <- sqrt (colSums (m ^ 2)); (v%*% m) / (n * sqrt (sum (v^ 2)))}
max.nindex <- function (m, n = 5) {
  i <- order (m, decreasing = TRUE)
  return (i [1: n])
}

min.nindex <- function(m, n=5) { 
  i <- order(m)
  return(i[1:n])
}
#################Q1

m1<-m[sample(nrow(m), 200), ]   #sample all the data of m matrix and dim(m1) is 20*200
#in order to get the 10 random votes for user 1 and compute the MAE at the last step, I ramdom all the data  
m1<-m1[,1:10]  #dim of m1 now is 200*10
q.row<-m1[,1]  #ramdom 10 votes, it is the first column

#####compute the rating estimate

dist.q.user <- sqrt(colSums((q.row-m1)^2,na.rm=T))  
neighbors<-min.nindex(dist.q.user,10) 
print(neighbors)

#use cosine as a weight
v=q.row
cosm<-cosinus.vm(v,t(m))
newcos<-cosm[neighbors]
k<-1/(sum(abs(newcos))) # calculation of k
#mean of v_a
mean.1<- mean(q.row)
#calculate vij-vi(mean)
mmeans <- m1-rowMeans(m1,na.rm=T)
newmeans2<-mmeans[neighbors,]   
vij.minus.vi<-newcos%*%newmeans2
#estimate the missing vote: v2
v2<-mean.1+k*colSums(vij.minus.vi)
print(v2)
max.v2<- max.nindex(v2, 10)
print(max.v2) 
m1[max.v2]  #the result for rating of user-user approach:  5 4 5 3 4 3 4 5 4 4

#the MAE value for all 10 estimates
v  <- as.matrix(v)
v2 <- as.matrix(v2)
for (val in max.v2) {
  Q1.mae<-mean(abs(q.row - m1[,val]), na.rm=T) 
  print(Q1.mae)
}

#the result for MAE of Q1:
#[1] 0.735
#[1] 0.66
#[1] 0
#[1] 0.62
#[1] 0.72
#[1] 1.065
#[1] 0.94
#[1] 0.51
#[1] 0.85
#[1] 0.835

###Q2
m2<-t(m1)  #transpose the matrix m1 to do the item-item approach
q.row1<-m2[1,]

#####compute the rating estimate

dist.q.user1 <- sqrt(colSums((q.row1-m2)^2,na.rm=T))  
neighbors1<-min.nindex(dist.q.user1,10) 
print(neighbors1)
#the result of neighbours: [1] 177  32  86  94 106  60  66 117 134 140
#use cosine as a weight
v11=q.row1
cosm1<-cosinus.vm(v,t(m))
newcos1<-cosm[neighbors1]
k1<-1/(sum(abs(newcos1))) # calculation of k
#mean of v_a, according to the formula 
mean.11<- mean(q.row1)
#calculate vij-vi(mean)
mmeans1 <- m2-rowMeans(m2,na.rm=T)
newmeans1<-mmeans[neighbors1,]   
vij.minus.vi.1<-newcos%*%newmeans2
#estimate the missing vote: v2
v3<-mean.11+k1*colSums(vij.minus.vi.1)
print(v3)
#the result of v3 is:
#[1] 4.459608 4.953276 4.260462 4.348476 3.756406 5.153630 3.859059 3.648601 3.657543 4.052938

max.v3<- max.nindex(v3, 10)
print(max.v3)
m2[max.v3]  #the result for rating of item-item approach:  [1] 5 3 5 4 4 4 3 1 4 4

#the MAE value for all 10 estimates
v11  <- as.matrix(v11)
v3 <- as.matrix(v3)
for (val in max.v3) {
  Q2.mae<-mean(abs(q.row1 - m2[,val]), na.rm=T) 
  print(Q2.mae)
}

#the result for MAE of Q2:
#[1] 1.415
#[1] 0.985
#[1] 0.815
#[1] 0.655
#[1] 0.915
#[1] 0.645
#[1] 0.775
#[1] 0.805
#[1] 0.625
#[1] 0.715


####Q3
R<-m
R[R==0]<-NA
Rnorm<-R-rowMeans(R, na.rm=TRUE)
Rnorm[is.na(Rnorm)]<-0
Rsvd<-svd(Rnorm)
str(Rsvd)
S<-Rsvd$d
U<-Rsvd$u
V<-Rsvd$v

# Estimate votes based on SVD with 10 dimensions
dim<-10
Sprime<-diag(c(S[1:10],rep(0, length(S)-dim)))
Rsvd.10dimens<-rowMeans(R, na.rm=TRUE)+ U %*% Sprime %*% t(V)

###without cross-validation : MAE:0.3094709
mae<- function(m1, m2) mean(abs(m1-m2), na.rm=T)
mae(Rsvd.10dimens ,R)

###with cross-validation 
#We will cross-validate 10 folds for 10-50 dimensions.
val.i <- which(!is.na(R))
length(val.i) 
replis <- matrix(sample(length(val.i)),length(val.i)/10)  #10 folds ==> test on 10% of items and train on 90%
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
sim.replis(R, 10:50, replis[,1])

result <- apply(replis, 2, function(j) sim.replis(R, 2:50, j))
plot(2:50,rowMeans(result))

#the result : [1] 0.4085847 0.3906906 0.3937637 0.4009311 0.4103235 0.4184169 0.4240814 0.4343808 0.4423798 0.4508571
#[11] 0.4556465 0.4625597 0.4704571 0.4782227 0.4837412 0.4919820 0.5002137 0.5075065 0.5138903 0.5216500
#[21] 0.5265725 0.5335065 0.5375401 0.5413379 0.5480989 0.5511783 0.5577442 0.5643567 0.5688686 0.5719589
#[31] 0.5750126 0.5790030 0.5833706 0.5882273 0.5926037 0.5975798 0.6027817 0.6060720 0.6090280 0.6138919
#[41] 0.6169432
#the result plot is like a Checkmark, the lowest point is about 11



###Q4
m.baseline <- outer(rowMeans(R, na.rm=T),colMeans(R, na.rm=T),'+') / 2
mean(abs(m.baseline - R), na.rm=T)
# the result is: 0.70423



###Q5
#compute the similarity of user matirx
user.means <- users- rowMeans(users)
user.weight <-cosinus.vm(user.means, t(users))  #user.weight represents distance between user-user
#user-user approach
user.k<-1/sum(abs(user.weight))
v5 <- mean(users) + user.k * (user.weight %*% (users - rowMeans(users, na.rm = T)))

#compute the similarity of item matirx
newitems<- t(items)
newitems[newitems==0]<-NA      #neglecting NA
item.weight<-cor(newitems, use="pairwise.complete.obs") #use correlation to compute items similarity :200*200
#item-item approach
item.weight[is.na(item.weight)]<-0
item.k<-1/sum(abs(item.weight))
newitems[is.na(newitems)]<-0 
v6<- mean(newitems) + item.k * (item.weight %*% t(newitems - rowMeans(newitems, na.rm = T)))
###compare###
###cross-validation
m1<-m  #m is the original data, i use our predict.ui compared with m 
val.i1 <- which(!is.na(m1))
length(val.i1) 
replis1 <- matrix(sample(length(val.i1)),length(val.i1)/10)  #10 folds ==> test on 10% of items and train on 90%
sim.replis1 <- function(m.v, ndim, test.i) {
  m.train <- m.v
  m.train[test.i] <- NA
  m.train.baseline <- outer(rowMeans(m.train, na.rm=T),colMeans(m.train, na.rm=T),'+') / 2
  m.train.imp <- m.train
  m.train.imp[is.na(m.train)] <- m.train.baseline[is.na(m.train)]
  m.train.imp.rowm <- rowMeans(m.train.imp)
  m.train.svd <- svd(m.train.imp - m.train.imp.rowm)
  ## iterations on the dimensions provided
  res.est <- sapply(ndim, function(i) {
    #m.est <- m.train.svd$u[,1:i] %*% diag(m.train.svd$d[1:i]) %*% t(m.train.svd$v[,1:i]) + m.train.imp.rowm
    predict.ui<-v5 %*% t(v6) 
    mean(abs( predict.ui[test.i] - m.v[test.i]), na.rm=T)
  })
  return(res.est)
}
## This calculation takes a few tens of minutes
sim.replis(m1, 10:50, replis1[,1])

result1 <- apply(replis1, 2, function(j) sim.replis(m1, 2:50, j))
plot(2:50,rowMeans(result1))

#the result :  [1] 0.3861119 0.3722925 0.3748073 0.3857268 0.3972716 0.4043255 0.4127292 0.4211305 0.4301011 0.4353961
#[11] 0.4409726 0.4516955 0.4583803 0.4648027 0.4727027 0.4787311 0.4835855 0.4919485 0.4987526 0.5052842
#[21] 0.5094361 0.5155988 0.5215843 0.5286777 0.5352607 0.5410235 0.5463939 0.5521196 0.5563475 0.5613171
#[31] 0.5673623 0.5724781 0.5762893 0.5798823 0.5848061 0.5886973 0.5919911 0.5969299 0.6022161 0.6053678
#[41] 0.6105865

#the result plot is like a Checkmark, the lowest point is about 11.
#the plot is similar with the question 3, but the value on the plot is totally different.
