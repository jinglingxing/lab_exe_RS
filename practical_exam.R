library(curl)
library(ggplot2)
load(url('http://www.groupes.polymtl.ca/log6308/Public/20173/data.Rsave'))

############Question 7####################
#the 10 most similar items based on cosine

cosinus.vm <- function (v, m) {n <- sqrt (colSums (m ^ 2)); (v%*% m) / (n * sqrt (sum (v^ 2)))}
max.nindex <- function (m, n = 5) {
  i <- order (m, decreasing = TRUE)
  return (i [1: n])
}
q.col<-as.vector(m[,q.item])
cosvalue= t(cosinus.vm(q.col,m))
result<- max.nindex(cosvalue, 11)
print(result)

#verify similarity by RMSE

q.vote <- sum(m[,q.item]>0, na.rm=T)   #the n of the RMSE formula
for (val in result) {
  #the RMSE value for all 10 most similar items
  val.sqrt <-sqrt(sum((m[,q.item] - m[,val])^2 / q.vote, na.rm=T))  
  print(val.sqrt)
}

################Question 8#####################
common.vote <- (rowSums((m[q.user,] * m) > 0))# number of common votes
#find the neighbors of q.user
dist.q.user <- sqrt(colSums((m[q.user,]-m)^2,na.rm=T))  

min.nindex <- function(m, n=5) { 
  i <- order(m)
  return(i[1:n])
}

dist.q.user[common.vote==0]<-NA  #don't consider the common vote is 0
neighbors<-min.nindex(dist.q.user,11) 
print(neighbors)

common.vote[neighbors]  #Not all neighbors have common votes
remove<-neighbors[common.vote[neighbors]==0]
neighbors<-setdiff(neighbors,remove) # Delete neighbors who do not have common votes
print(neighbors)

#use cosine as a weight
v=m[q.user,]
cosm<-cosinus.vm(v,t(m))
newcos<-cosm[neighbors]
k<-1/(sum(abs(newcos))) # calculation of k
#mean of v_a
mean.1<- mean(m[q.user,])
#calculate vij-vi(mean)
mmeans <- m-rowMeans(m,na.rm=T)
newmeans2<-mmeans[neighbors,]   
vij.minus.vi<-newcos%*%newmeans2
#estimate the missing vote: v2
v2<-mean.1+k*colSums(vij.minus.vi)
print(v2)


####################Question 9##################
R<-m
R[R==0]<-NA
Rnorm<-R-rowMeans(R, na.rm=TRUE)
Rnorm[is.na(Rnorm)]<-0
Rsvd<-svd(Rnorm)
str(Rsvd)
S<-Rsvd$d
U<-Rsvd$u
V<-Rsvd$v

mae<- function(m1, m2) mean(abs(m1-m2), na.rm=T)
calculate_error <- function(dim){
  Sprime <- diag(c(S[1:dim],rep(0,length(S)-dim)))
  Rsvd.dimens <- rowMeans(R,na.rm = T)+U %*% Sprime %*% t(V)
  return(mae(Rsvd.dimens,R))
}

dimensions <- seq(10, 200, 10)
results <- sapply(dimensions,calculate_error)
qplot(dimensions,results) 

###use cross-validation###
val.i <- which(!is.na(R))  #index of cells containing votes
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
  ## it¨¦rations sur les dimensions fournies
  res.est <- sapply(ndim, function(i) {
    m.est <- m.train.svd$u[,1:i] %*% diag(m.train.svd$d[1:i]) %*% t(m.train.svd$v[,1:i]) + m.train.imp.rowm
    mean(abs(m.est[test.i] - m.v[test.i]), na.rm=T)
  })
  return(res.est)
}
sim.replis(R, 10:200, replis[,1])
result <- apply(replis, 2, function(j) sim.replis(m, 2:50, j))
plot(2:50,rowMeans(result))

##################Question 10####################
#compute the similarity of user matirx
user.weight <-cosinus.vm(user.means, t(users))  #user.weight represents distance between user-user
#user-user approach
user.k<-1/sum(abs(user.weight))
v3 <- mean(users) + user.k * (user.weight %*% (users - rowMeans(users, na.rm = T)))

#compute the similarity of item matirx
newitems<- t(items)
newitems[newitems==0]<-NA      #neglecting NA
item.weight<-cor(newitems, use="pairwise.complete.obs") #use correlation to compute items similarity :200*200
#item-item approach
item.weight[is.na(item.weight)]<-0
item.k<-1/sum(abs(item.weight))
newitems[is.na(newitems)]<-0 
v4<- mean(newitems) + item.k * (item.weight %*% t(newitems - rowMeans(newitems, na.rm = T)))
###compare###
predict.ui<-v3 %*% t(v4)   ##1000*200
origin.ui <- users %*% t(items)    #if the genres for users and items are aligned
Q10.Sqrt <- sqrt(mean((predict.ui - origin.ui)^2, na.rm=T))
print(Q10.Sqrt)
##################Question 11####################
#compute RMSE of Q8
v  <- as.matrix(v)
v2 <- as.matrix(v2)
Q8.sqrt <-sqrt(sum((v2-v)^2 / 200, na.rm=T)) 
print(Q8.sqrt)
#compute RMSE of Q9
dim<-200
Sprime<-diag(c(S[1:200],rep(0, length(S)-dim)))
Rsvd.200dimens<-rowMeans(R, na.rm=TRUE)+ U %*% Sprime %*% t(V)
Q9.Sqrt<-sqrt(mean((Rsvd.200dimens - R)^2, na.rm=T))
print(Q9.Sqrt)
##compute RMSE of Q10
Q10.Sqrt <- sqrt(mean((predict.ui - origin.ui)^2, na.rm=T))
print(Q10.Sqrt)




