
#Q1
library(Matrix)
m = read.table("http://www.cours.polymtl.ca/log6308/Public/citeseer.rtable")
m<- as.matrix(m)
PageRank<- rep(1,1090)   #all ones


delta<-Inf
d<-0.85

max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}

while(delta>0.05)
{
  v<-PageRank/rowSums(m)
  v[is.infinite(v)]<-0
  P <- (1-d)/length(PageRank) + (d * (t(m) %*% v))
  delta<- max(PageRank-P)
  PageRank<-P
  
}


#sons(the direct children of the article: what the article refers to)
s<- m['422908',]*PageRank
result<-max.nindex(s,10)
colnames(m)[result] #show the names of the most recommended items for 'X422908'
#parents
p<- m[,'X422908']*PageRank
resultt<-max.nindex(p,10)
rownames(m)[resultt]

#####Recommendations with consideration of a larger neighborhood

#Sons 
sons<-m['422908',]
PageRank1 <- sons * PageRank

#sons of sons 
sonsOfSons<-(m%*%m)['422908',]
PageRank2 <- sonsOfSons * PageRank * 0.9 # add a weight to express the fact that children's children
# are less favored (more distant) of direct children

#sons of sons of sons
sonsOfSonsOfSons<-((m%*%m)%*%m)['422908',]
PageRank3 <- sonsOfSonsOfSons * PageRank * 0.9 * 0.9 # still a weight to take into account the distance
# (the distance) with respect to the article in question


#Parents
l<- t(m) # the transposed matrix is used to facilitate the calculation of adjacency matrices

parents<-l['X422908',]
PageRank11 <- parents * PageRank

#parents of parents 
parentsOfParents<-(l%*%l)['X422908',]
PageRank22 <- parentsOfParents * PageRank * 0.9# add a weight to express the fact that parents
# of parents are less favored (more distant) of direct relatives

#parents of parents of parents
parentsOfParentsOfParents<-((l%*%l)%*%l)['X422908',]
PageRank33 <- parentsOfParentsOfParents * PageRank * 0.9 * 0.9 # still a weight to take into account
#the distance (distance) from the article in question

# Assemble the values of PageRank in the same vector to compare them
PageRank1[which(PageRank2>0)]<- PageRank2[which(PageRank2>0)]# to put pagerank of children of children
PageRank1[which(PageRank3>0)]<- PageRank3[which(PageRank3>0)]# to put pagerank of children of children of children
PageRank1[which(PageRank11>0)]<- PageRank11[which(PageRank11>0)]#move parents' pagerank
PageRank1[which(PageRank22>0)]<- PageRank22[which(PageRank22>0)]#make parents' pageranks
PageRank1[which(PageRank33>0)]<- PageRank33[which(PageRank33>0)]#make pagerank parents parents parents
max.nindex(PageRank1,10)
rownames(m)[max.nindex(PageRank1,10)] #The best 10 articles of pagerank point of view compared to the article '422908'


####################Q2#######################
#Compare the results obtained with an approach based on the similarity of articles in a vector space,
#similar to the similarity calculation of the item-item approach.
# The measure of similarity and how to use it to estimate the relevance of similar items is left to your discretion.


cosinus.vm <- function (v, m) {n <- sqrt (colSums (m ^ 2)); (v %*% m)/(n * sqrt (sum (v ^ 2)))} 

#calculate similarities between '422908' and other articles via cosine
cosvalue= cosinus.vm(m[,'X422908'],m)
cosvalue[is.na(cosvalue)] <- 0

# Take the 10 closest articles
r<-max.nindex(cosvalue, 11)
rownames(m)[r]


#################### Q3 #######################
#Use cross-validation to evaluate the performance of the item-item approach.



i.observed <- which(m > 0) # Non-zero values of m
i.hasard <- sample(i.observed, length(i.observed)) # sampling of non-zero values of m
fold.size <- round(length(i.hasard) / 20)  # 20 folds ==> test on 5% of items and train on 95%
i.false <- rep(FALSE, length(m))
fold.number <- 16  # The index of the test set


## Boolean Indices for Test and Training Cells
i.test.b <- i.false
## The indexed cells of the corresponding fold are set to TRUE for the test ...
i.test.b[ i.hasard[((fold.number-1) * fold.size):((fold.number) * fold.size)] ] <- TRUE
## ... and FALSE for training
i.train.b <-  !i.test.b
m.na.train <- m
m.na.train[i.test.b] <- 0   # we remove test data for training
table(m.na.train)

cosinematrix = m;
for( i in 1:1090){
  cosinematrix[i,]<-cosinus.vm(t(m)[,i],t(m))
}
cosinematrix[is.na(cosinematrix)]<-0


m.prediction <- rowMeans(m.na.train,na.rm=T)+(cosinematrix%*% (m.na.train-rowMeans(m.na.train,na.rm=T)))/rowSums(abs(cosinematrix))
m.prediction[is.na(m.prediction)]<-0


mae <- function(m1, m2) mean(abs(m1 - m2), na.rm=T)
mae(m.prediction[i.test.b], m[i.test.b])
mean(abs(m.prediction[i.test.b] - m[i.test.b]), na.rm=T)

sqrt(mean((m.prediction[i.test.b] - m[i.test.b])^2, na.rm=T))

hist(m.prediction[i.test.b] - m[i.test.b])
