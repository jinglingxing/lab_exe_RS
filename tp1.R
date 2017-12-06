library(RCurl)
library (Matrix)
library(readr)
library(dplyr)
library(plyr)
library(lattice)
library(ggplot2)
###import data
u.data <- read.csv(text = getURL('http://www.groupes.polymtl.ca/log6308/Tp/20173/u.data.csv', userpwd = '20113:20113'), sep='|', header=T)
m <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')
u.item <- read.csv(text=getURL('http://www.groupes.polymtl.ca/log6308/Tp/20173/u.item.csv', userpwd = '20113:20113'), sep='|', header=T)
u.user <- read.csv(text=getURL('http://www.groupes.polymtl.ca/log6308/Tp/20173/u.user.csv', userpwd = '20113:20113'), sep='|', header=T)


###Q1


#1.What is the average of the votes by profession ("job") and by age?
userdata <- merge(u.user, u.data, by.x = 'id', by.y = 'user.id') 
###job
aggregate(rating ~ job, data=userdata, FUN=mean)
plot(aggregate(rating ~ job, data=userdata, FUN=mean))  #not very well to show 
barchart(rating ~ job, data=(aggregate(rating ~ job, data=userdata, FUN=mean)), scales=list(x=list(rot=45)))
###age
aggregate(rating ~ age, data=userdata, FUN=mean)
plot(aggregate(rating ~ age, data=userdata, FUN=mean))
###Q1.1
#Exploring the link between age and average votes
uvi<- merge(userdata, u.item, by.x='item.id', by.y='movie.id')  #merge userdata and u.item
uvi <- transform(uvi, release.date=as.Date(release.date, '%d-%b-%Y'))
qplot(age, rating, data=userdata) + geom_smooth()


###Q2


#2.What are the 10 most similar films to "Star Trek V: Final Frontier (1989)" 
#according to the measure of cosine and the correlation with the voting matrix, respectively.

##################cosine######################

index<-u.item$movie.id[u.item[,2] == "Star Trek V: The Final Frontier (1989)"] #index=450
v=m[,index]  #extract the 450th column
cosinus.vm <- function(v,m) { n <- sqrt(colSums(m^2)); (v %*% m)/(n * sqrt(sum(v^2))) } 
max.nindex <- function(m, n=5) { 
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}
min.nindex <- function(m, n=5) {
  i <- order(m)
  return(i[1:n])
}
#calculation of the cosine between Star Trek V and the other films according to the vote matrix
sim.cos=cosinus.vm(v,m)
result<-max.nindex(sim.cos,11)
u.item$movie.title[result]

#########################correlation###################
m1 <- as.matrix(m)
m0<-m1
m1[m1==0] <- NA
sim.cor <- cor(m1[,450], m1, use='pairwise.complete.obs')
result1<-max.nindex(sim.cor,11)
u.item$movie.title[result1]

####some exploration####
cosinus.vm(m0[,450],m0[,556:557])  #i556, 0.1290169; i557, 0.06809223
cor(m1[,450],m1[,556],use='pairwise.complete.obs') #1
table('Star Trek V'=m1[,450], 'Dumbo Drop'=m1[,110])  ##compare the number of common vote
table('Star Trek V'=m1[,450], 'Star Trek 1979'=m1[,449])
table('Star Trek V'=m1[,450], 'Wild Bill'=m1[,556])


###Q3


#Use an item-item approach to calculate the vote for the movie 
#"Star Trek V: The Final Frontier (1989)" for users who do not have a vote for it.
#Take the 10 nearest neighbors according to the Euclidean distance and use the cosine as weight.
#If no common vote exists, then the predicted value is set to NA.
#Use an item-item approach to calculate the vote for the movie 
#"Star Trek V: The Final Frontier (1989)" for users who do not have a vote for it.
#Take the 10 nearest neighbors according to the Euclidean distance and use the cosine as weight.
#If no common vote exists, then the predicted value is set to NA.

#The distance between the column "Star Trek V" and the other columns of m (the other films)
distance.450 <- sqrt(colSums((m[,index]-m)^2)) 
neighbors<-min.nindex(distance.450,21)

common.votes<- (colSums((m[,450] * m) > 0)) ## number of common votes
common.votes[neighbors]   ##all neighbors have common votes


newcos<-sim.cos[neighbors]  #take into account only 20 neighbors
mmeans <- t(t(m)-colMeans(m))
newmeans<-mmeans[,neighbors] #take into account only 20 neighbors

k<-1/(sum(abs(newcos))) # k value
v1<-mean(m[,450])+k*colSums(newcos%*%t(newmeans))  # vector predicted values

###Missing values are ignored
distance1.450 <- sqrt(colSums((m1[,index]-m1)^2,na.rm=T)) 
neighbors1<-min.nindex(distance1.450,21)

common.votes1<- (colSums((m1[,450] * m1) > 0,na.rm=T)) ## number of common votes
common.votes1[neighbors1]   ##all neighbors have common votes
remove<-neighbors1[common.votes1[neighbors1]==0]
neighbors1<-setdiff(neighbors1,remove)

newcos1<-sim.cos[neighbors1]  
mmeans1 <- t(t(m1)-colMeans(m1,na.rm=T))  #na.rm=T  ignore NA

newmeans1<-mmeans1[,neighbors1] 
k1<-1/(sum(abs(newcos1))) 
v2<-mean(m1[,450],na.rm=T)+k1*colSums(newcos1%*%t(newmeans1),na.rm=T) 

##check result

#RMSE
n.votes.450 <- sum(m1[,450]>0, na.rm=T)
sqrt(sum((m1[,450] - v2)^2 / n.votes.450, na.rm=T))  #1.162096
sqrt(sum((m1[,450] - mean(m1[,450], na.rm=T))^2 / n.votes.450, na.rm=T)) #1.162096

#MAE
mean(abs(v2 - m1[,450]), na.rm=T)        #0.9931973
mean(abs(mean(m1[,450], na.rm=T) - m1[,450]), na.rm=T)#0.9931973

hist(v2 - m1[,450], breaks=50)


###Q4


#Calculate the mean square error of the prediction of the item-item approach to the previous 
#question by comparing it to the observed values.

n.votes.450 <- sum(m[,450]>0, na.rm=T)
sqrt(sum((m[,450] - v1)^2 / n.votes.450, na.rm=T))
sqrt(sum((m[,450] - mean(m[,450], na.rm=T))^2 / n.votes.450, na.rm=T)) 


###q5


#One user rated the lowest rating (1) for all Star Wars movies and the highest rating (5) 
#for all Star Trek movies. What 10 movies do you recommend? 
#Use a user-user approach for the answer and 20 close neighbors.
newuser<-vector(length = 1682)
newuser[172]<-1  # remplir newuser avec ses votes (172 et 181 sont les indices des deux films de Star wars)
newuser[181]<-1
indices.star.trek <- grep("trek", as.character(u.item$movie.title), ignore.case=T) # Les indices des films Star Trek
newuser[indices.star.trek]<-5

distance<-sqrt(rowSums((newuser-m1)^2, na.rm=T))
nearest<-min.nindex(distance,20)

cos.vm <- function(v,m) { n <- sqrt(rowSums(m^2)); (v %*% t(m))/(n * sqrt(sum(v^2))) }
cosmm=cos.vm(newuser,m)

cosmm<-cosmm[nearest] # Take into account the 20 closest neighbors
k2<-1/(sum(abs(cosmm))) 
# calculation of the predicted vote values for the new user from the 20 neighbors
newuser2<-mean(newuser)+k2*(cosmm%*%(m1-rowMeans(m1,na.rm=T))[nearest,])
#v2<-mean(m1[,450],na.rm=T)+k1*colSums(newcos1%*%t(newmeans1),na.rm=T) 
listfilm<-max.nindex(newuser2,10)
u.item$movie.title[listfilm]


###Q6


#I am a new user. You know my profession, my sex and my age.
#Develop a Bayesian algorithm to recommend 10 movies based on these three categories.
o<-matrix(nrow=1682, ncol=9)   #The table of the different probabilities
o[,1]<-(1:1682) # The indices of the films on the first column
##definition of the function ratio.chances
ratio.chances <- function(rating.vec, seuil=3) sum(rating.vec > seuil) / sum(rating.vec <= seuil)

for(s in 1:1682){  
  s1<-userdata$item.id==s & userdata$rating>3   #users who liked the movie s
  s2<-userdata$item.id==s & userdata$rating<=3  # users who did not like the movie s
  o[s,2]<-ratio.chances(userdata[userdata$item.id==s,'rating']) #Calculation of the initial chances (O (H) == O (to like the film))
  o[s,3]<-(table(userdata[s1, 'job'])/sum(table(userdata[s1, 'job'])))['engineer']  # P (engineer / likes)
  o[s,4]<-(table(userdata[s2, 'job'])/sum(table(userdata[s2, 'job'])))['engineer']  #P (engineer / not like)
  o[s,5]<-(table(userdata[s1, 'age'] > 15 & userdata[s1, 'age'] < 25)/sum(table(userdata[s1, 'age'] > 15 & userdata[s1, 'age'] < 25)))['TRUE']  #P (age in [15..25] / likes)
  o[s,6]<-(table(userdata[s2, 'age'] > 15 & userdata[s2, 'age'] < 25)/sum(table(userdata[s2, 'age'] > 15 & userdata[s2, 'age'] < 25)))['TRUE']  #P (age in [15..25] / not like)
  o[s,7]<-(table(userdata[s1, 'gender'])/sum(table(userdata[s1, 'gender'])))['M'] # P (M / likes)
  o[s,8]<-(table(userdata[s2, 'gender'])/sum(table(userdata[s2, 'gender'])))['M']  #P(M/not like)
  o[s,9]<-o[s,2]*(o[s,3]/o[s,4])*(o[s,5]/o[s,6])*(o[s,7]/o[s,8]) #Application of the formula to calculate O (likes / (engineer, age in [15..25], M))
}

resultat<-max.nindex(o[,9],10) #Take the 10 most likely movies to be loved by the new user
u.item$movie.title[resultat]  # Display the titles the 10 resulting films, the calculation takes time because of the loops