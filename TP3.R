

#Importation des librairies
library(RCurl)
library (Matrix)
library(readr)
library(dplyr)
library(plyr)
library(ggplot2)


#Importation de data
u.data <- read.csv(text = getURL('http://www.groupes.polymtl.ca/log6308/Tp/20173/u.data.csv', userpwd = '20113:20113'), sep='|', header=T)
R <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(R) <- paste('u', 1:nrow(R), sep='')
colnames(R) <- paste('i', 1:ncol(R), sep='')
R <- as.matrix(R)



###############  Question 1   ####################

R[R==0]<-NA
Rmeans<-rowMeans(R, na.rm = TRUE)
Cmeans<-colMeans(R, na.rm = TRUE)
nc<-length(R[1,])
nr<-length(R[,1])
a <- matrix(Rmeans, nrow=nr, ncol=nc)  # matrice des moyennes des lignes
b <- t(matrix(Cmeans, nrow=nc, ncol=nr)) # matrice des moyennes des colonnes
c <-(a+b)/2  # moyenne arithmétique de a et b
mae<- function(m1, m2) mean(abs(m1-m2), na.rm=T) # erreur absolue moyenne
mae(c,R)
sqrt(mean((c- R)^2, na.rm=T)) #erreur quadratique moyenne


################  Question 2  #############


####### Normalisation #############

Rnorm <- R-rowMeans(R,na.rm = T) # soustraire les moyennes des lignes
Rnorm[is.na(Rnorm)] <- 0 # remettre les valeurs à 0 pour appliquer SVD

###############  SVD  ###########

Rsvd <- svd(Rnorm) # application de SVD
str(Rsvd)
S<-Rsvd$d
U<-Rsvd$u
V<-Rsvd$v

########### Questions 3+4 : Reduction de la dimension Et estimation#######

Sprime <- diag(c(S[1:10],rep(0,length(S)-10))) # réduction de la dimension de S à 10
R.10dimens <- rowMeans(R,na.rm = T)+U %*% Sprime %*% t(V)

# Calcul de l'erreur absolue moyenne
mae(R.10dimens,R)

#calcul de l'erreur quadratique moyenne
sqrt(mean((R.10dimens - R)^2, na.rm=T))


############ Question 5 : nombre de dimensions optimal #################


calculate_error <- function(dim){ # calcule de l'erreur en fonction de la dimension
  Sprime <- diag(c(S[1:dim],rep(0,length(S)-dim)))
  R.dimens <- rowMeans(R,na.rm = T)+U %*% Sprime %*% t(V)
  return(mae(R.dimens,R))
}

dimensions <- seq(10, 943, 10) # l'ensemble des dimensions étudiées
results <- sapply(dimensions,calculate_error)
qplot(dimensions,results) ### dimension optimale = 943 : faux car on a pas fait une validation croisée



################# Question 6 ################################


err<-c(1:5)
errors <- seq(8, 14, 1)
index=0

observed <- which(R > 0) # Les valeurs non nulles de R
hasard <- sample(observed, length(observed)) # echantillonnage des indices des valeurs non nulles de R
fold.size <- round(length(hasard) / 5)  # 5 plis ==> tester sur 20% des articles et s'entrainer sur 90%
i.false <- rep(FALSE, length(R))

for(di in seq(8,14,1)){ # les dimensions étudiées sont comprises entre 8 et 14
  
  index=index+1
  
  for(k in 1:5){ # itération sur les plis
    
    fold.number <- k # L'indice de l'ensemble de test
    
    ## Indices booleens pour les cellules de test et d'entrainement
    test.b <- i.false
    ## Les cellules indexees du replis correspondant sont fixees a TRUE pour le test...
    test.b[ hasard[((fold.number-1) * fold.size):((fold.number) * fold.size)] ] <- TRUE
    ## ...et a FALSE pour l'entrainement
    train.b <-  !test.b
    R.train <- R
    R.train[R.train==0]<-NA
    R.train[test.b] <- NA    # on enlève les données de test pour l'entrainement
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
    err[k]<-mae(R.10dimens.train[test.b], R[test.b]) # l'erreur absolue moyenne pour le plis en question
    
  }
  errors[index]<-min(err) # l'erreur minimale entre tous les plis pour la dimension en question

}

qplot(seq(8, 14, 1),errors) # diagramme erreur = f(dimension)


################## Question 7 ######################### 

cosinus.vm <- function (v, m) {n <- sqrt (colSums (m ^ 2)); (v %*% m)/(n * sqrt (sum (v ^ 2)))} 

## Index aléatoire des données de tests

i.observed <- which(R > 0) # Les valeurs non nulles de R
i.hasard <- sample(i.observed, length(i.observed)) # echantillonnage des indices des valeurs non nulles de R
fold.size <- round(length(i.hasard) / 10)  # 10 plis ==> tester sur 10% des articles et s'entrainer sur 90%
i.false <- rep(FALSE, length(R))
fold.number <- 1 # L'indice de l'ensemble de test



## Indices booleens pour les cellules de test et d'entrainement
i.test.b <- i.false
## Les cellules indexees du replis correspondant sont fixees a TRUE pour le test...
i.test.b[ i.hasard[((fold.number-1) * fold.size):((fold.number) * fold.size)] ] <- TRUE
## ...et a FALSE pour l'entrainement
i.train.b <-  !i.test.b
R[is.na(R)] <- 0
m.na.train <- R
m.na.train[i.test.b] <- 0    # on enlève les données de test pour l'entrainement



cosinematrix = matrix(nrow=943,ncol=943);
for( i in 1:943){
  cosinematrix[i,]<-cosinus.vm(t(m.na.train)[,i],t(m.na.train))  #calcul du cosinus entre les lignes de la matrice m.na.train
}
cosinematrix[is.na(cosinematrix)]<-0  #eliminer les valeurs négatives dans la matrice des cosinus

m.na.train[m.na.train==0]<-NA
Rmeans<-rowMeans(m.na.train, na.rm = TRUE)
nc<-length(R[1,])
nr<-length(R[,1])
a <- matrix(Rmeans, nrow=nr, ncol=nc)
m.na.train[is.na(m.na.train)]<-0


#calcul de la matrice de valeurs predites selon l'approche Item-Item avec les cosinus comme poids

m.prediction <- a+(cosinematrix%*% (m.na.train-a))/rowSums(abs(cosinematrix))
m.prediction[m.prediction < 0]<-0  # eliminer les valeurs négatives de la matrice de predictions


# Calcul de l'erreur absolue moyenne

mae(m.prediction[i.test.b], R[i.test.b])
mean(abs(m.prediction[i.test.b] - R[i.test.b]), na.rm=T)


#calcul de la racine carrée de l'erreur quadratique moyenne
sqrt(mean((m.prediction[i.test.b] - R[i.test.b])^2, na.rm=T))


############ méthode collaborative sans validation croisée###########

cosinematrix = matrix(nrow=943,ncol=943);
for( i in 1:943){
  cosinematrix[i,]<-cosinus.vm(t(R)[,i],t(R))  #calcul du cosinus entre les lignes de la matrice m 
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