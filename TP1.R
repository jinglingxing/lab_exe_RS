#Importation des librairies
library(RCurl)
library (Matrix)
library(readr)
library(dplyr)
library(plyr)

#Importation de data
u.data <- read.csv(text = getURL('http://www.groupes.polymtl.ca/log6308/Tp/20173/u.data.csv', userpwd = '20113:20113'), sep='|', header=T)
m <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')
u.item <- read.csv(text=getURL('http://www.groupes.polymtl.ca/log6308/Tp/20173/u.item.csv', userpwd = '20113:20113'), sep='|', header=T)
u.user <- read.csv(text=getURL('http://www.groupes.polymtl.ca/log6308/Tp/20173/u.user.csv', userpwd = '20113:20113'), sep='|', header=T)


#########################################################################################################################################

#Question 1
userdata <- merge(u.data,u.user,by.x='user.id',by.y='id') #Jointure des deux tables u.user et u.data

i<-aggregate(userdata[, 3], list(userdata$age), mean)# moyenne des votes par age (on peut remplacer userdata[,3] par userdata$rating)
rename(i,c("Group.1"="age","x"="means")) #renommer les colonnes age et moyenne

j<-aggregate(userdata[, 3], list(userdata$job), mean)#moyenne des votes par profession (job)
rename(j,c("Group.1"="jobs","x"="means"))  #renommer les colonnes profession et moyenne

############################################################################################################################################





#Question 2

index<-u.item$movie.id[u.item[,2] == "Star Trek V: The Final Frontier (1989)"]# trouver l'identifiant du film "Star Trek V"
suoyin<- u.item$movie.id[u.item[,2] == "Star Trek V: The Final Frontier (1989)"]
cosinus.vm <- function (v, m) {n <- sqrt (colSums (m ^ 2)); (v %*% m) / (n * sqrt (sum (v ^ 2)))}
cosvalue= cosinus.vm(v,m)
max.nindex <- function (m, n = 5) {
  i <- order (m, decreasing = TRUE)
  return (i [1: n])
}
result<- max.nindex(cosvalue, 11)
u.item$movie.title[result]

#Q3
distance.450 <- sqrt(colSums((m[,index] - m)^2))
min.nindex <- function(m, n=5) {
  i <- order(m)
  return(i[1:n])
}
neighbors<- min.nindex(distance.450,21)



#############################################################################################################################

#######################################################################
##                     Question 3                                    ##
#######################################################################


####################### En fixant les valeurs manquantes ¨¤ 0 #########################

distance.450 <- sqrt(colSums((m[,index]-m)^2)) # La distance entre la colonne "Star Trek V" et les autres colonnes de m (les autres films)

min.nindex <- function(m, n=5) { # d¨¦finition de la fonction min.nindex (donne les n valeurs les plus basses d'un vecteur)
  i <- order(m)
  return(i[1:n])
}

voisins<-min.nindex(distance.450,21) #20 plus proches voisins + "le film lui-m¨ºme" (ceux qui ont la distance la plus petite avec le film)

### Consid¨¦ration des votes communs

votes.communs <- (colSums((m[,index] * m) > 0))
votes.communs[voisins] #tous les voisins ont des votes communs

newcos<-cosm[voisins]  #prendre en compte que 20 voisins
newmeans<-mmeans[,voisins] #prendre en compte que 20 voisins
k<-1/(sum(abs(newcos))) # calcul de la constante k de normalisation
v1<-mean(m[,450])+k*colSums(newcos%*%t(newmeans))  # vecteur des valeurs pr¨¦dites


####################Les valeurs manquantes sont ignor¨¦es ##########################


distance.450 <- sqrt(colSums((m1[,index]-m1)^2,na.rm=T)) # La distance entre la colonne "Star Trek V" et les autres colonnes de m (les autres films)
min.nindex <- function(m, n=5) { # d¨¦finition de la fonction min.nindex
  i <- order(m)
  return(i[1:n])
}
voisins<-min.nindex(distance.450,21) #20 plus proches voisins + "le film lui-m¨ºme"

### votes communs
votes.communs <- (colSums((m1[,index] * m1) > 0,na.rm=T))
votes.communs[voisins] # Pas tous les voisins ont des votes communs
remove<-voisins[votes.communs[voisins]==0]
voisins<-setdiff(voisins,remove) # Supprimer les voisins qui n'ont pas des votes communs

newcos<-cosm[voisins]  #prendre en compte que les voisins trouv¨¦s
mmeans2 <- t(t(m1)-colMeans(m1,na.rm=T))
newmeans2<-mmeans2[,voisins] #prendre en compte que les voisins trouv¨¦s
k<-1/(sum(abs(newcos))) # calcul de la constante k de normalisation
v2<-mean(m1[,450],na.rm=T)+k*colSums(newcos%*%t(newmeans2),na.rm=T)  # vecteur des valeurs pr¨¦dites



#####################################################################################################################################
##                           Question 4                                                             ##
######################################################################################################


# Premi¨¨re m¨¦thode de pr¨¦diction (avec les z¨¦ros)
E1<-sqrt((sum((v1[v!=0]-v[v!=0])*(v1[v!=0]-v[v!=0])))/(length(v[v!=0])))  # calcul de l'¨¦cart entre les valeurs donn¨¦es par v (les valeurs qui sont d¨¦j¨¤ non nulles) et les valeurs pr¨¦dites par v1


#Deuxi¨¨me m¨¦thode de pr¨¦diction (en n¨¦gligeant les NA)
E2<-sqrt((sum((v2[v!=0]-v[v!=0])*(v2[v!=0]-v[v!=0])))/(length(v[v!=0])))



###############################################################################################################
##                                                 Question 5                                                ##
###############################################################################################################



newuser<-vector(length = 1682) # cr¨¦ation d'un nouveau utilisateur
newuser[172]<-1  # remplir newuser avec ses votes (172 et 181 sont les indices des deux films de Star wars)
newuser[181]<-1
indices.star.trek <- grep("trek", as.character(u.item$movie.title), ignore.case=T) # Les indices des films Star Trek
newuser[indices.star.trek]<-5


distance<-sqrt(rowSums((newuser-m1)^2, na.rm=T)) # calcul de la distance entre le nouveau utilisateur et les autres utilisateurs enregistr¨¦s dans m
nearest<-min.nindex(distance,20) # les 20 voisins les plus proches du nouveau user (20 seulement car le nouveau utilisateur n'est pas dans la matrice m)
cos.vm <- function(v,m) { n <- sqrt(rowSums(m^2)); (v %*% t(m))/(n * sqrt(sum(v^2))) } # fonction cosinus pour l'approche utilisateur-utilisateur (cosinus entre les lignes)
cosmm=cos.vm(newuser,m)  # Calcul du cosinus entre le nouveau utilisateur et les autres utilisateurs
cosmm<-cosmm[nearest]  # Prendre en compte les 20 plus proche voisins
k1<-1/(sum(abs(cosmm))) 
newuser2<-mean(newuser)+k*(cosmm%*%(m1-rowMeans(m1,na.rm=T))[nearest,]) # calcul des valeurs de votes pr¨¦dites pour le nouveau utilisateur ¨¤ partir des 20 voisins
listefilm<-max.nindex(newuser2,10)# les 10 films propos¨¦s (on peut enlever ceux qu'il a d¨¦j¨¤ regard¨¦)
u.item$movie.title[listefilm]


###############################################################################################################
##                          Question 6 Proba conditionnelle                                                  ##
###############################################################################################################



## On prend comme exemple pour l'exp¨¦rience : un ing¨¦nieur (M) dont l'age est 20 ans

o<-matrix(nrow=1682, ncol=9) # Le tableau des diff¨¦rentes probabilit¨¦s 
o[,1]<-(1:1682) # Les indices des films 
ratio.chances <- function(rating.vec, seuil=3) sum(rating.vec > seuil) / sum(rating.vec <= seuil) # D¨¦finition de la fonction ratio.chances  
for(s in 1:1682){  # inspir¨¦ du code bay¨¦sien na??f (Code R sur le site de ce cours)
  s1<-userdata$item.id==s & userdata$rating>3   # les utilisateurs ayant aim¨¦ le film s
  s2<-userdata$item.id==s & userdata$rating<=3  # les utilisateurs n'ayant pas aim¨¦ le film s
  o[s,2]<-ratio.chances(userdata[userdata$item.id==s,'rating']) # Calcul des chances initiales (O(H) == O(aimer le film))
  o[s,3]<-(table(userdata[s1, 'job'])/sum(table(userdata[s1, 'job'])))['engineer']  #P(ing¨¦nieur/aime)
  o[s,4]<-(table(userdata[s2, 'job'])/sum(table(userdata[s2, 'job'])))['engineer']  #P(ing¨¦nieur/aime pas)
  o[s,5]<-(table(userdata[s1, 'age'] > 15 & userdata[s1, 'age'] < 25)/sum(table(userdata[s1, 'age'] > 15 & userdata[s1, 'age'] < 25)))['TRUE']  #P(age dans [15..25]/aime)
  o[s,6]<-(table(userdata[s2, 'age'] > 15 & userdata[s2, 'age'] < 25)/sum(table(userdata[s2, 'age'] > 15 & userdata[s2, 'age'] < 25)))['TRUE']  #P(age dans [15..25]/aime pas)
  o[s,7]<-(table(userdata[s1, 'gender'])/sum(table(userdata[s1, 'gender'])))['M']  #P(M/aime)
  o[s,8]<-(table(userdata[s2, 'gender'])/sum(table(userdata[s2, 'gender'])))['M']  #P(M/aime pas)
  o[s,9]<-o[s,2]*(o[s,3]/o[s,4])*(o[s,5]/o[s,6])*(o[s,7]/o[s,8]) #Application de la formule pour calculer O(aime/ (ing¨¦nieur, age dans [15..25], M) ) 
}

resultat<-max.nindex(o[,9],10) # Prendre les 10 films les plus probables d'¨ºtre aim¨¦ par le nouveau utilisateur
u.item$movie.title[resultat]  # Afficher les titres les 10 films r¨¦sultants, le calcul prend du temps ¨¤ cause des boucles

