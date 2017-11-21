#Load libraries
library(Matrix)

#load data
m = read.table("http://www.cours.polymtl.ca/log6308/Public/citeseer.rtable")
m<-as.matrix(m)

#d¨¦clarer et initialiser les variables utilis¨¦es pour le calcul de pagerank
PageRank <- rep(1,1090)
delta <-Inf
d<-0.85

#fonction calcul du maximum
max.nindex <- function(m, n=5) {   
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}

#Calcul du pagerank avec une condition d'arr¨ºt : l'¨¦cart maximal entre les valeurs de pagerank entre deux
# it¨¦rations est inf¨¦rieur ou ¨¦gal ¨¤ 0.05
while(delta>0.05) {
  
  v <-PageRank/rowSums(m);
  v[is.infinite(v)]<-0;
  P <- (1-d)/length(PageRank) + (d * (t(m) %*% v));
  delta <- max(PageRank-P);
  PageRank <-P;
  
}


#############################################################################
#Recommendations directes (les enfants ou les parents de l'article)
############################################################################

#Sons (les enfants directs de l'article: ce ¨¤ quoi l'article refere)
s<-m['422908',] * PageRank
result<-max.nindex(s,10)
colnames(m)[result] #afficher les noms des articles les plus recommand¨¦s pour 'X422908'

#Parents  (les parents directs de l'article: qui ref¨¨rent ¨¤ l'article)
p<-m[,'X422908'] * PageRank
resultt<-max.nindex(p,10)
rownames(m)[resultt]


#############################################################################
#Recommendations avec consid¨¦ration d'un voisinage plus grand
#############################################################################

#Sons 
sons<-m['422908',]
PageRank1 <- sons * PageRank

#sons of sons 
sonsOfSons<-(m%*%m)['422908',]
PageRank2 <- sonsOfSons * PageRank * 0.9 # ajouter un poids pour exprimer le fait que les enfants des enfants
# sont moins favoris¨¦s (plus loins) des enfants directs

#sons of sons of sons
sonsOfSonsOfSons<-((m%*%m)%*%m)['422908',]
PageRank3 <- sonsOfSonsOfSons * PageRank * 0.9 * 0.9 # encore un poids pour prendre en compte la distance 
# (l'¨¦loignement) par rapport ¨¤ l'article en question


#Parents
l<- t(m) # on utilise la matrice transpos¨¦e pour faciliter le calcul des matrices d'adjacence

parents<-l['X422908',]
PageRank11 <- parents * PageRank

#parents of parents 
parentsOfParents<-(l%*%l)['X422908',]
PageRank22 <- parentsOfParents * PageRank * 0.9 # ajouter un poids pour exprimer le fait que les parents
# des parents sont moins favoris¨¦s (plus loins) des parents directs

#parents of parents of parents
parentsOfParentsOfParents<-((l%*%l)%*%l)['X422908',]
PageRank33 <- parentsOfParentsOfParents * PageRank * 0.9 * 0.9 # encore un poids pour prendre en compte 
#la distance (l'¨¦loignement) par rapport ¨¤ l'article en question


# Assembler les valeurs des PageRank dans un m¨ºme vecteur pour les comparer
PageRank1[which(PageRank2>0)]<- PageRank2[which(PageRank2>0)]# mettre les pagerank des enfants des enfants
PageRank1[which(PageRank3>0)]<- PageRank3[which(PageRank3>0)]# mettre les pagerank des enfants des enfants des enfants
PageRank1[which(PageRank11>0)]<- PageRank11[which(PageRank11>0)]#mettre les pagerank des parents
PageRank1[which(PageRank22>0)]<- PageRank22[which(PageRank22>0)]#mettre les pagerank des parents des parents
PageRank1[which(PageRank33>0)]<- PageRank33[which(PageRank33>0)]#mettre les pagerank des parents des parents des parents
rownames(m)[max.nindex(PageRank1,10)]  #Les meilleurs 10 articles de point de vue pagerank par rapport ¨¤ l'article '422908'

####################Q2#######################
#fonction qui calcule le cosinus entre un vecteur et les colonnes d'une matrice
cosinus.vm <- function (v, m) {n <- sqrt (colSums (m ^ 2)); (v %*% m)/(n * sqrt (sum (v ^ 2)))} 

#calculer les similarit¨¦s entre l'article '422908' et les autres articles via le cosinus
cosvalue= cosinus.vm(m[,'X422908'],m)
cosvalue[is.na(cosvalue)] <- 0

# Prendre les 10 articles les plus proches
r<-max.nindex(cosvalue, 11)
rownames(m)[r]



########## Q3 : validation croisee avec le cosinus ###############

########## Q3 : validation croisee avec le cosinus ###############

## Index al¨¦atoire des donn¨¦es de tests

i.observed <- which(m > 0) # Les valeurs non nulles de m
i.hasard <- sample(i.observed, length(i.observed)) # echantillonnage des indices des valeurs non nulles de m
fold.size <- round(length(i.hasard) / 20)  # 20 plis ==> tester sur 5% des articles et s'entrainer sur 95%
i.false <- rep(FALSE, length(m))
fold.number <- 16  # L'indice de l'ensemble de test



## Indices booleens pour les cellules de test et d'entrainement
i.test.b <- i.false
## Les cellules indexees du replis correspondant sont fixees a TRUE pour le test...
i.test.b[ i.hasard[((fold.number-1) * fold.size):((fold.number) * fold.size)] ] <- TRUE
## ...et a FALSE pour l'entrainement
i.train.b <-  !i.test.b
m.na.train <- m
m.na.train[i.test.b] <- 0    # on enl¨¨ve les donn¨¦es de test pour l'entrainement
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