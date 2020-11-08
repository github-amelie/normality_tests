############################## BASE ##############################
install.packages("nortest")
install.packages("fBasics")
library("nortest")
library("fBasics")

alphalist=c(0.05,0.1) 
#vecteur contenant les valeurs de alpha
nlist=c(seq(20,200,5),seq(300,500,100),seq(1000,2000,500))
#vecteur contenant les 43 valeurs de n , tailles des samples

#Nos 5 tests de normalite
#Puisque les tests retournent une liste, on cr??e ces fonctions permettant de garder uniquement la valeur de la statistique de test:

shapiro.statistic=function(X){ #oui
  return(shapiro.test(X)$statistic)}

lillie.statistic=function(X){  #oui
  return(lillie.test(X)$statistic)}

ks.statistic=function(X){  #oui
  return(ks.test(X,'pnorm',mean(X),sd(X))$statistic)}

cvm.statistic=function(X){  #oui
  return(cvm.test(X)$statistic)}

ad.statistic=function(X){  #oui non distribution free compare au autre est ce que si modele gaussien cest distributuion free? marche si autre trucs
  return(ad.test(X)$statistic)}



