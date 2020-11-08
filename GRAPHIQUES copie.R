############################## GRAPHIQUE ##############################

N=50000
a=0.05
nlist=100
Z=replicate(N, rnorm(n=n,mean=0,sd=1)) 



########### SHAPIRO ############

Z=replicate(N, rnorm(n=50,mean=0,sd=1)) 
SW100=apply(Z,2,shapiro.statistic)
plot(density(SW100))
Z=replicate(N, rnorm(n=10,mean=0,sd=1)) 
SW50=apply(Z,2,shapiro.statistic)
lines(density(SW50),col=2)
Z=replicate(N, rnorm(n=20,mean=0,sd=1)) 
SW=apply(Z,2,shapiro.statistic)
lines(density(SW),col='green')
legend("topleft", legend=c('n=50','n=20','n=10'),col=c('black','green','red'),lty=1)





#Vecteurs contenant chacun N VA simul??es selon la loi sous H0 de l'estimateur
SW=apply(Z,2,shapiro.statistic)
CVM=apply(Z,2,cvm.statistic)
LL=apply(Z,2,lillie.statistic)
AD=apply(Z,2,ad.statistic)
CSQ=apply(Z,2,pearson.statistic)
DP=apply(Z,2,dagostino.statistic)

############################## STATISTIQUE D'ORDRE ##############################
SW_ordre=sort(SW, decreasing = FALSE)
CVM_ordre=sort(CVM, decreasing = FALSE)
LL_ordre=sort(LL, decreasing = FALSE)
AD_ordre=sort(AD, decreasing = FALSE)
CSQ_ordre=sort(CSQ, decreasing = FALSE)
DP_ordre=sort(DP, decreasing = FALSE)

############################## QUANTILES EMPIRIQUES ##############################
#left-tailed tests
q_SW=SW_ordre[ceiling(N*a)]

#right-tailed tests
q_AD=AD_ordre[ceiling(N*(1-a))]
q_LL=LL_ordre[ceiling(N*(1-a))]
q_CVM=CVM_ordre[ceiling(N*(1-a))]
q_DP=DP_ordre[ceiling(N*(1-a))]
q_CSQ=CSQ_ordre[ceiling(N*(1-a))]


############################## FONCTIONS QUANTILES ##############################
par(mfrow=c(2,3))
plot(seq(1/N,1,1/N),SW_ordre, main="SW", type="l")
plot(seq(1/N,1,1/N),CVM_ordre,main="CVM", type="l")
plot(seq(1/N,1,1/N),LL_ordre,main="LL", type="l")
plot(seq(1/N,1,1/N),AD_ordre,main="AD", type="l")
 plot(seq(1/N,1,1/N),CSQ_ordre,main="CSQ", type="l")
plot(seq(1/N,1,1/N),DP_ordre,main="DP", type="l")
mtext("Fonctions quantile empiriques des estimateurs sous H0 ", side = 3, line = -1.5, outer = T)

############################## FONCTION REPARTITION EMPIRIQUE ##############################
FDR_SW=function(t){return(sum(SW<t)/N)}
FDR_LL=function(t){return(sum(LL<t)/N)}
FDR_CVM=function(t){return(sum(CVM<t)/N)}
FDR_AD=function(t){return(sum(AD<t)/N)}
FDR_CSQ=function(t){return(sum(CSQ<t)/N)}
FDR_DP=function(t){return(sum(DP<t)/N)}

par(mfrow=c(2,3))
curve(Vectorize(FDR_SW)(x), main="SW",xlim = c(0,2))
curve(Vectorize(FDR_LL)(x),main="LL")
curve(Vectorize(FDR_CVM)(x),main="CVM")
curve(Vectorize(FDR_AD)(x),main="AD")
curve(Vectorize(FDR_CSQ)(x),main="CSQ",xlim = c(0,25))
curve(Vectorize(FDR_DP)(x),main="DP",xlim = c(0,11))

mtext("Fonctions de repartition empiriques des estimateurs sous H0 ", side = 3, line = -1.5, outer = T)


par(mfrow=c(1,1))
############################## DENSITE SOUS H0 ##############################
#Zone rejet SW
op=par(mfrow=c(2,3))
do.it <- function (x,q,ad=FALSE) {
  d <- density(x)
  plot(d, type='l')
  do.it <- function (i, col) {
    x <- d$x[i]
    y <- d$y[i]
    polygon( c(x,rev(x)), c(rep(0,length(x)),rev(y))/100, border=NA, col=col,title(""),main='Zone de rejet de SW',add=ad)
  }
  do.it(d$x <= q, 'red')
  lines(d, lwd=3,title(""))
}
do.it(SW,q_SW)
text(0.95,10,"Zone de rejet " , col = "red",cex = 0.8)
abline(v=q_SW, col="red",lwd=2)
text(0.997,200,"Zone de rejet de SW " , col = "black",cex = 1.2)

  
do.it <- function (x,q) {
  d <- density(x)
  plot(d, type='l')
  do.it <- function (i, col) {
    x <- d$x[i]
    y <- d$y[i]
    polygon( c(x,rev(x)), c(rep(0,length(x)),rev(y)), border=NA, col=col )
  }
  do.it(d$x >= q, 'red')
  lines(d, lwd=3)
}

do.it(CVM,q_CVM)
abline(v=q_CVM, col="blue",lwd=2)
text(0.3,3,"Zone de rejet " , col = "blue",cex = 0.8 )
text(0.3,15,"Zone de rejet de CVM " , col = "black",cex = 1.3)


do.it(LL,q_LL)
abline(v=q_LL, col="blue",lwd=2)
text(0.13,5,"Zone de rejet " , col = "blue",cex = 0.8)
text(0.12,15,"Zone de rejet de LL" , col = "black",cex = 1.3)



do.it(AD,q_AD)
abline(v=q_AD, col="blue",lwd=2)
text(1,0.4,"Zone de rejet " , col = "red",cex = 0.8 )
text(1.5,2.5,"Zone de rejet de AD" , col = "black",cex = 1.3)

do.it(CSQ,q_CSQ)
curve(dchisq(x, df =10), 
      type = 'l', 
      lwd = 1, 
      col = "red", 
      add = T)
text(3,0.09,expression(chi^2*(10)) , col = "red",cex = 1.5 )
abline(v=q_CSQ, col="blue",lwd=2)
text(21,0.02,"Zone de rejet " , col = "red",cex = 0.8 )
text(30,.08,paste("Zone de rejet de",expression(chi^2*(10))) , col = "black",cex = 1.3)



do.it(DP,q_DP)
curve(dchisq(x, df =2), 
      type = 'l', 
      lwd = 1, 
      col = "red", 
      add = T)
text(3,0.2,expression(chi^2*(2)) , col = "red",cex = 1.5 )
abline(v=q_DP, col="blue",lwd=2)
text(8.5,0.30,"Zone de rejet " , col = "blue",cex = 2 )
title('Zone rejet pour DP')
mtext(expression("Lois des estimateurs sous H"[0]*" pour n=100"), side = 3, line = -1.5, outer = T)



############################## LOI NON NORMALE ##############################


Z=replicate(N, rnorm(n=100,mean=0,sd=1)) 
X=replicate(N,runif(100))

SW_X=apply(X,2,shapiro.statistic)

par(mfrow=c(1,1))
plot(density(SW_X),
     main=expression("Comparaison de la loi de l'estimateur SW sous H"[0]* " et sous H"[1]*" pour n=100"),
     xlim=c(0.9,1),
     ylim=c(0,80))
abline(v=RC_SW["100"], col="blue",lwd=2)
lines(density(SW), main="SW")

text(0.95, 30, expression(H[1]) )
text(0.99, 70, expression(H[0]))
text(0.94,70,"Zone de rejet " , col = "blue" ,cex = 2 )

