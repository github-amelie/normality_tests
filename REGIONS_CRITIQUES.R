############################## REGION CRITIQUE ##############################

### table ###
rc_sw=c(0.905,0.918,0.927,0.934,0.94,0.945,0.947) #les 50 premieres valeurs de shapiro sont tabulées
rc_ks=c(0.29408,0.26404,0.24170,0.22425,0.21012,0.19837,0.18841,0.17981,0.17231,0.16567, 0.15975,0.15442,0.14960,0.14520,0.14117
        ,0.13746,0.13403,1.358/sqrt(105),1.358/sqrt(110),1.358/sqrt(115),1.358/sqrt(120),1.358/sqrt(125),1.358/sqrt(130),1.358/sqrt(135),
        1.358/sqrt(140),1.358/sqrt(145),1.358/sqrt(150),1.358/sqrt(155),1.358/sqrt(160),1.358/sqrt(165),1.358/sqrt(170),1.358/sqrt(175),
        1.358/sqrt(180),1.358/sqrt(185),1.358/sqrt(190),1.358/sqrt(195),1.358/sqrt(200),1.358/sqrt(300),1.358/sqrt(400),1.358/sqrt(500),
        1.358/sqrt(1000),1.358/sqrt(1500),1.358/sqrt(2000))

### empirique ###
RC=rep(0,length(nlist))
names(RC)=nlist

RC_SW=RC
RC_LL=RC
RC_CVM=RC
RC_AD=RC


N=50000
c=1
a=0.05
for (n in nlist){
  print(c) #permet de suivre l'avanc??e de la boucle en temps réel
  Z=replicate(N, rnorm(n=n,mean=0,sd=1)) #chaque colonne de Z corrrespond a 1 echantillon de taille n. Z est de taille (n,N)
  
  
  #Vecteurs contenant chacun N VA simul??es selon la loi sous H0 de l'estimateur
  SW=apply(Z,2,shapiro.statistic)
  CVM=apply(Z,2,cvm.statistic)
  LL=apply(Z,2,lillie.statistic)
  AD=apply(Z,2,ad.statistic)
  
  ############################## STATISTIQUE D'ORDES ##############################
  SW_ordre=sort(SW, decreasing = FALSE)
  CVM_ordre=sort(CVM, decreasing = FALSE)
  LL_ordre=sort(LL, decreasing = FALSE)
  AD_ordre=sort(AD, decreasing = FALSE)
  
  ############################## QUANTILE EMPIRIQUE ##############################
  
  #left-tailed tests
  q_SW=SW_ordre[ceiling(N*a)]
  
  #right-tailed tests
  q_AD=AD_ordre[ceiling(N*(1-a))]
  q_LL=LL_ordre[ceiling(N*(1-a))]
  q_CVM=CVM_ordre[ceiling(N*(1-a))]
  
  RC_SW[c]=q_SW
  RC_LL[c]=q_LL
  RC_CVM[c]=q_CVM
  RC_AD[c]=q_AD
  
  c=c+1
  
}#fin boucle for

RC_SW=c(rc_sw,RC_SW[8:43])
