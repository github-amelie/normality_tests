############################## DEVOIRS ##############################

N=10000
n=1000

#### Symetric short tailed ####
rtrunc(-2,2)
Unif <- replicate(N,runif(n,0,1))
Trunc <- replicate(N,rtrunc(n,'norm',a=-2,b=2))
GLD1_4 <- replicate(N,rgld(n,0,-1,-1/4,-1/4))
GLD1_2 <- replicate(N,rgld(n,0,1,1/2,1/2))
GLD3_4 <- replicate(N,rgld(n,0,1,3/4,3/4))
GLD5_4<- replicate(N,rgld(n,0,1,5/4,5/4))

#### Symetric longu tailed ####

Laplace <- replicate(N,rlaplace(n,0,1))
Logistic <- replicate(N,rlogis(n,0,1))
GLD0_1<- replicate(N,rgld(n,0,1,-0.1,-0.1))
GLD0_15<- replicate(N,rgld(n,0,1,-0.15,-0.1))
Student10 <- replicate(N,rstudent(n,10))
Student15 <- replicate(N,t(n,15))
ScconN02 <- replicate(N,rpaired.contaminated(n,0.2,9))
ScconN005 <- replicate(N,rpaired.contaminated(n,0.05,9))

#### Asymetric ####

Gam <- replicate(N,rgamma(n,4,1/5))
Beta1 <- replicate(N,rbeta(n,2,1))
Beta3 <- replicate(N,rbeta(n,3,2))
Chi4 <- replicate(N,rchisq(n,4))
Chi10 <- replicate(N,rchisq(n,10))
Chi20 <- replicate(N,rchisq(n,20))
Weibull <- replicate(N,rweibull(n,3,1))
Lognormal02<-replicate(N,rlnorm(n,0.2,3))
Lognormal005<-replicate(N,rlnorm(n,0.05,3))
