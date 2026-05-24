#Ecological modeling project maritime jags
#Rachel Pit
#4/28/26

model{
  
  for(i in 1:n.obs){
    
    z[i]~dbern(psi[i])
    
    log(lambda[i])<-b0+
      b1*PSR[i]+
      b2*TSR[i]+
      b3*TD[i]+
      b4*SM[i]+
      b5*SO[i]+
      b6*SS[i]+
      b7*date[i]
    
    mu[i]<-z[i]*lambda[i]+(1-z[i])*1e-3 #Otherwise wont work idk why
    
    MSR[i]~dpois(mu[i])
    MSR.pred[i]~dpois(lambda[i]*z[i])
    logit(psi[i])<-a0+a1*date[i]
  }
  
  b0~dnorm(0,0.0001)
  b1~dnorm(0,0.0001)
  b2~dnorm(0,0.0001)
  b3~dnorm(0,0.0001)
  b4~dnorm(0,0.0001)
  b5~dnorm(0,0.0001)
  b6~dnorm(0,0.0001)
  b7~dnorm(0,0.0001)
  
  a0~dnorm(0,0.0001)
  a1~dnorm(0,0.0001)
}



