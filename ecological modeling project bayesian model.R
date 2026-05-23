#Ecological modleing project bayesian hierarchical modeling
#Rachel Pitt
#4/28/26


#Packages
library(tidyverse)

data<-read.csv("ecological_modeling_maritime_v3.csv")

#Parse down variables
some_data<-data%>%
  select(-plot,-needle_coverage,-organic_layer_depth,-organic_content_25cm,-soil_percent_water_25cm,-soil_salinity_ppt_25cm,-survey)%>%
  dplyr::rename(MSR=plot_mushroom_richness,
                CC=canopy_coverage,
                PSR=plant_species_richness,
                SM=soil_percent_water_10cm,
                SO=organic_content_10cm,
                TD=tree_density,
                ND=needle_depth,
                TSR=tree_species_richness,
                date=julian_date,
                VC=vegtation_cover,
                SS=soil_salinity_ppt_10cm
                )%>%
  mutate(block=as.factor(block), #Make sure factors
         site=as.factor(site),
         plot=as.factor(plot_number))%>%
  mutate(
           TSR_c=as.numeric(scale(TSR, center = TRUE, scale = TRUE)),
           TD_c=as.numeric(scale(TD,  center = TRUE, scale = TRUE)),
           PSR_c=as.numeric(scale(PSR, center = TRUE, scale = TRUE)),
           SM_c=as.numeric(scale(SM,  center = TRUE, scale = TRUE)),
           SO_c=as.numeric(scale(SO,  center = TRUE, scale = TRUE)),
           SS_c=as.numeric(scale(SS,  center = TRUE, scale = TRUE)),
           date_c=as.numeric(scale(date,center=TRUE,scale=TRUE))
         ) #Center and scale everything

hist(some_data$MSR,
     xlab="Mushroom Species Richness",
     ylab="Frequency",
     main="") #Obviously 0 inflated- should be controlling for that



#Frequentist hierarchical model:
mod2<-glm(MSR~PSR_c+TSR_c+SM_c+SO_c+SS_c+date,data=some_data,family="poisson"(link="log"))

summary(mod2)

#Bayesian hierarchical model: 
jags.data<-list(
  n.obs=nrow(some_data),
  MSR=some_data$MSR,
  PSR=some_data$PSR_c,
  TSR=some_data$TSR_c,
  TD=some_data$TD_c,
  SM=some_data$SM_c,
  SO=some_data$SO_c,
  SS=some_data$SS_c,
  date=some_data$date_c
)


my_mod<-"model{
  
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

"
mod1<-R2jags::jags(data=jags.data,
                   parameters.to.save=c("a0","a1","b0","b1","b2","b3","b4","b5","b6","b7","lambda","psi","z","MSR.pred"),
                   n.chains=3,
                   n.iter=12000,
                   n.burnin=3000,
                   n.thin=5,
                   model.file=textConnection(my_mod))
summary(mod1)

#Check to see for convergence
R2jags::traceplot(mod1, varname="a1")
#Yes! a fuzzy caterpillar!

#Interpretation:

xvals<-seq(min(some_data$date_c), max(some_data$date_c))

px<-ui<-li<-c()

# pull posterior samples
b0<-mod1$BUGSoutput$sims.list$b0
b1<-mod1$BUGSoutput$sims.list$b1
b2<-mod1$BUGSoutput$sims.list$b2
b3<-mod1$BUGSoutput$sims.list$b3
b4<-mod1$BUGSoutput$sims.list$b4
b5<-mod1$BUGSoutput$sims.list$b5
b6<-mod1$BUGSoutput$sims.list$b6
b7<-mod1$BUGSoutput$sims.list$b7

a0<-mod1$BUGSoutput$sims.list$a0
a1<-mod1$BUGSoutput$sims.list$a1

#Loop through
for(i in 1:length(xvals)){
  
  log_lambda <- b0 + b1 * xvals[i]+ b2 * xvals[i]+ b3 * xvals[i]+ b4 * xvals[i]+ b5 * xvals[i]+ b6 * xvals[i]+ b7 * xvals[i]
  lambda <- exp(log_lambda)
  
  logit_psi <- a0 + a1 * xvals[i]
  psi <- 1 / (1 + exp(-logit_psi))   # inverse logit
  
  y_post <- lambda * psi 
  
  px[i] <- mean(y_post)
  ui[i] <- quantile(y_post, 0.95)
  li[i] <- quantile(y_post, 0.05)
}

plot(some_data$date_c, some_data$MSR,
     pch = 16,
     xlab = "date (centered and scaled)",
     ylab = "MSR")

lines(px ~ xvals, lwd = 2, col = "#00636d")

polygon(c(xvals, rev(xvals)),
        c(li, rev(ui)),
        col = "#006e6d88",
        border = NA)
#I just have no clue what to put on the x axis above....

#Interpreting:
mod1$BUGSoutput$summary[c("b0","b1","b2","b3","b4","b5","b6","b7","a0","a1"), ]


coef_table <- mod1$BUGSoutput$summary[c("b1","b2","b3","b4","b5","b6","b7"), ]

coef_table


#PUlling predicted values outj:
msr_pred <- mod1$BUGSoutput$sims.list$MSR.pred

msr_mean <- apply(msr_pred, 2, mean)
msr_low  <- apply(msr_pred, 2, quantile, 0.05)
msr_high <- apply(msr_pred, 2, quantile, 0.95)

plot(msr_mean, some_data$MSR,
     xlab = "Predicted MSR",
     ylab = "Observed MSR")

abline(0,1,lty=2)



msr_pred <- mod1$BUGSoutput$sims.list$MSR.pred

mean_pred <- apply(msr_pred, 2, mean)
low_pred  <- apply(msr_pred, 2, quantile, 0.05)
high_pred <- apply(msr_pred, 2, quantile, 0.95)
plot(some_data$date_c, some_data$MSR,
     pch = 16,
     xlab = "date (scaled)",
     ylab = "MSR")

points(some_data$date_c, mean_pred, col = "blue", pch = 16)



#EFfects:

b0 <- mod1$BUGSoutput$sims.list$b0
b1 <- mod1$BUGSoutput$sims.list$b1
b2 <- mod1$BUGSoutput$sims.list$b2
b3 <- mod1$BUGSoutput$sims.list$b3
b4 <- mod1$BUGSoutput$sims.list$b4
b5 <- mod1$BUGSoutput$sims.list$b5
b6 <- mod1$BUGSoutput$sims.list$b6
b7 <- mod1$BUGSoutput$sims.list$b7

a0 <- mod1$BUGSoutput$sims.list$a0
a1 <- mod1$BUGSoutput$sims.list$a1


exp_effects <- data.frame(
  param = c("b0","b1","b2","b3","b4","b5","b6","b7"),
  mean  = c(mean(exp(b0)), mean(exp(b1)), mean(exp(b2)), mean(exp(b3)),
            mean(exp(b4)), mean(exp(b5)), mean(exp(b6)), mean(exp(b7))),
  low   = c(quantile(exp(b0),0.05), quantile(exp(b1),0.05), quantile(exp(b2),0.05),
            quantile(exp(b3),0.05), quantile(exp(b4),0.05), quantile(exp(b5),0.05),
            quantile(exp(b6),0.05), quantile(exp(b7),0.05)),
  high  = c(quantile(exp(b0),0.95), quantile(exp(b1),0.95), quantile(exp(b2),0.95),
            quantile(exp(b3),0.95), quantile(exp(b4),0.95), quantile(exp(b5),0.95),
            quantile(exp(b6),0.95), quantile(exp(b7),0.95))
)

exp_effects

#Ugh
inv_logit <- function(x) 1 / (1 + exp(-x))

psi_mean <- inv_logit(a0)
psi_plus1 <- inv_logit(a0 + a1*1)
psi_minus1 <- inv_logit(a0 - a1*1)

psi_effects <- data.frame(
  scenario = c("mean (0)", "+1 SD", "-1 SD"),
  mean = c(mean(psi_mean), mean(psi_plus1), mean(psi_minus1)),
  low  = c(quantile(psi_mean,0.05), quantile(psi_plus1,0.05), quantile(psi_minus1,0.05)),
  high = c(quantile(psi_mean,0.95), quantile(psi_plus1,0.95), quantile(psi_minus1,0.95))
)

psi_effects


#Visualize:

coef_df <- bind_rows(
  
  data.frame(
    param = c("PSR","TSR","TD","SM","SO","SS","Date (MSR)"),
    mean = c(mean(b1), mean(b2), mean(b3), mean(b4),
             mean(b5), mean(b6), mean(b7)),
    low  = c(quantile(b1,0.05), quantile(b2,0.05), quantile(b3,0.05),
             quantile(b4,0.05), quantile(b5,0.05), quantile(b6,0.05),
             quantile(b7,0.05)),
    high = c(quantile(b1,0.95), quantile(b2,0.95), quantile(b3,0.95),
             quantile(b4,0.95), quantile(b5,0.95), quantile(b6,0.95),
             quantile(b7,0.95)),
    type = "Poisson"
  ),
  
  data.frame(
    param = "Date (presence)",
    mean = mean(a1),
    low  = quantile(a1,0.05),
    high = quantile(a1,0.95),
    type = "0 inflation"
  )
)

ggplot(coef_df, aes(x = param, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~type, scales = "free_x") +
  theme_classic() +
  labs(
    x = "Predictors",
    y = "Effect size (link scale)",
    title = "Model Coefficients with 90% Credible Intervals"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
