#Correlation matrix
#Rachel Pitt
#11/6/25


library(tidyverse)

#data set that has soil, mushrooms, + all variables:
str(complete_everything)

#data set that has all variables without soil and mushrooms since those ones were measured per day:
str(alt_all_vars)

#data set that has all variables and then it does soil and mushrooms in wide format with a column of those measurments per survey day
str(actually_all)

#Make data numeric
alt_all_vars_numeric <- alt_all_vars%>%
  mutate(across(where(is.character), as.numeric))%>%
  select(-site, -plot, -plot_number)


res<-cor(alt_all_vars_numeric)
round(res,2)
cor(alt_all_vars_numeric, use="complete.obs")

library(Hmisc)

res2<-rcorr(as.matrix(alt_all_vars_numeric))
res2
res2$r
res2$p
PflattenCorrMatrix<-function(cormat,pmat) {
  ut<-upper.tri(cormat)
  data.frame(
    row=rownames(cormat)[row(cormat)[ut]],
    column=rownames(cormat)[col(cormat)[ut]],
    cor=(cormat)[ut],
    p=pmat[ut]
  )
}
  

symnum(res2, cutpoints= c(0.3,-.6,0.8,0.9,0.95),
       symbols=c(" ",".",",","+","*","B"),
       abrr.colnames=TRUE)






