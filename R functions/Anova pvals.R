anovaFUN<-function(x){
  modelsw<-glm(Comparison.2~x,family=quasibinomial,data =data_train,maxit=50)
  summary(modelsw)
  pvalanov<-anova(modelsw,test="Chisq")$"Pr(>Chi)"[2]
}
