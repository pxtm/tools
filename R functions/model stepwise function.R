stepwise<-function(full,initial){
initial<-glm(Status~1,family=binomial,data=train_otus,maxit=30)
full<-glm(Status~.,family=binomial, data=train_otus,maxit=30)
print(summ$coefficients)
p<-dim(summ$coefficients)[1]

while (TRUE){
  summ<-summary(initial)
  rnames<-rownames(summ$coefficients)
  print(summ$coefficients)
  p<-dim(summ$coefficients)[1]

  write("--",file="")
  ##afegir variablesummary(initial)


  a<-tryCatch(add1(initial,scope=full,test="LRT",k=2))
  if (is.null(a)){
    break
  }
  AICmin<-min(a$AIC)
  if (AICmin<(summary(initial)$aic)){
    var<-rownames(a)[a$AIC==AICmin]
    if (length(var)>1){
      var<-var[2]
    }
    write(paste("+++ Adding", var, "\n"), file="")
    f<-formula(initial)
    f<-as.formula(paste(f[2],"~",paste(f[3],var,sep="+")))
    initial<-glm(f,family=binomial, data=train_otus, maxit=30)
    next
  }
if (p>1){
  d<-drop1(initial,test="LRT",k=2)
  pmax<-max(d$`Pr(>Chi)`[-1])
  if (pmax>0.05){
    var<-rownames(d)[d$`Pr(>Chi)`==pmax]
    if (length(var)>1){
      var<-var[2]
    }
  write(paste("--- Dropping", var, "\n"), file="")
  f<-formula(initial)
  f<-as.formula(paste(f[2],"~",paste(f[3],var,sep="-")))
  initial<-glm(f,family=binomial,data=train_otus,maxit=30)
    }

}
  
 }
break
}

