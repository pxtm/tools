sampling<-function(x){
  rNames<-row.names(x)
  sampRows<-sample(rNames,round(nrow(x)*0.8))
  data80<-subset(x,rNames%in%sampRows)
  data20<-subset(x,!rNames%in%sampRows)
  list(d80 = data80, d20 = data20)
}

