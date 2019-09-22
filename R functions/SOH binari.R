SOH_bi<-vector(length=length(my_data$SOH2))
for (i in 1:length(my_data$SOH2)){
  SOH_bi[i]<-if (my_data$SOH2[i]>=100) 1 else if (my_data$SOH2[i]<100)0
}