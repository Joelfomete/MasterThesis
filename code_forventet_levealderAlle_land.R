

########    Life expectency all countries period 1920 to 2014     ########

periodj = function(yearj){
  a_spa = Hm.period_spa(yearj)
  a_swe = Hm.period_swe(yearj)
  a_nor = Hm.period(yearj)
  a_ita = Hm.period_ita(yearj)
  #Vil ha en data frame som ineholder en kohort
  
  Mu_hat_spa=a_spa$Mu_hat
  Mu_hat_swe=a_swe$Mu_hat
  Mu_hat_ita=a_ita$Mu_hat
  Mu_hat_nor=a_nor$Mu_hat
  #Mu_hat = Mu_hat[!is.na(Mu_hat)]  #Vil fjerne NAN 
  #dim(Mu_hat) = c(length(Mu_hat),1) # vil ha mu2_hat som en kolonne (dette er ikke noe poeng her)
  
  omega = 110
  vec_nor = rep(0, omega)                 #lager tom vektor med lengden omega
  vec_swe = rep(0, omega)
  vec_spa = rep(0, omega)
  vec_ita = rep(0, omega)
  vec_nor[1] = (1/Mu_hat_nor[1])*(1-exp(-Mu_hat_nor[1]))
  vec_swe[1] = (1/Mu_hat_swe[1])*(1-exp(-Mu_hat_swe[1]))
  vec_spa[1] = (1/Mu_hat_spa[1])*(1-exp(-Mu_hat_spa[1]))
  vec_ita[1] = (1/Mu_hat_ita[1])*(1-exp(-Mu_hat_ita[1]))
  for (k in 2:omega) {
    # vec[k] = ifelse(Mu_hat[k]>0, exp(-sum(Mu_hat[0:(k-1)]))*(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
    vec_nor[k] = exp(-sum(Mu_hat_nor[1:(k-1)]))*ifelse(Mu_hat_nor[k]>0,(1/Mu_hat_nor[k])*(1-exp(-Mu_hat_nor[k])),1)
    vec_swe[k] = exp(-sum(Mu_hat_swe[1:(k-1)]))*ifelse(Mu_hat_swe[k]>0,(1/Mu_hat_swe[k])*(1-exp(-Mu_hat_swe[k])),1)
    vec_spa[k] = exp(-sum(Mu_hat_spa[1:(k-1)]))*ifelse(Mu_hat_spa[k]>0,(1/Mu_hat_spa[k])*(1-exp(-Mu_hat_spa[k])),1)
    vec_ita[k] = exp(-sum(Mu_hat_ita[1:(k-1)]))*ifelse(Mu_hat_ita[k]>0,(1/Mu_hat_ita[k])*(1-exp(-Mu_hat_ita[k])),1)
  }
  return(list(nor = sum(vec_nor),swe = sum(vec_swe),ita = sum(vec_ita),spa = sum(vec_spa)))
}

periodMatrix = matrix(0,ncol=4,nrow=81)
tempMat = matrix(0,ncol=4,nrow=14)
years = 1920:2000
years2 = 2001:2014
years[81]
length(years2)
for (i in (1:81) ){
  periodMatrix[i,] = c(periodj(years[i])$nor,periodj(years[i])$swe,periodj(years[i])$ita,periodj(years[i])$spa)
}
for (i in (1:14)){
  print(i)
  tempMat[i,] = c(periodj(years2[i])$nor,periodj(years2[i])$swe,periodj(years2[i])$ita,periodj(years2[i])$spa)
}

periodMatrix.1920to2014 = rbind(periodMatrix,tempMat)

periodj(1920)$nor
length(1910:2000)
plot(1920:2014,0:94,col="white", ylim=c(40,85), main = "Period life expectancy",ylab="Ages",xlab="years")
axis(1,seq(1920,2014,10),seq(1920,2014,10))
axis(2,seq(0,75,5),seq(0,75,5))
#norway 
lines(1920:2014,periodMatrix.1920to2014[,1],col="blue",lwd=2)
#sweden
lines(1920:2014,periodMatrix.1920to2014[,2],col="orange",lwd=2)
#italy
lines(1920:2014,periodMatrix.1920to2014[,3],col="red",lwd=2)
#spain
lines(1920:2014,periodMatrix.1920to2014[,4],col="green3",lwd=2)
legend("bottomright", legend=c("Italia", "Spania", "Sweden", "Norway"),col=c("red","green3","orange", "blue"), pch="-",
       cex=1,lwd=2, box.col = "white")
box()




#years = 1900:2014
#p = rep(NA, length(years))
#for (j in 1:length(years)) {
# p[j] = periodj(years[j])
#}
#plot(years, p,type='l',col="orange", ylab ="Life expectancy",xaxt="n",lwd=2,main="Period Life expectancy")
#axis(1,seq(1900,2014,5),seq(1900,2014,5))