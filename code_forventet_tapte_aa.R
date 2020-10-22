########### Antall tapte aar all countries ####################

#swe
cohortj_swe = function(yearHj,Omega){
  a = Hm.cohort_swe(yearHj)
  omega= min(Omega,length(a$Mu_hat)-1)
  
  Mu_hat=a$Mu_hat
  vec = rep(0, omega)              
  vec[1] = (1/Mu_hat[1])*(1-exp(-Mu_hat[1]))
  for (k in (2:omega)) {
    if( k %in%1 ){
      vec[k] = exp(-sum(Mu_hat[1]))*ifelse(Mu_hat[k]>0,(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
    } else {
      vec[k] = exp(-sum(Mu_hat[1:(k-1)]))*ifelse(Mu_hat[k]>0,(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
    }
    
  }
  if (omega %in% 1){return(omega-vec[2])} else {return(lost = omega-sum(vec))} 
}
#ita
cohortj_ita = function(yearHj,Omega){
  a = Hm.period_ita(yearHj)
  omega= min(Omega,length(a$Mu_hat)-1)
  
  Mu_hat=a$Mu_hat
  vec = rep(0, omega)              
  vec[1] = (1/Mu_hat[1])*(1-exp(-Mu_hat[1]))
  for (k in (2:omega)) {
    if( k %in%1 ){
      vec[k] = exp(-sum(Mu_hat[1]))*ifelse(Mu_hat[k]>0,(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
    } else {
      vec[k] = exp(-sum(Mu_hat[1:(k-1)]))*ifelse(Mu_hat[k]>0,(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
    }
    
  }
  if (omega %in% 1){return(omega-vec[2])} else {return(lost = omega-sum(vec))} 
}
#spa
cohortj_spa = function(yearHj,Omega){
  a = Hm.period_spa(yearHj)
  omega= min(Omega,length(a$Mu_hat)-1)
  
  Mu_hat=a$Mu_hat
  vec = rep(0, omega)              
  vec[1] = (1/Mu_hat[1])*(1-exp(-Mu_hat[1]))
  for (k in (2:omega)) {
    if( k %in%1 ){
      vec[k] = exp(-sum(Mu_hat[1]))*ifelse(Mu_hat[k]>0,(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
    } else {
      vec[k] = exp(-sum(Mu_hat[1:(k-1)]))*ifelse(Mu_hat[k]>0,(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
    }
    
  }
  if (omega %in% 1){return(omega-vec[2])} else {return(lost = omega-sum(vec))} 
}


#nor
cohortj_nor = function(yearHj,Omega){
  a = Hm.period(yearHj)
  omega= min(Omega,length(a$Mu_hat)-1)
  
  Mu_hat=a$Mu_hat
  vec = rep(0, omega)              
  vec[1] = (1/Mu_hat[1])*(1-exp(-Mu_hat[1]))
  for (k in (2:omega)) {
    if( k %in%1 ){
      vec[k] = exp(-sum(Mu_hat[1]))*ifelse(Mu_hat[k]>0,(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
    } else {
      vec[k] = exp(-sum(Mu_hat[1:(k-1)]))*ifelse(Mu_hat[k]>0,(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
    }
    
  }
  if (omega %in% 1){return(omega-vec[2])} else {return(lost = omega-sum(vec))} 
}



#regner ut antall tapte år for cohorts 1900,1905,...,2000 med maks alder 100, 80, 60, 40 og 20

#par(mfrow=c(3,2))
cohorts90= seq(1910,1924,1)

yearsLost90nor = NULL
yearsLost90swe = NULL
yearsLost90ita = NULL
yearsLost90spa = NULL

for (i in 1:length(cohorts90)){
  print(i)
  yearsLost90nor[i]= cohortj_nor(cohorts90[i],90)
  yearsLost90swe[i]= cohortj_swe(cohorts90[i],90)
  yearsLost90ita[i]= cohortj_ita(cohorts90[i],90)
  yearsLost90spa[i]= cohortj_spa(cohorts90[i],90)

}
max(c(yearsLost90nor,yearsLost90swe,yearsLost90ita,yearsLost90spa))


plot(cohorts90,yearsLost90nor,ylim=c(1,45),type="l", ylab= "Years lost", xlab = "Cohorts", main="90 years",col="blue",lwd=2)
lines(cohorts90,yearsLost90swe,col="orange",lwd=2)
lines(cohorts90,yearsLost90spa,col="green3",lwd=2)
lines(cohorts90,yearsLost90ita,col="red",lwd=2)
#legend("topright", legend=c("Spania", "Italia", "Sweden", "Norway"),col=c("green3","red","orange", "blue"), pch="-",
#       cex=1,lwd=2, box.col = "white")
box()


#### alder 80


cohorts80= seq(1910,1934,1)

yearsLost80nor = NULL
yearsLost80swe = NULL
yearsLost80ita = NULL
yearsLost80spa = NULL

for (i in 1:length(cohorts80)){
  print(i)
  yearsLost80nor[i]= cohortj_nor(cohorts80[i],80)
  yearsLost80swe[i]= cohortj_swe(cohorts80[i],80)
  yearsLost80ita[i]= cohortj_ita(cohorts80[i],80)
  yearsLost80spa[i]= cohortj_spa(cohorts80[i],80)
  
}
max(c(yearsLost80nor,yearsLost80swe,yearsLost80ita,yearsLost80spa))


plot(cohorts80,yearsLost80nor,ylim=c(1,35),type="l", ylab= "Years lost", xlab = "Cohorts", main="80 years",col="blue",lwd=2)
lines(cohorts80,yearsLost80swe,col="orange",lwd=2)
lines(cohorts80,yearsLost80spa,col="green3",lwd=2)
lines(cohorts80,yearsLost80ita,col="red",lwd=2)
#legend("topright", legend=c("Spania", "Italia", "Sweden", "Norway"),col=c("green3","red","orange", "blue"), pch="-",
#       cex=1,lwd=2, box.col = "white")
box()

#### alder 70


cohorts70= seq(1910,1944,1)

yearsLost70nor = NULL
yearsLost70swe = NULL
yearsLost70ita = NULL
yearsLost70spa = NULL

for (i in 1:length(cohorts70)){
  print(i)
  yearsLost70nor[i]= cohortj_nor(cohorts70[i],70)
  yearsLost70swe[i]= cohortj_swe(cohorts70[i],70)
  yearsLost70ita[i]= cohortj_ita(cohorts70[i],70)
  yearsLost70spa[i]= cohortj_spa(cohorts70[i],70)
  
}
max(c(yearsLost70nor,yearsLost70swe,yearsLost70ita,yearsLost70spa))


plot(cohorts70,yearsLost70nor,ylim=c(1,30),type="l", ylab= "Years lost", xlab = "Cohorts", main="70 years",col="blue",lwd=2)
lines(cohorts70,yearsLost70swe,col="orange",lwd=2)
lines(cohorts70,yearsLost70spa,col="green3",lwd=2)
lines(cohorts70,yearsLost70ita,col="red",lwd=2)
#legend("topright", legend=c("Spania", "Italia", "Sweden", "Norway"),col=c("green3","red","orange", "blue"), pch="-",
#       cex=1,lwd=2, box.col = "white")
box()


#### alder 60
cohorts60= seq(1910,1954,1)

yearsLost60nor = NULL
yearsLost60swe = NULL
yearsLost60ita = NULL
yearsLost60spa = NULL

for (i in 1:length(cohorts60)){
  print(i)
  yearsLost60nor[i]= cohortj_nor(cohorts60[i],60)
  yearsLost60swe[i]= cohortj_swe(cohorts60[i],60)
  yearsLost60ita[i]= cohortj_ita(cohorts60[i],60)
  yearsLost60spa[i]= cohortj_spa(cohorts60[i],60)
  
}
max(c(yearsLost60nor,yearsLost60swe,yearsLost60ita,yearsLost60spa))


plot(cohorts60,yearsLost60nor,ylim=c(1,25),type="l", ylab= "Years lost", xlab = "Cohorts", main="60 years",col="blue",lwd=2)
lines(cohorts60,yearsLost60swe,col="orange",lwd=2)
lines(cohorts60,yearsLost60spa,col="green3",lwd=2)
lines(cohorts60,yearsLost60ita,col="red",lwd=2)
#legend("topright", legend=c("Spania", "Italia", "Sweden", "Norway"),col=c("green3","red","orange", "blue"), pch="-",
#       cex=1,lwd=2, box.col = "white")
box()



#### alde 50

cohorts50= seq(1910,1964,1)

yearsLost50nor = NULL
yearsLost50swe = NULL
yearsLost50ita = NULL
yearsLost50spa = NULL

for (i in 1:length(cohorts50)){
  print(i)
  yearsLost50nor[i]= cohortj_nor(cohorts50[i],50)
  yearsLost50swe[i]= cohortj_swe(cohorts50[i],50)
  yearsLost50ita[i]= cohortj_ita(cohorts50[i],50)
  yearsLost50spa[i]= cohortj_spa(cohorts50[i],50)
  
}
max(c(yearsLost50nor,yearsLost50swe,yearsLost50ita,yearsLost50spa))


plot(cohorts50,yearsLost50nor,ylim=c(1,20),type="l", ylab= "Years lost", xlab = "Cohorts", main="50 years",col="blue",lwd=2)
lines(cohorts50,yearsLost50swe,col="orange",lwd=2)
lines(cohorts50,yearsLost50spa,col="green3",lwd=2)
lines(cohorts50,yearsLost50ita,col="red",lwd=2)
#legend("topright", legend=c("Spania", "Italia", "Sweden", "Norway"),col=c("green3","red","orange", "blue"), pch="-",
#       cex=1,lwd=2, box.col = "white")
box()



#### alde 40

cohorts40= seq(1910,1974,1)

yearsLost40nor = NULL
yearsLost40swe = NULL
yearsLost40ita = NULL
yearsLost40spa = NULL

for (i in 1:length(cohorts40)){
  print(i)
  yearsLost40nor[i]= cohortj_nor(cohorts40[i],40)
  yearsLost40swe[i]= cohortj_swe(cohorts40[i],40)
  yearsLost40ita[i]= cohortj_ita(cohorts40[i],40)
  yearsLost40spa[i]= cohortj_spa(cohorts40[i],40)
  
}
max(c(yearsLost40nor,yearsLost40swe,yearsLost40ita,yearsLost40spa))


plot(cohorts40,yearsLost40nor,ylim=c(1,15),type="l", ylab= "Years lost", xlab = "Cohorts", main="40 years",col="blue",lwd=2)
lines(cohorts40,yearsLost40swe,col="orange",lwd=2)
lines(cohorts40,yearsLost40spa,col="green3",lwd=2)
lines(cohorts40,yearsLost40ita,col="red",lwd=2)
legend("topright", legend=c("Spania", "Italia", "Sweden", "Norway"),col=c("green3","red","orange", "blue"), pch="-",
       cex=1,lwd=2, box.col = "white")
box()










###################     regner ut antall tapte år for fiksert cohort over alle aldre    ###########################
#par(mfrow=c(2,2))
#### kohort 1910
ages=1:100
yearsLost100age10nor= NULL
yearsLost100age10swe= NULL
yearsLost100age10ita= NULL
yearsLost100age10spa= NULL

#years lost for each country in 1910
for (i in 1:length(ages)){
  print(i)
  yearsLost100age10nor[i]= cohortj_nor(1910,ages[i])
  yearsLost100age10swe[i]= cohortj_swe(1910,ages[i])
  yearsLost100age10ita[i]= cohortj_ita(1910,ages[i])
  yearsLost100age10spa[i]= cohortj_spa(1910,ages[i])

} #can also get $expected for each of the countries using the same functions

plot(ages,yearsLost100age10nor,ylim=c(0,52),type="l", ylab= "Years lost", xlab = "Age",col="blue",main="1910",lwd=2)
lines(ages,yearsLost100age10swe,col="orange",lwd=2)
lines(ages,yearsLost100age10ita,col="red",lwd=2)
lines(ages,yearsLost100age10spa,col="green3",lwd=2)
legend("topleft", legend=c("Spania", "Ita", "Sweden", "Norway"),col=c("green3","red","orange", "blue"), pch="-",
       cex=1,lwd=2, box.col = "white")
box()


#### kohort 1930

ages=1:84
yearsLost84age34nor= NULL
yearsLost84age34swe= NULL
yearsLost84age34ita= NULL
yearsLost84age34spa= NULL

#years lost for each country in 1930
for (i in 1:length(ages)){
  print(i)
  yearsLost84age34nor[i]= cohortj_nor(1934,ages[i])
  yearsLost84age34swe[i]= cohortj_swe(1934,ages[i])
  yearsLost84age34ita[i]= cohortj_ita(1934,ages[i])
  yearsLost84age34spa[i]= cohortj_spa(1934,ages[i])
  
} #can also get $expected for each of the countries using the same functions

plot(ages,yearsLost84age34nor,ylim=c(0,25),type="l", ylab= "Years lost", xlab = "Age",col="blue",main="1930",lwd=2)
lines(ages,yearsLost84age34swe,col="orange",lwd=2)
lines(ages,yearsLost84age34ita,col="red",lwd=2)
lines(ages,yearsLost84age34spa,col="green3",lwd=2)
legend("topleft", legend=c("Spania", "Italia", "Sweden", "Norway"),col=c("green3","red","orange", "blue"), pch="-",
       cex=1,lwd=2, box.col = "white")
box()


#### kohort 1950

ages=1:64
yearsLost64age54nor= NULL
yearsLost64age54swe= NULL
yearsLost64age54ita= NULL
yearsLost64age54spa= NULL

#years lost for each country in 1950
for (i in 1:length(ages)){
  print(i)
  yearsLost64age54nor[i]= cohortj_nor(1950,ages[i])
  yearsLost64age54swe[i]= cohortj_swe(1950,ages[i])
  yearsLost64age54ita[i]= cohortj_ita(1950,ages[i])
  yearsLost64age54spa[i]= cohortj_spa(1950,ages[i])
  
} #can also get $expected for each of the countries using the same functions

plot(ages,yearsLost64age54nor,ylim=c(0,10),type="l", ylab= "Years lost", xlab = "Age",col="blue",main="1950",lwd=2)
lines(ages,yearsLost64age54swe,col="orange",lwd=2)
lines(ages,yearsLost64age54ita,col="red",lwd=2)
lines(ages,yearsLost64age54spa,col="green3",lwd=2)
legend("topleft", legend=c("Spania", "Italia", "Sweden", "Norway"),col=c("green3","red","orange", "blue"), pch="-",
       cex=1,lwd=2, box.col = "white")
box()



#### kohort 1970
ages=1:44
yearsLost44age70nor= NULL
yearsLost44age70swe= NULL
yearsLost44age70ita= NULL
yearsLost44age70spa= NULL

#years lost for each country in 1910
for (i in 1:length(ages)){
  print(i)
  yearsLost44age70nor[i]= cohortj_nor(1970,ages[i])
  yearsLost44age70swe[i]= cohortj_swe(1970,ages[i])
  yearsLost44age70ita[i]= cohortj_ita(1970,ages[i])
  yearsLost44age70spa[i]= cohortj_spa(1970,ages[i])
  
} #can also get $expected for each of the countries using the same functions

plot(ages,yearsLost44age70nor,ylim=c(0,2),type="l", ylab= "Years lost", xlab = "Age",col="blue",main="1970",lwd=2)
lines(ages,yearsLost44age70swe,col="orange",lwd=2)
lines(ages,yearsLost44age70ita,col="red",lwd=2)
lines(ages,yearsLost44age70spa,col="green3",lwd=2)
legend("topleft", legend=c("Spania", "Italia", "Sweden", "Norway"),col=c("green3","red","orange", "blue"), pch="-",
       cex=1,lwd=2, box.col = "white")
box()








