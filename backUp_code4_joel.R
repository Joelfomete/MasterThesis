

###############  Period data  ###############



Hm.period <- function(year = x ){
  Death <- read.table("dataMortality.txt", sep=";",dec=".",header=TRUE)
  Death <- Death[,2:7]
  Death_subset <- subset(Death, Year == year)
  Death_subset <- Death_subset[1:220,]
  Death_subset$Age <- as.numeric(paste(Death_subset$Age))
  female_Death <- aggregate(Female~Year+Age, Death_subset, sum)
  female_Death$Cohorts <- female_Death$Year - female_Death$Age
  
  Exposure <- read.table("dataMortality2.txt", sep=";",dec=".",header=TRUE) 
  Exposure <- Exposure[,2:7]# fjerner den tomme kolonnen
  Exposure_subset <- subset(Exposure, Year == year) 
  Exposure_subset <- Exposure_subset[1:220,] # for ? fjerne 110+
  Exposure_subset$Age <- as.numeric(paste(Exposure_subset$Age)) # Age er faktor og jeg vil ha det som numeric
  female_Exposure <- aggregate(Female~Year+Age, Exposure_subset, sum)# adderer to og to
  female_Exposure$Cohorts <- female_Exposure$Year - female_Exposure$Age
  
  #Mu_hat <- female_Death$Female/female_Exposure$Female
  Mu_hat <- ifelse(female_Exposure$Female>0,female_Death$Female/female_Exposure$Female,0)
  # alle verdier av Mu_hat mindre enn 0 satt til 0
  # ANTAR at alle verdier mindre enn 0 behandles som 0! 
  return(list(Mu_hat = Mu_hat, subset_death = female_Death,subset_exp = female_Exposure))}

a = Hm.period(year =1900)
a_data_exp = a$subset_exp
a$subset_death

b = Hm.period(year =1930)
b_data_exp = b$subset_exp

c = Hm.period(year =1960)
c_data_exp = c$subset_exp

d = Hm.period(year =1990)
d_data_exp = d$subset_exp

e = Hm.period(year =2014)
e_data_exp = e$subset_exp

plot(min(a_data_exp$Age) :max(a_data_exp$Age),a$Mu_hat,col="red", type= "l",log="y",xlab= "Age", 
     ylab="Mortality rate",lwd=2, ylim=c(0.000040,1.4))
lines(min(b_data_exp$Age) :max(b_data_exp$Age),b$Mu_hat,col="magenta",lwd=2)
lines(min(c_data_exp$Age) :max(c_data_exp$Age),c$Mu_hat,col="darkgreen",lwd=2)
lines(min(d_data_exp$Age) :max(d_data_exp$Age),d$Mu_hat,col="gold3",lwd=2)
lines(min(e_data_exp$Age) :max(e_data_exp$Age),e$Mu_hat,col="blue",lwd=2)
legend("bottomright",legend=c("1900", "1930", "1960", "1990","2014"), col =c("red", "magenta", "darkgreen", "gold3", "blue"),
       pch="-",cex=1,lwd=3,box.col = "white")
box()


##############   Ploting the period Life expectancy   ##############


periodj = function(yearj){
  a = Hm.period(yearj)
  #Vil ha en data frame som ineholder en kohort
  
  Mu_hat=a$Mu_hat
  #Mu_hat = Mu_hat[!is.na(Mu_hat)]  #Vil fjerne NAN 
  #dim(Mu_hat) = c(length(Mu_hat),1) # vil ha mu2_hat som en kolonne (dette er ikke noe poeng her)
  
  omega = 110
  vec = rep(0, omega)                 #lager tom vektor med lengden omega
  vec[1] = (1/Mu_hat[1])*(1-exp(-Mu_hat[1]))
  for (k in 2:omega) {
    # vec[k] = ifelse(Mu_hat[k]>0, exp(-sum(Mu_hat[0:(k-1)]))*(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
    vec[k] = exp(-sum(Mu_hat[1:(k-1)]))*ifelse(Mu_hat[k]>0,(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
  }
  return(sum(vec))
}

periodj(1994)

years = 1900:2014
p = rep(NA, length(years))
for (j in 1:length(years)) {
  p[j] = periodj(years[j])
}
plot(years, p,type='l',col="orange", ylab ="Life expectancy",xaxt="n",lwd=2,main="Period Life expectancy")
axis(1,seq(1900,2014,5),seq(1900,2014,5))
#axis(1,seq(1880,2014,4),seq(1880,2014,4))
#periodj(1950)





###############  Cohorts data  ###############

Hm.cohort<- function(cohort = x){
  dataDeath <- read.table("dataMortality.txt", sep=";",dec=".",header=TRUE) 
  dataDeath <- dataDeath[,2:7]# fjerner den tomme kolonnen
  dataDeath_subset_cohort <- subset(dataDeath, Cohort == cohort)
  #dataDeath_subset_cohort<-dataDeath_subset_cohort[1:220,]
  #dim(dataDeath_subset_cohort) # dim er 221
  dataDeath_subset_cohort$Age <- as.numeric(paste(dataDeath_subset_cohort$Age))
  female_dataDeath_subset_cohort <- aggregate(Female~Cohort+Age, dataDeath_subset_cohort, sum)
  
  
  dataExposure <- read.table("dataMortality2.txt", sep=";",dec=".",header=TRUE) 
  dataExposure_subset_cohort <- subset(dataExposure, Cohort == cohort)
  #dataExposure_subset_cohort <- dataExposure_subset_cohort[1:220,]
  dataExposure_subset_cohort$Age = as.numeric(paste(dataExposure_subset_cohort$Age))
  #plot(dataExposure_subset_cohort$Year,dataExposure_subset_cohort$Age)
  female_dataExposure_subset_cohort = aggregate(Female~Cohort+Age, dataExposure_subset_cohort, sum)# adderer to og to
  #mu2_hat = dataDeath_subset_cohort$Female/dataExposure_subset_cohort$Female# d?de delt p? person years 
  mu2_hat = ifelse(female_dataDeath_subset_cohort$Female>0,female_dataDeath_subset_cohort$Female/female_dataExposure_subset_cohort$Female,0) # d?de delt p? person years 
  #mu2_hat[1] # verifisere ved hente den f?rste: 1753/15618.67 = 0.1122375
  return(list(Mu_hat=mu2_hat, subset_death = female_dataDeath_subset_cohort, subset_exp = female_dataExposure_subset_cohort))
}

A = Hm.cohort(cohort= 1900)
A_data_exp = A$subset_exp

B = Hm.cohort(cohort= 1920)
B_data_exp = B$subset_exp

C = Hm.cohort(cohort= 1940)
C_data_exp = C$subset_exp

D = Hm.cohort(cohort= 1960)
D_data_exp = D$subset_exp

E = Hm.cohort(cohort= 1980)
E_data_exp = E$subset_exp

plot(A_data_exp$Age,A$Mu_hat,type="l",col="red",log="y",xlab= "Age", ylab="Mortality rate ",ylim=c(0.000040,1.4),lwd=2) 
lines(B_data_exp$Age,B$Mu_hat,col="magenta",lwd=2)
lines(C_data_exp$Age,C$Mu_hat,col="darkgreen",lwd=2)
lines(D_data_exp$Age,D$Mu_hat,col="gold3",lwd=2)
lines(E_data_exp$Age,E$Mu_hat,col="blue",lwd=2)
legend("bottomright", legend=c("1900", "1920", "1940", "1960", "1980"),col=c("red", "magenta", "darkgreen", "gold3", "blue"),
       pch="-", cex=1,lwd=3, box.col = "white")
box()



##############   Ploting the cohorts Life expectancy ###################
##############   Here we plot age as a function of cohort ##############

cohortj = function(yearHj, omega=100){
  a = Hm.cohort(yearHj)
  #Vil ha en data frame som ineholder en Cohort
  
  Mu_hat=a$Mu_hat
  #Mu_hat = Mu_hat[!is.na(Mu_hat)]  #Vil fjerne NAN 
  #dim(Mu_hat) = c(length(Mu_hat),1) # vil ha mu2_hat som en colone
  vec = rep(0, omega)                 #lager tom vektor med lengden omega
  vec[1] = (1/Mu_hat[1])*(1-exp(-Mu_hat[1]))
  for (k in 2:omega) {
    vec[k] = exp(-sum(Mu_hat[1:(k-1)]))*ifelse(Mu_hat[k]>0,(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
    #vec[k] = ifelse(Mu_hat[k]>0, exp(-sum(Mu_hat[0:(k-1)]))*(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
    #vec[k] = exp(-sum(Mu_hat[0:(k-1)]))*(1/Mu_hat[k])*(1-exp(-Mu_hat[k]))
  }
  return(sum(vec))
}
years = 1900:1994
p = rep(NA, length(years))
for (j in 1:length(years)) {
  p[j] = cohortj (years[j])
}


plot(years, p,type='l', ylab = "Life expectancy" , xlab = "Cohorts",xaxt="n",yaxt="n",lwd=2,main="Life expectancy",
     col="red",xlim=c(1900,2000),ylim=c(0,75))
axis(1,seq(1900,2000,5),seq(1900,2000,5))
axis(2,seq(0,75,5),seq(0,75,5))
lines(years, p,lwd=2, col="magenta")
lines(years, p,lwd=2, col="green")
lines(years, p,lwd=2, col="gold3")
lines(years, p,lwd=2, col="blue")
legend("topright", legend=c("100", "80", "60", "40","20"),col=c("red", "magenta","green", "gold3","blue"), pch="-",
       cex=1,lwd=3, box.col = "white", title = " Age a = ")
box()







##########################  Antall Tapte år ###########################
lossedYears = function(omega){
  years = 1900:1994
  L = rep(NA, length(years))
  for (j in 1:length(years)) {
    L[j] = omega - cohortj (years[j], omega=omega)
  }
  return(L)
}

omega=c(20,40,60,80,100)
LY=rep(NA, length(omega))
for (i in 1:length(omega)) {
  LY[i]=lossedYears(omega[i])
}
LY







#################### We fix the cohort as a function of age. ########################

#a = Hm.cohort(1994)

cohortj = function(yearHj,Omega){
  a = Hm.cohort(yearHj)
  omega= min(Omega,length(a$Mu_hat)-1)
  #Vil ha en data frame som ineholder en Cohort
  Mu_hat=a$Mu_hat
  vec = rep(0, omega)                 #lager tom vektor med lengden omega
  vec[1] = (1/Mu_hat[1])*(1-exp(-Mu_hat[1]))
  for (k in (2:omega)) {
    if( k==1 ){
      vec[k] = exp(-sum(Mu_hat[1]))*ifelse(Mu_hat[k]>0,(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
    } else {
      vec[k] = exp(-sum(Mu_hat[1:(k-1)]))*ifelse(Mu_hat[k]>0,(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
    }
    #vec[k] = ifelse(Mu_hat[k]>0, exp(-sum(Mu_hat[0:(k-1)]))*(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
    #vec[k] = exp(-sum(Mu_hat[0:(k-1)]))*(1/Mu_hat[k])*(1-exp(-Mu_hat[k]))
  }
  return(expLivingAge=sum(vec))
}



cohort1914 = NULL
for (i in (1:100)){
  cohort1914[i] = cohortj(1914,i)
}

plot(1:100, 1:100, col="white", ylim=c(0,75),xlab="Age",ylab="Life expectancy")
lines(1:100,cohort1914,col="red",lwd=2)

##

cohort1934 = NULL
for (i in (1:80)){
  cohort1934[i] = cohortj(1934,i)
}

lines(1:80,cohort1934,col="magenta",lwd=2)

##

cohort1954 = NULL
for (i in (1:60)){
  cohort1954[i] = cohortj(1954,i)
}

lines(1:60,cohort1954,col="green",lwd=2)

##

cohort1974 = NULL
for (i in (1:40)){
  cohort1974[i] = cohortj(1974,i)
}

lines(1:40,cohort1974,col="gold3",lwd=2)

##

cohort1994 = NULL
for (i in (1:20)){
  cohort1994[i] = cohortj(1994,i)
}

lines(1:20,cohort1994,col="blue",lwd=2)

legend("bottomright", legend=c("100", "80", "60", "40","20"),col=c("red", "magenta","green", "gold3","blue"), pch="-",
       cex=1,lwd=3, box.col = "white")
box()

############################################   VVVVVVVVVvvvvvv     ###################################################



ages = 0:100
p = rep(NA, length(ages))
for (j in 1:length(ages)) {
  p[j] = cohortj (ages[j])
}

plot(ages, p,type='l', ylab = "Life expectancy" , xlab = "Cohorts",xaxt="n",yaxt="n",lwd=2,main="Life expectancy",
     col="red",xlim=c(1900,2000),ylim=c(0,75))
axis(1,seq(1900,2000,5),seq(1900,2000,5))
axis(2,seq(0,75,5),seq(0,75,5))
lines(ages, p,lwd=2, col="magenta")
lines(ages, p,lwd=2, col="green")
lines(ages, p,lwd=2, col="gold3")
lines(ages, p,lwd=2, col="blue")
legend("topright", legend=c("100", "80", "60", "40","20"),col=c("red", "magenta","green", "gold3","blue"), pch="-",
       cex=1,lwd=3, box.col = "white", title = " Age a = ")
box()








########### Antall tapte aa ####################

#regner ut antall tapte år for cohorts 1900,1905,...,2000 med maks alder 100, 80, 60, 40 og 20
cohorts= seq(1900,2000,5)
length(cohorts)
yearsLost100= NULL
yearsLost80= NULL
yearsLost60= NULL
yearsLost40= NULL
yearsLost20= NULL

for (i in 1:length(cohorts)){
  yearsLost100[i]= cohortj(cohorts[i],100)$lost
  yearsLost80[i]= cohortj(cohorts[i],80)$lost
  yearsLost60[i]= cohortj(cohorts[i],60)$lost
  yearsLost40[i]= cohortj(cohorts[i],40)$lost
  yearsLost20[i]= cohortj(cohorts[i],20)$lost
}

plot(cohorts,yearsLost100,ylim=c(0,40),type="l", ylab= "Years lost", xlab = "Cohorts",lwd=2,
     col="red")
lines(cohorts,yearsLost80, col="magenta",lwd=2)
lines(cohorts,yearsLost60, col="green",lwd=2)
lines(cohorts,yearsLost40, col="gold3",lwd=2)
lines(cohorts,yearsLost20, col="blue",lwd=2)

#regner ut antall tapte år for fiksert cohort over alle aldre
ages=1:100
yearsLost100age14= NULL
yearsLost100age34= NULL
yearsLost100age54= NULL
yearsLost100age74= NULL
yearsLost100age94= NULL

for (i in 1:length(ages)){
  yearsLost100age14[i]= cohortj(1914,ages[i])$lost
  yearsLost100age34[i]= cohortj(1934,ages[i])$lost
  yearsLost100age54[i]= cohortj(1954,ages[i])$lost
  yearsLost100age74[i]= cohortj(1974,ages[i])$lost
  yearsLost100age94[i]= cohortj(1994,ages[i])$lost
}

plot(ages,yearsLost100age14,ylim=c(0,30),type="l", ylab= "Years lost", xlab = "Age",lwd=2,col="red")
lines(ages,yearsLost100age34, col="magenta",lwd=2,lwd=2)
lines(ages,yearsLost100age54, col="green",lwd=2)
lines(ages,yearsLost100age74, col="gold3",lwd=2)
lines(ages,yearsLost100age94, col="blue",lwd=2)


#B = Hm.cohort(cohort= 1960)
#Vil ha en data frame som ineholder en Cohort
#Mu_hat=mu2_hat
#Mu_hat=B$Mu_hat
#alder = 100
#  vec = rep(0,alder+1)
#vec[1] = (1/Mu_hat[1])*(1-exp(-Mu_hat[1]))
#for(k in 2:(alder+1)){
#  vec[k] = exp(-sum(Mu_hat[1:(k-1)]))*ifelse(Mu_hat[k]>0,(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
#}
#sum(vec)






