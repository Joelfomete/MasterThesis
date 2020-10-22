#ITALY DATA
Hm.period_ita <- function(year = x ){
  Death_ita <- read.table("ItadataMortality.txt", sep=";",dec=".",header=TRUE)
  Death_ita <- Death_ita[,2:7]
  Death_ita_subset <- subset(Death_ita, Year == year)
  Death_ita_subset <- Death_ita_subset[1:220,]
  Death_ita_subset$Age <- as.numeric(paste(Death_ita_subset$Age))
  female_Death_ita <- aggregate(Female~Year+Age, Death_ita_subset, sum)
  female_Death_ita$Cohorts <- female_Death_ita$Year - female_Death_ita$Age
  
  Exposure_ita <- read.table("ItadataMortality2.txt", sep=";",dec=".",header=TRUE) 
  Exposure_ita <- Exposure_ita[,2:7]# fjerner den tomme kolonnen
  Exposure_ita_subset <- subset(Exposure_ita, Year == year) 
  Exposure_ita_subset <- Exposure_ita_subset[1:220,] # for ? fjerne 110+
  Exposure_ita_subset$Age <- as.numeric(paste(Exposure_ita_subset$Age)) # Age er faktor og jeg vil ha det som numeric
  female_Exposure_ita <- aggregate(Female~Year+Age, Exposure_ita_subset, sum)# adderer to og to
  female_Exposure_ita$Cohorts <- female_Exposure_ita$Year - female_Exposure_ita$Age
  
  #Mu_hat_ita <- female_Death_ita$Female/female_Exposure_ita$Female
  Mu_hat_ita <- ifelse(female_Exposure_ita$Female>0,female_Death_ita$Female/female_Exposure_ita$Female,0)
  # alle verdier av Mu_hat_ita mindre enn 0 satt til 0
  # ANTAR at alle verdier mindre enn 0 behandles som 0! 
  return(list(Mu_hat_ita = Mu_hat_ita, subset_death = female_Death_ita,subset_exp = female_Exposure_ita))}

#example death rates all contries in 1920
a_ita = Hm.period_ita(year =1920)
a_spa = Hm.period_spa(year =1920)
a_swe = Hm.period_swe(year =1920)
a_nor = Hm.period(year = 1920)

plot(0:109,a_ita$Mu_hat_ita,type="l",col="red",ylim=c(0.002,1), log="y",ylab="Mortality rate",xlab="age", main="1920",lwd=2)
lines(0:109,a_nor$Mu_hat,col="blue",lwd=2)
lines(0:109,a_spa$Mu_hat,col="green3",lwd=2)
lines(0:109,a_swe$Mu_hat,col="brown",lwd=2)


Hm.cohort_ita<- function(cohort = x){
  dataDeath_ita <- read.table("ItadataMortality.txt", sep=";",dec=".",header=TRUE) 
  dataDeath_ita <- dataDeath_ita[,2:7]# fjerner den tomme kolonnen
  dataDeath_ita_subset_cohort <- subset(dataDeath_ita, Cohort == cohort)
  #dataDeath_ita_subset_cohort<-dataDeath_ita_subset_cohort[1:220,]
  #dim(dataDeath_ita_subset_cohort) # dim er 221
  dataDeath_ita_subset_cohort$Age <- as.numeric(paste(dataDeath_ita_subset_cohort$Age))
  female_dataDeath_ita_subset_cohort <- aggregate(Female~Cohort+Age, dataDeath_ita_subset_cohort, sum)
  
  
  dataExposure_ita <- read.table("ItadataMortality2.txt", sep=";",dec=".",header=TRUE) 
  dataExposure_ita_subset_cohort <- subset(dataExposure_ita, Cohort == cohort)
  #dataExposure_ita_subset_cohort <- dataExposure_ita_subset_cohort[1:220,]
  dataExposure_ita_subset_cohort$Age = as.numeric(paste(dataExposure_ita_subset_cohort$Age))
  #plot(dataExposure_ita_subset_cohort$Year,dataExposure_ita_subset_cohort$Age)
  female_dataExposure_ita_subset_cohort = aggregate(Female~Cohort+Age, dataExposure_ita_subset_cohort, sum)# adderer to og to
  #mu2_hat = dataDeath_ita_subset_cohort$Female/dataExposure_ita_subset_cohort$Female# d?de delt p? person years 
  mu2_hat = ifelse(female_dataDeath_ita_subset_cohort$Female>0,female_dataDeath_ita_subset_cohort$Female/female_dataExposure_ita_subset_cohort$Female,0) # d?de delt p? person years 
  #mu2_hat[1] # verifisere ved hente den f?rste: 1753/15618.67 = 0.1122375
  return(list(Mu_hat=mu2_hat, subset_death = female_dataDeath_ita_subset_cohort, subset_exp = female_dataExposure_ita_subset_cohort))
}

cohortj_ita = function(yearHj, omega=100){
  a = Hm.cohort_ita(yearHj)
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
  return(list(expected = sum(vec),lost = omega-sum(vec)))
}

cohortj_ita(1914,100)





