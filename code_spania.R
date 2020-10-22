#SPAIN DATA
Hm.period_spa <- function(year = x ){
  Death_spa <- read.table("SpadataMortality.txt", sep=";",dec=".",header=TRUE)
  Death_spa <- Death_spa[,2:7]
  Death_spa_subset <- subset(Death_spa, Year == year)
  Death_spa_subset <- Death_spa_subset[1:220,]
  Death_spa_subset$Age <- as.numeric(paste(Death_spa_subset$Age))
  female_Death_spa <- aggregate(Female~Year+Age, Death_spa_subset, sum)
  female_Death_spa$Cohorts <- female_Death_spa$Year - female_Death_spa$Age
  
  Exposure_spa <- read.table("SpadataMortality2.txt", sep=";",dec=".",header=TRUE) 
  Exposure_spa <- Exposure_spa[,2:7]# fjerner den tomme kolonnen
  Exposure_spa_subset <- subset(Exposure_spa, Year == year) 
  Exposure_spa_subset <- Exposure_spa_subset[1:220,] # for ? fjerne 110+
  Exposure_spa_subset$Age <- as.numeric(paste(Exposure_spa_subset$Age)) # Age er faktor og jeg vil ha det som numeric
  female_Exposure_spa <- aggregate(Female~Year+Age, Exposure_spa_subset, sum)# adderer to og to
  female_Exposure_spa$Cohorts <- female_Exposure_spa$Year - female_Exposure_spa$Age
  
  #Mu_hat_spa <- female_Death_spa$Female/female_Exposure_spa$Female
  Mu_hat_spa <- ifelse(female_Exposure_spa$Female>0,female_Death_spa$Female/female_Exposure_spa$Female,0)
  # alle verdier av Mu_hat_spa mindre enn 0 satt til 0
  # ANTAR at alle verdier mindre enn 0 behandles som 0! 
  return(list(Mu_hat_spa = Mu_hat_spa, subset_death = female_Death_spa,subset_exp = female_Exposure_spa))}

Hm.cohort_spa<- function(cohort = x){
  dataDeath <- read.table("SpadataMortality.txt", sep=";",dec=".",header=TRUE) 
  dataDeath <- dataDeath[,2:7]# fjerner den tomme kolonnen
  dataDeath_subset_cohort <- subset(dataDeath, Cohort == cohort)
  #dataDeath_subset_cohort<-dataDeath_subset_cohort[1:220,]
  #dim(dataDeath_subset_cohort) # dim er 221
  dataDeath_subset_cohort$Age <- as.numeric(paste(dataDeath_subset_cohort$Age))
  female_dataDeath_subset_cohort <- aggregate(Female~Cohort+Age, dataDeath_subset_cohort, sum)
  
  
  dataExposure <- read.table("SpadataMortality2.txt", sep=";",dec=".",header=TRUE) 
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

cohortj_spa = function(yearHj, omega=100){
  a = Hm.cohort_spa(yearHj)
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
