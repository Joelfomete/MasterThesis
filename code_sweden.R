#SWEDEN DATA
Hm.period_swe <- function(year = x ){
  Death_swe <- read.table("SwedataMortality.txt", sep=";",dec=".",header=TRUE)
  Death_swe <- Death_swe[,2:7]
  Death_swe_subset <- subset(Death_swe, Year == year)
  Death_swe_subset <- Death_swe_subset[1:220,]
  Death_swe_subset$Age <- as.numeric(paste(Death_swe_subset$Age))
  female_Death_swe <- aggregate(Female~Year+Age, Death_swe_subset, sum)
  female_Death_swe$Cohorts <- female_Death_swe$Year - female_Death_swe$Age
  
  Exposure_swe <- read.table("SwedataMortality2.txt", sep=";",dec=".",header=TRUE) 
  Exposure_swe <- Exposure_swe[,2:7]# fjerner den tomme kolonnen
  Exposure_swe_subset <- subset(Exposure_swe, Year == year) 
  Exposure_swe_subset <- Exposure_swe_subset[1:220,] # for ? fjerne 110+
  Exposure_swe_subset$Age <- as.numeric(paste(Exposure_swe_subset$Age)) # Age er faktor og jeg vil ha det som numeric
  female_Exposure_swe <- aggregate(Female~Year+Age, Exposure_swe_subset, sum)# adderer to og to
  female_Exposure_swe$Cohorts <- female_Exposure_swe$Year - female_Exposure_swe$Age
  
  #Mu_hat_swe <- female_Death_swe$Female/female_Exposure_swe$Female
  Mu_hat_swe <- ifelse(female_Exposure_swe$Female>0,female_Death_swe$Female/female_Exposure_swe$Female,0)
  # alle verdier av Mu_hat_swe mindre enn 0 satt til 0
  # ANTAR at alle verdier mindre enn 0 behandles som 0! 
  return(list(Mu_hat_swe = Mu_hat_swe, subset_death = female_Death_swe,subset_exp = female_Exposure_swe))}

Hm.cohort_swe<- function(cohort = x){
  dataDeath <- read.table("SwedataMortality.txt", sep=";",dec=".",header=TRUE) 
  dataDeath <- dataDeath[,2:7]# fjerner den tomme kolonnen
  dataDeath_subset_cohort <- subset(dataDeath, Cohort == cohort)
  #dataDeath_subset_cohort<-dataDeath_subset_cohort[1:220,]
  #dim(dataDeath_subset_cohort) # dim er 221
  dataDeath_subset_cohort$Age <- as.numeric(paste(dataDeath_subset_cohort$Age))
  female_dataDeath_subset_cohort <- aggregate(Female~Cohort+Age, dataDeath_subset_cohort, sum)
  
  
  dataExposure <- read.table("SwedataMortality2.txt", sep=";",dec=".",header=TRUE) 
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
# cohortj_swe = function(yearHj, omega=100){
#   a = Hm.cohort_swe(yearHj)
#   #Vil ha en data frame som ineholder en Cohort
#   
#   Mu_hat=a$Mu_hat
#   #Mu_hat = Mu_hat[!is.na(Mu_hat)]  #Vil fjerne NAN 
#   #dim(Mu_hat) = c(length(Mu_hat),1) # vil ha mu2_hat som en colone
#   vec = rep(0, omega)                 #lager tom vektor med lengden omega
#   vec[1] = (1/Mu_hat[1])*(1-exp(-Mu_hat[1]))
#   for (k in 2:omega) {
#     vec[k] = exp(-sum(Mu_hat[1:(k-1)]))*ifelse(Mu_hat[k]>0,(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
#     #vec[k] = ifelse(Mu_hat[k]>0, exp(-sum(Mu_hat[0:(k-1)]))*(1/Mu_hat[k])*(1-exp(-Mu_hat[k])),1)
#     #vec[k] = exp(-sum(Mu_hat[0:(k-1)]))*(1/Mu_hat[k])*(1-exp(-Mu_hat[k]))
#   }
#   return(list(expected = sum(vec),lost = omega-sum(vec)))
# }
