Hm.period<- function(year = x )
{
  data<-read.table("dataMortality.txt",header=TRUE)
  data_subset <- subset(data, Year == year)
  data_subset<-data_subset[1:220,] # for ? fjerne 110+
  data_subset$Age <- as.numeric(paste(data_subset$Age)) # Age er faktor og jeg vil ha det som numeric
  female_data_subset <- aggregate(Female~Age, data_subset, sum) # adderer to og to
  data2 = read.table("dataMortality2.txt",header=TRUE) 
  data2_subset <- subset(data2, Year == year) 
  data2_subset = data2_subset[1:220,] # for ? fjerne 110+
  data2_subset$Age = as.numeric(paste(data2_subset$Age)) # Age er faktor og jeg vil ha det som numeric
  female_data2_subset = aggregate(Female~Age, data2_subset, sum)# adderer to og to
  Mu_hat = female_data_subset$Female/female_data2_subset$Female
  return(list(Mu_hat=Mu_hat, subset_death=female_data_subset,subset_exp = female_data2_subset))
}

a = Hm.period(year =1920)
b = Hm.period(year =1950)
bb = Hm.period(year =1980)
cc = Hm.period(year =2010)

plot(0:109,a$Mu_hat,col="red", type= "l",log="y",xlab= "Age", ylab="Mortality rate", ylim=c(0.00005,0.5),xlim=c(0,100))  # Vi kan stoppe plottingen for 100 ?r
lines(0:109,b$Mu_hat,col="blue")
lines(0:109,bb$Mu_hat,col="darkgreen")
lines(0:109,cc$Mu_hat,col="magenta")
legend("topleft", legend=c("1920", "1950", "1980", "2010"),col=c("red", "blue", "darkgreen", "magenta"), pch="-",cex=1,lwd=2)




############ le code qui marche ############


Hm.cohort<- function(cohort = x){
  data = read.table("period.data.txt", sep=";",dec=".",header=TRUE) 
  data=data[,2:7]# fjerner den tomme kolonen
  data_subset_kohort <- subset(data, Cohort == cohort)
  data_subset_kohort$Age = as.numeric(paste(data_subset_kohort$Age))
  data_subset_kohort$Age[is.na(data_subset_kohort$Age)] = 110
  #dim(data_subset_kohort) # dim er 221
  female_data_subset_cohort = aggregate(Female~Age, data_subset_kohort, sum)
  #male_data_subset_cohort = aggregate(Male~Age+Cohort, data_subset_kohort, sum)
  #data_subset1_cohort = cbind( female_data_subset_cohort, male_data_subset_cohort[,3],female_data_subset_cohort[,3]+male_data_subset_cohort[,3])# [,3] for ? slipper ? ha de 3 forste
  #plot(data_subset_kohort$Year,data_subset_kohort$Age)# plotter ?r og alder for ? se hvordan det ser ut
  
  data2 = read.table("cohort.data.txt", sep=";",dec=".",header=TRUE) 
  data2_subset_kohort <- subset(data2, Cohort == cohort, select = c("Year","Age","Cohort","Female","Male","Total")) # vil bare har ?r 2014
  data2_subset_kohort$Age = as.numeric(paste(data2_subset_kohort$Age))
  data2_subset_kohort$Age[is.na(data2_subset_kohort$Age)] = 110
  #plot(data2_subset_kohort$Year,data2_subset_kohort$Age)
  female_data2_subset_cohort = aggregate(Female~Age, data2_subset_kohort, sum)# adderer to og to
  #male_data2_subset_cohort = aggregate(Male~Age+Cohort, data2_subset_kohort, sum)
  #data2_subset2_cohort = cbind(female_data2_subset_cohort, male_data2_subset_cohort[,3],female_data2_subset_cohort[,3]+male_data2_subset_cohort[,3])# [,3] for ? slipper ? ha de 3 forste
  mu2_hat = (female_data_subset_cohort/female_data2_subset_cohort)# d?de delt p? person years 
  #mu2_hat[1] # verifisere ved hente den f?rste: 1753/15618.67 = 0.1122375
  return(list(Mu_hat=mu2_hat, subset_death = female_data_subset_cohort, subset_exp = female_data2_subset_cohort))
  
}
c = Hm.cohort(cohort= 1880)
c_data_exp = c$subset_exp

d = Hm.cohort(cohort= 1890)
d_data_exp = d$subset_exp

e = Hm.cohort(cohort= 1900)
e_data_exp = e$subset_exp

f = Hm.cohort(cohort= 1905)
f_data_exp = f$subset_exp

plot(min(c_data_exp$Age):max(c_data_exp$Age),c$Mu_hat[,2],type="l",log="y",xlab= "Age", ylab="Mortality rate ") 

lines(min(d_data_exp$Age):max(d_data_exp$Age),d$Mu_hat[,2],log="y",col="red")

lines(min(e_data_exp$Age):max(e_data_exp$Age),e$Mu_hat[,2],log="y",col="yellow")

lines(min(f_data_exp$Age):max(f_data_exp$Age),f$Mu_hat[,2],log="y",col="darkgreen")

legend("topleft", legend=c("1900", "1910", "1920", "1930"),col=c("red", "magenta","darkgreen", "yellow"), pch="-",cex=1,lwd=2)
