install.packages("LexisPlotR")
library(LexisPlotR)
# Plot a Lexis grid from year 1900 to year 1905, representing the ages from 0 to 5
lexis.grid(year.start = 1990, year.end = 1995, age.start = 0, age.end = 5)
mylexis <- mylexis + theme(axis.title = element_text(face = "bold", colour = "red"))
mylexis <- lexis.grid(year.start = 1990, year.end = 1995, age.start = 0, age.end = 5)
mylexis

# Highlight all points that belong to the age of 2 and
# Change the fill colour to "red" and make the layer nearly non-transparent

lexis.age(lg = mylexis, age = 2, fill = "yellow", alpha = 0.4)
mylexis <- mylexis + theme(axis.title = element_text(face = "bold", colour = "red"))
mylexis
# Add a title
#mylexis <- mylexis + labs(title = "Lexis, highlight of age 2")
#mylexis
# Highlight the year 1992
lexis.year(lg = mylexis, year = 1992,alpha = 0.4)
# Add a title
mylexis <- mylexis + labs(title = "LexisPlotR")
mylexis
# Highlight the cohort 1990
lexis.cohort(lg = mylexis, cohort = 1990,alpha = 0.4)

mylexis <- lexis.grid(year.start = 1990, year.end = 1995, age.start = 0, age.end = 5)
mylexis <- mylexis + theme(axis.title = element_text(face = "bold", colour = "red"))
mylexis <- lexis.age(lg = mylexis, age = 2)
mylexis <- lexis.year(lg = mylexis, year = 1992)
mylexis <- lexis.cohort(lg = mylexis, cohort = 1990)
mylexis
