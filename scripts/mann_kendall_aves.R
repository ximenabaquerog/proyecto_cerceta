################################ ANALISIS MANN-KENDALL TENDENCIAS DE AVES #################################

#https://www.geeksforgeeks.org/r-machine-learning/how-to-perform-a-mann-kendall-trend-test-in-r/

#Para llevarloa cabo necesito una tabla con una columande anos y otra columna de datos de aves (abunda y otra columna con riqueza), seran 2 mann kendall

#Instalo y cargo paquetes necesarios para Mann-Kendall
install.packages("Kendall")
library(Kendall)

#Cargo variables del Mann-Kendall
tend_riqueza <- read.csv("")

tend_abundancia <- read.csv("")

#Llevo a cabo los tests
MannKendall(x)

#Visualizamos
#Plot the time series data
plot(x)
#Add a smooth line to visualize the trend 
lines(lowess(time(x),x), col='blue')


