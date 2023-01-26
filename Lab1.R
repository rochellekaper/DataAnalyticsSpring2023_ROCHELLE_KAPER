library(readxl)
library(dplyr)

setwd("/Users/rochellekaper/Desktop/Data Analytics/Labs/Lab 1")
file.rename("2010EPI_data.xls", "EPI2010_data.xls")
EPI_data_2010 <- read_excel("EPI2010_data.xls", sheet = 4)
View(EPI_data_2010)
dim(EPI_data_2010)
#----------EPI column-----------
#filter out NA values
tf <- is.na(EPI_data_2010$EPI)
EPI_2010_filtered <- EPI_data_2010$EPI[!tf]

#### there are no null values for this sheet so using the original sheet version below

#Exercise 1: exploring the distribution for EPI column
summary(EPI_data_2010$EPI)
fivenum(EPI_data_2010$EPI, na.rm = TRUE)
stem(EPI_data_2010$EPI)
hist(EPI_data_2010$EPI)
hist(EPI_data_2010$EPI, seq(30., 95., 1.0), prob = TRUE)
lines(density(EPI_data_2010$EPI, na.rm = TRUE, bw=1.))
rug(EPI_data_2010$EPI)

#Exercise 1: fitting a distribution beyond histograms
plot(ecdf(EPI_data_2010$EPI), do.points=FALSE, verticals=TRUE) #cumulative density function

par(pty = "s")
qqnorm(EPI_data_2010$EPI); qqline(EPI_data_2010$EPI) #normal quantile-quantile plot

x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

#----------DALY column-----------
#filter out NA values
tf <- is.na(EPI_data_2010$DALY)
DALY_filtered <- EPI_data_2010$DALY[!tf]


#Exercise 1: exploring the distribution for DALY column
summary(DALY_filtered)
fivenum(DALY_filtered, na.rm = TRUE)
stem(DALY_filtered)
hist(DALY_filtered)
hist(DALY_filtered, seq(30., 95., 1.0), prob = TRUE)
lines(density(DALY_filtered, na.rm = TRUE, bw=1.))
rug(DALY_filtered)

#Exercise 1: fitting a distribution beyond histograms
plot(ecdf(DALY_filtered), do.points=FALSE, verticals=TRUE) #cumulative density function

par(pty = "s")
qqnorm(DALY_filtered); qqline(DALY_filtered) #normal quantile-quantile plot

x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

#----------WATER_H Column-----------
#filter out NA values
tf <- is.na(EPI_data_2010$WATER_H)
WATER_H_filtered <- EPI_data_2010$WATER_H[!tf]


#Exercise 1: exploring the distribution for WATER_H column
summary(WATER_H_filtered)
fivenum(WATER_H_filtered, na.rm = TRUE)
stem(WATER_H_filtered)
hist(WATER_H_filtered)
hist(WATER_H_filtered, seq(30., 95., 1.0), prob = TRUE)
lines(density(WATER_H_filtered, na.rm = TRUE, bw=1.))
rug(WATER_H_filtered)

#Exercise 1: fitting a distribution beyond histograms
plot(ecdf(WATER_H_filtered), do.points=FALSE, verticals=TRUE) #cumulative density function

par(pty = "s")
qqnorm(WATER_H_filtered); qqline(WATER_H_filtered) #normal quantile-quantile plot

x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)


#Comparing distributions
boxplot(EPI_data_2010$EPI, EPI_data_2010$DALY)
qqplot(EPI_data_2010$EPI, EPI_data_2010$DALY)

### Exercise 2: filtering ###
#conditional filtering
EPILand <- EPI_data_2010$Landlock[!0]
Eland <- EPILand[!is.na(EPILand)]
hist(EPILand, seq(30., 95., 1.0), prob = TRUE) #not working

#filter on EPI Regions
South_Asia_and_Europe <- EPI_data_2010 %>% filter(EPI_regions == 'Europe' & EPI_regions == 'South Asia') 

#filter on GEO_subregion
EPI_data_2010$GEO_subregion
Carribean_and_South_Africa <- EPI_data_2010 %>% filter(GEO_subregion == 'Southern Africa' & GEO_subregion == 'Caribbean') 


#Exercise 1: No_surface_water
#filter out NA values
tf <- is.na(EPI_data_2010$No_surface_water)
No_surface_water_filtered <- EPI_data_2010$No_surface_water[!tf]

summary(No_surface_water_filtered)
fivenum(No_surface_water_filtered, na.rm = TRUE)
stem(No_surface_water_filtered)
hist(No_surface_water_filtered)
hist(No_surface_water_filtered, seq(30., 95., 1.0), prob = TRUE)
lines(density(No_surface_water_filtered, na.rm = TRUE, bw=1.))
rug(No_surface_water_filtered)

#Exercise 1: fitting a distribution beyond histograms
plot(ecdf(No_surface_water_filtered), do.points=FALSE, verticals=TRUE) #cumulative density function

par(pty = "s")
qqnorm(No_surface_water_filtered); qqline(No_surface_water_filtered) #normal quantile-quantile plot

x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

#Exercise 1: Desert
#filter out NA values
tf <- is.na(EPI_data_2010$Desert)
Desert_filtered <- EPI_data_2010$Desert[!tf]

summary(Desert_filtered)
fivenum(Desert_filtered, na.rm = TRUE)
stem(Desert_filtered)
hist(Desert_filtered)
hist(Desert_filtered, seq(30., 95., 1.0), prob = TRUE)
lines(density(Desert_filtered, na.rm = TRUE, bw=1.))
rug(Desert_filtered)

#Exercise 1: fitting a distribution beyond histograms
plot(ecdf(Desert_filtered), do.points=FALSE, verticals=TRUE) #cumulative density function

par(pty = "s")
qqnorm(Desert_filtered); qqline(Desert_filtered) #normal quantile-quantile plot

x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

#Exercise 1: High_Population_Density
#filter out NA values
tf <- is.na(EPI_data_2010$High_Population_Density)
High_Population_Density_filtered <- EPI_data_2010$High_Population_Density[!tf]

summary(High_Population_Density_filtered)
fivenum(High_Population_Density_filtered, na.rm = TRUE)
stem(High_Population_Density_filtered)
hist(High_Population_Density_filtered)
hist(High_Population_Density_filtered, seq(30., 95., 1.0), prob = TRUE)
lines(density(High_Population_Density_filtered, na.rm = TRUE, bw=1.))
rug(High_Population_Density_filtered)

#Exercise 1: fitting a distribution beyond histograms
plot(ecdf(High_Population_Density_filtered), do.points=FALSE, verticals=TRUE) #cumulative density function

par(pty = "s")
qqnorm(High_Population_Density_filtered); qqline(High_Population_Density_filtered) #normal quantile-quantile plot

x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

