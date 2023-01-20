setwd("/Users/rochellekaper/Desktop/Data Analytics/Labs/Lab 1")
file.rename("2010EPI_data.xls", "EPI2010_data.xls")
EPI_data <- read_excel("EPI2010_data.xls", sheet = 4)

dim(EPI_data)
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

#### there are no null values for this sheet so using the original sheet version below

#Exercise 1: exploring the distribution for EPI column
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



