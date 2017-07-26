#load Quandl library
library(Quandl)

#Grap US GDP time series data from FRED
gdp_ts = Quandl("FRED/GDP")

#Display
View(gdp_ts)