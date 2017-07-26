#Load googleway package (assuming it is already installed)
library(googleway)

#Get the timezone offset information for a pair of latitude and longitude coordinates
tz <- google_timezone(location = c(41.882702,-87.619392), 
                       timestamp = as.POSIXct("2017-03-05"), 
                       key = "< Replace everything between these quotes with your API Key >")

#view results
View (tz)