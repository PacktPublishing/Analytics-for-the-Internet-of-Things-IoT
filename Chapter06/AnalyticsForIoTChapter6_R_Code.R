library(readr)
#load 15 min precipitation file.  Change the file location if needed.
NOAA15minPrecipColorado <- read_csv("~/Downloads/NOAA15minPrecipColorado.csv")

#view the dataset in an R studio window
View(NOAA15minPrecipColorado)

#view summary statistics
summary(NOAA15minPrecipColorado)

#install dplyr package if needed
if(!require(dplyr)){
  install.packages("dplyr")  
}

library(dplyr)

#filter records to remove extreme values
NOAAfiltered <- filter(NOAA15minPrecipColorado, QPCP > 0, QPCP <900, QGAG >0, QGAG<900)
#show statistical summary
summary(NOAAfiltered)

