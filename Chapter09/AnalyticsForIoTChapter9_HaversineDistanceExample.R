#code adapted from RosettaCode.org

#Coordinates for the two points
#Chicago, USA O'Hare airport (ORD)
Point1Lat = 41.978194
Point1Long = -87.907739
#Coordinates for Chhatrapati Shivaji International Airport near Mumbai, India airport (BOM)
Point2Lat = 19.0895595
Point2Long = 72.8656144

#convert decimal degrees to radians
degrees_to_rad <- function(deg) (deg * pi / 180)

# Volumetric mean radius is 6371 km for the Earth, see http://nssdc.gsfc.nasa.gov/planetary/factsheet/earthfact.html
# The diameter is thus 12742 km

#function to calculate great circle distance using haversine method
great_circle_distance <- function(lat1, long1, lat2, long2) {
  a <- sin(0.5 * (lat2 - lat1))
  b <- sin(0.5 * (long2 - long1))
  12742 * asin(sqrt(a * a + cos(lat1) * cos(lat2) * b * b))
}

#calculate distance for the two points
haversine_distance <- great_circle_distance(
  degrees_to_rad(Point1Lat), degrees_to_rad(Point1Long),   # Nashville International Airport (BNA)
  degrees_to_rad(Point2Lat), degrees_to_rad(Point2Long))  # Los Angeles International Airport (LAX)

#result shown in kilometers
haversine_distance
# 12,942.77km