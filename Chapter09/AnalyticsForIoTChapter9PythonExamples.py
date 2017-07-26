# -*- coding: utf-8 -*-
"""
Sample code for Analytics for IoT
Chapter 9
"""

"""
MBR Example
"""
#Import linestring class
from shapely.geometry import LineString

#create a linestring from a series of points
MBRline = LineString([(1, 0), (1, 1),(3,5),(2,2)])

#determine MBR bounding box
MBRboundingBox = MBRline.bounds

#output results
MBRboundingBox

#(1.0, 0.0, 3.0, 5.0)


"""
Contains example
"""
#import polygon class
from shapely.geometry import Polygon, Point

#create a square polygon
polysquare = Polygon([(0,0),(0,2),(2,2),(2,0),(0,0)])

#test if polygon contains the point
print(polysquare.contains(Point(1,1)))

#test if point is within the polygon
print(Point(1,1).within(polysquare))

#test if other point is within the polygon
print(Point(5,7).within(polysquare))

"""
Buffer example
"""
#import linestring class, cap and join styles
from shapely.geometry import LineString, CAP_STYLE, JOIN_STYLE

unbufferedLine = LineString([(0, 0), (1, 1), (0, 2), (2, 2), (3, 1), (1, 0)])
dilatedBufferedLine = unbufferedLine.buffer(0.5, cap_style = CAP_STYLE.square)
erodedBufferedLine = dilatedBufferedLine.buffer(-0.3, join_style = JOIN_STYLE.round)

#Show the polygon detail.  This gets more complicated than a line!
print(erodedBufferedLine)

"""
Simplify example
"""
#print area of polygon
print(erodedBufferedLine.area)
#show number of coordinates needed to define exterior of the polygon
print(len(erodedBufferedLine.exterior.coords))

#simplify
erodedSimplified = erodedBufferedLine.simplify(0.05, preserve_topology=False)
print(erodedSimplified.area) #note minimal change in area
print(len(erodedSimplified.exterior.coords)) #big reduction in coordinates
print(erodedSimplified)

