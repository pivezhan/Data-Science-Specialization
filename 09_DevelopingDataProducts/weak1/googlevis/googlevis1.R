suppressPackageStartupMessages( library (googleVis))
# M <- gvisMotionChart(Fruits,  "Fruit",  "Year", options = list(width =  600, height =  400 ))
# print(M,"chart")

G <- gvisGeoChart(Exports, locationvar="Country" ,colorvar="Profit", options= list(width=height=400))
print(G,"chart")