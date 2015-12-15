#Author: Blair Gemmer
#CSCI 548 - Pattern Recognition
#Spring 2013
#Graduate Project - Edge Detection
#Date: 05/14/13
#Last edited: 05/17/13

#Clear variables:
rm(list=ls(all=TRUE))

#Import our functions:
source("gradFuncs.R")



library(biOps)
#x <- readJpeg(system.file("samples", "violet.jpg", package="biOps"))

#Text images:
x = readJpeg("Images/diffnormal.jpg")
name = "diffnormal"
runPlots(x, plotType=10, name=name)

x = readJpeg("Images/normaltext.jpg")
name = "normaltext"
runPlots(x, plotType=10, name=name)

x = readJpeg("Images/weirdtext.jpg")
name = "weirdtext"
runPlots(x, plotType=10, name=name)

x = readJpeg("Images/thankyou.jpg")
name = "thankyou"
runPlots(x, plotType=10, name=name)


#Fingerprints:
x = readJpeg("Images/patterns.jpg")
name = "patterns"
runPlots(x, plotType=10, name=name)

x = readJpeg("Images/bigfinger.jpg")
name = "bigfinger"
runPlots(x, plotType=10, name=name)

x = readJpeg("Images/darkhand.jpg")
name = "darkhand"
runPlots(x, plotType=10, name=name)

x = readJpeg("Images/handprint.jpg")
name = "handprint"
runPlots(x, plotType=10, name=name)


#Faces:
x = readJpeg("Images/young.jpg")
name = "young"
runPlots(x, plotType=10, name=name)

x = readJpeg("Images/old.jpg")
name = "old"
runPlots(x, plotType=10, name=name)

x = readJpeg("Images/smiling.jpg")
name = "smiling"
runPlots(x, plotType=10, name=name)


#PlotType 0=Original, 1=Sobel, 2=Canny, 3=LePlace, 4=LePlace with Gaussian, 5=LePlace with Gaussian threshold=100, 6=FreiChen, 7=Prewitt, 8=DifferenceEdge, 9=HomoegeneityEdge, 10=AllofTheAbove
#runPlots(x, plotType=10, name=name)

