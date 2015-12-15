#Author: Blair Gemmer
#CSCI 548 - Pattern Recognition
#Spring 2013
#Project 7 - Edge Detection
#Date: 05/06/13
#Last edited: 05/08/13

#Clear variables:
rm(list=ls(all=TRUE))

#Import our functions:
source("edgeFuncs.R")



#Part 1, Sobel/Canny:
library(biOps)
x <- readJpeg(system.file("samples", "violet.jpg", package="biOps"))

plotSobel(x)

plotCanny(x)



#Part 2, LePlacian Edge Detection:
lennaRGB <- readJpeg("Lenna.jpg")
par(mar=c(0,0,0,0),mgp = c(1.5, .5, 0))
#plot(lennaRGB) #No need to plot this, except if you want to see original image

passedSize = 5 #Set the size of the matrix

laplacianMask = getLPMask(passedSize) #Get Laplacian mask of size passedSize

lennaGrey = imgRGB2Grey(lennaRGB)

m = getMaskedImage(lennaGrey, laplacianMask, threshold=0) #Regular Leplacian

plot(m)



#Part 3, Gaussian Edge Detection:
gaussianMask = getDistMatrix() #Create a Gaussian mask

m = getMaskedImage(lennaGrey, gaussianMask) #Smoothed with Gaussian mask
m = getMaskedImage(m, laplacianMask)
plot(m)

m = getMaskedImage(lennaGrey, gaussianMask, threshold=100) #Smoothed + higher threshold
m = getMaskedImage(m, laplacianMask)
plot(m)

plotCanny(lennaGrey) #Finally, Canny algorithm on Lenna

