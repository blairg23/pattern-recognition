#Author: Blair Gemmer
#CSCI 548 - Pattern Recognition
#Spring 2013
#Graduate Project - Edge Detection
#Date: 05/14/13
#Last edited: 05/17/13

#Plots the original image and saves as a jpg:
plotOriginal = function(x, name="defaultImage"){
  par(mar=c(0,0,0,0))
  name=paste("FinalPlots/", name, ".png")
  png(name, width=ncol(x), height=nrow(x))
  plot(x)
  dev.off()
}

#Plots an image x using the Sobel algorithm:
plotSobel = function(x, name="defaultImage"){  
  #Sobel algorithm plot:
  #plot(imgSobel(x)) #Don't need this, but might be nice to check
  
  #Sobel inverted:
  par(mar=c(0,0,0,0))
  name=paste("FinalPlots/", name, "Sobel.png")
  png(name, width=ncol(x), height=nrow(x))
  plot(imgNegative(imgSobel(x)))
  dev.off()
}

#Plots an image x using the Canny algorithm:
plotCanny = function(x, name="defaultImage"){
  #Canny algorithm:
  par(mar=c(0,0,0,0))
  name=paste("FinalPlots/", name, "Canny.png")
  png(name, width=ncol(x), height=nrow(x))
  plot(imgCanny(x,1.4,1,255))
  dev.off()
}

#Returns a mask of size passedSize:
getLPMask = function(size=5){
  #Apply the laplacian mask
  laplacianMask = matrix(-1,size,size)
  
  #Find the middle row/col:
  center = ceiling(size/2)
  #Create a value for it:
  laplacianMask[center,center] = size*size - 1
  
  #Finally, return the mask:
  return(laplacianMask)
}

#Returns the sum of the mask multiplied by a portion of the image
getMaskedPortion = function(portionOfImage, mask){
  myTempMatrix = mask*portionOfImage
  return(sum(myTempMatrix))
}

#Returns the entire image, after masking:
getMaskedImage = function(imageData, mask, max=5, middle=3, threshold=0){ #Change max and middle if you are using a different sized matrix than 5x5
  #Get the height and width (in case not a square image):
  height = nrow(imageData)
  width = ncol(imageData)
  
  #Convert the image to a matrix, make byrow=FALSE so that the image is right-side up:
  imageDataMatrix = matrix(imageData, nrow=height, ncol=width, byrow=FALSE) 
  
  #Create a copy of the image data so we can change the pixels without changing our image:
  copyMatrix = imageDataMatrix
  
  distance = max - middle #This will get our distance between the middle and max/min (doesn't matter which one we use)
  
  #Set the top edge to white:
  for(i in 1:distance){
    for(j in 1:width){
      copyMatrix[i,j] = 255
    }
  }
  #Set the right edge to white:
  for(i in 1:height){
    for(j in (width-distance):width){
      copyMatrix[i,j] = 255
    }
  }
  #Set the bottom edge to white:
  for(i in (height-distance):height){
    for(j in 1:width){
      copyMatrix[i,j] = 255
    }
  }
  #Set the left edge to white:
  for(i in 1:height){
    for(j in 1:distance){
      copyMatrix[i,j] = 255
    }
  }
  
  #For each pixel in the image data:
  for(i in middle:(height-distance)){
    for(j in middle:(width-distance)){
      #Set the portion of the image to be the size of our matrix, where the middle is our (i,j) point in the image:
      portionOfImage = imageDataMatrix[(i-distance):(i+distance), (j-distance):(j+distance)]
      maskedPortion = getMaskedPortion(portionOfImage, mask) #Get the sum of the product of the mask and the image portion
      
      #NORMALIZATION:
      #Set areas that don't meet our thresholds to white or black:
      if(maskedPortion < threshold){
        copyMatrix[i,j] = 0
      }
      else if(maskedPortion > 255){
        copyMatrix[i,j] = 255
      }
      else{
        copyMatrix[i,j] = maskedPortion
      }
      copyMatrix[i,j] = 255 - copyMatrix[i,j]
    }
  }
  copyMatrix = imagedata(copyMatrix) #Convert the copy back to an imagedata object
  return(copyMatrix) #Finally, return the copy
}

#Returns the gaussian weights of a given radius r and sigma:
getGaussWeights = function(r,sigma){
  return(exp( -(r**2) / (2 * sigma**2) ))
}

#Returns the distance between two points (x1,x2) and (y1,y2):
getDistance = function(x1,y1,x2,y2){
  return(sqrt((x2-x1)^2 + (y2-y1)^2))
}

#Returns a distance matrix, given a matrix and a middle point:
getDistMatrix = function(size=5, middle=3){
  #Create our distance matrix
  m = matrix(0, ncol=size, nrow=size)
  for(i in 1:size){
    for(j in 1:size){
      m[i,j] = getDistance(i,j,middle,middle) #Gets the distance from the midpoint of the matrix
    }
  }
  #Create a gaussian weighted matrix from the distance matrix:
  GWs = round(15*getGaussWeights(m,1.4))
  
  GWs = getNormalMatrix(GWs) #Normalize the matrix based on the sum of its parts
  return(GWs)
}


#Returns a matrix normalized by the sum of its parts:
getNormalMatrix = function(m){
  return(m/sum(m))
}


#Runs Sobel, Canny, Leplacian, and Gaussian algorithms to compare and then plot:
#PlotType 1=Sobel, 2=Canny, 3=LePlace, 4=LePlace with Gaussian, 5=LePlace with Gaussian threshold=100, 6=FreiChen, 7=Prewitt, 8=DifferenceEdge, 9=HomoegeneityEdge, 10=AllofTheAbove
runPlots = function(x, passedSize=5, threshold=0, bias=2, plotType=10,name="defaultImage"){ 
  if(plotType == 0){
    plotOriginal(x, name) #Plot the original image
  }
  else if(plotType == 1){
    plotSobel(x, name) #Plot using Sobel algorithm
  }
  else if(plotType == 2){
    plotCanny(x, name) #Plot using Canny algorithm  
  }
  else if(plotType == 3){
    passedSize = passedSize #The size of our mask
    
    leplaceMask = getLPMask(passedSize) #Our LaPlacian mask
    
    m = getMaskedImage(x, leplaceMask, threshold=0) #Run through BLOBs with LePlacian mask
    
    #Save as a png:
    name=paste("FinalPlots/", name, "LePlace.png")
    png(name, width=ncol(x), height=nrow(x))
    plot(m) #Finally, plot.
    dev.off()
  }
  else if(plotType == 4){
    passedSize = passedSize #The size of our mask
    
    leplaceMask = getLPMask(passedSize) #Our LaPlacian mask
  
    gaussMask = getDistMatrix(passedSize) #Our Gaussian mask
    
    m = getMaskedImage(x, gaussMask, threshold=0) #Run through BLOBs with Gaussian mask
    
    #m = imgNegative(m) #Have to do this to get a better picture when running through LePlace
    
    n = getMaskedImage(m, leplaceMask, threshold=0) #Run through BLOBs with LePlacian mask
    
    #Save as a png:
    name=paste("FinalPlots/", name, "LePlaceGauss.png")
    png(name, width=ncol(x), height=nrow(x))
    plot(n) #Finally, plot.
    dev.off()
  }
  else if (plotType == 5){
    passedSize = passedSize #The size of our mask
    
    leplaceMask = getLPMask(passedSize) #Our LaPlacian mask
    
    gaussMask = getDistMatrix(passedSize) #Our Gaussian mask
    
    m = getMaskedImage(x, gaussMask, threshold=0) #Run through BLOBs with Gaussian mask
    
    #m = imgNegative(m) #Have to do this to get a better picture when running through LePlace
    
    threshold = 100 #Set the threshold to 100 and plot again:
    
    n = getMaskedImage(m, leplaceMask, threshold=threshold) #Run through BLOBs with LePlacian mask and threshold=100
    
    #Save as a png:
    name=paste("FinalPlots/", name, "LePlaceGaussThreshold.png")
    png(name, width=ncol(x), height=nrow(x))
    plot(n) #Finally, plot
    dev.off()
  }
  else if(plotType == 6){
    plotFreiChen(x, name)
  }  
  else if(plotType == 7){
    plotPrewitt(x, name)
  }
  else if(plotType == 8){
    plotDiffEdge(x,bias, name)
  }
  else if(plotType == 9){
    plotHomoEdge(x,bias, name)
  }
  else if(plotType == 10){
    runPlots(x, passedSize=5, threshold=threshold, bias=bias, plotType=0, name=name) 
    runPlots(x, passedSize=5, threshold=threshold, bias=bias, plotType=1, name=name) 
    runPlots(x, passedSize=5, threshold=threshold, bias=bias, plotType=2, name=name)
    runPlots(x, passedSize=5, threshold=threshold, bias=bias, plotType=3, name=name)
    runPlots(x, passedSize=5, threshold=threshold, bias=bias, plotType=4, name=name)
    runPlots(x, passedSize=5, threshold=threshold, bias=bias, plotType=5, name=name)
    runPlots(x, passedSize=5, threshold=threshold, bias=bias, plotType=6, name=name)
    runPlots(x, passedSize=5, threshold=threshold, bias=bias, plotType=7, name=name)
    runPlots(x, passedSize=5, threshold=threshold, bias=bias, plotType=8, name=name)
    runPlots(x, passedSize=5, threshold=threshold, bias=bias, plotType=9, name=name)
  }
}

#Plots using the Frei-Chen edge detection algorithm:
plotFreiChen = function(x,name="defaultImage"){
  y = imgFreiChen(x) #Use Frei-Chen algorithm to create y-value
  y = imgNegative(y) #Inverse y-value to create edges
  #Save as a png:
  name=paste("FinalPlots/", name, "FreiChen.png")
  png(name, width=ncol(x), height=nrow(x))
  plot(y) #Finally, plot
  dev.off()
}

#Plots using the Prewitt edge detection algorithm:
plotPrewitt = function(x,name="defaultImage"){
  y = imgPrewitt(x) #Use Prewitt algorithm to create y-value
  y = imgNegative(y) #Inverse y-value to create edges
  #Save as a png:
  name=paste("FinalPlots/", name, "Prewitt.png")
  png(name, width=ncol(x), height=nrow(x))
  plot(y) #Finally, plot
  dev.off()
}

#Plots using the Difference Edge Detection algorithm (lower bias = lighter background of image):
plotDiffEdge = function(x, bias=32,name="defaultImage"){
  y = imgDifferenceEdgeDetection(x, bias=bias) #Use difference edge detection algorithm to create y-value
  y = imgNegative(y) #Inverse y-value to create edges
  #Save as a png:
  name=paste("FinalPlots/", name, "DiffEdge.png")
  png(name, width=ncol(x), height=nrow(x))
  plot(y) #Finally, plot
  dev.off()
}

#Plots using the Homogeneity Edge Detection algorithm (lower bias = lighter background of image):
plotHomoEdge = function(x, bias=32,name="defaultImage"){
  y = imgHomogeneityEdgeDetection(x, bias=bias) #Use homogeneity edge detection algorithm to create y-value
  y = imgNegative(y) #Inverse y-value to create edges
  #Save as a png:
  name=paste("FinalPlots/", name, "HomoEdge.png")
  png(name, width=ncol(x), height=nrow(x))
  plot(y) #Finally, plot
  dev.off()
}
