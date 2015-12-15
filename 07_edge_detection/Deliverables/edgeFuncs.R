#Author: Blair Gemmer
#CSCI 548 - Pattern Recognition
#Spring 2013
#Project 7 - Edge Detection
#Date: 05/06/13
#Last edited: 05/08/13


#Plots an image x using the Sobel algorithm:
plotSobel = function(x){  
  #Sobel algorithm plot:
  #plot(imgSobel(x)) #Don't need this, but might be nice to check
  
  #Sobel inverted:
  par(mar=c(0,0,0,0))
  plot(imgNegative(imgSobel(x)))
}

#Plots an image x using the Canny algorithm:
plotCanny = function(x){
  #Canny algorithm:
  par(mar=c(0,0,0,0))
  plot(imgCanny(x,1.4,1,255))
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

  gaussMask = getDistMatrix() #Get a gaussian mask to use
  
  #Set the top edge to white:
  for(i in 1:width){
    for(j in 1:distance){
      copyMatrix[i,j] = 255
    }
  }
  #Set the right edge to white:
  for(i in (width-distance):width){
    for(j in 1:height){
      copyMatrix[i,j] = 255
    }
  }
  #Set the bottom edge to white:
  for(i in 1:width){
    for(j in (height-distance):height){
      copyMatrix[i,j] = 255
    }
  }
  #Set the left edge to white:
  for(i in 1:distance){
    for(j in 1:height){
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