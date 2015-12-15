#Author: Blair Gemmer
#CSCI 548 - Pattern Recognition
#Spring 2013
#Project 5 - Feature Selection
#Date: 03/29/13

#Clear variables:
rm(list=ls(all=TRUE))

#Include the functions we use in this script:
source("myFunctions.R")

#Iris Data:
irisFunc = function(){
  #Iris Data:
  irisData = iris
  classes = irisData$Species
  irisData$Species = NULL
  
  #Set threshold and percent of sample:
  threshold = 20
  percent = .75
  
  #Plot, accuracy, and J-scores:
  doStuff(irisData, classes, threshold, percent, "Iris Data")
}

#Fruit data:
fruitFunc = function(){
  fruitData = read.csv("fruit.csv")
  classes = fruitData[,5]
  fruitData[,5] = NULL
  
  #Set threshold and percent of sample:
  threshold = 20
  percent = .75
  
  #Finally plot, accuracy, and J-scores:
  doStuff(fruitData, classes, threshold, percent, "Fruit Data")
}

#Tumor data:
cancerFunc = function(){
  #Read in the cancer data:
  cancerData = read.table("cancer.data", sep=",", header=TRUE, na.strings="?")
  
  #Clear up the NAs:
  for(i in 1:length(cancerData)){
    nonNAs = which(!is.na(cancerData[,i]))
    cancerData[-nonNAs,i]=mean(cancerData[nonNAs,i])
  }
  
  #Now, remove that 1st column, so we can do dist() later:
  cancerData = cancerData[,-1]
  
  #Save the class names:
  classes = cancerData[,10]
  
  #And shave off the end as well
  cancerData[,10] = NULL
  
  #Do some regular expressions to pull out the benign vs. malignant tumors:
  benign = grep("2", classes)
  malignant = grep("4", classes)
  
  #Create a name vector using the benign or malignant class:
  myClasses = as.vector(classes)
  myClasses[benign] = "benign"
  myClasses[malignant] = "malignant"
  
  #Set the threshold and percent of sample:
  threshold = 20
  percent = .75
  
  #Finally, plot, accuracy, and J-scores:
  doStuff(cancerData, myClasses, threshold, percent, "Tumor Data")
}

#Read in the mouse data:
mouseFunc = function(){
  mouseData = read.table("mouse.txt", sep="\t", header=TRUE)
  
  #Now, remove that 1st column, so we can do dist() later:
  mouseData = mouseData[,-1]
  
  #Remove the taxon information:
  mouseData$taxon = NULL
  
  #Transpose the mouseData:
  mouseTransposed <- data.frame(t(mouseData))
  
  #Get experiment names:
  expNames <- row.names(mouseTransposed)
  
  #Class names are the experiment names:
  classes = expNames
  
  #Do some regular expressions to pull out the proximal and distal experiments:
  proximal = grep("[A-Z]+P[0-9]", expNames)
  distal = grep("[A-Z]+D[0-9]", expNames)
  
  #Create a color vector using the proximal or distal class::
  myColors = as.vector(classes)
  myColors[proximal] = "red"
  myColors[distal] = "green"
  
  #Set up a classes vector:
  myClasses = as.vector(classes)
  myClasses[proximal] = "proximal"
  myClasses[distal] = "distal"
  
  #Set threshold:
  threshold = 1.78
  
  #Finally, plot and get accuracy:
  mouseStuff(mouseTransposed, myClasses, threshold)
}


#Sperm Data:
spermFunc = function(){
  #Read in the sperm data:
  spermData = read.table("sperm.txt", sep=",", header=TRUE)
  
  #Class names are N:
  classes = spermData$N
  #Remove the class information:
  spermData$N = NULL
  
  #Do some regular expressions to pull out the proximal and distal experiments:
  normal = grep("N", classes)
  altered = grep("O", classes)
  
  #Set up a classes vector:
  myClasses = as.vector(classes)
  myClasses[normal] = "normal"
  myClasses[altered] = "altered"
  
  #Set threshold and percentage:
  threshold = 1
  percent = .75
  
  #Finally, plot and get accuracy:
  doStuff(spermData, myClasses, threshold, percent, "Sperm Data")
}
	  
#If you're having problems displaying plots all at once, try using the functions one at a time
#It also helps clean up some of the console output:
irisFunc()
fruitFunc()
cancerFunc()
mouseFunc()
spermFunc()