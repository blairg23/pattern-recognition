#Author: Blair Gemmer
#CSCI 548 - Pattern Recognition
#Spring 2013
#Project 4 - LDA
#Date: 03/14/13

rm(list=ls(all=TRUE))
###########################################################
#Create a vector of the color data:
classes = iris[,5]
myColors = as.vector(classes)
myColors[which(classes == "setosa")] = "red"
myColors[which(classes == "versicolor")] = "green"
myColors[which(classes == "virginica")] = "blue"

#Create a new window:
X11(height=2.2,width=6.6)
par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))
layout(matrix(1:3, 1, 3), widths=c(1, 1, 1)  ); 
layout.show(3)

#Iris LDA:
mydata = iris
classes = mydata$Species
mydata$Species <- NULL #This has to be set to NULL for the script to work

library(MASS)
train <- sample(1:150, 125) #not 75
z <- lda(mydata[train,], classes[train])

#actual classes:
actual <- classes[-train]

#Predicted classes:
predicted <- predict(z, mydata[-train, ])$class

#Test accuracy by dividing the length of predicted by actual:
accuracy <- ( length(actual) - length(which(actual != predicted)) ) / length(actual)
print(paste("Accuracy: ", accuracy*100, "%"))


#Plot command (LDA):
projectionOntoFirst = as.matrix(mydata) %*% z$scaling[,1]
projectionOntoSec = as.matrix(mydata) %*% z$scaling[,2]
plot(projectionOntoFirst, projectionOntoSec,pch = 20, 
  cex = .5,col = myColors,
  xlab = "Linear Discriminant 1",
  ylab = "Linear Discriminant 2")

mtext("(A)",line = -2)


#Plot Iris MDS:
points = cmdscale(dist(mydata))
plot(points,col=myColors, cex=.5,
  xlab = "First Principal Component",
  ylab = "Second Principal Component",pch = 20)

mtext("(B)", line=-2)


#Plot Iris PCA:
pcdat = prcomp(iris[,1:4])

#Plot using the new color vector:
plot(pcdat$x[,1], pcdat$x[,2],pch = 20, cex = .5,col = myColors,
  xlab = "First Principal Component",
  ylab = "Second Principal Component")

mtext("(C)", line=-2)


###########################################################
#Read in the fruit data:
fruitData = read.csv("fruit.csv")

#Create a color vector:
classes = fruitData[,5]
myColors = as.vector(classes)
myColors[which(classes == "apple")] = "red"
myColors[which(classes == "orange")] = "orange"
myColors[which(classes == "lemon")] = "yellow"
myColors[which(classes == "peach")] = "burlywood1"

#Create a new window:
X11(height=2.2,width=6.6)
par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))
layout(matrix(1:3, 1, 3), widths=c(1, 1, 1)  ); 
layout.show(3)


#Fruit LDA:
mydata = fruitData
classes = mydata$Class
mydata$Class <- NULL #This has to be set to NULL for the script to work

library(MASS)
train <- sample(1:150, 125) #not 75
z <- lda(mydata[train,], classes[train])

#actual classes:
actual <- classes[-train]

#Predicted classes:
predicted <- predict(z, mydata[-train, ])$class

#Test accuracy by dividing the length of predicted by actual:
accuracy <- ( length(actual) - length(which(actual != predicted)) ) / length(actual)
print(paste("Accuracy: ", accuracy*100, "%"))


#Plot command (LDA):
projectionOntoFirst = as.matrix(mydata) %*% z$scaling[,1]
projectionOntoSec = as.matrix(mydata) %*% z$scaling[,2]
plot(projectionOntoFirst, projectionOntoSec,pch = 20, 
  cex = .5,col = myColors,
  xlab = "Linear Discriminant 1",
  ylab = "Linear Discriminant 2")

mtext("(A)",line = -2)


#Plot Fruit MDS:
points = cmdscale(dist(fruitData))
plot(points,col=myColors, cex=.5,
  xlab = "First Principal Component",
  ylab = "Second Principal Component",pch = 20)

mtext("(B)", line=-2)


#Plot Fruit PCA:
pcdat = prcomp(fruitData[,1:4])

#Plot using the new color vector:
plot(pcdat$x[,1], pcdat$x[,2],pch = 20, cex = .5,col = myColors,
  xlab = "First Principal Component",
  ylab = "Second Principal Component")

mtext("(C)", line=-2)


###########################################################
#Read in the mouse data:
mouseData = read.table("mouse.txt", sep="\t", header=TRUE)


#Now, remove that 1st column, so we can do dist() later:
mouseData = mouseData[,-1]

#Set your row names:
#row.names(mouseData) <- mouseData[,1]

#Get row names:
#rowNames = row.names(mouseData)


#Transpose the mouseData:
mouseTransposed <- data.frame(t(mouseData))

#Get experiment names:
expNames <- row.names(mouseTransposed)

#Create row names for transposed mouse data:
#row.names(mouseTransposed) <- expNames

#Do some regular expressions to pull out the proximal and distal experiments:
proximal = grep("[A-Z]+P[0-9]", expNames)
distal = grep("[A-Z]+D[0-9]", expNames)


#Create a color vector using the proximal or distal class::
classes = expNames
myColors = as.vector(classes)
myColors[proximal] = "red"
myColors[distal] = "green"

#Set up a classes vector:
myClasses = as.vector(classes)
myClasses[proximal] = "proximal"
myClasses[distal] = "distal"


#Create a new window:
X11(height=2.2,width=6.6)
par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))
layout(matrix(1:3, 1, 3), widths=c(1, 1, 1)  ); 
layout.show(3)


#Mouse LDA:
mydata = mouseTransposed
#classes = mydata$Class
#mydata$Class <- NULL #This has to be set to NULL for the script to work

library(MASS)
train <- sample(1:length(myClasses), length(myClasses)) #not 75
z <- lda(mydata[train, ], myClasses[train], tol=0)

#actual classes:
actual <- myClasses[-train]

#Predicted classes:
predicted <- predict(z, mydata[-train, ])$class

#Test accuracy by dividing the length of predicted by actual:
accuracy <- ( length(actual) - length(which(actual != predicted)) ) / length(actual)
print(paste("Accuracy: ", accuracy*100, "%"))


#Plot command (LDA):
projectionOntoFirst = as.matrix(mydata) %*% z$scaling[,1]
projectionOntoSec = as.matrix(mydata) %*% z$scaling[,2]
plot(projectionOntoFirst, projectionOntoSec,pch = 20, 
  cex = .5,col = myColors,
  xlab = "Linear Discriminant 1",
  ylab = "Linear Discriminant 2")

mtext("(A)",line = -2)


#Plot Mouse MDS:
points = cmdscale(dist(mouseTransposed))
plot(points,col=myColors, cex = .5,
  xlab = "First Principal Component",
  ylab = "Second Principal Component",pch = 20)

mtext("(B)", line=-2)


#Plot Mouse PCA:
pcdat = prcomp(mouseTransposed[,1:4])

#Plot using the new color vector:
plot(pcdat$x[,1], pcdat$x[,2],pch = 20, cex = .5,col = myColors,
  xlab = "First Principal Component",
  ylab = "Second Principal Component")

mtext("(C)", line=-2)



###########################################################
#Read in the cancer data:
cancerData = read.table("cancer.data", sep=",", header=TRUE, na.strings="?")

for(i in 1:length(cancerData)){
    nonNAs = which(!is.na(cancerData[,i]))
    cancerData[-nonNAs,i]=mean(cancerData[nonNAs,i])
}


#Now, remove that 1st column, so we can do dist() later:
cancerData = cancerData[,-1]

#Save the class names:
classNames = cancerData[,10]

#And shave off the end as well
cancerData[,10] = NULL

#Do some regular expressions to pull out the benign vs. malignant tumors:
benign = grep("2", classNames)
malignant = grep("4", classNames)

#Create a color vector using the benign or malignant class:
classes = classNames
myColors = as.vector(classes)
myColors[benign] = "red"
myColors[malignant] = "blue"

#Set up a classes vector:
myClasses = as.vector(classes)
myClasses[benign] = "benign"
myClasses[malignant] = "malignant"




#Create a new window:
X11(height=2.2,width=6.6)
par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))
layout(matrix(1:3, 1, 3), widths=c(1, 1, 1)  ); 
layout.show(3)


#Tumor LDA:
mydata = cancerData
#classes = mydata$Class
#mydata$Class <- NULL #This has to be set to NULL for the script to work

library(MASS)
train <- sample(1:length(myClasses), length(myClasses)*.1) #not 75
z <- lda(mydata[train, ], myClasses[train], tol=0)

#actual classes:
actual <- myClasses[-train]

#Predicted classes:
predicted <- predict(z, mydata[-train, ])$class

#Test accuracy by dividing the length of predicted by actual:
accuracy <- ( length(actual) - length(which(actual != predicted)) ) / length(actual)
print(paste("Accuracy: ", accuracy*100, "%"))


#Plot command (LDA):
projectionOntoFirst = as.matrix(mydata) %*% z$scaling[,1]
projectionOntoSec = as.matrix(mydata) %*% z$scaling[,2]
plot(projectionOntoFirst, projectionOntoSec,pch = 20, 
  cex = .5,col = myColors,
  xlab = "Linear Discriminant 1",
  ylab = "Linear Discriminant 2")

mtext("(A)",line = -2)


#Plot Tumor MDS:
points = cmdscale(dist(cancerData))
plot(points,col=myColors,cex=.5,
  xlab = "First Principal Component",
  ylab = "Second Principal Component",pch = 20)

mtext("(B)", line=-2)


#Plot Tumor PCA:
pcdat = prcomp(cancerData)

#Plot using the new color vector:
plot(pcdat$x[,1], pcdat$x[,2],pch = 20, cex = .5,col = myColors,
  xlab = "First Principal Component",
  ylab = "Second Principal Component")

#Put on the bottom, because it is buried in the data points:
mtext("(C)", line=-2, side =1)


#Create a new window:
X11(height=4,width=4)
par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0)
)


#Plot the density:
myDens = density(projectionOntoFirst)
myDensBenign = density(projectionOntoFirst[benign])
myDensMal = density(projectionOntoFirst[malignant])

myXrange = range(myDens$x)


plot(myDensBenign$x, myDensBenign$y, type = "l", col = "blue", xlim = myXrange, xlab = "Linear Discriminant 1",ylab = "Density")

#Can't seem to get this working with the above line:
lines(myDensBenign$x, myDensBenign$y, type = "l", col = "red")
