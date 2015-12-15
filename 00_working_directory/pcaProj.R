#Sample commands from project workthrough:
#class(iris)
#summary(iris)
#labels(iris)
#colnames(iris)
#print(iris[,5])
#help(plot)


#Author: Blair Gemmer
#CSCI 548 - Pattern Recognition
#Spring 2013
#Project 2 - PCA
#Date: 02/04/13

rm(list=ls(all=TRUE))

#Store the iris species:
irisClasses = iris$Species

#Store the iris data (sans species)
irisData = iris[1:4]

#Do PCA on the data and store:
irisPCA = prcomp(irisData)

#Store x and y data for plotting:
x = irisPCA$x[,1]
y = irisPCA$x[,2]

#Plot the data:
#plot(x,y)

#Create a vector of the color data:
#classes = iris[,5]
#myColors = as.vector(classes)
#myColors[which(classes == "setosa")] = "red"
#myColors[which(classes == "versicolor")] = "green"
#myColors[which(classes == "virginica")] = "blue"

#Perform PCA and plot the data:
#pcdat = prcomp(iris[,1:4])
#plot(pcdat$x[,1], pcdat$x[,2],pch = 20, cex = 2,col = myColors,
#  main = "Iris Data",
#  xlab = "principal component 1",ylab = "principal component 2")
#legend("topright",levels(iris[,5]),col = c("red","green","blue"),pch = 20)

#Indices of each species:
setosaIndices = seq(1:150)[1:50]
versicolorIndices = seq(1:150)[51:100]
virginicaIndices = seq(1:150)[101:150]

#Create color vector using these indices:
classes = iris[,5]
myColors = as.vector(classes)
myColors[setosaIndices] = "red"
myColors[versicolorIndices] = "green"
myColors[virginicaIndices] = "blue"


#Perform PCA:
pcdat = prcomp(iris[,1:4])

#Create a new window:
X11(height=4,width=4)
par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))

#Plot using the new color vector:
plot(pcdat$x[,1], pcdat$x[,2],pch = 20, cex = 2,col = myColors,
  main = "PCA Plot of Iris Data",
  xlab = "First Principal Component",ylab = "Second Principal Component")
legend("topright",levels(iris[,5]),col = c("red","green","blue"),pch = 20)


#Create a new window:
X11(height=4,width=4)
par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))

#Get standard deviation:
stdDev = irisPCA$sdev

#Square standard deviation to get variances:
variances = stdDev * stdDev

#Sum the variances:
varSum = sum(variances)

#Take the proportional variances:
variances = variances / varSum

#Create a barplot of the proportion of variance:
barplot(variances, main="Proportion of Variance",names.arg = c("PC1", "PC2", "PC3", "PC4"))


#Read in the fruit data:
fruitData = read.csv("fruit.csv")

#Create a color vector:
classes = fruitData[,5]
myColors = as.vector(classes)
myColors[which(classes == "apple")] = "red"
myColors[which(classes == "orange")] = "orange"
myColors[which(classes == "lemon")] = "yellow"
myColors[which(classes == "peach")] = "burlywood1"

#Perform PCA on the data:
pcdat = prcomp(fruitData[,1:4])


#Create a new window:
X11(height=4,width=4)
par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))


#Plot the results:
plot(pcdat$x[,1], pcdat$x[,2],pch = 20, cex = 2,col = myColors,
  main = "PCA Plot of Fruit Data",
  xlab = "First Principal Component",ylab = "Second Principal Component")
legend("topleft",levels(fruitData[,5]),col = c("red","orange","yellow", "burlywood1"),pch = 20)


#Create a new window:
X11(height=4,width=4)
par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))

#Get standard deviation:
stdDev = pcdat$sdev

#Square standard deviation to get variances:
variances = stdDev * stdDev

#Sum the variances:
varSum = sum(variances)

#Take the proportional variances:
variances = variances / varSum

#Create a barplot of the proportion of variance:
barplot(variances, main="Proportion of Variance",names.arg = c("PC1", "PC2", "PC3", "PC4"))

#Use the 3D plotting library:
library(rgl)

#Create a new window:
#X11(height=4,width=4)
#par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))


#Plot the results as a 3D plot:
plot3d(pcdat$x[,1], pcdat$x[,2], pcdat$x[,3],pch = 20, cex = 2,col = myColors,
  main = "PCA Plot of Fruit Data",
  xlab = "First Principal Component",ylab = "Second Principal Component", zlab = "Third Principal Component")


#Create a new window:
X11(height=4,width=4)
par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))


#Plot the results:
plot(pcdat$x[,2], pcdat$x[,3],pch = 20, cex = 2,col = myColors,
  main = "PCA Plot of Fruit Data",
  xlab = "Second Principal Component",ylab = "Third Principal Component")
legend("topleft",levels(fruitData[,5]),col = c("red","orange","yellow", "burlywood1"),pch = 20)
