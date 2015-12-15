#Author: Blair Gemmer
#CSCI 548 - Pattern Recognition
#Spring 2013
#Project 3 - MDS
#Date: 02/27/13

rm(list=ls(all=TRUE))

irisData = iris[1:4]

#Create a vector of the color data:
classes = iris[,5]
myColors = as.vector(classes)
myColors[which(classes == "setosa")] = "red"
myColors[which(classes == "versicolor")] = "green"
myColors[which(classes == "virginica")] = "blue"

#Create a new window:
X11(height=4,width=4)
par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))

#Plot Iris MDS:
points = cmdscale(dist(irisData))
plot(points,col=myColors,
  main = "Iris Data Using MDS",
  xlab = "Eigen 1",
  ylab = "Eigen 2",pch = 20)

legend("topright",
  levels(iris[,5]),
  col = c("red","green","blue"),
  pch = 20)


############################

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
X11(height=4,width=4)
par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))

#Plot Fruit MDS:
points = cmdscale(dist(fruitData))
plot(points,col=myColors,
  main = "Fruit Data Using MDS",
  xlab = "Eigen 1",
  ylab = "Eigen 2",pch = 20)

legend("topleft",
  levels(fruitData[,5]),
  col = c("red","orange","yellow", "burlywood1"),
  pch = 20)


##########################

#Read in the mouse data:
mouseData = read.table("mouse.txt", sep="\t", header=TRUE)


#Now, remove that 1st column, so we can do dist() later:
mouseData = mouseData[,-1]

#Set your row names:
#row.names(mouseData) <- mouseData[,1]

#Get row names:
#rowNames = row.names(mouseData)


#Transpose the mouseData:
mouseTransposed = t(mouseData)

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

#Create a new window:
X11(height=4,width=4)
par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))

#Plot Mouse MDS (proximal/distal):
points = cmdscale(dist(mouseTransposed))
plot(points,col=myColors,
  main = "Mouse Data Using MDS\n(Proximal vs. Distal)",
  xlab = "Eigen 1",
  ylab = "Eigen 2",pch = 20)

legend("topright",
  c("proximal", "distal"),
  col = c("red", "green"),
  pch = 20)



#Do some regular expressions to pull out the mouse types:
typeB= grep("^B.*", expNames)
typeC = grep("^C.*", expNames)


#Create a color vector using the proximal or distal class::
classes = expNames
myColors = as.vector(classes)
myColors[typeB] = "red"
myColors[typeC] = "green"


#Create a new window:
X11(height=4,width=4)
par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))

#Plot Mouse MDS (type of mouse):
points = cmdscale(dist(mouseTransposed))
plot(points,col=myColors,
  main = "Mouse Data Using MDS\n(Type B vs. Type C)",
  xlab = "Eigen 1",
  ylab = "Eigen 2",pch = 20)

legend("topright",
  c("B", "C"),
  col = c("red", "green"),
  pch = 20)



##########################

#Read in the cancer data:
cancerData = read.table("cancer.data", sep=",", header=TRUE)


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

#Create a new window:
X11(height=4,width=4)
par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))

#Plot Cancer MDS (Benign/Malignant):
points = cmdscale(dist(cancerData))
plot(points,col=myColors,
  main = "Breast Cancer Data Using MDS\n(Benign vs. Malignant)",
  xlab = "Eigen 1",
  ylab = "Eigen 2",pch = 20)

legend("topright",
  c("Benign", "Malignant"),
  col = c("red", "blue"),
  pch = 20)