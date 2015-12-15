#Author: Blair Gemmer
#CSCI 548 - Pattern Recognition
#Spring 2013
#Project 6 - Clustering
#Date: 04/13/13
#Last edited: 04/25/13

#Clear variables:
rm(list=ls(all=TRUE))

#Import our functions:
source("clusterFuncs.R")

#Tutorial 1:
library(mlbench)
data <-  mlbench.smiley() #Import smiley data!
myData = data$x #Just want the data matrix
classes = data$classes #and the classes

numClusters = 4 #number of clusters desired
myClusters = getKmeans(myData, numClusters) #Perform k-means, get clusters
myColors = getColors(classes, myClusters) #use cluster vector and classes to colorize for plotting

plotStuff(myData, myColors, name="smileyColor.png", labelX="x-axis", labelY="y-axis") #plot it (Tutorial 1).

plotHier(myData, labelX="x-axis", labelY="y-axis", name="smileyHier.png") #Plot, based on hierarchical clustering coloration (Tutorial 1)

plotWardedHier(myData, labelX="x-axis", labelY="y-axis", name="smileyWard.png") #Plot the same, but with method = ward command (Tutorial 1)



#Tutorial 2:
myData = iris[1:4]
plotClust(myData) #Plot the clusters vs. different theta values (Tutorial 2)
plotPCA(myData, name="irisPCA.png")


#Tutorial 3:
myData = read.csv("ecoli.csv", sep=",",fill=TRUE)
idref = myData$idref #Keep the first column
desc = myData$desc #Keep the second column
myData = myData[-1] #Remove the first column
myData = myData[-1] #Remove the second column
myData = as.matrix(myData) #Convert to a data matrix

plotHeat(myData) #Plot the heatmap (Tutorial 3)

plotPCA(myData, name="genePCA.png") #Plot the PCA (Tutorial 3)

#Misread the instructions and included a graph of the hierarchical plot (uncomment to see results):
#plotHier(myData, labelX="diff4", labelY="diff7",name="geneHier.png") #Plot the hclust/cutree (Tutorial 3)

plotLines(myData) #Plot the triptych (tri-panel plot) (Tutorial 3)


