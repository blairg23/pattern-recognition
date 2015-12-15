#Author: Blair Gemmer
#CSCI 548 - Pattern Recognition
#Spring 2013
#Project 6 - Clustering
#Date: 04/13/13
#Last edited: 04/25/13

#Part 1, plot the data without clustering (not actually needed in assignment document):
plotData = function(myData){
  plot(myData) #Plot your data like a g
}

#Get k-means and return the cluster vector:
getKmeans = function(myData, numClusters){
  #Get the k-means clusters. Stored in $cluster
  k = kmeans(myData,numClusters)
  clusters = k$cluster
  return(clusters)
}

#Colorize the data:
getColors = function(classes, clusters){    
  #Do some regular expressions to pull out the benign vs. malignant tumors:
  green = grep("1", clusters)
  blue = grep("2", clusters)
  red = grep("3", clusters)
  black = grep("4", clusters)
  
  #Create a color vector using the class types (in this case, I just named them the colors to be more general):
  myColors = as.vector(classes)
  myColors[green] = "green"
  myColors[blue] = "blue"
  myColors[red] = "red"
  myColors[black] = "black"
  
  return(myColors)
}

#Plot your data, based on your color vector:
plotStuff = function(myData, myColors, labelX='', labelY='', name=''){
  #Create a new window:
  X11(height=4,width=4)
  par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))
  png(filename=name)
  plot(myData, col=myColors, xlab=labelX, ylab=labelY)
  dev.off()
}

#Get membership vector based on your data:
getMemberVect=function(myData, k = 4, w=FALSE){
  if (w==FALSE){
    clusterObj = hclust(dist(myData)) #Get distance matrix of my data, then get clustering based on that
  }
  else{ #If we want warded
    clusterObj = hclust(dist(myData), method="ward") #Get distance matrix of my data, then get clustering based on that
  }
  membershipVect = cutree(clusterObj, k = k) #Get a color vector based on the clustering
  return(membershipVect)
}

#Plot based on hierarchical coloring: 
plotHier = function(myData, k=4, labelX='', labelY='', name=''){
  membershipVect = getMemberVect(myData, k=k) #Get membership vector
  plotStuff(myData, membershipVect, labelX, labelY, name) #Plot the results
}

#Plot the same as above, but with method = "ward" in hclust command:
plotWardedHier = function(myData, k=4, labelX='', labelY='', name=''){
  membershipVect = getMemberVect(myData, k=k, w=TRUE) #Get membership vector
  plotStuff(myData, membershipVect, labelX, labelY, name) #Plot the results
}

#Get euclidean distance:
getEuclid = function(x1, x2){
  return(sqrt(sum((x1-x2)^2)))
}

#Find C_k:
findCk = function(xi, C, m){
  #Get all distances:
  distances = sapply(1:m, function(j){
    getEuclid(xi, C[[j]])
  })
  index = which.min(distances)

  return(list(minValue = distances[index], minIndex = index)) #Return the smallest distance
}

#Get BSAS cluster vector:
getBSAS = function(myData, numClusters=Inf, theta=7){
  m = 1 #Number of desired clusters
  theta = theta #Threshold value
  numClusters = numClusters #Set the number of max clusters
  clustID = c()#Holds out identifiers for what cluster these belong to.
  clustID[1] = m #Set our initial index for our cluster IDs
  C = list() #Column array to hold cluster vectors
  xi = myData[1,] #1st row of data
  N = nrow(myData) #Number of rows in the data
  C[[m]] = xi #1st element of column matrix = 1st row of data
  for(i in 2:N){ #For each row in the data
    xi = myData[i,] #Get x_i
    minDist = findCk(xi, C, m) #get the minimum distances
    k = minDist$minIndex #Our minimum distance index
    minC = minDist$minValue #Our minimum distance

    #Check our tolerances:
    if (minC > theta && m<numClusters){ #If we have exceded our threshold and we are past our max index
      m = m + 1 #Increment our m value
      C[[m]] = myData[i,] #Add a new vector into our column matrix
      clustID[i]= m #This is our new cluster, so add it to our IDs
    }
    else{
      C[[k]] = colMeans(rbind(C[[k]], myData[i,])) #Otherwise, concatenate C_k with x_i and take the mean of the vector
      clustID[i] = k #Now k is our new cluster, add it to our IDs
    }
  }
  return(length(unique(clustID))) #Finally, return the array of cluster vectors
}

#Get the best possible theta value to use (return as a list):
getBestTheta = function(myData){
  d = dist(myData) #Create a distance matrix from our data
  
  results = c() #Hold the results of each cluster size
  indexes = c() #Hold the results of each theta value
  sizeOfTheta = length(min(d):max(d))/150 #This will get us the number of values between min and max
  for (i in seq(from=1, to=max(d), by=sizeOfTheta)){ #From the minimum to the maximum of our data, stepping by size of theta
    results = c(results, getBSAS(myData, theta=i)) #the theta value is our ith value
    indexes = c(indexes, i)
  }
  return(list(valueVector = results, indexVector = indexes))
}

#Plot the clusters, based on differing theta values:
plotClust = function(myData){
  #Create a new window:
  X11(height=4,width=4)
  par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))
  
  solution = getBestTheta(myData) #Get the best theta value
  results = solution$valueVector #The vector of cluster sizes
  indexes = solution$indexVector #The vector of theta values
  png(filename="irisClust.png")
  plot(indexes, results, xlab="Theta-Value", ylab="Number of Clusters") #Plot them
  dev.off()
}

#Plot a heatmap:
plotHeat = function(myData){
  #Create a new window:
  X11(height=4,width=4)
  par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))
  
  #heatmap(myData, Colv=NA)
  library(gplots) #Import gplots
  png(filename="geneHeat.png")
  heatmap(myData,Colv=NA, col = redgreen(75),labRow=NA) 
  dev.off()
}

#Plot the line graphs:
plotLines = function(myData){
  #Create a new window:
  X11(height=2.2,width=8)
  par(mar=c(2.5,3.5,2.5,.1),mgp=c(1.5,.5,0)) #Sets up parameters with tight margins
  layout(matrix(1:3, 1, 3), widths=c(1, 1, 1)  ); #Sets up 3 column layout
  membershipVect = getMemberVect(myData, k=3) #Get 3 clusters
  end = length(unique(membershipVect)) #We want the max number of dimensions (in this case, 4)
  for(curCluster in 1:end){
    #Stuff from tutorial given:
    rows = which(membershipVect==curCluster) #Get indexes for y-values
    numInCluster = length(rows)
    xvals = c(0:(numInCluster*4-1))
    xvals = xvals %%4+1
    xvals = matrix(xvals,ncol=4,byrow=TRUE) #Generate x-values
    
    yvals = myData[rows,] #Generate y-values using expression data
    
    #plot(xvals, yvals)
    if(curCluster == 1){ #Panel 1
      par(mar=c(2.5,4,.3,.1), mfg=c(1,1))
      #Instead of doing type='l' to get lines, I do type='n' to do invisible plotting, to allow for lines() to work later:
      plot(xvals,yvals, type='n', ylim=range(myData),ylab="Expression Level", xlab='') #Far left plot
    }
    else if(curCluster == 2){ #Panel 2
      par(mar=c(2.5,.5,.3,.1), mfg=c(1,2))
      plot(xvals,yvals, type='n', ylim=range(myData),yaxt = 'n', xlab="Time") #Middle plot
    }
    else{ #Panel 3
      par(mar=c(2.5,.5,.3,.1), mfg=c(1,3))
      plot(xvals,yvals, type='n', ylim=range(myData),yaxt = 'n', xlab='') #Far right plot
    }
    
    par(mfg=c(1,curCluster)) #Tell the stupid program where to put the current line plot
    
    for(i in 1:numInCluster){ #Plot the rest of the lines:
      index = rows[i] #Get index from the row's ith place
      yval = myData[index,] #Get y-value from index
      lines(xvals[curCluster,], yval, type='l')
    }
  }
  dev.copy(jpeg,"geneLines.jpg")
  dev.off()
}


#Plot the Principle Components Analysis:
plotPCA = function(myData, name=''){
  myColors = getMemberVect(myData,k=3) #Create colors from the membership vector for 3 clusters
  
  #Create a new window:
  X11(height=4,width=4)
  par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))
  #Do PCA on the data and store:
  dataPCA = prcomp(myData)
  
  #Store x and y data for plotting:
  x = dataPCA$x[,1]
  y = dataPCA$x[,2]
  
  #Plot it:
  png(filename=name)
  plot(x,y, col=myColors, xlab="Principle Component 1", ylab="Principle Component 2")
  dev.off()
}