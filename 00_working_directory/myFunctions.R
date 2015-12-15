#Author: Blair Gemmer
#CSCI 548 - Pattern Recognition
#Spring 2013
#Project 5 - Feature Selection
#Date: 03/29/13

#Flag to turn off plots:
plots <<- FALSE

# For each dimension, calculate the average of the
# T-statistics (their absolute value) for each pair
# of classes:
getTscores = function(myData,classes){
#BIG thanks to Eric Spaulding for helping me get this working!
  
  #get the number of dimensions
  numDimensions = length(myData[1,])
  
  #get number of classes
  myLevels = unique(classes)
  numClasses = length(myLevels)
  
  #acquire list of indices for each class 
  myList = list()
  for(i in 1:numClasses){ myList[[i]] = which(classes==myLevels[i]) }
  
  allStats = c() #Holds our list of t-statistics
  
  
  # For each dimension, calculate the average of the
  # T-statistics (their absolute value) for each pair of classes.
  for (d in 1:numDimensions ){
    statsToAverage = c()
    currentStat = NULL
    for (i in 1:(numClasses-1)){
      for (j in (i+1):numClasses){
        #do t-test, then take the statistic and take the absolute value of it:
        currentStat = tryCatch({ #Use try-catch to make sure we don't try to t.test on zeros:
          currentStat = abs(t.test( myData[myList[[i]],d] , myData[myList[[j]],d] )$statistic)
        }, warning = function(w) {
          #return(0)
          next
        }, error = function(e) {
          return(0)
        }, finally = function(f){
          print("We're good.")
        })
        #add the result to our t-statistic matrix:
        statsToAverage = append(statsToAverage,currentStat)
      }
    }
    #Average the values from tStat:
    average = ave(statsToAverage)[1]
    #Add that average t-stat to the list of tstats:
    allStats = c(allStats, average)
  }
  #Return the tStat as a vector of tStats:
  allStats = as.vector(allStats)
  return(allStats)
}

#Gets the best performing t-stats:
getTop = function(tStats, threshold){
  tStats = which(tStats > threshold)
  return(tStats)
}

#Returns the LDA accuracy from running LDA:
runLDA = function(myData, classes, percent=NULL, train=NULL){
  library(MASS)
  #If we haven't set the training data:
  if(is.null(train)){
    train <- sample(1:nrow(myData), percent*nrow(myData)) #Sample size of 75
  }
  #else, if we have already set the training data:
  z <- lda(myData[train,], classes[train], tol=0)
  
  #actual classes:
  actual <- classes[-train]
  
  #Predicted classes:
  predicted <- predict(z, myData[-train, ])$class
  
  #Test accuracy by dividing the length of predicted by actual:
  accuracy <- ( length(actual) - length(which(actual != predicted)) ) / length(actual)
  
  return(accuracy)
}

#Returns an LDA object for use in plotting the LDA in mouse data:
getLDAobj = function(myData, classes, percent=NULL, train=NULL){
  library(MASS)
  #If we haven't set the training data:
  if(is.null(train)){
    train <- sample(1:nrow(myData), percent*nrow(myData)) #Sample size of 75
  }
  #else, if we have already set the training data:
  z <- lda(myData[train,], classes[train], tol=0)

  return(z)
}

#General function for plotting and getting accuracy:
doStuff = function(myData, classes, threshold, percent, dataType){
  #Grab the t-scores:
  tScores = NULL
  tScores = getTscores(myData, classes)
    
  #Keep track of the number of dimensions:
  numDimensions = ncol(myData)
  
  if(plots==TRUE){
    #Create a new window:
    X11(height=4,width=4)
    par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))
  
    #Plot the t-scores:
    barplot(tScores, xlab = "Dimensions", ylab = "T-Statistic Scores", names.arg=seq(1:numDimensions))
  }
   
  #LDA:
  print(paste("Using the ", dataType)) #Prints what data set we are using
  accuracy = runLDA(myData, classes, percent)
  print(paste("Accuracy with all dimensions: ", accuracy*100, "%"))  
  
  #Pull out top dimensions:
  topDims = getTop(tScores, threshold)
  print("Best Dimensions using T-Test:")
  print(topDims)
  
  #Use only the these top dimensions:
  newData = myData[, topDims]
  
  #Test accuracy by running LDA again:
  newAccuracy = runLDA(newData, classes, percent)
  print(paste("Accuracy with best dimensions: ", newAccuracy*100, "%"))
  
  #Run floating search and pick the best dimensions for all sizes of k:
  bestDims = NULL
  print("Best Dimensions of each set of size k:")
  for(i in 2:numDimensions-1){ #Start at 2 since we need at least 2 dimensions to compare
    bestDims = floatingSearch(myData, classes, i) #Pick the best dimensions for each value of k
    #If we have more than 1 number to look at, let's print it out:
    print(paste("k = ", i)) #Print the size k and the resulting dimensions to use
    print(bestDims[[i]])
  }
  
  #Pick out the desired number of dimensions:
  floatDims = bestDims[[length(topDims)]]
  print("Best Dimensions using Floating Search Algorithm:")
  print(floatDims)
  
  #Use the desired number of dimensions from floating search:
  floatData = myData[, floatDims]
  
  #Test accuracy by running LDA again:
  floatAccuracy = runLDA(floatData, classes, percent)
  print(paste("Accuracy using Floating Search Dimensions: ", floatAccuracy*100, "%"))
  
  #Calculate J-scores for full data set:
  printJscores(myData, classes, paste(dataType))
  
  #Calculate J-scores for reduced data set:
  printJscores(newData, classes, paste(dataType, ", Reduced Set"))
  
}

#General function for plotting and getting accuracy (for the mouse):
mouseStuff = function(myData, classes, threshold){
  
  #Grab the t-scores:
  tScores = NULL
  tScores = getTscores(myData, classes)
    
  #Keep track of the number of dimensions:
  numDimensions = ncol(myData)

  print("Using the Mouse Data.") #Prints what data set we are using
  #Pull out best dimensions:
  topDims = getTop(tScores, threshold)
  print("Top Dimensions using T-Test are: ")
  print(topDims)
  
  #Run floating search and pick the best dimensions for all sizes of k:
  bestDims = NULL
  print("Best Dimensions of each set of size k:")
  
  #WORKED ON THIS FOR 10 HOURS ALONE, HAD TO GIVE UP AND MOVE ON TO NEW DATA:
#   for(i in 2:numDimensions-1){#Start at 2, since we need at least 2 dimensions
#     bestDims = floatingSearch(myData, classes, i, TRUE) #Pick the best dimensions for each value of k
#     print(paste("k = ", i, ": ", bestDims)) #Print the size k and the resulting dimensions to use
#   }
  
  #Use only the best dimensions:
  newData = myData[, topDims]
  
  #Plot the full set now:
  percent = 1 #100%
  z = getLDAobj(newData, classes, percent) #get our LDA obj for plotting
  
  #Plot command (LDA):
  projectionOntoFirst = as.matrix(newData) %*% z$scaling[,1]
  projectionOntoSec = as.matrix(newData) %*% z$scaling[,2]
  
  #Do some regular expressions to pull out the proximal and distal experiments:
  proximal = which(classes == "proximal")
  distal = which(classes == "distal")
  
  #Plot the density:
  myDens = density(projectionOntoFirst)
  myDensProximal = density(projectionOntoFirst[proximal])
  myDensDistal = density(projectionOntoFirst[distal])
  
  #The axis lengths:
  myXrange = range(myDens$x)
  myYrange = range(myDensProximal$y)

  if(plots == TRUE){
    #Create a new window:
    X11(height=4,width=4)
    par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))
    
    #Plot the t-scores:
    barplot(tScores, xlab = "Dimensions", ylab = "T-Statistic Scores", names.arg=seq(1:numDimensions))
    
    #Create a new window:
    X11(height=4,width=4)
    par(mar=c(2.5,2.5,2.5,.1),mgp=c(1.5,.5,0))
    
    #Density plot:
    plot(myDensProximal$x, myDensProximal$y, type = "l", col = "blue", xlim = myXrange, ylim = myYrange, xlab = "Linear Discriminant 1",ylab = "Density")
    #Adds the red line to the above plot:
    lines(myDensDistal$x, myDensDistal$y, type = "l", col = "red")
  
    legend("topleft",
           c("proximal", "distal"),
           col = c("blue", "red"),
           pch = 20)
  }
  
  #Test accuracy by running LDA again:
  crossValidation(newData, classes)  
}

#Drops a given element x from the list l
dropElement = function(x, l){
  return(l[-which(l == x)])
}

#Performs cross-validation (leave one out)
crossValidation = function(myData, classes){
  entries = 1:20 #List from 1-20
  accuracies = c() #New vector to hold the accuracy data 
  for(i in entries){
    train = dropElement(i, entries) #Leave one out
    accu = runLDA(myData, classes, train=train) #runs LDA on my data
    print(paste("The accuracy for fold #", i , ": ", accu*100, "%")) #Prints out the accuracy for this fold
    accuracies = c(accuracies, accu) #Holds all the accuracies for the folds
  }
  print(paste("The average performance for all folds: ", ave(accuracies)[1]*100, "%")) #Prints out the average accuracy
}

#Returns list of indices:
getIndices = function(classes){
  #Store the number and type of classes we are using:
  myLevels = unique(classes)
  #This needs to be set based on the unique values,
  #otherwise, will not work with cancer data since
  #there are no levels for the classes in tumor data.
  levels(classes) <- myLevels
  numClasses = length(myLevels)
  
  #acquire list of indexes for each class:
  myList = list()
  
  for(i in 1:numClasses){
    myList[[i]] = which(classes==myLevels[i])
  }
  return(myList)
}

#Returns the Sw:
Sw = function(x,classes){
  #Store the number and type of classes we are using:
  myLevels = unique(classes)
  #This needs to be set based on the unique values,
  #otherwise, will not work with cancer data since
  #there are no levels for the classes in tumor data.
  levels(classes) <- myLevels
  numClasses = length(myLevels)
  
  #Grab indices:
  myList = getIndices(classes)
  
  #Get global means:
  globalMeans = colMeans(x)
  
  #Instantiate our matrix as dimensions by dimensions (zeroes matrix):
  returnMatrix = matrix(nrow=ncol(x), ncol=ncol(x), data=0)
  
  #Create our return matrix:
  for(i in 1:numClasses){
    prior = length(myList[[i]])/length(classes)
    #Create our covariance matrix:
    covarMatrix = tryCatch({ #Use try-catch to make sure we don't try to covariance on zeros:
      covarMatrix = cov(x[myList[[i]],])
    }, warning = function(w) {
      #return(0)
      next
    }, error = function(e) {
      return(0)
    }, finally = function(f){
      print("We're good.")
    })
    #Sum of prior[i] * covarMatrix[i]:
    returnMatrix = returnMatrix + prior * covarMatrix
  }
  return(returnMatrix)
}

#Returns the Sb:
Sb = function(x,classes){
  #Store the number and type of classes we are using:
  myLevels = unique(classes)
  #This needs to be set based on the unique values,
  #otherwise, will not work with cancer data since
  #there are no levels for the classes in tumor data.
  levels(classes) <- myLevels
  numClasses = length(myLevels)
  
  #Grab indices:
  myList = getIndices(classes)
  
  #Get global means:
  globalMeans = colMeans(x)
  
  #Instantiate our matrix as dimensions by dimensions (zeroes matrix):
  returnMatrix = matrix(nrow=ncol(x), ncol=ncol(x), data=0)
  
  #Create our return matrix:
  for(i in 1:numClasses){
    prior = length(myList[[i]])/length(classes)
    #Get the class means:
    classMeans = colMeans(x[myList[[i]],])
    returnMatrix = returnMatrix + prior *
      (classMeans - globalMeans) %*%
      t(classMeans - globalMeans)
  }
  return(returnMatrix)
}

#Returns the J1:
J1 = function(passedSm, passedSw){
  return(sum(diag(passedSm)) / sum(diag(passedSw)))
}

#Returns the J2:
J2 = function(passedSm, passedSw){
  return(det(passedSm)/det(passedSw))
}

#Returns the J3:
J3 = function(passedSw,passedSb){
  #solve does inverse for square matrix
  # (ginv is general)
  return(sum(diag(solve(passedSw) %*%
                    passedSb)))
}

#Returns the Sm:
Sm = function(x){
  return(cov(x))
}

#Returns the J-scores:
printJscores = function(myData, classes, dataType){
  #Gather Sw, Sm, and Sb:
  passedSw = Sw(myData, classes)
  passedSm = Sm(myData)
  passedSb = Sb(myData, classes)
  
  #J1:
  print(paste("J1 score for ", dataType, ":", J1(passedSm, passedSw)))
  #J2:
  print(paste("J2 score for ", dataType, ":", J2(passedSm, passedSw)))
  #J3:
  print(paste("J3 score for ", dataType, ":", J3(passedSw, passedSb)))
}

#Get all combinations of the list:
getCombos = function(lst, k){
  return(combn(1:ncol(lst), k)) #returns all combinations of size k
}

#Return the list of scores:
getScores = function(myData, classes, element=NULL, k){
  #If we're at the beginning:
  if(k == 0){
    score = getTscores(myData, classes)
  }
  else{
    #Increment k:
    k = k + 1
    #Get indices of combinations of the data set columns:
    indexCombos = getCombosWithElement(myData,element, k)
    score = c()
      #Find scores:
      for(i in 1:length(indexCombos)){
        #Keep track of the index of the combination we're on:
        indexCols = indexCombos[[i]]
        #Find and score (e.g. with J3) 
        #all sets of size k+1 features,
        score = c(score, J3(Sw(myData[,indexCols],classes),Sb(myData[,indexCols],classes)))
      }
  }
  return(score[sort.list(score)])
}

#Returns true or false, depending if the smaller is an element of the larger:
isSubset = function(small, large){
  newSet = intersect(small, large) #Find the intersection of the two sets 
  #If the new set is the same length as the small set:
  if(length(newSet) == length(small)){ 
    isSubset = TRUE
  }
  else{
    isSubset = FALSE
  }
  return(isSubset)
}

#Returns the combinations with that element in it:
getCombosWithElement = function(lst, element, k){
  allComb = getCombos(lst,k) #List of all combinations
  limComb = c() #Limited list of combinations
  for(i in 1:ncol(allComb)){ #for all the elements in all combinations
    checkCol = allComb[,i] #Check each column
    if(isSubset(element, checkCol)){ #see if our given element is in the column
      limComb = c(limComb, list(checkCol)) #if so, add it to our limited combinations list
    }
  }
  return(limComb)
}

#Return the list of scores:
getBestSet = function(myData, classes, element=NULL, k, mouse){
  #If we're at the beginning:
  if(k == 0){
    score = getTscores(myData, classes) #create list of scores for each dimension
    #Finds the best scoring dimension in the list and returns the index:
    bestDims = seq(along=score)[score == max(score)]
  }
  else{
    #Increment k:
    k = k + 1
    #Get indices of combinations of the data set columns:
    indexCombos = getCombosWithElement(myData,element, k)
    score = c()
    #Find scores:
    for(i in 1:length(indexCombos)){
      #Keep track of the index of the combination we're on:
      indexCols = indexCombos[[i]]
      #Find and score (e.g. with J3) 
      #all sets of size k+1 features,
      score = tryCatch({ #Use try-catch to make sure we don't try to score on zeros:
        score = c(score, J3(Sw(myData[,indexCols],classes),Sb(myData[,indexCols],classes)))
      }, warning = function(w) {
        #return(0)
        next
      }, error = function(e) {
        return(0)
      }, finally = function(f){
        print("We're good.")
      })
      #Add the set that scored the best to our list of best dimensions:
      if(mouse){
        bestDims = indexCombos[[runif(1, 1, length(indexCombos)-1)]] #some random dimensions to make mouse work, because covariance matrix won't work with zero values  
      }
      else{
        bestDims = indexCombos[[seq(along=score)[score == max(score)]]]
      }
    }
  }
  return(bestDims)
}

#Get best score:
getBestScore = function(myData, classes, element=NULL, k){
  #Get all scores for this set:
  scores = getScores(myData, classes, element, k=k)
  #Find the best by picking the tail end of the score list after sorting (n=1),
  #Since sort.list returns an index, so use score[] to get value:
  return(tail(scores, n=1))
}

#Get worst score:
getWorstScore = function(myData, classes, k){
  #Get all scores for this set:
  scores = getScores(myData, classes, k)
  #Find the worst by picking the head end of the score list after sorting (n=1),
  #Since sort.list returns an index, so use score[] to get value:
  return(head(scores, n=1))
}

#Returns the least effective dimension:
getLeastEffect = function(myData, classes, setVector){
  workingSet = setVector[[1]] #Cos it sucks to write [[1]] everytime
  #Drop an element and score the remainder:
  allScores = c()
  #max score so far:
  maxScore = 0
  for(i in 1:length(workingSet)){
    tempSet = NULL
    tempSet = workingSet[-i] #This excludes the index we are looking at
    score = c()
    #Find scores:
    score = c(score, J3(Sw(myData[,tempSet],classes),Sb(myData[,tempSet],classes)))
    allScores = c(allScores, score) #Record every score
    maxScore = max(allScores) #Record the maximum score so far
  }
  maxIndex = which(allScores == maxScore) #Keep track of the maximum score index
  #The original set, minus the least effective dimension:
  return(list("index"=workingSet[-maxIndex], "score"=maxScore)) #Get index by doing getLeastEffect$index, get score by getLeastEffect$score
}

#Find the best dimensions to perform pattern recognition on:
floatingSearch = function(myData, classes, numDesiredDims, mouse=FALSE){
  #myListOfSets[[k-1]] = newKsetLessLeastEffect
  #myListOfScores[[k-1]] = newScoreOfKminusOne
  
  myListOfSets = list() #Something to hold the list of sets in Y={0}
  myListOfScores = list() #Something to hold the list of scores in
  
  maxScore = 0 #Instantiate to 0
  
  for(k in 0:(numDesiredDims - 1)){ #Can't go above the number of dimensions 
    #Get the best scoring set and its score:
    if(k==0){ #First pass
      myListOfSets[[k+1]] = getBestSet(myData, classes, k=k, mouse)
      myListOfScores[[k+1]] = getBestScore(myData, classes, k=k)
      maxScore = myListOfSets[k+1]#If we are just starting, set the max score to our first score
    }
    else{ #Every other pass:
      elementToKeep = myListOfSets[[k]]
      myListOfSets[[k+1]] = getBestSet(myData, classes, element=elementToKeep, k=k, mouse)
      myListOfScores[[k+1]] = getBestScore(myData, classes, element=elementToKeep, k=k)
    }
    #If we will have 3 elements:
    if(k==2){
      #Find the index with least effect and remove it:
      leastEffect = getLeastEffect(myData, classes, myListOfSets[k+1])
      indexWithLeastEffect = leastEffect$index
      scoreOfLeastEffect = leastEffect$score
      
      #Check to see if the index we found is equivalent to the one we added before:
      #Find the intersection of the two lists:
      interSect = intersect(myListOfSets[[k]], indexWithLeastEffect)
      #If the intersect is equal in size to the original sets, then we have a match:
      if(length(interSect) == length(myListOfSets[[k]] && length(intersect) == length(indexWithLeastEffect))){
        #Increment k:
        k = k+1
      }
      #However, if our current max score is less than our previous score:
      else if(maxScore <= myListOfScores[[k]]){
      #If so, increment k and loop
      k = k+1
      }
      #If we've reached k = 2:
      else if(k==2){
        #replace set with k+1 minus the least affecting feature:
        myListOfSets[[k]] = indexWithLeastEffect
        myListOfScores[[k]] = scoreOfLeastEffect
        maxScore = scoreOfLeastEffect #set new max score
      }
      else{
        improved = TRUE
        #While we are above 2:
        while(k>2 && improved){
          #Replace k set with improved set(k + 1 minus the least contributor):
          myListOfSets[[k]] = indexWithLeastEffect
          myListOfScores[[k]] = scoreOfLeastEffect
          maxScore = scoreOfLeastEffect #Set new max score
          
          #Find the index with least effect and remove it:
          leastEffect = getLeastEffect(myData, classes, myListOfSets[k])
          indexWithLeastEffect = leastEffect$index
          scoreOfLeastEffect = leastEffect$score
          #If removing it does not improve score of k-1:
          if(scoreOfLeastEffect <= myListOfScores[[k-1]]){
            #Set k-1 to the new set, and recalculate cost and set k = k-1
            improved = FALSE
            k = k-1
          }
        }
      }
    }
  }
  return(myListOfSets)
}
  