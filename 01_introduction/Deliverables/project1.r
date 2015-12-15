#Start of script:
rm(list=ls(all=TRUE)) 

#Reading in the Iris dataset and storing it in a variable:
irisVariable = iris

#Printing the data:
print(irisVariable)

#Printing the list of variables (just the 1 so far)
print(ls()) 

#Writing the Iris data to a CSV file:
write.table(irisVariable,"IRIS.csv", col.names=TRUE, sep=",")

#Reading the CSV file into another variable:
irisCSV = read.csv("IRIS.csv")

#Prints out the last row of Iris data:
print(tail(irisCSV,1))

#Prints out the data type of the variable:
print(class(irisCSV))

#Prints out the first row of Iris data:
print(head(irisCSV,1))
      
#Do a tapply command:
#print(tapply(irisCSV, seq(1,5), print()))
tapply(irisCSV, c(1:length(iris)), print)