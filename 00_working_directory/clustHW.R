numcases <- 5000                           #how many cases to generate

myData = iris[1:4]

library(random)
r <- randomNumbers(numcases, max = nrow(myData), col = 1)
myData = myData[r, ]

d1 = dist(myData)
hc = hclust(d1,"ave")
d2 = cophenetic(x=hc)
correlation = cor(d1,d2)

print(paste("The correlation is: ", correlation))