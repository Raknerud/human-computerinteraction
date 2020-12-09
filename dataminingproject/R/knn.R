setwd("C:/Users/Raknerud/Documents/GitHub/introtodatamining/dataminingproject/R")
dataFile = "supermarket.txt"
X=as.data.frame(read.table(dataFile))
Xtrain=X[1:500,c(2,7,8,9,10)]
Xtrain[,2:5]=lapply(Xtrain[,2:5], function(x) as.numeric(as.character(x)))
print(Xtrain[2,2]-Xtrain[2,3])
n=dim(Xtrain)[1]
Xtest=X[501:1000, c(2,7,8,9,10)]
nn=dim(Xtest)[1]
k=7
euclDistance <- function(x,y) {
  return (sqrt(sum((x-y)^2)))
}
labelGuess='A'
trueA=0
trueB=0
trueC=0
falseA=0
falseB=0
falseC=0
for(element in 1:nn){
  print(element)
  point=(Xtest[element,2:5])
  PointDistances = apply(Xtrain[,2:5], 1, function(x) euclDistance(point,x))
  neighbors = sort(PointDistances,index.return=TRUE)$ix
  kneighbors = neighbors[1:k]
  totalA=0
  totalB=0
  totalC=0
  for(w in 1:k){
    index=kneighbors[w]
    if(Xtrain[index,1]=='A'){
      totalA=totalA+1
    }
    else if(Xtrain[index,1]=='B'){
      totalB=totalB+1
    }
    else{
      totalC=totalC+1
    }
  }
  if(totalA>=totalB & totalA>=totalC){
    labelGuess='A'
    if(Xtest[element,1]==labelGuess){
      trueA=trueA+1
    }
    else{
      falseA=falseA+1
    }
  }
  else if(totalB>=totalA & totalB>=totalC){
    labelGuess='B'
    if(Xtest[element,1]==labelGuess){
      trueB=trueB+1
    }
    else{
      falseB=falseB+1
    }
  }
  else{
    labelGuess='C'
    if(Xtest[element,1]==labelGuess){
      trueC=trueC+1
    }
    else{
      falseC=falseC+1
    }
  }

}
accuracy=(trueA+trueB+trueC)/(trueA+trueB+trueC+falseA+falseB+falseC)
print(accuracy)

