#setwd("C:/Users/Raknerud/Documents/GitHub/introtodatamining/dataminingproject/R")
#set the directory and read from file to X.
setwd("C:/Users/dd-sa/OneDrive/Documents/GitHub/introtodatamining/dataminingproject/R")
dataFile = "supermarket.txt"
X=as.data.frame(read.table(dataFile))


#Branch Classifier: Branch in Column 2, columns other than 7, or 9 can be used if they contain numeric data.
Xtrain=X[1:500,c(2,7,9)]
n=dim(Xtrain)[1]
Xtest=X[501:1000, c(2,7,9)]
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
  point=(Xtest[element,2:3])
  PointDistances = apply(Xtrain[,2:3], 1, function(x) euclDistance(point,x))
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


##Gender Classifier: Gender in column 5, other columns can be changed to work with different numerics.
Xtrain=X[1:500,c(5,10, 16)]
n=dim(Xtrain)[1]
Xtest=X[501:1000, c(5,10,16)]
nn=dim(Xtest)[1]
k=3
tM=0
tF=0
fM=0
fF=0
for(element in 1:nn){
  print(element)
  point=(Xtest[element,2:3])
  PointDistances = apply(Xtrain[,2:3], 1, function(x) euclDistance(point,x))
  neighbors = sort(PointDistances,index.return=TRUE)$ix
  kneighbors = neighbors[1:k]
  totalM=0
  totalF=0
  for(w in 1:k){
    index=kneighbors[w]
    if(Xtrain[index,1]=='Male'){
      totalM=totalM+1
    }
    else{
      totalF=totalF+1
    }

  }
  if(totalM>=totalF){
    labelGuess='Male'
    if(Xtest[element,1]==labelGuess){
      tM=tM+1
    }
    else{
      fM=fM+1
    }
  }
  else{
    labelGuess='Female'
    if(Xtest[element,1]==labelGuess){
      tF=tF+1
    }
    else{
      fF=fF+1
    }
  }

}
accuracy=(tM+tF)/(tM+tF+fM+fF)
print(accuracy)


##Membership Classifier: Membership in column 4, other columns can be changed to work with different numerics.
Xtrain=X[1:500,c(4,8,17)]
n=dim(Xtrain)[1]
Xtest=X[501:1000, c(4,8,17)]
nn=dim(Xtest)[1]
k=25
tM=0
tN=0
fM=0
fN=0
for(element in 1:nn){
  point=(Xtest[element,2:3])
  PointDistances = apply(Xtrain[,2:3], 1, function(x) euclDistance(point,x))
  neighbors = sort(PointDistances,index.return=TRUE)$ix
  kneighbors = neighbors[1:k]
  totalM=0
  totalN=0
  for(w in 1:k){
    index=kneighbors[w]
    if(Xtrain[index,1]=='Member'){
      totalM=totalM+1
    }
    else{
      totalN=totalN+1
    }

  }
  if(totalM>=totalN){
    labelGuess='Member'
    if(Xtest[element,1]==labelGuess){
      tM=tM+1
    }
    else{
      fM=fM+1
    }
  }
  else{
    labelGuess='Normal'
    if(Xtest[element,1]==labelGuess){
      tN=tN+1
    }
    else{
      fN=fN+1
    }
  }

}

accuracy=(tM+tN)/(tM+tN+fM+fN)
print(accuracy)



