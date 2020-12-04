# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

setwd("C:/Users/Raknerud/Documents/GitHub/introtodatamining/dataminingproject/R")
dataFile = "murder_2016_final.txt"
dataFile2= "murder_2015_final.txt"
Xtest=as.matrix(read.table(dataFile))
Xtest=apply(Xtest[ , 4], 1, function(x) as.numeric(as.character(x)))
Xtrain=as.matrix(read.table(dataFile2))
n=dim(X)[1]
nn=dim(Xtest)[1]

euclDistance <- function(x,y) {
  return (sqrt(sum((x-y)^2)))
}
Xtest[1]
rightGuess=0
wrongGuess=0
for(element in 1:nn){

  minimumMurderDistance=9999
  positionTracker=1
  for(testElement in 1:n){
    avgPrevYears=(as.numeric(Xtrain[testElement,3])+as.numeric(Xtrain[testElement,4]))/2
    z=abs(as.numeric(Xtest[element,4])-avgPrevYears)
    if(z<minimumMurderDistance){
      minimumMurderDistance=z
      positionTracker=testElement
    }
  }
  if(Xtest[element,1]==Xtrain[positionTracker,1]){
    rightGuess=rightGuess+1
  }
  else{
    wrongGuess=wrongGuess+1
  }
}
print(rightGuess/wrongGuess)
if("dog"=="dog"){
  print("Hello")
}




