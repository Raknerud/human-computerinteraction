




setwd("C:/Users/dd-sa/OneDrive/Documents/GitHub/introtodatamining/dataminingproject/R")

dataFile = "supermarket.txt"
marketData=as.data.frame.matrix(read.table(dataFile))
aData=subset(marketData,marketData[2]=="A")
bData=subset(marketData,marketData[2]=="B")
cData=subset(marketData,marketData[2]=="C")

firstHalf=marketData[1:500,c(5,14,16,17)]
secondHalf=marketData[501:1000,c(5,14,16,17)]


Xtrain =firstHalf

n = dim(Xtrain)[1]#Rows
d = dim(Xtrain)[2]#Columns

idp = which(Xtrain[ ,1] =="Male") #Points that are male in the first position
np = length(idp)
Xpositive = Xtrain[idp,2:d]
avgPositive=colMeans(Xpositive)

sdp=apply(Xpositive,2,sd)

idn = which(Xtrain[,1] =="Female")

pn=length(idn)/n
Xnegative= Xtrain[idn,2:d]
avgNegative=colMeans(Xnegative)
sdn=apply(Xnegative,2,sd)

#Test
Xtest=secondHalf
nn = dim(Xtest)[1] # Number of points in the testing data.


tp = 0 #True Positive
fp = 0 #False Positive
tn = 0 #True Negative
fn = 0 #False Negative


for (i in 1:nn) {

  pv=dnorm(c(Xtest[i,2],Xtest[i,3],Xtest[i,4]),avgPositive,sdp )
  PxPos=prod(pv)
  productPos=prod(PxPos,np)
  nv=dnorm(c(Xtest[i,2],Xtest[i,3],Xtest[i,4]),avgNegative,sdn )
  PxNeg=prod(nv)
  productNeg=prod(PxNeg,pn)

  if(productPos >= productNeg){
    expectVal=1
  }else{expectVal=-1}
  if(Xtest[i,1]=="Male"){
    if(expectVal==1){
      tp=tp+1
    }else{
      fp=fp+1
    }
  }
  else{
    if(Xtest[i,1]=="Female"){
      if(expectVal==1){
        tn=tn+1
      }else{
        fn=fn+1
      }
    }
  }

}


#Calculate all the measures required..
Accuracy=(tp+tn)/(tp+fp+tn+fn)
precision=(tp)/(tp+fp)
recall=(tp)/(tp+fn)
testData <- matrix(c(Accuracy, tp, tn, fp, fn, precision, recall),ncol=7, byrow = TRUE)
colnames(testData)<-c("Accuracy", "TP","TN","FP", "FN", "Precision", "Recall")
testData <- as.table(testData)
testData

write.table(testData, file = "testRatingsData.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
