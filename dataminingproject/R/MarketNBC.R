




setwd("C:/Users/dd-sa/OneDrive/Documents/GitHub/introtodatamining/dataminingproject/R")

trainingFile = "irisTraining.txt"
testingFile = "irisTesting.txt"
Xtrain = as.matrix(read.table(trainingFile))
n = dim(Xtrain)[1]
d = dim(Xtrain)[2] - 1 # the last attribute is the class label, so it does not count.
#Training... Collect mean and standard deviation for each dimension for each class..
#Also, calculate P(C+) and P(C-)
idp = which(Xtrain[,d+1] ==1) # points that have 1 as the class label
np = length(idp)
Xpositive = Xtrain[idp,1:d]

#Testing .....
Xtest=as.matrix(read.table(testingFile))
nn = dim(Xtest)[1] # Number of points in the testing data.

tp = 0 #True Positive
fp = 0 #False Positive
tn = 0 #True Negative
fn = 0 #False Negative


for (i in 1:nn) {

  #For each point find the P(C+|Xi) and P(C-|Xi) and decide if the point belongs to C+ or C-..
  #Recall we need to calculate P(Xi|C+)*P(C+) ..
  #P(Xi|C+) = P(Xi1|C+) * P(Xi2|C+)....P(Xid|C+)....Do the same for P(Xi|C-)
  #Now that you've calculate P(Xi|C+) and P(Xi|C-), we can decide which is higher
  #P(Xi|C-)*P(C-) or P(Xi|C-)*P(C-) ..
  #increment TP,FP,FN,TN accordingly, remember the true lable for the ith point is in Xtest[i,(d+1)]

}
