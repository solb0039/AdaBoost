setwd("/Users/Sean/Documents/School/Stats315B/Homework2")

library(gbm)
reflabs<-c("Occupation", "HomeType", "Sex", "Maritial", "Age", "Education", "HouseIncome", "TimeInBay", "DualIncomes", "PersonsHouse", "PersonsUnder18", "HomeOwnerStatus", "Ethnicity", "Language")

col.Classes=c(Occupation="factor",HomeType="factor", Sex="factor",Maritial="factor", Age="numeric",Education="numeric",HouseIncome="numeric",TimeInBay="numeric",DualIncomes="factor",PersonsHouse="numeric", PersonsUnder18="numeric", HomeOwnerStatus="factor", Ethnicity="factor", Language="factor")

occupData<- read.csv(file="Occupation_Data.txt", header=FALSE, sep=",") , col.names=names(col.Classes), colClasses=col.Classes)

colnames(occupData)<-reflabs

set.seed(201)
aSamp<- sample(1:nrow(occupData), 0.8*nrow(occupData), replace=F)
trainOccupData<- occupData[aSamp,]
testOccupData<- occupData[-aSamp,]

gbmOccupation<-gbm(Occupation~.,data=trainOccupData, train.fraction=1, interaction.depth=4, shrinkage=.01,
n.trees=5000, bag.fraction=0.5, cv.folds=5, distribution="multinomial", verbose=T)

opt.trees<- gbm.perf(gbmOccupation, method="cv")

occup.predict<- predict(gbmOccupation, testOccupData, type="response", n.tree=opt.trees)

pred<- matrix( nrow= (dim(occup.predict)[[1]]) , ncol=dim(occup.predict)[[2]] )

#convert probabilities into 1's and 0's
for (i in 1:(dim(occup.predict)[[1]]) ){
    for (j in 1:(dim(occup.predict)[[2]]) ) {
        ifelse(occup.predict[i,j,1] > 0.5, pred[i,j] <- 1, pred[i,j] <-  0)
        
    }
}

#Determine misclassification errors
actual<- model.matrix(~testOccupData$Occupation-1)
net<- pred-actual
classErrors<- apply(net,2,function(x){sum(x==-1)})
classErrorRates<- classErrors/dim(net)[[1]]

totalError <- sum(classErrors)
(totalErrorRate<- totalError/ (dim(a)[[1]]))

#Partial dependence plots
par(mfrow=c(4,4))
for (i in 2:13){
    #title<- sprintf("%s", names(occupData)[[i+1]])
    plot(x=gbmOccupation , i.var=c(i), n.trees=opt.trees , main="title")
}


