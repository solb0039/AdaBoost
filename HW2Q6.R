
library(gbm)
col.Classes=c(income="numeric", sex="factor", marital.status="factor", age="numeric", education="numeric", occup="factor", time.here="numeric", dual.incomes= "factor", persons="numeric", persons.under18="numeric", owner.status="factor", home.type="factor", ethnicity="factor", language="factor")

#colClasses=col.Classes,

#Read in data
incomeData<-read.csv(file="Income_Data.txt", header=FALSE,  col.names=names(col.Classes))

#create loop to scan through model parameters and output matrix of resutlts
set.seed(102)

samp<- sample(1:nrow(incomeData), 0.8*nrow(incomeData), replace=F)
trainIncomeData<- incomeData[samp,]
testIncomeData<- incomeData[-samp,]

shrinks<- as.vector(c(0.05, 0.03, 0.01, 0.009, 0.007, 0.005, 0.001))
matrix6<- matrix(nrow=20,ncol=8)
for (i in 4:6){
    for (j in 1:7){

    gbmIncome<-gbm(income~.,data=trainIncomeData, train.fraction=1, interaction.depth=i, shrinkage=shrinks[j],
    n.trees=5000, bag.fraction=0.5, cv.folds=5, distribution="gaussian", verbose=F)

    opt.its<- gbm.perf(gbmIncome, method="cv")

    income.predict<-predict(gbmIncome,testIncomeData,type="response", n.trees=opt.its)

    results6<-data.frame(predict=income.predict, actual=testIncomeData[,1] )

    error<- sum(results6[,1]-results6[,2])^2/dim(results6)[[1]]

    matrix6[j,i]<- error

 }
}


gbmIncome<-gbm(income~.,data=trainIncomeData, train.fraction=1, interaction.depth=4, shrinkage=.001,
n.trees=5000, bag.fraction=0.5, cv.folds=5, distribution="gaussian", verbose=F)

opt.its<- gbm.perf(gbmIncome, method="cv")

income.predict<-predict(gbmIncome,testIncomeData,type="response", n.trees=opt.its)

results<-data.frame(predict=income.predict, actual=testIncomeData[,1] )

error<- sum(results[,1]-results[,2])^2/dim(results)[[1]]

#permutation plot
summary(gbmIncome, method=permutation.test.gbm)

par(mfrow=c(3,4))
for (i in 1:12){
    title<- sprintf("%s", names(incomeData)[[i+1]])
    plot(x=gbmIncome ,i.var=i, n.trees=opt.its , main=title)
}

plot(x=gbmIncome ,i.var=c(1,3), n.trees=opt.its , main="")


1,6 1,7 1,8 1,9 1,12 1,13



