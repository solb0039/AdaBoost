setwd("/Users/Sean/Documents/School/Stats315B/Homework2")

library(gbm)
housedata<-read.table(file="California_Data.txt", sep=",")
houselabs<-c("value", "income", "age", "rooms", "bedrooms", "population", "occup",
"latitude","longitude")

colnames(housedata)<- houselabs

#split data set
set.seed(100)

aSample<- sample(1:nrow(housedata), 0.8*nrow(housedata), replace=F)
trainHouseData<- housedata[aSample,]
testHouseData<- housedata[-aSample,]

#iterate through parameters to find best model
shrinks<- as.vector(c(0.05, 0.03, 0.01, 0.009, 0.007, 0.005, 0.001))

amatrix<- matrix(nrow=20,ncol=8)
for (i in 4:6){
    for (j in 1:7){
        
        gbmhouse<-gbm(value~.,data=trainHouseData,train.fraction=1, interaction.depth=i , shrinkage=shrinks[j],
        n.trees=5000,bag.fraction=0.5,cv.folds=5, distribution="gaussian",verbose=T)
        
        #gbm.perf(gbmhouse,method="test") #min at 1920 iterations
        
        optimal.iterations<- gbm.perf(gbmhouse,method="cv") #min at 2497 iterations
        
        house.predict<- predict(gbmhouse, testHouseData, type="response", n.trees=optimal.iterations)
        
        house_results<-data.frame(predict=house.predict, actual=testHouseData[,1] )
        
        house_error<- sum(house_results[,1]-house_results[,2])^2/dim(house_results)[[1]]
        
        amatrix[j,i]<- house_error
    }
}

#for the best model error was 0.23%, shirnkage = 0.005 and treedepth=4

gbmhouse<-gbm(value~.,data=trainHouseData,train.fraction=1, interaction.depth=4, shrinkage=.005,
n.trees=5000,bag.fraction=0.5,cv.folds=5, distribution="gaussian",verbose=T)

optimal.iterations<- gbm.perf(gbmhouse,method="cv") #min at 2497 iterations

house.predict<- predict(gbmhouse, testHouseData, type="response", n.trees=optimal.iterations)

house_results<-data.frame(predict=house.predict, actual=testHouseData[,1] )

house_error<- sum(house_results[,1]-house_results[,2])^2/dim(house_results)[[1]]
#result is 0.34% (0.00345)

#permutation plot
summary(gbmhouse, method=permutation.test.gbm)

#partial dependence plot

par(mfrow=c(3,3))
for (i in 1:8){
 title<- sprintf("%s", houselabs[[i+1]])
plot(x=gbmhouse ,i.var=i, n.trees=optimal.iterations , main=title)
}

#plot interactions
plot(x=gbmhouse ,i.var=c(4,1), n.trees=optimal.iterations , main="a title")



