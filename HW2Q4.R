library(gbm)
spam<-read.table(file="Spam_Train.txt", sep=",")

rflabs<-c("make", "address", "all", "3d", "our", "over", "remove",
"internet","order", "mail", "receive", "will",
"people", "report", "addresses","free", "business",
"email", "you", "credit", "your", "font","000","money",
"hp", "hpl", "george", "650", "lab", "labs",
"telnet", "857", "data", "415", "85", "technology", "1999",
"parts","pm", "direct", "cs", "meeting", "original", "project",
"re","edu", "table", "conference", ";", "(", "[", "!", "$", "#",
"CAPAVE", "CAPMAX", "CAPTOT","type")
# Names for predictors and response

colnames(spam)<-rflabs

#count no.1 and 0 in dataset
sum1<-0
sum0<-0
for (i in 1:nrow(spam)){
    if (spam[i,58]==1){sum1<-sum1+1}
    if (spam[i,58]==0){sum0<-sum0+1}
    i<-i+1
}

#randomize rows
set.seed(131)
x<-spam[sample(nrow(spam)),]

set.seed(444)# Random for bag.fraction
gbm0<-gbm(type~.,data=x,train.fraction=1.0, interaction.depth=4, shrinkage=.05,
n.trees=2500,bag.fraction=0.5,cv.folds=5, distribution="bernoulli",verbose=T)

#create plot of training and test error(red)
#gbm.perf(gbm0,method="test")

#create plot of training and test error and cross validatgion error(green)
spam.its<-gbm.perf(gbm0,method="cv")
#min at 1117 iterations

#predictions for responses converted to probabilities
gbm0.predict<-predict(gbm0,x,type="response", n.trees=spam.its)

#estimate the training error
training_error<-  x[,58] - round(gbm0.predict)
spam_misclass<- 0
non_spam_misclass<- 0
for(i in 1:length(training_error)){
    if (training_error[[i]] == -1){
        non_spam_misclass <- non_spam_misclass +1 }
    if (training_error[[i]] == 1){
        spam_misclass<- spam_misclass +1 }
    i<-i+1
}

spam_misclass_rate<- spam_misclass/length(training_error)
non_spam_misclass_rate<- non_spam_misclass/length(training_error)
spam_misclass_rate
non_spam_misclass_rate
#result is 0.163%% and 0.033%

#read in test data

spam_test<-read.table(file="Spam.Test.txt", sep=",")
colnames(spam_test)<-rflabs

xtest<-spam_test[sample(nrow(spam_test)),]
gbm0.testpredict<-predict(gbm0,xtest,type="response",n.trees=1117)

test_error<-  xtest[,58] - round(gbm0.testpredict)

testspam_misclass<- 0
testnon_spam_misclass<- 0
for(i in 1:length(test_error)){
    if (test_error[[i]] == -1){
        testnon_spam_misclass <- testnon_spam_misclass +1 }
    if (test_error[[i]] == 1){
        testspam_misclass<- testspam_misclass +1 }
    i<-i+1
}
testspam_misclass_rate<-  testspam_misclass/length(test_error)
testnon_spam_misclass_rate<- testnon_spam_misclass/length(test_error)
testspam_misclass_rate
testspam_misclass_rate
#result is  and 1.04% and 1.04%

#create weight vector
spamweight<- 24
newweights<- abs(( x[,58]-1 )*spamweight )+1

set.seed(444)# Random for bag.fraction
gbmweighted<-gbm(type~.,data=x,train.fraction=1, interaction.depth=4, shrinkage=.05,
n.trees=2500,bag.fraction=0.5,cv.folds=5, weights=newweights, distribution="bernoulli",verbose=T)

opttree<- gbm.perf(gbmweighted,method="cv")
#optimal is 730

gbmweighted.predict<- predict(gbmweighted, x, type="response", n.trees=opttree , w=newweights)

weighted_train_error<- x[,58] - round(gbmweighted.predict)

weightedspam_misclass<- 0
weightednon_spam_misclass<- 0
for(i in 1:length(weighted_train_error)){
    if (weighted_train_error[[i]] == 1){
        weightedspam_misclass <- weightedspam_misclass +1 }
    if (weighted_train_error[[i]] == -1){
        weightednon_spam_misclass<- weightednon_spam_misclass +1 }
    i<-i+1
}

weightedspam_misclass_rate<-   weightedspam_misclass /length(weighted_train_error)
weightednon_spam_misclass_rate<- weightednon_spam_misclass/length(weighted_train_error)

weightedspam_misclass_rate
weightednon_spam_misclass_rate

#now check test set with weights
testweights<- abs(( xtest[,58]-1 )*spamweight )+1

gbmweighted.test<- predict(gbmweighted, xtest, type="response", n.trees=opttree , w=newweights)

weighted_test_error<- xtest[,58] - round(gbmweighted.test)

weightedspam_test_misclass<- 0
weightednon_spam_test_misclass<- 0

for(i in 1:length(weighted_test_error)){
    if (weighted_test_error[[i]] == 1){
        weightedspam_test_misclass <- weightedspam_test_misclass +1 }
    if (weighted_test_error[[i]] == -1){
        weightednon_spam_test_misclass<- weightednon_spam_test_misclass +1 }
    i<-i+1
}

weightedspam_test_misclass_rate<-   weightedspam_test_misclass /length(weighted_test_error)
weightednon_spam_test_misclass_rate<- weightednon_spam_test_misclass/length(weighted_test_error)

weightedspam_test_misclass_rate
weightednon_spam_test_misclass_rate

summary(gbmweighted)

#Partial dependence plots
par(mfrow=c(4,4))
for (i in 17:32){
    #title<- sprintf("%s", names(occupData)[[i+1]])
    plot(x=gbmweighted , i.var=i, n.trees=opt.trees , main="title")
}



