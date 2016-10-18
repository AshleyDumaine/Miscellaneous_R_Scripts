trainingdata <<- read.csv(file="~/Desktop/AllExprs.csv", header = F)#
ts <- cbind(trainingdata[,23],trainingdata[,seq(4,19)])#
names(ts)[1] = 'Blink'#
train <- ts[seq(1,20000),]#
test <- ts[seq(20001,nrow(ts)),]#
testdata <- test[,-1]#
model <- svm(Blink~.,data=train,type='C-classification', kernel='linear')#
prediction <- predict(model,testdata)#
tab <- table(pred = prediction, true = test[,1])#

