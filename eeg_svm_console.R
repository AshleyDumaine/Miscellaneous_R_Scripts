library("e1071")#
macrofilter <- function(input) {#
	threshold <- 1000#
	window <- 30#
	len <- length(input)#
	range <- seq(len - window)#
	for(i in seq_along(range)){#
		gap <- abs(input[i + window] - input[i])#
  		if (gap > threshold) {#
  			end <- i + window#
  			subrange <- seq(i, end, by = 1)#
  			for (j in seq_along(subrange)) {#
  				input[j + i - 1] <- input[j] #flatline the spike#
  			}#
  		}#
	}#
	return(input)#
}#

trainingdata <<- read.csv(file="~/Desktop/AllExprs.csv", header = F)
data2 <<- read.csv(file="~/Desktop/SpeedRun.csv", header = F)#

trainingdata[,4] <- macrofilter(trainingdata[,4])#
trainingdata[,5] <- macrofilter(trainingdata[,5])#
data2[,4] <- macrofilter(data2[,4])#
data2[,5] <- macrofilter(data2[,5])#
train <- cbind(trainingdata[,23],trainingdata[,seq(4,19)])
test <- cbind(data2[,23],data2[,seq(4,19)])
names(train)[1] = 'Blink'
names(test)[1] = 'Blink'
testdata <- test[,-1]
model <- svm(Blink~.,data=train,type='C-classification')
prediction <- predict(model,testdata)
tab <- table(pred = prediction, true = test[,1])





trainingdata <<- read.csv(file="~/Desktop/AllExprs.csv", header = F)#
testdata <<- read.csv(file="~/Desktop/SpeedRun.csv", header = F)#
trainingdata[,4] <- macrofilter(trainingdata[,4])#
trainingdata[,5] <- macrofilter(trainingdata[,5])#
testdata[,4] <- macrofilter(testdata[,4])#
testdata[,5] <- macrofilter(testdata[,5])#
train <- cbind(trainingdata[,seq(23,38)],trainingdata[,seq(4,19)])# 
test <- cbind(testdata[,seq(23,38)],testdata[,seq(4,19)])#
names(train)[1] = 'Blink'#
names(train)[2] = 'LeftWink'#
names(train)[3] = 'RightWink'#
names(train)[4] = 'Furrow'#
names(train)[5] = 'RaiseBrow'#
names(train)[6] = 'Smile'#
names(train)[7] = 'Clench'#
names(train)[8] = 'LookLeft'#
names(train)[9] = 'LookRight'#
names(train)[10] = 'Laugh'#
names(train)[11] = 'SmirkLeft'#
names(train)[12] = 'SmirkRight'#
model <- svm(Blink+LeftWink+RightWink+Furrow+RaiseBrow+Smile+Clench+LookLeft+LookRight+Laugh+SmirkLeft+SmirkRight~.,data=train,type='C-classification', kernel='linear')#
prediction <- predict(model,test)#
tab <- table(pred = prediction, true = test[,1])#
