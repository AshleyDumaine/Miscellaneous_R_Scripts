library("neuralnet")#

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

normalize <- function(input, upperLimit, lowerLimit) {#
	(input - lowerLimit) / (upperLimit - lowerLimit)#
}#

format_input <- function(file_in, file_out) {
trainingdata <<- read.csv(file=file_in, header = F)#"~/Desktop/SHORT.csv"#
max <- nrow(trainingdata)#
f1 <- (trainingdata[,1])#
f2 <- (trainingdata[,2])#
f3 <-(trainingdata[,3])#
f4 <-(trainingdata[,4])#
f5 <-(trainingdata[,5])#
f6 <-(trainingdata[,6])#
f7 <-(trainingdata[,7])#
f8 <-(trainingdata[,8])#
f9 <-(trainingdata[,9])#
f10 <-(trainingdata[,10])#
f11 <-(trainingdata[,11])#
f12 <-(trainingdata[,12])#
f13 <-(trainingdata[,13])#
f14 <-(trainingdata[,14])#

raw1 <- (trainingdata[,15])#
raw2 <- (trainingdata[,16])#
raw3 <-(trainingdata[,17])#
raw4 <-(trainingdata[,18])#
raw5 <-(trainingdata[,19])#
raw6 <-(trainingdata[,20])#
raw7 <-(trainingdata[,21])#
raw8 <-(trainingdata[,22])#
raw9 <-(trainingdata[,23])#
raw10 <-(trainingdata[,24])#
raw11 <-(trainingdata[,25])#
raw12 <-(trainingdata[,26])#
raw13 <-(trainingdata[,27])#
raw14 <-(trainingdata[,28])#

#median filter here
for (i in 1:max) {
	med <- median(c(f1[i], f2[i], f3[i], f4[i], f5[i], f6[i], f7[i], f8[i], f9[i], f10[i], f11[i], f12[i], f13[i], f14[i]))
	f1[i] <- f1[i] - med
	f2[i] <- f2[i] - med
	f3[i] <- f3[i] - med
	f4[i] <- f4[i] - med
	f5[i] <- f5[i] - med
	f6[i] <- f6[i] - med
	f7[i] <- f7[i] - med
	f8[i] <- f8[i] - med
	f9[i] <- f9[i] - med
	f10[i] <- f10[i] - med
	f11[i] <- f11[i] - med
	f12[i] <- f12[i] - med
	f13[i] <- f13[i] - med
	f14[i] <- f14[i] - med
}

sensor_1 <- as.vector(mapply(normalize, f1, MoreArgs = list(max(f1, na.rm=TRUE), min(f1, na.rm=TRUE)) ))#
sensor_2 <-as.vector(mapply(normalize, f2, MoreArgs = list(max(f2, na.rm=TRUE), min(f2, na.rm=TRUE)) ))#
sensor_3 <-as.vector(mapply(normalize, f3, MoreArgs = list(max(f3, na.rm=TRUE), min(f3, na.rm=TRUE)) ))#
sensor_4 <-as.vector(mapply(normalize, f4, MoreArgs = list(max(f4, na.rm=TRUE), min(f4, na.rm=TRUE)) ))#
sensor_5  <-as.vector(mapply(normalize, f5, MoreArgs = list(max(f5, na.rm=TRUE), min(f5, na.rm=TRUE)) ))#
sensor_6  <-as.vector(mapply(normalize, f6, MoreArgs = list(max(f6, na.rm=TRUE), min(f6, na.rm=TRUE)) ))#
sensor_7 <-as.vector(mapply(normalize, f7, MoreArgs = list(max(f7, na.rm=TRUE), min(f7, na.rm=TRUE)) ))#
sensor_8  <-as.vector(mapply(normalize, f8, MoreArgs = list(max(f8, na.rm=TRUE), min(f8, na.rm=TRUE)) ))#
sensor_9  <-as.vector(mapply(normalize, f9, MoreArgs = list(max(f9, na.rm=TRUE), min(f9, na.rm=TRUE)) ))#
sensor_10  <-as.vector(mapply(normalize, f10, MoreArgs = list(max(f10, na.rm=TRUE), min(f10, na.rm=TRUE)) ))#
sensor_11  <-as.vector(mapply(normalize, f11, MoreArgs = list(max(f11, na.rm=TRUE), min(f11, na.rm=TRUE)) ))#
sensor_12  <-as.vector(mapply(normalize, f12, MoreArgs = list(max(f12, na.rm=TRUE), min(f12, na.rm=TRUE)) ))#
sensor_13  <-as.vector(mapply(normalize, f13, MoreArgs = list(max(f13, na.rm=TRUE), min(f13, na.rm=TRUE)) ))#
sensor_14  <-as.vector(mapply(normalize, f14, MoreArgs = list(max(f14, na.rm=TRUE), min(f14, na.rm=TRUE)) ))#

rsensor_1 <- as.vector(mapply(normalize, raw1, MoreArgs = list(max(raw1, na.rm=TRUE), min(raw1, na.rm=TRUE)) ))#
rsensor_2 <-as.vector(mapply(normalize, raw2, MoreArgs = list(max(raw2, na.rm=TRUE), min(raw2, na.rm=TRUE)) ))#
rsensor_3 <-as.vector(mapply(normalize, raw3, MoreArgs = list(max(raw3, na.rm=TRUE), min(raw3, na.rm=TRUE)) ))#
rsensor_4 <-as.vector(mapply(normalize, raw4, MoreArgs = list(max(raw4, na.rm=TRUE), min(raw4, na.rm=TRUE)) ))#
rsensor_5  <-as.vector(mapply(normalize, raw5, MoreArgs = list(max(raw5, na.rm=TRUE), min(raw5, na.rm=TRUE)) ))#
rsensor_6  <-as.vector(mapply(normalize, raw6, MoreArgs = list(max(raw6, na.rm=TRUE), min(raw6, na.rm=TRUE)) ))#
rsensor_7 <-as.vector(mapply(normalize, raw7, MoreArgs = list(max(raw7, na.rm=TRUE), min(raw7, na.rm=TRUE)) ))#
rsensor_8  <-as.vector(mapply(normalize, raw8, MoreArgs = list(max(raw8, na.rm=TRUE), min(raw8, na.rm=TRUE)) ))#
rsensor_9  <-as.vector(mapply(normalize, raw9, MoreArgs = list(max(raw9, na.rm=TRUE), min(raw9, na.rm=TRUE)) ))#
rsensor_10  <-as.vector(mapply(normalize, raw10, MoreArgs = list(max(raw10, na.rm=TRUE), min(raw10, na.rm=TRUE)) ))#
rsensor_11  <-as.vector(mapply(normalize, raw11, MoreArgs = list(max(raw11, na.rm=TRUE), min(raw11, na.rm=TRUE)) ))#
rsensor_12  <-as.vector(mapply(normalize, raw12, MoreArgs = list(max(raw12, na.rm=TRUE), min(raw12, na.rm=TRUE)) ))#
rsensor_13  <-as.vector(mapply(normalize, raw13, MoreArgs = list(max(raw13, na.rm=TRUE), min(raw13, na.rm=TRUE)) ))#
rsensor_14  <-as.vector(mapply(normalize, raw14, MoreArgs = list(max(raw14, na.rm=TRUE), min(raw14, na.rm=TRUE)) ))#


#for (i in 1:window_length) {
#make AF3
#tack on another sample for 1 to window length
#sensor_1[i:(max-(window_length-i))]
#}
#tack on raw data
#rsensor_1[window_length:(max)]

AF3_0 <<- sensor_1[1:(max - 4)]#
AF3_1 <<- sensor_1[2:(max - 3)]#
AF3_2 <<- sensor_1[3:(max - 2)]#
AF3_3 <<- sensor_1[4:(max - 1)]#
AF3_4 <<- sensor_1[5:(max)]#
AF3_raw <<- rsensor_1[5:(max)]#

AF4_0 <<- sensor_2[1:(max - 4)]#
AF4_1 <<- sensor_2[2:(max - 3)]#
AF4_2 <<- sensor_2[3:(max - 2)]#
AF4_3 <<- sensor_2[4:(max - 1)]#
AF4_4 <<- sensor_2[5:(max)]#
AF4_raw <<- rsensor_2[5:(max)]#

F3_0 <<- sensor_3[1:(max - 4)]#
F3_1 <<- sensor_3[2:(max - 3)]#
F3_2 <<- sensor_3[3:(max - 2)]#
F3_3 <<- sensor_3[4:(max - 1)]#
F3_4 <<- sensor_3[5:(max)]#
F3_raw <<- rsensor_3[3:(max - 2)]#

F4_0 <<- sensor_4[1:(max - 4)]#
F4_1 <<- sensor_4[2:(max - 3)]#
F4_2 <<- sensor_4[3:(max - 2)]#
F4_3 <<- sensor_4[4:(max - 1)]#
F4_4 <<- sensor_4[5:(max)]#
F4_raw <<- rsensor_4[3:(max - 2)]#

FC5_0 <<- sensor_5[1:(max - 4)]#
FC5_1 <<- sensor_5[2:(max - 3)]#
FC5_2 <<- sensor_5[3:(max - 2)]#
FC5_3 <<- sensor_5[4:(max - 1)]#
FC5_4 <<- sensor_5[5:(max)]#
FC5_raw <<- rsensor_5[3:(max - 2)]#

FC6_0 <<- sensor_6[1:(max - 4)]#
FC6_1 <<- sensor_6[2:(max - 3)]#
FC6_2 <<- sensor_6[3:(max - 2)]#
FC6_3 <<- sensor_6[4:(max - 1)]#
FC6_4 <<- sensor_7[5:(max)]#
FC6_raw <<- rsensor_6[3:(max - 2)]#

F7_0 <<- sensor_7[1:(max - 4)]#
F7_1 <<- sensor_7[2:(max - 3)]#
F7_2 <<- sensor_7[3:(max - 2)]#
F7_3 <<- sensor_7[4:(max - 1)]#
F7_4 <<- sensor_7[5:(max)]#
F7_raw <<- rsensor_7[3:(max - 2)]#

F8_0 <<- sensor_8[1:(max - 4)]#
F8_1 <<- sensor_8[2:(max - 3)]#
F8_2 <<- sensor_8[3:(max - 2)]#
F8_3 <<- sensor_8[4:(max - 1)]#
F8_4 <<- sensor_8[5:(max)]#
F8_raw <<- rsensor_8[3:(max - 2)]#

T7_0 <<- sensor_9[1:(max - 4)]#
T7_1 <<- sensor_9[2:(max - 3)]#
T7_2 <<- sensor_9[3:(max - 2)]#
T7_3 <<- sensor_9[4:(max - 1)]#
T7_4 <<- sensor_9[5:(max)]#
T7_raw <<- rsensor_9[3:(max - 2)]#

T8_0 <<- sensor_10[1:(max - 4)]#
T8_1 <<- sensor_10[2:(max - 3)]#
T8_2 <<- sensor_10[3:(max - 2)]#
T8_3 <<- sensor_10[4:(max - 1)]#
T8_4 <<- sensor_10[5:(max)]#
T8_raw <<- rsensor_10[3:(max - 2)]#

P7_0 <<- sensor_11[1:(max - 4)]#
P7_1 <<- sensor_11[2:(max - 3)]#
P7_2 <<- sensor_11[3:(max - 2)]#
P7_3 <<- sensor_11[4:(max - 1)]#
P7_4 <<- sensor_11[5:(max)]#
P7_raw <<- rsensor_11[3:(max - 2)]#

P8_0 <<- sensor_12[1:(max - 4)]#
P8_1 <<- sensor_12[2:(max - 3)]#
P8_2 <<- sensor_12[3:(max - 2)]#
P8_3 <<- sensor_12[4:(max - 1)]#
P8_4 <<- sensor_12[5:(max)]#
P8_raw <<- rsensor_12[3:(max - 2)]#

O1_0 <<- sensor_13[1:(max - 4)]#
O1_1 <<- sensor_13[2:(max - 3)]#
O1_2 <<- sensor_13[3:(max - 2)]#
O1_3 <<- sensor_13[4:(max - 1)]#
O1_4 <<- sensor_13[5:(max)]#
O1_raw <<- rsensor_13[3:(max - 2)]#

O2_0 <<- sensor_14[1:(max - 4)]#
O2_1 <<- sensor_14[2:(max - 3)]#
O2_2 <<- sensor_14[3:(max - 2)]#
O2_3 <<- sensor_14[4:(max - 1)]#
O2_4 <<- sensor_14[5:(max)]#
O2_raw <<- rsensor_14[3:(max - 2)]#

Blink <<- as.vector(trainingdata[1:(max - 4),29])#
LeftWink <<- as.vector(trainingdata[1:(max - 4),30])#
RightWink <<- as.vector(trainingdata[1:(max - 4),31])#
Furrow <<- as.vector(trainingdata[1:(max - 4),32])#
RaiseBrow <<- as.vector(trainingdata[1:(max - 4),33])#
Smile <<- as.vector(trainingdata[1:(max - 4),34])#
Clench <<- as.vector(trainingdata[1:(max - 4),35])#
LookLeft <<- as.vector(trainingdata[1:(max - 4),36])#
LookRight <<- as.vector(trainingdata[1:(max - 4),37])#
Laugh <<- as.vector(trainingdata[1:(max - 4),38])#
SmirkLeft <<- as.vector(trainingdata[1:(max - 4),39])#
SmirkRight <<- as.vector(trainingdata[1:(max - 4),40])#
#Cognitive_0 <<- as.vector(trainingdata[1:(max - 4),41])#
#Cognitive_1 <<- as.vector(trainingdata[1:(max - 4),42])#
#Cognitive_2 <<- as.vector(trainingdata[1:(max - 4),43])#
#Cognitive_3 <<- as.vector(trainingdata[1:(max - 4),44])#

#, LeftWink, RightWink, Furrow, RaiseBrow, Smile, Clench, LookLeft, LookRight, Laugh, SmirkLeft, SmirkRight, Cognitive_0, Cognitive_1, Cognitive_2, Cognitive_3#
input <- data.frame(AF3_0, AF3_1, AF3_2, AF3_3, AF3_4,  AF3_raw, AF4_0, AF4_1, AF4_2, AF4_3, AF4_4, AF4_raw, F3_0, F3_1, F3_2, F3_3, F3_4, F3_raw, F4_0, F4_1, F4_2, F4_3, F4_4, F4_raw, FC5_0, FC5_1, FC5_2, FC5_3, FC5_4, FC5_raw, FC6_0, FC6_1, FC6_2, FC6_3, FC6_4, FC6_raw, F7_0, F7_1, F7_2, F7_3, F7_4, F7_raw, F8_0, F8_1, F8_2, F8_3, F8_4, F8_raw, T7_0, T7_1, T7_2, T7_3, T7_4, T7_raw, T8_0, T8_1, T8_2, T8_3, T8_4, T8_raw, P7_0, P7_1, P7_2, P7_3, P7_4, P7_raw, P8_0, P8_1, P8_2, P8_3, P8_4, P8_raw, O1_0, O1_1, O1_2, O1_3, O1_4, O1_raw, O2_0, O2_1, O2_2, O2_3, O2_4, O2_raw, Blink, LeftWink, RightWink, Furrow, RaiseBrow, Smile, Clench, LookLeft, LookRight, Laugh, SmirkLeft, SmirkRight)#

write.table(input, file = file_out, sep=",", row.names=F, col.names=F)#"SHORTMod.csv"
}
	
#+LeftWink+RightWink+Furrow+RaiseBrow+Smile+Clench+LookLeft+LookRight+Laugh+SmirkLeft+SmirkRight+Cognitive_0+Cognitive_1+Cognitive_2+Cognitive_3#
net.ans <<- neuralnet(Blink~AF3_0+AF3_1+AF3_2+AF3_3+AF3_4+AF4_0+AF4_1+AF4_2+AF4_3+AF4_4+F3_0+F3_1+F3_2+F3_3+F3_4+F4_0+F4_1+F4_2+F4_3+F4_4+FC5_0+FC5_1+FC5_2+FC5_3+FC5_4+FC6_0+FC6_1+FC6_2+FC6_3+FC6_4+F7_0+F7_1+F7_2+F7_3+F7_4+F8_0+F8_1+F8_2+F8_3+F8_4+T7_0+T7_1+T7_2+T7_3+T7_4+T8_0+T8_1+T8_2+T8_3+T8_4+P7_0+P7_1+P7_2+P7_3+P7_4+P8_0+P8_1+P8_2+P8_3+P8_4+O1_0+O1_1+O1_2+O1_3+O1_4+O2_0+O2_1+O2_2+O2_3+O2_4, input, stepmax = 100, hidden=36, threshold=0.01)