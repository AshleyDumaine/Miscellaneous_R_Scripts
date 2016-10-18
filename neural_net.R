install.packages('neuralnet')
library("neuralnet")
#-----------MACROFILTER-----------#
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
}
#-----------NORMALIZE-------------#
#use the following equation: (x - L) / (U - L) where L is min and U is max
normalize <- function(input, upperLimit, lowerLimit) {#
	(input - lowerLimit) / (upperLimit - lowerLimit)#
}

#--------------PRINT---------------#
print <- function(x, ...) {
    if (is.numeric(x)) base::print(round(x, digits=2), ...) 
    else base::print(x, ...)
}

#----------------GET TRAINING DATA--------------#
#take a csv and extract sensor values in chunks of 5 for all 14 sensors
getData <- function(inputFile, training) {
	if (training) {
		trainingdata <<- read.csv(file=inputFile, header = F)
	}
	#print(trainingdata) #debug
	#col 1 = sample counter
	#col 2 = unknown, always 0
	#col 3 = heartbeats?
	#col 4 - 17 = sensors 1 - 14
	#col 18, 19 = references?
	#col 20 - 21 = ???
	#col 23 - 38 = EEG event on/off	
	max <- nrow(trainingdata)#
	f1 <- macrofilter(trainingdata[,4])#
	f2 <- macrofilter(trainingdata[,5])#
	f3 <-(trainingdata[,6])#
	f4 <-(trainingdata[,7])#
	f5 <-(trainingdata[,8])#
	f6 <-(trainingdata[,9])#
	f7 <-(trainingdata[,10])#
	f8 <-(trainingdata[,11])#
	f9 <-(trainingdata[,12])#
	f10 <-(trainingdata[,13])#
	f11 <-(trainingdata[,14])#
	f12 <-(trainingdata[,15])#
	f13 <-(trainingdata[,16])#
	f14 <-(trainingdata[,17])#
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
	
	#List of sensor names: AF3, AF4, F3, F4, FC5, FC6, F7, F8, T7, T8, P7, P8, O1, O2
	#oh no, globals!!
	AF3_0 <<- sensor_1[1:(max - 4)]#
	AF3_1 <<- sensor_1[2:(max - 3)]#
	AF3_2 <<- sensor_1[3:(max - 2)]#
	AF3_3 <<- sensor_1[4:(max - 1)]#
	AF3_4 <<- sensor_1[5:(max)]#
	AF4_0 <<- sensor_2[1:(max - 4)]#
	AF4_1 <<- sensor_2[2:(max - 3)]#
	AF4_2 <<- sensor_2[3:(max - 2)]#
	AF4_3 <<- sensor_2[4:(max - 1)]#
	AF4_4 <<- sensor_2[5:(max)]#
	F3_0 <<- sensor_3[1:(max - 4)]#
	F3_1 <<- sensor_3[2:(max - 3)]#
	F3_2 <<- sensor_3[3:(max - 2)]#
	F3_3 <<- sensor_3[4:(max - 1)]#
	F3_4 <<- sensor_3[5:(max)]#
	F4_0 <<- sensor_4[1:(max - 4)]#
	F4_1 <<- sensor_4[2:(max - 3)]#
	F4_2 <<- sensor_4[3:(max - 2)]#
	F4_3 <<- sensor_4[4:(max - 1)]#
	F4_4 <<- sensor_4[5:(max)]#
	FC5_0 <<- sensor_5[1:(max - 4)]#
	FC5_1 <<- sensor_5[2:(max - 3)]#
	FC5_2 <<- sensor_5[3:(max - 2)]#
	FC5_3 <<- sensor_5[4:(max - 1)]#
	FC5_4 <<- sensor_5[5:(max)]#
	FC6_0 <<- sensor_6[1:(max - 4)]#
	FC6_1 <<- sensor_6[2:(max - 3)]#
	FC6_2 <<- sensor_6[3:(max - 2)]#
	FC6_3 <<- sensor_6[4:(max - 1)]#
	FC6_4 <<- sensor_7[5:(max)]#
	F7_0 <<- sensor_7[1:(max - 4)]#
	F7_1 <<- sensor_7[2:(max - 3)]#
	F7_2 <<- sensor_7[3:(max - 2)]#
	F7_3 <<- sensor_7[4:(max - 1)]#
	F7_4 <<- sensor_7[5:(max)]#
	F8_0 <<- sensor_8[1:(max - 4)]#
	F8_1 <<- sensor_8[2:(max - 3)]#
	F8_2 <<- sensor_8[3:(max - 2)]#
	F8_3 <<- sensor_8[4:(max - 1)]#
	F8_4 <<- sensor_8[5:(max)]#
	T7_0 <<- sensor_9[1:(max - 4)]#
	T7_1 <<- sensor_9[2:(max - 3)]#
	T7_2 <<- sensor_9[3:(max - 2)]#
	T7_3 <<- sensor_9[4:(max - 1)]#
	T7_4 <<- sensor_9[5:(max)]#
	T8_0 <<- sensor_10[1:(max - 4)]#
	T8_1 <<- sensor_10[2:(max - 3)]#
	T8_2 <<- sensor_10[3:(max - 2)]#
	T8_3 <<- sensor_10[4:(max - 1)]#
	T8_4 <<- sensor_10[5:(max)]#
	P7_0 <<- sensor_11[1:(max - 4)]#
	P7_1 <<- sensor_11[2:(max - 3)]#
	P7_2 <<- sensor_11[3:(max - 2)]#
	P7_3 <<- sensor_11[4:(max - 1)]#
	P7_4 <<- sensor_11[5:(max)]#
	P8_0 <<- sensor_12[1:(max - 4)]#
	P8_1 <<- sensor_12[2:(max - 3)]#
	P8_2 <<- sensor_12[3:(max - 2)]#
	P8_3 <<- sensor_12[4:(max - 1)]#
	P8_4 <<- sensor_12[5:(max)]#
	O1_0 <<- sensor_13[1:(max - 4)]#
	O1_1 <<- sensor_13[2:(max - 3)]#
	O1_2 <<- sensor_13[3:(max - 2)]#
	O1_3 <<- sensor_13[4:(max - 1)]#
	O1_4 <<- sensor_13[5:(max)]#
	O2_0 <<- sensor_14[1:(max - 4)]#
	O2_1 <<- sensor_14[2:(max - 3)]#
	O2_2 <<- sensor_14[3:(max - 2)]#
	O2_3 <<- sensor_14[4:(max - 1)]#
	O2_4 <<- sensor_14[5:(max)]#
	
	if (training) { #only applicable to training data, the client will not send such values
		#expected outputs (not averaged, truncated)
		#TODO: get vector of averages for EEG events for 5 samples at a time
		Blink <<- as.vector(trainingdata[1:(max - 4),23])#
		LeftWink <<- as.vector(trainingdata[1:(max - 4),24])#
		RightWink <<- as.vector(trainingdata[1:(max - 4),25])#
		Furrow <<- as.vector(trainingdata[1:(max - 4),26])#
		RaiseBrow <<- as.vector(trainingdata[1:(max - 4),27])#
		Smile <<- as.vector(trainingdata[1:(max - 4),28])#
		Clench <<- as.vector(trainingdata[1:(max - 4),29])#
		LookLeft <<- as.vector(trainingdata[1:(max - 4),30])#
		LookRight <<- as.vector(trainingdata[1:(max - 4),31])#
		Laugh <<- as.vector(trainingdata[1:(max - 4),32])#
		SmirkLeft <<- as.vector(trainingdata[1:(max - 4),33])#
		SmirkRight <<- as.vector(trainingdata[1:(max - 4),34])#
		Cognitive_0 <<- as.vector(trainingdata[1:(max - 4),35])
		Cognitive_1 <<- as.vector(trainingdata[1:(max - 4),36])
		Cognitive_2 <<- as.vector(trainingdata[1:(max - 4),37])
		Cognitive_3 <<- as.vector(trainingdata[1:(max - 4),38])
	}
}

#------------------TRAIN NETWORK-----------------#
trainNetwork <- function() {
getData(inputFile=file.choose(), training=T)

print("Got the data") #RaiseBrow, Smile, Clench, LookLeft, LookRight, Laugh, SmirkLeft, SmirkRight, Cognitive_0, Cognitive_1, Cognitive_2, Cognitive_3
	input <- data.frame(AF3_0, AF3_1, AF3_2, AF3_3, AF3_4, AF4_0, AF4_1, AF4_2, AF4_3, AF4_4, F3_0, F3_1, F3_2, F3_3, F3_4, F4_0, F4_1, F4_2, F4_3, F4_4, FC5_0, FC5_1, FC5_2, FC5_3, FC5_4, FC6_0, FC6_1, FC6_2, FC6_3, FC6_4, F7_0, F7_1, F7_2, F7_3, F7_4, F8_0, F8_1, F8_2, F8_3, F8_4, T7_0, T7_1, T7_2, T7_3, T7_4, T8_0, T8_1, T8_2, T8_3, T8_4, P7_0, P7_1, P7_2, P7_3, P7_4, P8_0, P8_1, P8_2, P8_3, P8_4, O1_0, O1_1, O1_2, O1_3, O1_4, O2_0, O2_1, O2_2, O2_3, O2_4, Blink, LeftWink, RightWink, Furrow)
	
	#train the network
	print("Training network...") #+RaiseBrow+Smile+Clench+LookLeft+LookRight+Laugh+SmirkLeft+SmirkRight+Cognitive_0+Cognitive_1+Cognitive_2+Cognitive_3
	net.ans <<- neuralnet(Blink+LeftWink+RightWink+Furrow~AF3_0+AF3_1+AF3_2+AF3_3+AF3_4+AF4_0+AF4_1+AF4_2+AF4_3+AF4_4+F3_0+F3_1+F3_2+F3_3+F3_4+F4_0+F4_1+F4_2+F4_3+F4_4+FC5_0+FC5_1+FC5_2+FC5_3+FC5_4+FC6_0+FC6_1+FC6_2+FC6_3+FC6_4+F7_0+F7_1+F7_2+F7_3+F7_4+F8_0+F8_1+F8_2+F8_3+F8_4+T7_0+T7_1+T7_2+T7_3+T7_4+T8_0+T8_1+T8_2+T8_3+T8_4+P7_0+P7_1+P7_2+P7_3+P7_4+P8_0+P8_1+P8_2+P8_3+P8_4+O1_0+O1_1+O1_2+O1_3+O1_4+O2_0+O2_1+O2_2+O2_3+O2_4, input, hidden=40, threshold=0.1)
	
	print("DONE")
}

#-------------MAIN---------------#
#prints out a stream of sensor readings from a socket
server <- function(){
	#train neural net first
	trainNetwork()
	print("Network trained!")
	con <- socketConnection(host="localhost", port=3333, blocking=TRUE, server=TRUE, open="r")
	on.exit(close(con))
	writeLines("Listening...")
	repeat{
		data <- readLines(con, 5) #read 5 samples
		if(!is.null(data)) {
			vals <- as.numeric(unlist(strsplit(data,",")))
			if (length(data) != 0) {
				#print(vals) #do something here
				#get test data directly from the socket
				#convert data to dataframe here
				#data2 <- as.data.frame(vals)
				#getData(inputFile=data2, training=F)
				
				#test <- data.frame(AF3_0, AF3_1, AF3_2, AF3_3, AF3_4, AF4_0, AF4_1, AF4_2, AF4_3, AF4_4, F3_0, F3_1, F3_2, F3_3, F3_4, F4_0, F4_1, F4_2, F4_3, F4_4, FC5_0, FC5_1, FC5_2, FC5_3, FC5_4, FC6_0, FC6_1, FC6_2, FC6_3, FC6_4, F7_0, F7_1, F7_2, F7_3, F7_4, F8_0, F8_1, F8_2, F8_3, F8_4, T7_0, T7_1, T7_2, T7_3, T7_4, T8_0, T8_1, T8_2, T8_3, T8_4, P7_0, P7_1, P7_2, P7_3, P7_4, P8_0, P8_1, P8_2, P8_3, P8_4, O1_0, O1_1, O1_2, O1_3, O1_4, O2_0, O2_1, O2_2, O2_3, O2_4)
				
				#net.results <- compute(net.ans, test)
				
				#see what properties net.ans has
				#ls(net.results)
				 
				#see the results
				#print(net.results$net.result)
				
				#Plot the neural network
				#plot(net.ans, rep="best",radius=2, dimension = 300, fontsize = 8)
			}
		}
	}
}
server()