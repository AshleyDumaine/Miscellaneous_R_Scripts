#condense the results down to 2 columns -- ANN output and expected
#not needed for SVMs since already in condensed format
condense <- function(mod, out) {
Blink <<- as.vector(mod[,1])#
LeftWink <<- as.vector(mod[,3])#
RightWink <<- as.vector(mod[,5])#
Furrow <<- as.vector(mod[,7])#
RaiseBrow <<- as.vector(mod[,9])#
Smile <<- as.vector(mod[,11])#
Clench <<- as.vector(mod[,13])#
LookLeft <<- as.vector(mod[,15])#
LookRight <<- as.vector(mod[,17])#
Laugh <<- as.vector(mod[,19])#
SmirkLeft <<- as.vector(mod[,21])#
SmirkRight <<- as.vector( mod[,23])#

BlinkEx <<- as.vector(mod[,2])#
LeftWinkEx <<- as.vector(mod[,4])#
RightWinkEx <<- as.vector(mod[,6])#
FurrowEx <<- as.vector(mod[,8])#
RaiseBrowEx <<- as.vector(mod[,10])#
SmileEx <<- as.vector(mod[,12])#
ClenchEx <<- as.vector(mod[,14])#
LookLeftEx <<- as.vector(mod[,16])#
LookRightEx <<- as.vector(mod[,18])#
LaughEx <<- as.vector(mod[,20])#
SmirkLeftEx <<- as.vector(mod[,22])#
SmirkRightEx <<- as.vector( mod[,24])#

max <- nrow(mod)
for (i in 1:max) {
	if (LeftWink[i] == 1) {Blink[i] <- 2;}
	if (RightWink[i] == 1) {Blink[i] <- 3;}
	if (Furrow[i] == 1) {Blink[i] <- 4;}
	if (RaiseBrow[i] == 1) {Blink[i] <- 5;}
	if (Smile[i] == 1) {Blink[i] <- 6;}
	if (Clench[i] == 1) {Blink[i] <- 7;}
	if (LookLeft[i] == 1) {Blink[i] <- 8;}
	if (LookRight[i] == 1) {Blink[i] <- 9;}
	if (Laugh[i] == 1) {Blink[i] <- 10;}
	if (SmirkLeft[i] == 1) {Blink[i] <- 11;}
	if (SmirkRight[i] == 1) {Blink[i] <- 12;}
	
	if (LeftWinkEx[i] == 1) {BlinkEx[i] <- 2;}
	if (RightWinkEx[i] == 1) {BlinkEx[i] <- 3;}
	if (FurrowEx[i] == 1) {BlinkEx[i] <- 4;}
	if (RaiseBrowEx[i] == 1) {BlinkEx[i] <- 5;}
	if (SmileEx[i] == 1) {BlinkEx[i] <- 6;}
	if (ClenchEx[i] == 1) {BlinkEx[i] <- 7;}
	if (LookLeftEx[i] == 1) {BlinkEx[i] <- 8;}
	if (LookRightEx[i] == 1) {BlinkEx[i] <- 9;}
	if (LaughEx[i] == 1) {BlinkEx[i] <- 10;}
	if (SmirkLeftEx[i] == 1) {BlinkEx[i] <- 11;}
	if (SmirkRightEx[i] == 1) {BlinkEx[i] <- 12;}
}
input <- data.frame(Blink, BlinkEx)# #ann res then expected result
print("R value is:")
print(cor(Blink, BlinkEx))
write.csv(input, file=out, row.names=FALSE)
}


#convert ANN output to binary and condense the columns down to 2 -- output and expected
scale_and_condense <- function(file_in, file_out, threshold) {
res <- read.csv(file_in, header = F)#="~/Desktop/ann_res_for_48_neurons.csv"
Blink <- as.vector(res[,1])#
LeftWink <- as.vector(res[,3])#
RightWink <- as.vector(res[,5])#
Furrow <- as.vector(res[,7])#
RaiseBrow <- as.vector(res[,9])#
Smile <- as.vector(res[,11])#
Clench <- as.vector(res[,13])#
LookLeft <- as.vector(res[,15])#
LookRight <- as.vector(res[,17])#
Laugh <- as.vector(res[,19])#
SmirkLeft <- as.vector(res[,21])#
SmirkRight <- as.vector( res[,23])#

max <- nrow(res)
for (i in 1:max) {
	Blink[i] <- ifelse(Blink[i] >= threshold, 1, 0)
	LeftWink[i] <- ifelse(LeftWink[i] >= threshold, 1, 0)
	RightWink[i] <- ifelse(RightWink[i] >= threshold, 1, 0)
	Furrow[i] <- ifelse(Furrow[i] >= threshold, 1, 0)
	RaiseBrow[i] <- ifelse(RaiseBrow[i] >= threshold, 1, 0)
	Smile[i] <- ifelse(Smile[i] >= threshold, 1, 0)
	Clench[i] <- ifelse(Clench[i] >= threshold, 1, 0)
	LookLeft[i] <- ifelse(LookLeft[i] >= threshold, 1, 0)
	LookRight[i] <- ifelse(LookRight[i] >= threshold, 1, 0)
	Laugh[i] <- ifelse(Laugh[i] >= threshold, 1, 0)
	SmirkLeft[i] <- ifelse(SmirkLeft[i] >= threshold, 1, 0)
	SmirkRight[i] <- ifelse(SmirkRight[i] >= threshold, 1, 0)
}
input <- data.frame(Blink, res[,2], LeftWink, res[,4], RightWink, res[,6], Furrow, res[,8], RaiseBrow, res[,10], Smile, res[,12], Clench, res[,14], LookLeft, res[,16], LookRight, res[,18], Laugh, res[,20], SmirkLeft, res[,22], SmirkRight, res[,24])#
condense(input, file_out)
}