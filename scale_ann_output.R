
scale_ann_output <- function(file_in, file_out) {
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
	Blink[i] <- ifelse(Blink[i] >= 0.5, 1, 0)
	LeftWink[i] <- ifelse(LeftWink[i] >= 0.5, 1, 0)
	RightWink[i] <- ifelse(RightWink[i] >= 0.5, 1, 0)
	Furrow[i] <- ifelse(Furrow[i] >= 0.5, 1, 0)
	RaiseBrow[i] <- ifelse(RaiseBrow[i] >= 0.5, 1, 0)
	Smile[i] <- ifelse(Smile[i] >= 0.5, 1, 0)
	Clench[i] <- ifelse(Clench[i] >= 0.5, 1, 0)
	LookLeft[i] <- ifelse(LookLeft[i] >= 0.5, 1, 0)
	LookRight[i] <- ifelse(LookRight[i] >= 0.5, 1, 0)
	Laugh[i] <- ifelse(Laugh[i] >= 0.5, 1, 0)
	SmirkLeft[i] <- ifelse(SmirkLeft[i] >= 0.5, 1, 0)
	SmirkRight[i] <- ifelse(SmirkRight[i] >= 0.5, 1, 0)
}

input <- data.frame(Blink, res[,2], LeftWink, res[,4], RightWink, res[,6], Furrow, res[,8], RaiseBrow, res[,10], Smile, res[,12], Clench, res[,14], LookLeft, res[,16], LookRight, res[,18], Laugh, res[,20], SmirkLeft, res[,22], SmirkRight, res[,24])#
#write.csv(input, file_out, row.names=FALSE, col.names=FALSE)#"MODANNRES.csv"
write.table(input, file=file_out, sep=",",  col.names=FALSE, row.names=FALSE)

}