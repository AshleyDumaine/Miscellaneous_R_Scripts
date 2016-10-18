#for transforming from binary expression columns to 0-12 scale for multiclass SVM usage
to_SVM_input <- function(file_in, file_out) {
mod <<- read.csv(file=file_in, header = F)#"~/Desktop/SHORTMod.csv"
Blink <<- as.vector(mod[,85])#
LeftWink <<- as.vector(mod[,86])#
RightWink <<- as.vector(mod[,87])#
Furrow <<- as.vector(mod[,88])#
RaiseBrow <<- as.vector(mod[,89])#
Smile <<- as.vector(mod[,90])#
Clench <<- as.vector(mod[,91])#
LookLeft <<- as.vector(mod[,92])#
LookRight <<- as.vector(mod[,93])#
Laugh <<- as.vector(mod[,94])#
SmirkLeft <<- as.vector(mod[,95])#
SmirkRight <<- as.vector( mod[,95])#

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
}

input <- data.frame(mod[,1:84], Blink)#
write.table(input, file = file_out, sep=",", row.names=F, col.names=F)#"SHORTModCOND.csv"
}