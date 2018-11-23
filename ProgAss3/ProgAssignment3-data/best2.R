
best <- function(state, outcome){
	file <- read.table("outcome-of-care-measures.csv", header=T, sep=",")
	states <- unique(file[['State']])
	diseases <- c('heart failure', 'heart attack', 'pneumonia')
	if ((state %in% states) && (outcome %in% diseases)){
		if (outcome == "pneumonia") col_name = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
		else if (outcome == "heart failure") col_name = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"  
		else if (outcome == "heart attack") col_name = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
		temp = subset(file, col_name != 'Not Available', select=c("Hospital.Name",col_name,"State"))
		temp2 = subset(temp, State == state, select=c("Hospital.Name",col_name))
		temp2[,col_name] <- as.numeric(levels(temp2[,col_name]))[temp2[,col_name]]
		temp3 <- na.omit(temp2)
		min_ind <- min(temp3[,col_name])
		temp4 = subset(temp3, temp2[[col_name]] == min_ind, select=c("Hospital.Name"))
		temp4
	}
	else if (! (state %in% states)) "Error invalid state"
	else if (! (outcome %in% diseases)) "Error invalid outcome"
}