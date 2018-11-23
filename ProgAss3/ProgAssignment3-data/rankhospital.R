rankhospital <- function(state, outcome, num){
	file <- read.table("outcome-of-care-measures.csv", header=T, sep=",")
	states <- unique(file[['State']])
	diseases <- c('heart failure', 'heart attack', 'pneumonia')
	if ((state %in% states) && (outcome %in% diseases)){
		if (outcome == "pneumonia") col_name = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
		else if (outcome == "heart failure") col_name = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"  
		else if (outcome == "heart attack") col_name = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
		rm_not_avail = subset(file, col_name != 'Not Available', select=c("Hospital.Name",col_name,"State"))
		subset_state = subset(rm_not_avail, State == state, select=c("Hospital.Name",col_name))
		subset_state[,col_name] <- as.numeric(levels(subset_state[,col_name]))[subset_state[,col_name]]
		subset_clean <- na.omit(subset_state)
		subset_order <- subset_clean[order(subset_clean[,col_name], subset_clean$Hospital.Name), ]
		if (num == "worst") tail(subset_order[,1],1)
		else if (num == "best") head(subset_order[,1],1)
		else subset_order[num, 1]
	}
	else if (! (state %in% states)) "Error invalid state"
	else if (! (outcome %in% diseases)) "Error invalid outcome"
}