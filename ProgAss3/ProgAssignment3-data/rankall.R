rankall <- function(outcome, num="best"){
	file <- read.table("outcome-of-care-measures.csv", header=T, sep=",")
	states <- unique(file[['State']])
	
	diseases <- c('heart failure', 'heart attack', 'pneumonia')
	if (outcome %in% diseases){
		if (outcome == "pneumonia") col_name = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
		else if (outcome == "heart failure") col_name = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"  
		else if (outcome == "heart attack") col_name = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
		rm_not_avail = subset(file, col_name != 'Not Available', select=c("Hospital.Name",col_name,"State"))
		rm_not_avail[,col_name] <- as.numeric(levels(rm_not_avail[,col_name]))[rm_not_avail[,col_name]]
		hospital <- data.frame()

		for (state in states[order(states)]) {
			subset_state = subset(rm_not_avail, State == state, select=c("Hospital.Name",col_name))
			subset_clean <- na.omit(subset_state)
			subset_order <- subset_clean[order(subset_clean[,col_name], subset_clean$Hospital.Name), ]
			if (num == "worst") temp_df <- data.frame(tail(subset_order[,1],1), state)
			else if (num == "best") temp_df <- data.frame(head(subset_order[,1],1), state)
			else temp_df <- data.frame(subset_order[num, 1], state)
			hospital <- rbind(hospital, temp_df)
		}
		hospital
	}
	else if (! (outcome %in% diseases)) "Error invalid outcome"
}