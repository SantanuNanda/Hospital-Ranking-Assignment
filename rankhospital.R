rankhospital <- function(state, outcome, num = "best") {
  # Read outcome data
  hospitals <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  #check if outcome is invalid or not
  
  outcomes <- if (outcome == "heart attack") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  #subset hospital data based on state,hospital name and outcomes
  
  states_data <- hospitals[hospitals$State == state, c("Hospital.Name", outcomes)]
  
  #check if state is invalid
  
  if (nrow(states_data) == 0) {
    stop("invalid state")	
  }
  
  #change class of death rate from character to integer
  
  states_data[,2] <- as.numeric(states_data[,2])
  ranking <- order(states_data[outcomes], states_data$Hospital.Name, na.last=NA)
  
  #check the rank and return the name of the hospital accordingly
  
  if (num == "best") {
    as.character(states_data$Hospital.Name[ranking[1]])
  } else if (num == "worst") {
    as.character(states_data$Hospital.Name[ranking[length(ranking)]])
  } else if (is.numeric(num)) {
    as.character(states_data$Hospital.Name[ranking[num]])
  } else {
    stop("invalid num")
  }
}