best <- function(state, outcome) {
  #read outcome data
  hospital_data <- read.csv("outcome-of-care-measures.csv")
  
  #store possible outcomes in a vector
  
  outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  
  # store the death rate as per outcome in a vector
  
  indices <- c(11, 17, 23)
  
  #check if state and outcomes are valid
  
  if (!state %in% hospital_data$State) stop("invalid state")
  if (!outcome %in% outcomes) stop("invalid outcome")
  
  #match the indices of outcomes
  
  i <- indices[match(outcome, outcomes)]
  
  #subset the hospital data based on selected state and outcome
  
  hospital_list <- hospital_data[hospital_data$State == state, c(2, i)]
  
  #convert class of outcome to numeric
  
  hospital_list[, 2] <- as.numeric(as.character(hospital_list[, 2]))
  
  #remove missing values
  
  hospital_list <- na.omit(hospital_list)
  
  #name the columns of the subsetted vector for ease of understanding
  
  names(hospital_list) <- c("name", "deaths")
  
  #find the minimum number of deaths and return the result
  
  min_deaths <- min(hospital_list$deaths)
  result_list <- hospital_list[hospital_list$deaths == min_deaths, ]$name
  return(as.character(sort(result_list)[1]))
}