rankhospital <- function(state, outcome, num = "best") {
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  #Uses state input to best to create a subset of the outcomes data for the desired state
  state_outcomes <- subset(outcomes, State == state)

  #Error finder for the state, if a state creates a subset with zero rows, then an error is thrown
  if(nrow(state_outcomes) == 0) {
    stop("invalid state", call. = FALSE)
  }
  
  #Error finder for the condition, if a outcome is not within the three desired conditions, an error is thrown
  if(length(valid_outcomes[valid_outcomes == outcome]) == 0) {
    stop("invalid condition", call. = FALSE)
  }
  else if(outcome == "heart attack") {
    selector <- 11
  }
  else if(outcome == "heart failure") {
    selector <- 17
  }
  else if(outcome == "pneumonia") {
    selector <- 23
  }
  
  condition_state <- state_outcomes[, c(2,selector)]
  condition_state[, 2] <- as.numeric(condition_state[, 2])
  condition_state_complete <- condition_state[complete.cases(condition_state) ,]
  condition_state_complete <- condition_state_complete[order(condition_state_complete[, 2], condition_state_complete[, 1]) ,]
  
  if(num == "best") {
    num <- 1
  }
  else if(num == "worst"){
    num <- nrow(condition_state_complete)
  }
  else {
    num
  }
  
  condition_state_complete[num, 1]
  
}
