# Week 4 Quiz

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11]) 
outcome_histogram <- hist(outcome[, 11]) 
valid_outcomes <- c("heart attack", "heart failure", "pneumonia")

# Write a function called "best" that takes two arguments:  the 2-character # abbreviated name of a stae and an outcome name.

best <- function(state, outcome) {
  #Read outcomes file
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
  
  
  condition_state <- as.numeric(state_outcomes[, selector]) 
  minimum <- min(condition_state, na.rm = TRUE) 
  best_hospital <- subset(state_outcomes, as.numeric(state_outcomes[, selector]) == minimum)
  
  if(nrow(best_hospital) > 1) {
    best_hospital <- best_hospital[order(best_hospital[, selector], best_hospital[, 2])]
    best_hospital[, 2][1]
  }
  else 
    best_hospital[, 2]
  
}


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
  
}
