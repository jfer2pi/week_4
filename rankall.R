rankall <- function(outcome, num = "best") {
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  states <- unique(outcomes[, 7])
  
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
  
  condition <- outcomes[, c(2, 7, selector)]
  condition[, 3] <- as.numeric(condition[, 3])
  condition_complete <- condition[complete.cases(condition) ,]
  
  for(s in states) {
      
  }

  
  #ordering <- order(condition_complete[, 2], condition_complete[, 3], condition_complete[, 1])
  #condition_complete_ordered <- condition_complete[ordering ,]
  
  #state_subsets <- data.frame()
  
  #for(i in c(1:54)) {
  #  state_subsets[[i]] <- rbind(subset(condition_complete_ordered, condition_complete_ordered[, 2] == states[i]))
  #}
  #state_subsets
  
}
