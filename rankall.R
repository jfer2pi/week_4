rankall <- function(outcome, num = "best") {
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  states <- unique(outcomes[, 7])
  states <- states[order(states)]
  
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
  condition_complete <- condition[complete.cases(condition),]
  
  ordering <- order(condition_complete[, 2], condition_complete[, 3], condition_complete[, 1])
  
  condition_complete_ordered <- condition_complete[ordering ,]
  
  #print(tail(condition_complete_ordered))
  
  #print(subset(condition_complete_ordered, State == states[11]))
  
  if(num == "best") {
      num <- 1
  }
  else {
      num
  }
  
  out <- data.frame()
  
  for(state in 1:length(states))
  {
      tester <- subset(condition_complete_ordered, State == states[state])[, c(1,2)]
      print(c(nrow(tester), states[state]))
      
      if(nrow(tester) < num) {
         out <- rbind(out, c(NA, states[state]))
         print(out)
        }
      else {
        out <- rbind(out, tester)
         }
  }
  out
}
