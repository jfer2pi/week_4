# Programming Assignment 3

dir <- setwd("~/Dropbox/Coursera DS/week_4/hospital/rprog-data-ProgAssignment3-data")

outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcomes)
str(outcomes)

ncol(outcomes)
colnames <- names(outcomes)

outcome[, 11] <- as.numeric(outcomes[, 11])
histogram <- hist(outcomes[, 11])

best <- function(state, outcome) {
    outs <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    state_outs <- subset(outs, State == state)
    
}