best <- function(state, outcome) {
  
  ## Read the outcome data
  datf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!state %in% unique(datf[, 7])) {
    stop("Not a valid state")
  }
  switch(outcome, `heart attack` = {
    col = 11
  }, `heart failure` = {
    col = 17
  }, pneumonia = {
    col = 23
  }, stop("Not a valid outcome"))
  ## Return hospital name in that state with lowest 30-day death rate
  df = datf[datf$State == state, c(2, col)]
  df[which.min(df[, 2]), 1]
}
