rankall <- function(outcome, num = "best") {
  ## Read the outcome data
  datf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  states = unique(datf[, 7])
  switch(outcome, `heart attack` = {
    col = 11
  }, `heart failure` = {
    col = 17
  }, pneumonia = {
    col = 23
  }, stop("invalid outcome"))
  
  ## For each state, find the hospital of the given rank
  datf[, col] = as.numeric(datf[, col])
  datf = datf[, c(2, 7, col)]  # grab only the name, state, and death rate
  datf = na.omit(datf)
  
  rank_in_state <- function(state) {
    df = datf[datf[, 2] == state, ]
    nhospital = nrow(df)
    switch(num, best = {
      num = 1
    }, worst = {
      num = nhospital
    })
    if (num > nhospital) {
      result = NA
    }
    o = order(df[, 3], df[, 1])
    result = df[o, ][num, 1]
    c(result, state)
  }
  ##Return a data frame with the hospital names and the
  ## (abbreviated) state name
  output = do.call(rbind, lapply(states, rank_in_state))
  output = output[order(output[, 2]), ]
  rownames(output) = output[, 2]
  colnames(output) = c("hospital", "state")
  data.frame(output)
}