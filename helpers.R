#' Detect a step
#
# @param s a vector containing at least two numbers
#' @param thresh size of the delta required to constitute a change
#' @param rising detect rising edges (true by default)
#' @param falling detect falling edges (true by default)
isStep <- function(s, thresh=1, rising=T, falling=T) {
  if(length(s) < 2) return(F)

  edge <- s[2] - s[1]
  # TODO: find a concise way to do greater-then-or-equal-to assertions on numbers
  isStep <- ( 
    ifelse(rising, (identical(all.equal(edge, thresh), T) || (edge > thresh)), F) 
    || 
    ifelse(falling, (identical(all.equal(-edge, thresh), T) || (-edge > thresh)), F) )
}

#' Count level changes within a series
#
#' @param s numeric vector containing the clock/pulse samples
#' @param thresh size of the delta required to constitute a change
#' @param rising count rising edges
#' @param falling count falling edges
countSteps <- function(s, thresh=1, rising=T, falling=T) {
  prevS <- s[1]
  N <- 0

  for(currentS in s) {
    N <- N + ifelse(isStep(c(prevS, currentS), thresh=thresh, rising=rising, falling=falling), 1, 0)
    prevS <- currentS
  }
  return(N)
}


#' Returns the index of the N-th post-edge sample 
#
#' Upon specifying a invalid startingpoint startAt zero is returned
#' Setting N to 2 will return the index of the second step change

# @param N step count
getStepIndex <- function(s, startAt=1, thresh=1, rising=T, falling=T, N=1) {
  at <- 0;
  if((startAt < 1) || (startAt > (length(s)-1))) return(0)
  if(length(s) > 1) {
    for(i in (startAt+1):length(s)) {
      if(isStep(c(s[i-1], s[i]))) { 
        at <- at+1
        if(at == N) return(i) 
      }
    }
  }
  return(0)
}

getStepDataFromSample <- function(d, clock, sample=1) {
  if(length(d) != length(clock)) return(c)
  nextStep <- getStepIndex(clock, startAt=sample)
  return(d[sample:(ifelse((nextStep > 0), nextStep-1, length(clock)))])
}
