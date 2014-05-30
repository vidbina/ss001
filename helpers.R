#' Count level changes within a series
#
#' @param s numberic vector to be counted
#' @param thresh size of the delta required to constitute a change
#' @param rising count rising edges
#' @param falling count falling edges
countSteps <- function(s, thresh = 1, rising=T, falling=T) {
  prevS <- s[1]
  N <- 0

  for(currentS in s) {
    edge <- currentS - prevS
    # TODO: find a concise way to do greater-then-or-equal-to assertions on numbers
    isStep <- ( 
      ifelse(rising, (identical(all.equal(edge, thresh), T) || (edge > thresh)), F) 
      || 
      ifelse(falling, (identical(all.equal(-edge, thresh), T) || (-edge > thresh)), F) )
    N <- N + ifelse(isStep, 1, 0)
    prevS <- currentS
  }
  return(N)
}

#getStepIndex <- function(Nstep, s, thresh = 1, rising=T, falling=T) {
#  prevS <- s[1]
#  N <- 0
#
#  for(i in length(s)) {
#    isStep <- ifelse(rising, (s[i] - prevS) >= thresh, F) || ifelse(falling, (prevS - s[i]) >= thresh, F)
#    N <- N + ifelse(isStep, 1, 0);
#    prevS <- currentS
#    if(Nstep == step) return s[i]
#  }
#  return(-1)
#}
