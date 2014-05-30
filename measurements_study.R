last_measurement <- "poll_2014-05-23-161523.csv";

res <- read.csv(file=last_measurement, sep=";", header=TRUE);

par(mfrow=c(2,1));
par(mar=c(1,2,1,1));
plot(res$Time, res$OutA, col="blue", type="p");
plot(res$Time, res$OutB, col="red");

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
    isStep <- ifelse(rising, (currentS - prevS) >= thresh, F) || ifelse(falling, (prevS - currentS) >= thresh, F)
    N <- N + ifelse(isStep, 1, 0)
    prevS <- currentS
  }
  return(N)
}
