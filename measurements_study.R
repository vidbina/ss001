last_measurement <- "poll_2014-05-23-161523.csv";

res <- read.csv(file=last_measurement, sep=";", header=TRUE);

par(mfrow=c(2,1));
par(mar=c(1,2,1,1));
plot(res$Time, res$OutA, col="blue", type="p");
plot(res$Time, res$OutB, col="red");

countSteps <- function(s, thresh = 1)
{
  if(length(s) > 1) {
    step <- ifelse(s[2] - s[1] >= thresh, 1, 0);
    return(step + countSteps(s[2:length(s)]));
  } else {
    return(0);
  }
}
countMeasurements <- function(steps)
{
  prev <- 0;
  count <- 0;
  for(i in steps)
  {
    if(prev == 0 && i == 1) 
    {
      count <- count+1;
    }
    prev <- i;
  }
}
