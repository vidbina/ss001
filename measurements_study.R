source("helpers.R")

# On workbench: kept the magnet fixed at a given orientation for a while, then repositioned it a few times (which should explain the jumps).
stability_check <- "poll_2014-07-03-153851.csv";
# On rig: Kept blind perpendicular to rail, then started rotating until blind is parallel to rail
rig_fix_rot <- "poll_2014-07-03-173816.csv";
last_measurement <- rig_fix_rot;

data <- read.csv(file=last_measurement, sep=";", header=TRUE);

#plot(res$Time, res$OutA, col="blue", type="p");
#plot(res$Time, res$OutB, col="red");
#print(res$OutA[1:30])
#print(res$Step[1:30])
#print(max(res$OutA)-min(res$OutA))

filterSet <- function(sets, markers, sd_thresh=15) {
  result <- c();

  for(i in 1:length(markers)) {
    samples <- sets[markers[i]][[1]];

    if(ifelse(length(samples) == 1, TRUE, (sd(samples) <= sd_thresh))) {
      result <- c(result, mean(samples));
    } else {
      prev <- ifelse(length(result) > 0, result[length(result)], ifelse(i == 1, mean(samples), 0));
      result <- c(result, prev);
    }
  }
  return(result);
}

analyse2dSignal <- function(amplitude, time, clk, title, filter, range) {
  res <- new.env();

  res$s_mean <- c();
  res$s_var <- c();
  res$s_sd <- c();
  res$s_max <- c();
  res$s_min <- c();
  res$s_set <- list();
  res$indices <- getStepIndices(clk[range]);

  for(i in res$indices) {
    if(i > length(time)) { cat(printf("out of range")); break; }
    samples <- getStepData(amplitude[range], clock=clk[range], startAt=i);

    res$stamp <- c(res$stamp, time[i]);
    res$s_mean <- c(res$s_mean, mean(samples));
    res$s_median <- c(res$s_median, median(samples));
    res$s_var <- c(res$s_var, ifelse(length(samples) > 1, var(samples), 0));
    res$s_sd <- c(res$s_sd, ifelse(length(samples) > 1, sd(samples), 0));
    res$s_max <- c(res$s_max, max(samples));
    res$s_min <- c(res$s_min, min(samples));
    res$s_set[[i]] <- samples;

    if(ifelse(length(samples) > 1, var(samples) > 100, FALSE)) {
      cat(sprintf("\n\rσ²=%f, σ=%f @ %fs", var(samples), sd(samples), time[i]/1000));
    }
  }

  return(res);
}

plotSignal <- function(amplitude, time, range=1:length(time), name="unnamed set") {
  title = paste("Acquired samples from", name, sep=" ");
  print(title);
  print(length(range));
  print(length(amplitude));
  print(length(time));
  mean <- mean(amplitude[range])
  plot(time[range], amplitude[range], col="blue", type="l", main="signal", ann=FALSE);
#  lines(time[range], mean, col="green", pch="22", type="l", main="mean");
#  #axis(3, at=time[res$indices[seq(1, length(res$indices), length=5)]], labels=res$indices[seq(1, length(res$indices), length=5)]);
#  mtext(side=1, text="time in milliseconds", line=2);
#  mtext(side=2, text="amplitude", line=2);
#  print(name)
#  mtext(side=3, text=bquote(bold(.(title))), line=2);
#  legend("bottomleft", c("set samples", "set samples mean", "filtered set"), col=c("blue", "green", "red"), lty=c(1,1,1), bg="white");
}

plotSignalAndDeviation <- function(amplitude, input, time, clk, range=1:100, name="unnamed set") {
  title = paste("Acquired samples from", name, sep=" ");
  print(title);
  res <- analyse2dSignal(amplitude, time, clk, title, filter, range);
  res$filtered <- filterSet(res$s_set, res$indices, sd_thresh=1);

  cat(sprintf("\n\rl_time=%d, l_indices=%d", length(time), length(res$indices)));
  plot(time[range], amplitude[range], col="blue", type="l", main="Sample set", ann=FALSE);
  lines(time[res$indices], res$s_mean, col="green", pch=22, type="l", main="Sample mean");
  lines(time[res$indices], res$filtered, col="red", pch=22, type="l", main="Filtered set");
  axis(3, at=time[res$indices[seq(1, length(res$indices), length=5)]], labels=res$indices[seq(1, length(res$indices), length=5)]);
  mtext(side=1, text="time in milliseconds", line=2);
  mtext(side=2, text="amplitude", line=2);
  print(name)
  mtext(side=3, text=bquote(bold(.(title))), line=2);
  legend("bottomleft", c("set samples", "set samples mean", "filtered set"), col=c("blue", "green", "red"), lty=c(1,1,1), bg="white");

  par(mar=c(3,4,3,1) + 0.1);
  plot(time[res$indices], res$s_sd, type="h", pch=22, main="Standard deviation of samples in step", ann=FALSE);
  mtext(side=1, text="time in milliseconds", line=2);
  mtext(side=2, text="deviation (σ)", line=2);
  mtext(side=3, text=expression(bold("Standard deviation among position samples")), line=1);
}

analyseOutputData <- function(amplitude, input, time, clk, range=1:100) {
  res <- analyse2dSignal(amplitude, time, clk, title, filter, range)
  res$filtered <- filterSet(res$s_set, res$indices, sd_thresh=1);

  layout(matrix(c(1,1,2,2,3,3,4,4), 4, 2, byrow=TRUE), heights=c(2,1));

  par(mar=c(4,4,4.5,1) + 0.1, ps=7);
  cat(sprintf("\n\rl_time=%d, l_indices=%d", length(time), length(res$indices)));
  plot(time[range], amplitude[range], col="blue", type="l", main="Sample set", ann=FALSE);
  lines(time[res$indices], res$s_mean, col="green", pch=22, type="l", main="Sample mean");
  lines(time[res$indices], res$filtered, col="red", pch=22, type="l", main="Filtered set");
  axis(3, at=time[res$indices[seq(1, length(res$indices), length=5)]], labels=res$indices[seq(1, length(res$indices), length=5)]);
  mtext(side=1, text="time in milliseconds", line=2);
  mtext(side=2, text="amplitude", line=2);
  mtext(side=3, text=expression(bold("Nth sample set")), line=2);
  legend("bottomleft", c("set samples", "set samples mean", "filtered set"), col=c("blue", "green", "red"), lty=c(1,1,1), bg="white");

  par(mar=c(3,4,3,1) + 0.1);
  plot(time[res$indices], res$s_sd, type="h", pch=22, main="Standard deviation of samples in step", ann=FALSE);
  mtext(side=1, text="time in milliseconds", line=2);
  mtext(side=2, text="deviation (σ)", line=2);
  mtext(side=3, text=expression(bold("Standard deviation among position samples")), line=1);

  for(i in 1:length(input)) {
    #plot(time[range], amplitude[range], col="blue", type="l", main="Sample set", ann=FALSE);
    plot(time[range], input[[i]][range], type="l", pch=22, main=paste("Input", i, sep=" "), ann=FALSE);
  }
  #plot(time[indices], res$s_sd, type="h", pch=22, main="Standard deviation of samples in step", ann=FALSE);
  #plot(time[indices], res$s_sd, type="h", pch=22, main="Standard deviation of samples in step", ann=FALSE);

  cat(sprintf("\ravg_min = %f, avg_max = %f, avg_range=%f\n\r", min(res$s_mean), max(res$s_mean), (max(res$s_mean)-min(res$s_mean))));
  #cat(sprintf("\n\rlenght time = %d, length data = %d\n\r", length(time[range]), length(res$s_mean)));
  #cat(sprintf("\ravg_min = %f, avg_max = %f, avg_range=%f\n\r", min(res$s_mean), max(res$s_mean), (max(res$s_mean)-min(res$s_mean))));
  #cat(sprintf("\r%f < var < %f\n\r", min(res$s_var), max(res$s_var)));

  return(res);
}

analyseSet <- function(amplitude, time, clk, range=1:100) {
  par(mfrow=c(2,1));
  par(mar=c(1,2,1,1));

  res <- new.env();

  res$s_mean <- c();
  res$s_var <- c();
  res$s_sd <- c();
  res$s_max <- c();
  res$s_min <- c();
  #res$filtered <- c();
  res$s_set <- list();

  indices <- getStepIndices(clk)

  for(i in indices) {
    if(i > length(time)) { cat(printf("out of range")); break; }
    samples <- getStepData(amplitude, clock=clk, startAt=i);

    res$stamp <- c(res$stamp, time[i]);
    res$s_mean <- c(res$s_mean, mean(samples));
    res$s_median <- c(res$s_median, median(samples));
    res$s_var <- c(res$s_var, var(samples));
    res$s_sd <- c(res$s_sd, sd(samples));
    res$s_max <- c(res$s_max, max(samples));
    res$s_min <- c(res$s_min, min(samples));
    res$s_set[[i]] <- samples;
    #res$filtered <- c(res$filtered, processSet(samples));

    if(var(samples) > 100) {
      cat(sprintf("\n\rσ²=%f, σ=%f @ %fs", var(samples), sd(samples), time[i]/1000));
    }
  }

  res$filtered <- filterSet(res$s_set, indices, sd_thresh=1);
  #print(filterSet(res$s_set, indices));

  layout(matrix(c(1,1,2,2), 2, 2, byrow=TRUE), heights=c(2,1));

  par(mar=c(4,4,4.5,1) + 0.1, ps=7);
  cat(sprintf("\n\rl_time=%d, l_indices=%d", length(time), length(indices)));
  
  plot(time, amplitude, col="blue", type="l", main="Sample set", ann=FALSE);
  lines(time[indices], res$s_mean, col="green", pch=22, type="l", main="Sample mean");
  lines(time[indices], res$filtered, col="red", pch=22, type="l", main="Filtered set");
  #cat(sprintf("\n\rN_marks=%d N_labels=%d", length(time[pretty(indices)]), length(pretty(indices))));
  #print(pretty(indices));
  #print(time[pretty(indices)]);
  axis(3, at=time[indices[seq(1, length(indices), length=5)]], labels=indices[seq(1, length(indices), length=5)]);
  mtext(side=1, text="time in milliseconds", line=2);
  mtext(side=2, text="amplitude", line=2);
  mtext(side=3, text=expression(bold("Nth sample set")), line=2);
  legend("bottomleft", c("set samples", "set samples mean", "filtered set"), col=c("blue", "green", "red"), lty=c(1,1,1), bg="white");

  par(mar=c(3,4,3,1) + 0.1);
  #par(mar=c(2,2,2.5,1));
  #plot(mapply(function(x) x, time[indices]/1000), res$s_var, type="h", pch=22, main="Sample Variance");
  plot(time[indices], res$s_sd, type="h", pch=22, main="Standard deviation of samples in step", ann=FALSE);
  mtext(side=1, text="time in milliseconds", line=2);
  mtext(side=2, text="deviation (σ)", line=2);
  mtext(side=3, text=expression(bold("Standard deviation among position samples")), line=1);

  cat(sprintf("\ravg_min = %f, avg_max = %f, avg_range=%f\n\r", min(res$s_mean), max(res$s_mean), (max(res$s_mean)-min(res$s_mean))));
  #cat(sprintf("\n\rlenght time = %d, length data = %d\n\r", length(time[range]), length(res$s_mean)));
  #cat(sprintf("\ravg_min = %f, avg_max = %f, avg_range=%f\n\r", min(res$s_mean), max(res$s_mean), (max(res$s_mean)-min(res$s_mean))));
  #cat(sprintf("\r%f < var < %f\n\r", min(res$s_var), max(res$s_var)));

  return(res);

  print(res$filtered[1:10]);
}

killDevices <- function() {
#  print("devices:");
#  print(dev.list());
#  print("current:");
#  print(dev.cur());
  if(length(dev.list()) > 0) { 
    for(device in dev.list()) {
      dev.off(device); 
    }
  }
}

killDevices();

plotSingleSignal <- function(data, signal, title, range=1:length(data[[signal]])) {
  dev.new(width=8, height=5);
  layout(matrix(c(1), 1, 1, byrow=TRUE), heights=c(1,1));
  print(colnames(data))
  print("sig")
  print(signal)
  print(length(data[[signal]]))
  print("time")
  print(length(data$Time))
  #length(data$Time)
  plotSignal(data[[signal]], time=data$Time, range=range, name=title);
}

plotMultipleSignals <- function(data, signals, titles, ranges=lapply(signals, function(x){seq_along(data[[x]])}), horizontal=TRUE) {
  dev.new(width=8, height=5);
  layout(matrix(1:length(signals), ifelse(horizontal, 1, length(signals)), ifelse(horizontal, length(signals), 1), byrow=TRUE), heights=rep(1,length(signals)));
  print(colnames(data))
  print("sig")
  print(length(signals))
  print("time")
  print(length(data$Time))
  #length(data$Time)
  for(i in seq_along(signals)) {
    cat(sprintf("\nplotting %d samples for %s as %s\n", length(ranges[[i]]), signals[[i]], titles[[i]]));
    plotSignal(data[[ signals[[i]] ]], time=data$Time, range=ranges[[i]], name=titles[[i]]);
  }
}

plotSingleSignalWithDeviation <- function(data, signal, title, range) {
  dev.new(width=4, height=5);
  layout(matrix(c(1,1,2,2), 2, 2, byrow=TRUE), heights=c(2,1));
  par(mar=c(4,4,4.5,1) + 0.1, ps=7);
  plotSignalAndDeviation(
    data[[signal]], 
    time=data$Time, 
    clk=data$Step, 
    range=range, 
    name=title
  );
}

plotTwoSignalsWithDeviation <- function(data, signals, titles, range) {
  dev.new(width=6, height=5);
  layout(matrix(c(1,3,2,4), 2, 2, byrow=TRUE), heights=c(1.5,1));
  par(mar=c(4,4,4.5,1) + 0.1, ps=7);
  plotSignalAndDeviation(
    data[[signals[1]]], 
    time=data$Time, 
    clk=data$Step, 
    range=range, 
    name=titles[[1]]
  );
  par(mar=c(4,4,4.5,1) + 0.1, ps=7);
  plotSignalAndDeviation(
    data[[signals[2]]], 
    time=data$Time, 
    clk=data$Step, 
    range=range, 
    name=titles[[2]]
  );
}

plotFFT <- function(data) { 
  plot((1:length(data))/length(data), abs(fft(data))) 
};

plotMultipleSignals(data, c("A", "B", "Hall"), c("Signal A", "Signal B", "Hall Effect Sensor"), horizontal=FALSE);