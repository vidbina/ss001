source("helpers.R")

# On workbench: kept the magnet fixed at a given orientation for a while, then repositioned it a few times (which should explain the jumps).
stability_check <- "poll_2014-07-03-153851.csv";
# On rig: Kept blind perpendicular to rail, then started rotating until blind is parallel to rail
rig_fix_rot <- "poll_2014-07-03-173816.csv";
# On rig: Blinds are shut (perpendicular to rails with ridges pointing inwards)
fix_perpendicular <- "poll_2014-07-03-183851.csv";

fix_video <- "poll_2014-07-03-192140.csv";
fix_open <- "poll_2014-07-03-190916.csv";
fix_closing <- "poll_2014-07-03-191113.csv";
fix_closed <- "poll_2014-07-03-191233.csv";
fix_opening <- "poll_2014-07-03-191351.csv";
fix_reopened <- "poll_2014-07-03-191634.csv";

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

plotsignal <- function(amplitude, time, range=1:length(time), name="unnamed set") {
  title = paste("acquired samples from", name, sep=" ");
  print(title);
  print(length(range));
  print(length(amplitude));
  print(length(time));
  mean <- mean(amplitude[range])
  plot(time[range], amplitude[range], col="blue", type="l", main="signal", ann=false);
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

plotSensorInput <- function(data, range=seq_along(data$Time)) {
  dev.new(width=6, height=5);
  layout(matrix(c(1,1,2,2), 2, 2, byrow=TRUE), heights=c(2,1));
  par(mar=c(4,4,2,1) + 0.1, ps=7);
  #par(mar=c(2.5,3,3,1) + 0.1, ps=5, ylbias=1.2, tcl=-0.5, tck=-0.01);
  #par(mar=c(3,4,3,1) + 0.1, ps=7, ylbias=1.2, tcl=0, tck=-0.025);
  plot(data$Time[range], data$A[range], col="blue", type="l", main="A", ann=FALSE, axes=TRUE, ylim=range(c(data$B[range]), data$A[range]));
  lines(data$Time[range], data$B[range], col="green", pch="22", type="l", main="B");
  #axis(1, mgp=c(0,0,0));
  #axis(2, mgp=c(2,0.75,0));
  mtext(side=1, text="time in milliseconds", line=2);
  mtext(side=2, text="amplitude", line=2);
  mtext(side=3, text=bquote(bold(.("M"))), line=0);

  #par(mar=c(3,3,1,1) + 0.1, ps=7);
  #par(mar=c(2,2,2,2) + 0.1, ps=7, tcl=10, tck=1);
  par(mar=c(4,4,2,1) + 0.1);
  plot(data$Time[range], data$Hall[range], col="blue", type="l", main="A", ann=FALSE, axes=TRUE);
  #box("plot", col="red");
  #axis(1, mgp=c(0,0,0));
  #axis(2, mgp=c(2,0.75,0));
  mtext(side=3, text=bquote(bold(.("Hall"))), line=0);
  mtext(side=1, text="time in milliseconds", line=2);
  mtext(side=2, text="amplitude", line=2);
  #box("figure", col="blue");
}

plotHistogram <- function(data, signal, title, range=seq_along(data$Time)) {
  #plot(data$Time, data[[signal]], type="h");
  hist(data[[signal]][range], xlab=signal, main=title, breaks=100);
}

plotFFT <- function(data) { 
  plot((1:length(data))/length(data), abs(fft(data))) 
};

plotSensorsAndHist <- function(file,range=FALSE) {
  killDevices();
  data <- read.csv(file=file, sep=";", header=TRUE);
  r <- ifelse(range==FALSE, seq_along(data$Time), seq_along(data$Time));
  if(range == FALSE) {
    q <- seq_along(data$Time);
  } else {
    q <- min(range[1], length(data$Time)):min(c(length(range), length(data$Time)));
  }
  plotSensorInput(data, q);

  dev.new(width=6, height= 6);
  layout(matrix(c(1,1,2,2), 2, 2, byrow=TRUE), heights=c(1,1));
  plotHistogram(data, "A", "Occurrence frequency for A", range=q);
  plotHistogram(data, "B", "Occurrence frequency for B", range=q);

  dev.new(width=6, height= 3);
  plotHistogram(data, "Hall", "Occurrence frequency for hall sensor", range=q);
  cat(sprintf("\n\rOver %d samples signal A ranges from %d to %d (%d units), while B ranges from %d to %d (%d units)\n\r", length(data$Time), range(data$A[q])[1], range(data$A[q])[2], range(data$A[q])[2]-range(data$A[q])[1], range(data$B[q])[1], range(data$B[q])[2], range(data$B[q])[2]-range(data$B[q])[1]));
}

renderCase <- function(case, range=FALSE) {
  fixes <- c(rig_fix_rot, fix_perpendicular, fix_video, fix_open, fix_closing, fix_closed, fix_opening, fix_reopened, rig_fix_rot);
  plotSensorsAndHist(fixes[case], range);
}

cat(sprintf("\n\rrenderCase(X, RANGE)\n\r  there are 8 cases, see notes specify case by integer id\n\r  range is optional\n\r"));
