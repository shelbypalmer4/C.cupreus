################# ACOUSTIC MEASUREMENTS OF THE SONG OF CHRYSOCOCCYX CUPREUS #################

#### 10-17 September 2023: Write functions to extract the measurements listed in the cuckoo thoughts document, first at the note level ####
library(tuneR)
library(seewave)
## Turning the Podos mean frequency spectrum method into functions
# get the first note of an example recording
setwd("C:/Users/Shelby Palmer/Desktop/C.cupreus/filter_resamp")
ex <- readWave("filtered_Chrysococcyx-cupreus-280039_0.29.wav")
exT <- timer(ex, dmin = 0.05, envt = "hil", msmooth = c(512,95), threshold = 12, plot = F)
ex1 <- cutw(ex, from = exT$s.start[1], to = exT$s.end[1], output = "Wave")
spectro(ex,
        scale = F,
        flim = c(1, 5.1),
        wl = 256,
        ovlp = 95)

# if you eliminate all the object-making steps, the Podos method becomes this:
crit <- -25 # WARNING: arbitrary value
maxfreq <- max(meanspec(ex1, 
                        flim=c(1,8), 
                        wl = 1200, 
                        dB='max0')[,1][meanspec(ex1, 
                                                flim=c(1,8), 
                                                wl = 1200, 
                                                dB='max0')[,2]>crit])
minfreq <- min(meanspec(ex1, 
                        flim=c(1,8), 
                        wl = 1200, 
                        dB='max0')[,1][meanspec(ex1, 
                                                flim=c(1,8), 
                                                wl = 1200, 
                                                dB='max0')[,2]>crit])
meanfreq <- mean(meanspec(ex1, 
                          flim=c(1,8), 
                          wl = 1200, 
                          dB='max0')[,1][meanspec(ex1, 
                                                  flim=c(1,8), 
                                                  wl = 1200, 
                                                  dB='max0')[,2]>crit])
peakfreq <- meanspec(ex1, 
                     flim=c(1,8), 
                     wl = 1200, 
                     dB='max0')[,1][match(c(0),
                                          meanspec(ex1, 
                                                   flim=c(1,8), 
                                                   wl = 1200, 
                                                   dB='max0')[,2])]
bandwidth <- maxfreq-minfreq

# let's look at the distribution of the amplitude values
hist(meanspec(ex1, 
              flim=c(1,8), 
              wl = 1200, 
              dB='max0')[,2],
     breaks = 40)
# subset the mean frequency spectrum to only the values above -25
actualmeanspec <- meanspec(ex1, 
                           flim=c(1,8), 
                           wl = 1200, 
                           dB='max0')[which(meanspec(ex1, 
                                                     flim=c(1,8), 
                                                     wl = 1200, 
                                                     dB='max0')[,2]>-24),]
# histogram of this subset of amplitude values
hist(actualmeanspec[,2], breaks = 20)
# get the value marking the bottom of the upper quartile of these values (the top 25% threshold)
upperq <- quantile(actualmeanspec[,2], prob = 0.75, type = 1)
actualmeanspec[which(actualmeanspec[,2]>upperq),1]

#### FINALIZED FUNCTIONS START HERE ####
## Frequency measurements from the mean frequency spectrum
## Values that may need changing: wl, ovlp% !!!!!!!!!!!!!!
CCMaxFreq <- function(x,y) {
  z <- c()
  for (j in 1:length(y$s.start)) {
    z[j] <- max(meanspec(cutw(x,
                              from = y$s.start[j],
                              to = y$s.end[j],
                              output = "Wave"), 
                         flim=c(1, 5.1), 
                         wl = 512,
                         ovlp = 95,
                         dB='max0',
                         plot = F)[,1][meanspec(cutw(x,
                                                      from = y$s.start[j],
                                                      to = y$s.end[j],
                                                      output = "Wave"), 
                                                 flim=c(1, 5.1), 
                                                 wl = 512,
                                                 ovlp = 95, 
                                                 dB='max0',
                                                 plot = F)[,2]>crit])
  }
  return(z)
}

CCMinFreq <- function(x,y) {
  z <- c()
  for (j in 1:length(y$s.start)) {
    z[j] <- min(meanspec(cutw(x,
                              from = y$s.start[j],
                              to = y$s.end[j],
                              output = "Wave"), 
                         flim=c(1, 5.1), 
                         wl = 512,
                         ovlp = 95,
                         dB='max0',
                         plot = F)[,1][meanspec(cutw(x,
                                                     from = y$s.start[j],
                                                     to = y$s.end[j],
                                                     output = "Wave"), 
                                                flim=c(1, 5.1), 
                                                wl = 512,
                                                ovlp = 95, 
                                                dB='max0',
                                                plot = F)[,2]>crit])
  }
  return(z)
}

CCStdevFreq <- function(x,y) {
  z <- c()
  for (j in 1:length(y$s.start)) {
    z[j] <- sd(meanspec(cutw(x,
                              from = y$s.start[j],
                              to = y$s.end[j],
                              output = "Wave"), 
                         flim=c(1, 5.1), 
                         wl = 512,
                         ovlp = 95,
                         dB='max0',
                         plot = F)[,1][meanspec(cutw(x,
                                                     from = y$s.start[j],
                                                     to = y$s.end[j],
                                                     output = "Wave"), 
                                                flim=c(1, 5.1), 
                                                wl = 512,
                                                ovlp = 95, 
                                                dB='max0',
                                                plot = F)[,2]>crit])
  }
  return(z)
}


## Dominant frequency measurements
CCMaxDomFreq <- function(x,y) {
  z <- c()
  for (j in 1:length(y$s.start)) {
    z[j] <- max(dfreq(cutw(x,
                           from = y$s.start[j],
                           to = y$s.end[j],
                           output = "Wave",
                           plot = F), 
                      ovlp = 95,
                      wl = 512,
                      plot = F)[,2])
  }
  return(z)
}

CCMinDomFreq <- function(x,y) {
  z <- c()
  for (j in 1:length(y$s.start)) {
    z[j] <- min(dfreq(cutw(x,
                           from = y$s.start[j],
                           to = y$s.end[j],
                           output = "Wave",
                           plot = F), 
                      ovlp = 95,
                      wl = 512,
                      plot = F)[,2])
  }
  return(z)
}

CCMeanDomFreq<-function(x,y) {
  z <- c()
  for (j in 1:length(y$s.start)) {
    z[j] <- mean(dfreq(cutw(x,
                            from = y$s.start[j],
                            to = y$s.end[j],
                            output = "Wave",
                            plot = F),
                       ovlp = 95,
                       wl = 512,
                       plot = F)[,2])
  }
  return(z)
}

CCMedianDomFreq <- function(x,y) {
  z <- c()
  for (j in 1:length(y$s.start)) {
    z[j] <- median(dfreq(cutw(x,
                              from = y$s.start[j],
                              to = y$s.end[j],
                              output = "Wave",
                              plot = F),
                         ovlp = 95,
                         wl = 512,
                         plot = F)[,2])
  }
  return(z)
}

CCStdevDomFreq <- function(x,y) {
  z <- c()
  for (j in 1:length(y$s.start)) {
    z[j] <- sd(dfreq(cutw(x,
                          from = y$s.start[j],
                          to = y$s.end[j],
                          output = "Wave",
                          plot = F),
                     ovlp = 95,
                     wl = 512,
                     plot = F)[,2])
  }
  return(z)
}

CCDomFreqMaxSlope <- function(x,y) {
  z <- c()
  for (j in 1:length(y$s.start)) {
    z[j] <- max(abs(diff(dfreq(cutw(x,
                                    from = y$s.start[j],
                                    to = y$s.end[j],
                                    output = "Wave",
                                    plot = F),
                               ovlp = 95,
                               wl = 512,
                               threshold = 5,
                               plot = F)[,2])))
  }
  return(z)
}
