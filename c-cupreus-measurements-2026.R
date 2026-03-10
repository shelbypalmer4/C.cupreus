######## Initial measurement run ##########
setwd("C:/Users/spalm/OneDrive - University of Florida/Desktop/C.cupreus/Trial recs")
library(seewave)
library(tuneR)

# Functions for threshold-based frequency measurements

# test sound
test1 <- readWave(list.files()[4]) %>%
  fir(., from = 1000, to = 8000, bandpass = T, output = "Wave")


# Simply returns the mean frequency spectrum for song sections
fspec <- function(sound, section) {
  # compute over only first or second song section
  
  if (section == "first") {
    spect_fir <- meanspec(sound,
                          from = 0,
                          to = duration(sound)*0.47,
                          wl = 1024, ovlp = 98,
                          plot = F)
  }
  
  if (section == "second") {
    spect_fir <- meanspec(sound,
                          from = duration(sound)*0.47,
                          to = duration(sound),
                          wl = 1024, ovlp = 98,
                          plot = F)
  }
  
  # return frequency spectrum
  return(spect_fir)
  
}
fspec(test1, "first")

# Write a function that defines frequencies containing a certain percentage of energy of sound

# sound is the object name of a sound file in r
# lower = the lower percentile value (decimal)
# upper = the upper percentile value (decimal)
# section = binary (define "first" or "second"), will cut at predetermined time (s) value written into function
fspec_percentile <- function(sound, lower, upper, section) {
  # compute over only first or second song section
  if (section == "first") {
    spect_fir <- meanspec(sound,
                          from = 0,
                          to = duration(sound)*0.47,
                          wl = 1024, ovlp = 98,
                          plot = F
    )
  }
  
  if (section == "second") {
    spect_fir <- meanspec(sound,
                          from = duration(sound)*0.47,
                          to = duration(sound),
                          wl = 1024, ovlp = 98,
                          plot = F
    )
  }
  
  # make a vector of cumulative amplitude values
  cufr <- rep(NA, length = length(spect_fir[,1]))
  cufr[1] <- spect_fir[1,2]
  for (i in 2:length(spect_fir[,1])) {
    cufr[i] <- spect_fir[i,2]+cufr[i-1]
  }
  
  # define percentiles
  energy_lower <- sum(spect_fir[,2])*lower
  energy_upper <- sum(spect_fir[,2])*upper
  
  # return frequency values above and below upper and lower percenti
  return(spect_fir[which(cufr>energy_lower&cufr<energy_upper),1])
  
}

fspec_percentile(sound = test1, lower = 0.1, upper = 0.9, section = "first")

# Peak frequency function
peakfreq <- function(sound, section) {
  # compute over only first or second song section
  if (section == "first") {
    spect_fir <- meanspec(sound,
                          from = 0,
                          to = duration(sound)*0.47,
                          wl = 1024, ovlp = 98,
                          plot = F)
  }
  
  if (section == "second") {
    spect_fir <- meanspec(sound,
                          from = duration(sound)*0.47,
                          to = duration(sound),
                          wl = 1024, ovlp = 98,
                          plot = F)
  }
  
  
  # return peak frequency
  return(
    spect_fir[which(spect_fir[,2]==max(spect_fir[,2])),1]
  )
  
}
peakfreq(sound = test1, section = "second")

#### Initial measurement run ####
cc_run1 <- data.frame(filename = rep(list.files(), 2),
                      section = c(rep("first", 10), rep("second", 10)),
                      maxfreq = rep(NA, 20),
                      minfreq = rep(NA, 20),
                      peakfreq = rep(NA, 20),
                      specentropy = rep(NA, 20))
for (i in 1:length(list.files())) {
  a <- readWave(list.files()[i]) %>%
    fir(., from = 1000, to = 8000, bandpass = T, output = "Wave")
  if (a@samp.rate!=44100) {
    a <- resamp(a, g = 44100, output = "Wave")
  }
  for (j in which(cc_run1$filename==list.files()[i])) {
    cc_run1$maxfreq[j] <- max(
      fspec_percentile(
        sound = a, lower = 0.25, upper = 0.75, section = cc_run1$section[j]
      )
    )
    cc_run1$minfreq[j] <- min(
      fspec_percentile(
        sound = a, lower = 0.25, upper = 0.75, section = cc_run1$section[j]
      )
    )
    cc_run1$peakfreq[j] <- peakfreq(sound = a, section = cc_run1$section[j])
    cc_run1$specentropy[j] <- sh(fspec(sound = a, section = cc_run1$section[j]))
  }
}

# some maximum measurements are definitely affected by sound

# write
write.csv(cc_run1, 
          "C:/Users/spalm/OneDrive - University of Florida/Desktop/C.cupreus/test-measurements-2026.csv",
          row.names = F)
