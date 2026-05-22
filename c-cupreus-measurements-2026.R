######## Initial measurement run ##########
setwd("C:/Users/spalm/OneDrive - University of Florida/Desktop/C.cupreus/Trial recs")
library(seewave)
library(tuneR)
library(dplyr)

# Functions for threshold-based frequency measurements

# test sound
test1 <- readWave(list.files()[10]) %>%
  fir(., from = 1000, to = 8000, bandpass = T, output = "Wave")
if (test1@samp.rate!=44100) {
  test1 <- resamp(test1, g = 44100, output = "Wave")
}


# Simply returns the mean frequency spectrum for song sections
fspec <- function(sound, section, img) {
  # compute over only first or second song section
  if (section == "first") {
    spect_fir <- meanspec(sound,
                          from = 0,
                          to = duration(sound)*0.47,
                          wl = 1024, ovlp = 98,
                          plot = img)
  }
  
  if (section == "second") {
    spect_fir <- meanspec(sound,
                          from = duration(sound)*0.47,
                          to = duration(sound),
                          wl = 1024, ovlp = 98,
                          plot = img)
  }
  
  # return frequency spectrum
  return(spect_fir)
  
}
fspec(test1, "first", img = F)

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
  
  # return frequency values above and below upper and lower percentile
  return(spect_fir[which(cufr>energy_lower&cufr<energy_upper),1])
  
}

fspec_percentile(sound = test1, lower = 0.1, upper = 0.9, section = "first")

# 22 May 2026: Write a function that defines frequencies by measuring down from the peak (the old way)
fspec_crit <- function(sound, section, crit) {
  # compute over only first or second song section
  if (section == "first") {
    spect_fir <- meanspec(sound,
                          from = 0,
                          to = duration(sound)*0.47,
                          wl = 1024, ovlp = 98,
                          dB = "max0",
                          plot = F)
  }
  
  if (section == "second") {
    spect_fir <- meanspec(sound,
                          from = duration(sound)*0.47,
                          to = duration(sound),
                          wl = 1024, ovlp = 98,
                          dB = "max0",
                          plot = F)
  }
  
  
  # return frequency spectrum above the critical value
  return(
    spect_fir[which(spect_fir[,2]>crit),1]
  )
  
}
fspec_crit(sound = test1, section = "first", crit = -20)

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


#### Initial measurement run: Percentiles ####
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

#### Initial measurement run: Critical value (22 May 2026) ####
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
      fspec_crit(
        sound = a, section = cc_run1$section[j], crit = -20
      )
    )
    cc_run1$minfreq[j] <- min(
      fspec_crit(
        sound = a, section = cc_run1$section[j], crit = -20
      )
    )
    cc_run1$peakfreq[j] <- peakfreq(sound = a, section = cc_run1$section[j])
    cc_run1$specentropy[j] <- sh(fspec(sound = a, section = cc_run1$section[j], img = F))
  }
}

# write
write.csv(cc_run1, 
          "C:/Users/spalm/OneDrive - University of Florida/Desktop/C.cupreus/test-measurements-2026_crit.csv",
          row.names = F)


#### 28 October 2024: Determine number of unique lat/long combinations (this will be our proxy for individual) ####
setwd("C:/Users/spalm/Desktop/C.cupreus")
library(tools)
library(dplyr)

xcmeta.cc <- read.csv("XC_metadata.csv")
mlmeta.cc <- read.csv("ML_metadata.csv")
usable.cc <- read.csv("usable_recordings.csv")

# rename ID column for ML recordings
colnames(mlmeta.cc)[which(colnames(mlmeta.cc)=="ML.Catalog.Number")] <- "Recording_ID"

# get a dataframe with lat/long, recording ID, and locality
allmeta.cc <- rbind(xcmeta.cc[,which(colnames(xcmeta.cc)%in%c("Latitude", "Longitude", "Recording_ID", "Locality", "Country"))],
                    mlmeta.cc[,which(colnames(mlmeta.cc)%in%c("Latitude", "Longitude", "Recording_ID", "Locality", "Country"))])


# this is the full file path of the cleaned dataset
cleanedwd <- paste(getwd(), "/Clean_44_1_k", sep = "")

## How to extract the ID numbers?
# for ML recordings that were cleaned and renamed accordingly:
strsplit(list.files(cleanedwd)[400], split = "_")[[1]][2]
# for recordings from xeno-canto:
strsplit(
  strsplit(list.files(cleanedwd)[200], split = "-")[[1]][3],
  split = "_"
)[[1]][1]
# for ML recordings that weren't cleaned:
strsplit(list.files(cleanedwd)[1], split = "_")[[1]][1]


# get a vector with the names of all the recordings in the final dataset (before filteriing for locality duplicates)
# index with conditional statement based on the first part of the string

songids <- rep(NA, length = length(list.files(cleanedwd)))

for (i in 1:length(list.files(cleanedwd))) {
  if (!(strsplit(list.files(cleanedwd)[i], split = "_")[[1]][1]%in%c("clean", "Chrysococcyx"))){
    songids[i] <- strsplit(list.files(cleanedwd)[i], split = "_")[[1]][1]
  }
  if (strsplit(list.files(cleanedwd)[i], split = "_")[[1]][1]=="clean") {
    songids[i] <- strsplit(list.files(cleanedwd)[i], split = "_")[[1]][2]
  }
  if (strsplit(list.files(cleanedwd)[i], split = "-")[[1]][1]=="Chrysococcyx") {
    songids[i] <- strsplit(
      strsplit(list.files(cleanedwd)[i], split = "-")[[1]][3],
      split = "_"
    )[[1]][1]
  } 
  
}

songids

# Subset the metadata
allmeta.cc.final <- allmeta.cc[which(allmeta.cc$Recording_ID %in% songids),]
# How many unique lat/long?
unique(allmeta.cc.final$Longitude)
unique(allmeta.cc.final$Latitude)

# the lat and long duplicates occupy the same rows and thus are true locality duplicates
which(duplicated(allmeta.cc.final$Latitude)) == which(duplicated(allmeta.cc.final$Longitude))

# make a vector of unique lat/longs to be used for data simulations
uniquelatlongs <- data.frame(latitude = unique(allmeta.cc.final$Latitude),
                             longitude = unique(allmeta.cc.final$Longitude)) %>%
  subset(., !is.na(uniquelatlongs$latitude))

# Can the NAs be salvaged?
allmeta.cc.final[which(is.na(allmeta.cc.final$Latitude)),]
# We can possibly use locality and country to assign a rough lat/long for the NAs to save data
#
#
#
######## SIMULATIONS at the section level: 2026 ##########
# extract measurements from just 1 clean recording for simulations
setwd("C:/Users/spalm/OneDrive - University of Florida/Desktop/C.cupreus/sims_test")
library(tuneR)
library(seewave)
library(dplyr)
simstest <- readWave(list.files()[1]) %>%
  fir(., from = 1000, to = 5000, bandpass = T, output = "Wave")
dftest <- dfreq(simstest, threshold = 20, wl = 512, ovlp = 20, plot = T)
s1df <- dftest[1:round(length(dftest[,1])*0.47),]
s1df <- s1df[which(s1df[,2]>1),]
diff(na.omit(s1df[,2]))

sims2026 <- data.frame(filename = list.files(),
                      s1_maxfreq = rep(NA, length(list.files())),
                      s2_maxfreq = rep(NA, length(list.files())),
                      s1_minfreq = rep(NA, length(list.files())),
                      s2_minfreq = rep(NA, length(list.files())),
                      s1_peakfreq = rep(NA, length(list.files())),
                      s2_peakfreq = rep(NA, length(list.files())),
                      s1_specentropy = rep(NA, length(list.files())),
                      s2_specentropy = rep(NA, length(list.files())),
                      s1_maxslope = rep(NA, length(list.files())),
                      s2_maxslope = rep(NA, length(list.files())),
                      s1_minslope = rep(NA, length(list.files())),
                      s2_minslope = rep(NA, length(list.files()))
                      )

for (i in 1:length(list.files(pattern = "wav"))) {
  # read in, filter, resample if needed
  a <- readWave(list.files(pattern = "wav")[i]) %>%
    fir(., from = 1000, to = 5000, bandpass = T, output = "Wave")
  if (a@samp.rate!=44100) {
    a <- resamp(a, g = 44100, output = "Wave")
  }
  # get section 1 and 2 dominant frequency traces
  dftrace <- dfreq(a, threshold = 20, wl = 512, ovlp = 20, plot = F)
  s1df <- dftrace[1:round(length(dftest[,1])*0.47),]
  s2df <- dftrace[round(length(dftrace[,1])*0.47):length(dftrace[,1]),]
  # get measurements
  sims2026$s1_maxfreq[i] = max(
    fspec_crit(sound = a, crit = -20, section = "first")
    )
  sims2026$s2_maxfreq[i] = max(
    fspec_crit(sound = a, crit = -20, section = "second")
  )
  sims2026$s1_minfreq[i] = min(
    fspec_crit(sound = a, crit = -20, section = "first")
  )
  sims2026$s2_minfreq[i] = min(
    fspec_crit(sound = a, crit = -20, section = "second")
  )
  sims2026$s1_peakfreq[i] = peakfreq(sound = a, section = "first")
  sims2026$s2_peakfreq[i] = peakfreq(sound = a, section = "second")
  sims2026$s1_specentropy[i] = sh(
    fspec(sound = a, section = "first", img = F)
  )
  sims2026$s2_specentropy[i] = sh(
    fspec(sound = a, section = "second", img = F)
  )
  sims2026$s1_maxslope[i] = max(diff(s1df[which(between(x = s1df[,2], 1, 5)),2]))
  sims2026$s2_maxslope[i] = max(diff(s2df[which(between(x = s2df[,2], 1, 5)),2]))
  sims2026$s1_minslope[i] = min(diff(s1df[which(between(x = s1df[,2], 1, 5)),2]))
  sims2026$s2_minslope[i] = min(diff(s2df[which(between(x = s2df[,2], 1, 5)),2]))
}
View(sims2026)
# write out
write.csv(sims2026, 
          "C:/Users/spalm/OneDrive - University of Florida/Desktop/C.cupreus/simsdf_full_crit.csv",
          row.names = F)

#### Simulations at the note slice level 19 OCTOBER 2024; re-run at the song section level 24 March 2026 ####
#
setwd("C:/Users/spalm/OneDrive - University of Florida/Desktop/C.cupreus")

# Read in data: Measurements taken from a single clean recording
# simsdf <- read.csv("simsdf_full.csv")
simsdf <- sims2026
# unique numbers of songs produced
n_songs <- c(1:12,14,15,18,22,29)
# number of recordings for which the corresponding n_songs is true
number_of_samp_size_occurrence <- c(8,9,10,7,17,12,7,6,6,4,3,3,1,1,1,1,1)

# make a vector whose length = total number of recordings and whose content reflects number of songs per recording (sum of nrec = total number of songs in dataset)
nrec <- rep(n_songs[1], number_of_samp_size_occurrence[1])
for (i in 2:length(n_songs)) {
  nrec <- c(nrec, 
            rep(n_songs[i], number_of_samp_size_occurrence[i])
  )
}


# Make a vector of names of individuals to be simulated
names <- rep(paste("ind", 1, sep = "_"), 
             length = nrec[1])
for(i in 2:length(nrec)) {
  names <- c(names, rep(paste("ind", i, sep = "_"), 
                        length = nrec[i]))
}

names

# We need to simulate each measurement with means based on the real measurements from `simsdf`
i <- 1 # first individual
j <- 2 # second column, first measurement (first column is IDs)
rnorm(n=nrec[i], 
      mean=rnorm(n = 1,
                 mean=mean(simsdf[,j]), 
                 sd=sd(simsdf[,j])), # add noise to the individual mean that's sampled
      sd=sd(simsdf[,j])/5) # add less noise between an individual's songs

# The standard deviation values taken from `simsdf` can (should?) be replaced by constants


#####

# establish a dataframe where we'll put the simulated measurements
actualsims <- as.data.frame(
  matrix(data = NA,
         nrow = sum(nrec),
         ncol = length(colnames(simsdf)))
)

# give the columns names according to `simsdf`
colnames(actualsims) <- colnames(simsdf)

# add simulated individual names to the ID column
actualsims$filename <- names


# loop
# sorry for using h
# we start at 2 because the first column is just ID's
for (h in 2:length(colnames(actualsims))) {
  # simulate measurements of the hth column for only the first value of `nrec`
  varh <- rnorm(n=nrec[1], 
                mean=rnorm(n = 1,
                           mean=mean(simsdf[,h]), 
                           sd=sd(simsdf[,h])),
                sd=sd(simsdf[,h])/5)
  # fill in with the rest of the values indicated by `nrec`
  for(i in 2:length(nrec)) {
    varh <- c(varh, rep(
      rnorm(n=nrec[i], 
            mean=rnorm(n = 1,
                       mean=mean(simsdf[,h]), 
                       sd=sd(simsdf[,h])),
            sd=sd(simsdf[,h])/5),
      length = nrec[i]
    )
    )
  }
  # add the concatenated values to the hth column
  actualsims[,h] <- varh
}

# write out a .csv file
write.csv(actualsims, "sim-meas-2026.csv", row.names = F)

#### Spectrograms with frequency measurements for quality checks ####
png(filename = "qc-meas-test.png",
    height = 500,
    width = 800)
spectro(simstest, 
        wl = 512, ovlp = 95, 
        palette = reverse.gray.colors.2,
        flim = c(0,6), scale = F,
        grid = F,
        main = list.files(pattern = "wav")[1])
# delineate sections
abline(v = duration(simstest)*0.47)
# section 1
segments(
  y0 = max(
    fspec_percentile(sound = simstest, 
                     lower = 0.05, upper = 0.95, 
                     section= "first")),
  y1 = max(
    fspec_percentile(sound = simstest, 
                     lower = 0.05, upper = 0.95, 
                     section= "first")),
  x0 = 0, x1 = duration(simstest)*0.47,
  col = "red"
)
segments(
  y0 = min(
    fspec_percentile(sound = simstest, 
                     lower = 0.05, upper = 0.95, 
                     section= "first")),
  y1 = min(
    fspec_percentile(sound = simstest, 
                     lower = 0.05, upper = 0.95, 
                     section= "first")),
  x0 = 0, x1 = duration(simstest)*0.47,
  col = "green"
)
segments(
  y0 = peakfreq(sound = simstest, section = "first"),
  y1 = peakfreq(sound = simstest, section = "first"),
  x0 = 0, x1 = duration(simstest)*0.47,
  col = "cyan"
)
# section 2
segments(
  y0 = max(
    fspec_percentile(sound = simstest, 
                     lower = 0.05, upper = 0.95, 
                     section= "second")),
  y1 = max(
    fspec_percentile(sound = simstest, 
                     lower = 0.05, upper = 0.95, 
                     section= "second")),
  x0 = duration(simstest)*0.47, x1 = duration(simstest),
  col = "red"
)
segments(
  y0 = min(
    fspec_percentile(sound = simstest, 
                     lower = 0.05, upper = 0.95, 
                     section= "second")),
  y1 = min(
    fspec_percentile(sound = simstest, 
                     lower = 0.05, upper = 0.95, 
                     section= "second")),
  x0 = duration(simstest)*0.47, x1 = duration(simstest),
  col = "green"
)
segments(
  y0 = peakfreq(sound = simstest, section = "second"),
  y1 = peakfreq(sound = simstest, section = "second"),
  x0 = duration(simstest)*0.47, x1 = duration(simstest),
  col = "cyan"
)
dev.off()


#
#
#
#

# spectros with measurements only on the new batch
setwd("C:/Users/spalm/OneDrive - University of Florida/Desktop/C.cupreus/NEW_BATCH unfiltered")

for (i in 1:length(list.files(pattern = "wav"))) {
  a <- readWave(list.files(pattern = "wav")[i])
  if (a@samp.rate!=44100) {
    a <- resamp(a, g = 44100, output = "Wave")
  }
  a <- fir(a, from = 1500, to = 5000, bandpass = T, output = "Wave")
  png(filename = paste("C:/Users/spalm/OneDrive - University of Florida/Desktop/C.cupreus/quality-check-spectros-2026/",
                       list.files()[i],
                       "-spectro.png", sep = ""),
      height = 500,
      width = 800)
  spectro(a, 
          wl = 512, ovlp = 95, 
          palette = reverse.gray.colors.2,
          flim = c(0,6), scale = F,
          grid = F,
          main = list.files(pattern = "wav")[i])
  # delineate sections
  abline(v = duration(a)*0.47)
  # section 1
  segments(
    y0 = max(
      fspec_percentile(sound = a, 
                       lower = 0.2, upper = 0.8, 
                       section= "first")),
    y1 = max(
      fspec_percentile(sound = a, 
                       lower = 0.2, upper = 0.8, 
                       section= "first")),
    x0 = 0, x1 = duration(a)*0.47,
    col = "red"
  )
  segments(
    y0 = min(
      fspec_percentile(sound = a, 
                       lower = 0.2, upper = 0.8, 
                       section= "first")),
    y1 = min(
      fspec_percentile(sound = a, 
                       lower = 0.2, upper = 0.8, 
                       section= "first")),
    x0 = 0, x1 = duration(a)*0.47,
    col = "green"
  )
  segments(
    y0 = peakfreq(sound = a, section = "first"),
    y1 = peakfreq(sound = a, section = "first"),
    x0 = 0, x1 = duration(a)*0.47,
    col = "cyan"
  )
  # section 2
  segments(
    y0 = max(
      fspec_percentile(sound = a, 
                       lower = 0.2, upper = 0.8, 
                       section= "second")),
    y1 = max(
      fspec_percentile(sound = a, 
                       lower = 0.2, upper = 0.8, 
                       section= "second")),
    x0 = duration(a)*0.47, x1 = duration(a),
    col = "red"
  )
  segments(
    y0 = min(
      fspec_percentile(sound = a, 
                       lower = 0.2, upper = 0.8, 
                       section= "second")),
    y1 = min(
      fspec_percentile(sound = a, 
                       lower = 0.2, upper = 0.8, 
                       section= "second")),
    x0 = duration(a)*0.47, x1 = duration(a),
    col = "green"
  )
  segments(
    y0 = peakfreq(sound = a, section = "second"),
    y1 = peakfreq(sound = a, section = "second"),
    x0 = duration(a)*0.47, x1 = duration(a),
    col = "cyan"
  )
  dev.off()

}


# checking a case where min freq appears = peak freq
i <- 34
meanspec(a, from = duration(a)*0.47, to = duration(a), 
         flim = c(0,6),
         wl = 1024, ovlp = 98)
abline(v = max(
  fspec_percentile(sound = a, 
                   lower = 0.1, upper = 0.9, 
                   section= "second")))
abline(v = min(
  fspec_percentile(sound = a, 
                   lower = 0.1, upper = 0.9, 
                   section= "second")))
abline(v = peakfreq(sound = a, section = "second"))
# two frequency peaks in the second part of the song...how to handle?

spectro(a, wl = 512, ovlp = 98, scale = F, 
        tlim = c(0, duration(a)*0.47))
par(new = T)
timer(a, msmooth = c(512, 98), plot = T, threshold = 30,
      tlim = c(0, duration(a)*0.47))
