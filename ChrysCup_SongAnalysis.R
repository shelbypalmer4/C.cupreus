

#### Measuring Songs of African Emerald Cuckoos (Chrysococcyx cupreus)####

library(tuneR)
library(seewave)
library(warbleR)

# get metadata 
setwd("C:/Users/Shelby Palmer/Desktop/C.cupreus")
# CC_metadata<-data.frame(query_xc("Chrysococcyx cupreus"))
# write.csv(CC_metadata, file="CC_metadata.csv")

# test: get signal periods
setwd("C:/Users/Shelby Palmer/Desktop/C.cupreus/practice_recordings")
CC1 <- readMP3(list.files()[2])
CC1 <- fir(CC1,
           from = 1000,
           to = 6000, 
           bandpass = T,
           output = "Wave")

# can I get accurate signal periods across a whole recording?
CCtest <- timer(CC1,
                dmin=0.03, 
                envt="hil", 
                msmooth=c(512, 95), 
                threshold=5,
                plot = F #change to T to see where measurements are taken
                ) 
CCtest # not possible to tell just from the measurements/image

# use note onset intervals to find where songs separate (longer than 1s)
diff(CCtest$s.start)
which(diff(CCtest$s.start)>1)

# get just the first song
CCtest_1 <- cutw(CC1,
                 from = CCtest$s.start[1]-0.1,
                 to = CCtest$s.end[which(diff(CCtest$s.start)>1)][1]+0.1,
                 output = "Wave")
# check if it worked: yes
spectro(CCtest_1,
        ovlp = 95,
        scale = F,
        flim = c(0,6),
        listen = F,
        grid = F)
par(new = T)
timer(CCtest_1,
      dmin=0.03, 
      envt="hil", 
      msmooth=c(512, 95), 
      threshold=5)

# now get the second song
CCtest_2 <- cutw(CC1,
                 from = CCtest$s.start[which(diff(CCtest$s.start)>1)+1][1]-0.1,
                 to = CCtest$s.end[which(diff(CCtest$s.start)>1)][2]+0.1,
                 output = "Wave")
# check if it worked: yes
spectro(CCtest_2,
        ovlp = 95,
        scale = F,
        flim = c(0,6),
        listen = F)
par(new = T)
timer(CCtest_2,
      dmin=0.03, 
      envt="hil", 
      msmooth=c(512, 95), 
      threshold=5)

# does it work across the whole recording? Yes; each signal period corresponds to a note (this is an A+ quality file)
for (i in 1:length(which(diff(CCtest$s.start)>1))) {
  spectro(cutw(CC1,
               from = CCtest$s.start[which(diff(CCtest$s.start)>1)+1][i]-0.1,
               to = CCtest$s.end[which(diff(CCtest$s.start)>1)][i+1]+0.1,
               output = "Wave"),
          ovlp = 95,
          scale = F,
          flim = c(0,6),
          listen = F)
  par(new = T)
  timer(cutw(CC1,
             from = CCtest$s.start[which(diff(CCtest$s.start)>1)+1][i]-0.1,
             to = CCtest$s.end[which(diff(CCtest$s.start)>1)][i+1]+0.1,
             output = "Wave"),
        dmin=0.03, 
        envt="hil", 
        msmooth=c(512, 95), 
        threshold=5)
}


# best to make a dataframe with note measurements?
# WARNING. not replicable; specific to this recording
CC1df <- data.frame(note_number = rep(1:4, 22))
for (i in 1:length(CC1df$note_number)) {
  CC1df$duration[i] <- CCtest$s[i]
  CC1df$max_freq[i] <- max(dfreq(CC1,
                              tlim = c(CCtest$s.start[i],
                                       CCtest$s.end[i]),
                              ovlp = 98,
                              wl = 1024,
                              plot = F)[,2])
  CC1df$min_freq[i] <- min(dfreq(CC1,
                              tlim = c(CCtest$s.start[i],
                                       CCtest$s.end[i]),
                              ovlp = 98,
                              wl = 1024,
                              plot = F)[,2])
  CC1df$median_freq[i] <- median(dfreq(CC1,
                                 tlim = c(CCtest$s.start[i],
                                          CCtest$s.end[i]),
                                 ovlp = 98,
                                 wl = 1024,
                                 plot = F)[,2])
  CC1df$onset_freq[i] <- dfreq(CC1,
                               tlim = c(CCtest$s.start[i],
                                        CCtest$s.end[i]),  
                               ovlp = 98,
                               wl = 1024,)[,2][1]
  CC1df$offset_freq[i] <- dfreq(CC1,
                                tlim = c(CCtest$s.start[i],
                                        CCtest$s.end[i]),  
                                ovlp = 98,
                                wl = 1024,)[,2][length(dfreq(CC1,
                                                             tlim = c(CCtest$s.start[i],
                                                                      CCtest$s.end[i]),  
                                                             ovlp = 98,
                                                             wl = 1024,)[,2])]
  CC1df$sd_freq[i] <- sd(dfreq(CC1,
                               tlim = c(CCtest$s.start[i],
                                          CCtest$s.end[i]),
                               ovlp = 98,
                               wl = 1024,
                               plot = F)[,2])
  CC1df$abs_slope[i] <- max(abs(diff(dfreq(CC1,
                                        tlim = c(CCtest$s.start[i],
                                                 CCtest$s.end[i]),
                                        ovlp = 98,
                                        wl = 1024,
                                        plot = F)[,2])))
  CC1df$peak_freq[i] <- fpeaks(meanspec(cutw(CC1, 
                                          from = CCtest$s.start[i],
                                          to = CCtest$s.end[i], 
                                          output = "Wave")), 
                            plot = F, 
                            nmax = 1)[1,1]
}

head(CC1df)

CC1

spectro(b1, 
        wl = 512, 
        ovlp = 95,
        collevels = seq(-42,0,6), 
        flim = c(0, 10),
        osc = F, 
        scale = F, 
        grid = F, 
        cexlab = 0.8, 
        cexaxis = 1.1,
        palette = reverse.gray.colors.2,
        tlab = NULL,
        flab = NULL)

#### 28 July 2023: make a folder of usable recordings in the github repo ####
setwd("C:/Users/Shelby Palmer/Desktop/C.cupreus")
list.files()
scores <- read.csv("Recording_Scoring_July_20.csv")
head(scores)
length(rownames(scores))

# all of the recordings which we agreed were 3 or better
scores[which(scores$Dan<=3 & scores$Shelby<=3),]

# all of the recordings on which we disagreed
disagreements <- rbind(scores[which(scores$Dan>3 & scores$Shelby<=3),],
                       scores[which(scores$Dan<=3 & scores$Shelby>3),])
# the borderline cases
disagreements[which(disagreements$Dan %in% c(3,4) & disagreements$Shelby %in% c(3,4)),]

# the consensus cases
# oh no my battery is dying
disagreements[which(disagreements$Recording_ID %in% c(718994,582801601,567357981,390677901)),]

# dataframe with all the recordings that passed QC1
usables <- rbind(scores[which(scores$Dan<=3 & scores$Shelby<=3),],
                 disagreements[which(disagreements$Dan %in% c(3,4) & disagreements$Shelby %in% c(3,4)),],
                 disagreements[which(disagreements$Recording_ID %in% c(718994,582801601,567357981,390677901)),])

write.csv(usables, "usable_recordings.csv")


#### 1 September 2023: write a SNR function that works on chopped recordings ####
library(seewave)
library(tuneR)
setwd("C:/Users/Shelby Palmer/Desktop/C.cupreus/chopped_recordings")
cc <- readWave(list.files()[1])
# x is a wav file 
realSNR <- function (x, dmin, threshold) {
  per <- timer(x, 
               threshold = threshold, 
               dmin = dmin, 
               msmooth = c(512, 98),
               plot = F)
  noiseamp <- mean(env(cutw(x,
                            from = 0,
                            to = per$s.start[1],
                            output = "Wave"),
                       msmooth = c(512, 98),
                       plot = F))
  for (i in 1:length(per$s.start)) {
    sigamp <- mean(env(cutw(x,
                            from = per$s.start[i],
                            to = per$s.end[i],
                            output = "Wave"),
                       msmooth = c(512, 98),
                       plot = F))
  }
  return(realSNR)
}

realSNR(cc, 0.05, 10)

# Filter and resample all recordings with individual songs
## resample to 22050 kHz, 16 bits

filt_resamp <- function(x) {
  #set wd to input folder
  setwd("C:/Users/dzapa/OneDrive/Documents/GitHub/C.cupreus/chopped_recordings")
  
  #read in sound file
  wav <- readWave(x)
  
  #apply butterworth filter, order=5, low-pass cutoff=2000 Hz
  wav_fir <- fir(wav, from = 1000, to=5100, output="Wave")
  ## bwfilter automatically converts down to 16 bits (can't handle more)
  
  #downsample to 22050 Hz sampling rate
  wav_fir_rs <- 
    if (wav_fir@samp.rate != 22050) {
      resamp(wav_fir, g = 22050, output = "Wave")
    } else {
      wav_fir}
      
  ## also automatically converts anything >16 bits down to 16 bits
  
  #need to rescale wave object so it can be written to a Wave file
  wav_final <- normalize(wav_fir_rs, unit=c("16"))
  
  #change working directory to the output folder
  setwd("C:/Users/dzapa/OneDrive/Documents/GitHub/C.cupreus/Filtered")
  
  #save this modified sound in the output folder
  ## if extensible=T file won't open in some sound analysis programs
  writeWave(wav_final, filename=paste("filtered", x, sep="_"), extensible=F)
  
}

# apply to all recordings

files <- list.files()

lapply(files, FUN=filt_resamp)


#### 10 September 2023: Create spectrograms with timer() overlay for recording quality check and timer() amplitude threshold determination ####

# set working directory to the location of the sound files
setwd("C:/Users/Shelby Palmer/Desktop/C.cupreus/filter_resamp")
head(list.files())

# manually make a new folder in the working directory for the figures named "QC_10_figs"

# function for generating images
cutspec10 <- function(x) {
  a <- readWave(x)
  png(filename = paste(getwd(), "/QC_10_figs/", x, ".png", sep = ""),
      width = 800,
      height = 300)
  spectro(a, 
          wl = 512, 
          ovlp = 95, 
          collevels = seq(-42,0,6),
          flim = c(0, 7),
          osc = F, 
          scale = F, 
          colgrid = "gray", 
          cexlab = 0.8,
          cexaxis = 0.7)
  par(new=T)
  try(timer(a, 
            dmin = 0.05,
            envt = "hil",
            msmooth=c(512, 90),
            threshold = 10))
  dev.off()
}
# apply over working directory
lapply(list.files(pattern = ".wav"), cutspec10)

# now checking a higher threshold
# manually make a new folder in the working directory for the figures named "QC_12_figs"

# function for generating images with amplitude threshold of 12%
cutspec12 <- function(x) {
  a <- readWave(x)
  png(filename = paste(getwd(), "/QC_12_figs/", x, ".png", sep = ""),
      width = 800,
      height = 300)
  spectro(a, 
          wl = 512, 
          ovlp = 95, 
          collevels = seq(-42,0,6),
          flim = c(0, 7),
          osc = F, 
          scale = F, 
          colgrid = "gray", 
          cexlab = 0.8,
          cexaxis = 0.7)
  par(new=T)
  try(timer(a, 
            dmin = 0.05,
            envt = "hil",
            msmooth=c(512, 95),
            threshold = 12))
  dev.off()
}
# apply over working directory
lapply(list.files(pattern = ".wav"), cutspec12)


