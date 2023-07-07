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
        listen = F)
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
