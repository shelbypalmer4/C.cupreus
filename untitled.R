setwd("C:/Users/spalm/OneDrive - University of Florida/Desktop/playing with sound")
library(seewave)
library(tuneR)
library(dplyr)

#### CARW ####
a<-readWave("CARW-chatter-rasp-FL.wav")
# new wave object with bandpass filter applied:
b1<-fir(a, 
        from = 1000, 
        to = 15000, 
        bandpass = TRUE,
        output="Wave") %>%
  cutw(., from = 0, to = 6, output = "Wave")
  
#### SBWR ####
d<-readWave("SBWR-zipper-BZ.wav")
e1<-fir(d, 
        from = 500, 
        to = 15000, 
        bandpass = TRUE,
        output="Wave") %>%
  cutw(., from = 0, to = 6, output = "Wave")

# spectrograms 
png(filename = "CARW-SBWR.png", 
    width = 10, height = 6, 
    res = 600, units = "in")

par(mfrow = c(2,1), 
    oma = c(2,1.5,0,0), 
    mar = c(3,3,2,2))
c1<-spectro(b1, 
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
            flab = NULL,
            main = "Carolina Wren female 'chatter'")
f1<-spectro(e1, 
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
            flab = NULL,
            main = "Spot-breasted Wren 'zipper' call")
par(las = 0)
mtext(text = "Frequency (kHz)", side = 2, outer = TRUE, line = 0.3, padj = 1, 
      cex = 1.5)
mtext(text = "Time (s)", side = 1, outer = TRUE, line = -0.3, padj = 1, 
      cex = 1.5)

dev.off()


