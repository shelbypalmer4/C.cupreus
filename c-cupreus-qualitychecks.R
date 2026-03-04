#### Create spectrograms with timer() overlay for recording quality check and timer() amplitude threshold determination ####

# set working directory to the location of the sound files
setwd("C:/Users/Shelby Palmer/Desktop/C.cupreus/filter_resamp")
head(list.files())

# manually make a new folder in the working directory for the figures named "QC_10_figs"

# function for generating images
cutspec <- function(file, threshold) {
  a <- readWave(file)
  png(filename = paste(getwd(), "/QC_10_figs/", file, ".png", sep = ""),
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
            threshold = threshold))
  dev.off()
}
# apply over working directory
lapply(list.files(pattern = ".wav"), cutspec)

