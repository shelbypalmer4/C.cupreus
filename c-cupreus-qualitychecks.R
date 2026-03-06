#### Create spectrograms with timer() overlay for recording quality check and timer() amplitude threshold determination ####

# set working directory to the location of the sound files
setwd("C:/Users/spalm/OneDrive - University of Florida/Desktop/C.cupreus/filter_resamp")
head(list.files())

library(tuneR)
library(seewave)
library(dplyr)

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
#
#
#
# partitioning recordings into initial and final parts
# try with one
a <- readWave(list.files(pattern = "wav")[1])
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
abline(v = duration(a)*0.45)

sectionspec <- function(file) {
  a <- readWave(file)
  png(filename = paste(getwd(), "/section_figs/", file, ".png", sep = ""),
      width = 500,
      height = 250)
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
  abline(v = duration(a)*0.45)
  dev.off()
}

lapply(list.files(pattern = ".wav"), sectionspec)

############ Getting metadata for 2026 ##################
# ML
setwd("C:/Users/spalm/OneDrive - University of Florida/Desktop/C.cupreus/")
# recording request form
mlrequest <- read.csv("c-cupreus-ml-request-2026.csv", header = F)
# csv of metadata for all recordings uploaded since 2023
ml2026 <- read.csv("ml-metadata-2026.csv")
# subset ml2026 to only recordings requested
ml2026 <- ml2026[which(ml2026$ML.Catalog.Number %in% mlrequest[,1]),]

newids <- read.csv("new_batch_id_corrected.csv")
strsplit(newids$id[1], split = "_")[[1]][1]
newids$numberonly <- strsplit(newids$id, split = "_")[[1]][1]
for (i in 1:length(newids$id)) {
  newids$numberonly[i] <- strsplit(newids$id[i], split = "_")[[1]][1]
}
# overwrite original file which is no longer needed
write.csv(ml2026, "ml-metadata-2026.csv", row.names = F)

# subset ml2026 again based on recordings that passed quality check
ml2026 <- ml2026[which(ml2026$ML.Catalog.Number %in% newids$numberonly),]
# write new file with post-check subset of the data
write.csv(ml2026, "ml-metadata-2026-clean.csv", row.names = F)


# XC 
library(suwo)
# make a dataframe with just xc recordings
newids_xc <- newids[297:length(newids$id),]
# query xc using the suwo package
xc2026 <- query_xenocanto(species = "Chrysococcyx cupreus")

# remove the "XC" to match the metadata sheet
for (i in 1:length(newids_xc$numberonly)){
  newids_xc$numberonly_real[i] <- paste(
    strsplit(newids_xc$numberonly[i], split = "")[[1]][3:length(strsplit(newids_xc$numberonly[i], split = "")[[1]])], 
    collapse = ""
  )
}

# subset to the recordings we will use
xc2026 <- xc2026[which(xc2026$key %in% newids_xc$numberonly_real),]
# write to csv
write.csv(xc2026, "xc-metadata-2026-clean.csv", row.names = F)


