#### C. cupreus DATA SIMULATION. 19 OCTOBER 2024 ####
#
# Read in data: Measurements taken from a single clean recording
read.csv("C:/Users/spalm/Desktop/C.cupreus/simsdf_full.csv")
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
actualsims$ID <- names


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
write.csv(actualsims, "cuckoo_data_sims.csv", row.names = F)

#
#
#
#

#### 28 October 2024: Determine number of unique lat/long combinations (this will be our proxy for individual) ####
setwd("C:/Users/spalm/Desktop/C.cupreus")
library(tools)

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

# Can the NAs be salvaged?
allmeta.cc.final[which(is.na(allmeta.cc.final$Latitude)),]
# We can possibly use locality and country to assign a rough lat/long for the NAs to save data
