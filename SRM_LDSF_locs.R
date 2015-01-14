#' Sauri, Ruhiira & Mbola Millenium Village LDSF sample locations
#' M. Walsh, January 2015

# Required packages
# install.packages(c("downloader","rgdal")), dependencies=TRUE)
require(downloader)
require(rgdal)

#+ Data download -----------------------------------------------------------
# Create a "Data" folder in your current working directory
dir.create("SRM_Data", showWarnings=F)
dat_dir <- "./SRM_Data"

# Download Sauri, Ruhiira & Mbola LDSF locs to "./SRM_Data"
download("https://www.dropbox.com/s/rbf5u9exs6a9kbn/SRM_LDSF_locs.csv?dl=0", "./SRM_Data/SRM_LDSF_locs.csv", mode="wb")
ldsfloc <- read.table(paste(dat_dir, "/SRM_LDSF_locs.csv", sep=""), header=T, sep=",")

# Project to longlat from UTM36N
coordinates(ldsfloc) <- ~x+y
proj4string(ldsfloc) <- CRS("+proj=utm +ellps=WGS84 +zone=36 +north +units=m +no_defs")
ldsfloc <- as.data.frame(spTransform(ldsfloc, CRS("+proj=longlat +ellps=WGS84")))
colnames(ldsfloc) <- c("Site", "LID", "Lon", "Lat")
write.csv(ldsfloc, "./SRM_Data/SRM_LDSF_pts.csv", row.names=F)
