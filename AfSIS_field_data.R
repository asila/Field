#' AfSIS field data summaries
#' M. Walsh, October 2014

# Load packages -----------------------------------------------------------
# install.packages(c("downloader","arm", "wordcloud")), dependencies=TRUE)
require(downloader)
require(proj4)
require(arm)
require(wordcloud)

#+ Data download -----------------------------------------------------------
# Create a "Data" folder in your current working directory
dir.create("AfSIS_Data", showWarnings=F)
dat_dir <- "./AfSIS_Data"

# Download AfSIS LDSF data to ./AfSIS_Data"
download("https://www.dropbox.com/s/kpixjnerwi4d5vx/AfSIS_LDSF_data.csv?dl=0", "./AfSIS_data/AfSIS_LDSF_data.csv", mode="wb")
fdat <- read.table(paste(dat_dir, "/AfSIS_LDSF_data.csv", sep=""), header=T, sep=",")

#+ Generate coordinate reference and GID's ---------------------------------
# Project profile coords to Africa LAEA from LonLat
fdat.laea <- as.data.frame(project(cbind(fdat$Lon, fdat$Lat), "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"))
colnames(fdat.laea) <- c("x","y")
fdat <- cbind(fdat, fdat.laea)

# Generate AfSIS grid cell ID's (GID)
res.pixel <- 1000
xgid <- ceiling(abs(fdat$x)/res.pixel)
ygid <- ceiling(abs(fdat$y)/res.pixel)
gidx <- ifelse(fdat$x<0, paste("W", xgid, sep=""), paste("E", xgid, sep=""))
gidy <- ifelse(fdat$y<0, paste("S", ygid, sep=""), paste("N", ygid, sep=""))
GID <- paste(gidx, gidy, sep="-")
fdat.gid <- cbind(fdat, GID)

#+ GLMM models -------------------------------------------------------------
# Primarily vegetated areas
pva.glmer <- glmer(PVA~1+(1|Site), family=binomial(link="logit"), data=fdat.gid)
pva.coef <- coef(pva.glmer)
pva.se <- se.coef(pva.glmer)

# Reguarly flooded areas
wet.glmer <- glmer(WET~1+(1|Site), family=binomial(link="logit"), data=fdat.gid)
wet.coef <- coef(wet.glmer)
wet.se <- se.coef(wet.glmer)

# Cultivated & managed areas
cma.glmer <- glmer(CMA~1+(1|Site), family=binomial(link="logit"), data=fdat.gid)
cma.coef <- coef(cma.glmer)
cma.se <- se.coef(cma.glmer)

# Soil depth restrictions to 30 cm
dr30.glmer <- glmer(cbind(DR30,4-DR30)~1+(1|Site), family=binomial(link="logit"), data=fdat.gid)
dr30.coef <- coef(dr30.glmer)
dr30.se <- se.coef(dr30.glmer)

# Visible signs of soil erosion
vsse.glmer <- glmer(cbind(VSSE,4-VSSE)~1+(1|Site), family=binomial(link="logit"), data=fdat.gid)
vsse.coef <- coef(vsse.glmer)
vsse.se <- se.coef(vsse.glmer)

# Woody vegetation cover >60%
wcs.glmer <- glmer(cbind(WCS,4-WCS)~1+(1|Site), family=binomial(link="logit"), data=fdat.gid)
wcs.coef <- coef(wcs.glmer)
wcs.se <- se.coef(wcs.glmer)

# Assemble data frame of BLUP's by sentinel sites
fd <- as.data.frame(rownames(pva.coef$Site))
colnames(fd) <- c("Site")
fd$PVAp <- pva.coef$Site[,1]
fd$PVAs <- pva.se$Site[,1]
fd$WETp <- wet.coef$Site[,1]
fd$WETs <- wet.se$Site[,1]
fd$CMAp <- cma.coef$Site[,1]
fd$CMAs <- cma.se$Site[,1]
fd$DR30p <- dr30.coef$Site[,1]
fd$DR30s <- dr30.se$Site[,1]
fd$VSSEp <- vsse.coef$Site[,1]
fd$VSSEs <- vsse.se$Site[,1]
fd$WCSp <- wcs.coef$Site[,1]
fd$WCSs <- wcs.se$Site[,1]

write.csv(fd, "Field_data_summary.csv")

# Site comparison ---------------------------------------------------------
# Setup
fdvars <- c("PVAp","WETp","CMAp","DR30p","VSSEp","WCSp")
fdmod <- fd[fdvars]
colnames(fdmod) <- c("PVA","WET","CMA","DR30","VSSE","WCS")
fdmod$PVA <- invlogit(fdmod$PVA)
fdmod$WET <- invlogit(fdmod$WET)
fdmod$CMA <- invlogit(fdmod$CMA)
fdmod$DR30 <- invlogit(fdmod$DR30)
fdmod$VSSE <- invlogit(fdmod$VSSE)
fdmod$WCS <- invlogit(fdmod$WCS)
fdmod$Site <- fd$Site

# Wordcloud plots
set.seed(090813)
wordcloud(fdmod$Site, freq=fdmod$PVA, scale=c(1.5,0.01), random.order=F)
wordcloud(fdmod$Site, freq=fdmod$WET, scale=c(4,0.1), random.order=F)
wordcloud(fdmod$Site, freq=fdmod$CMA, scale=c(2.5,0.1), random.order=F)
wordcloud(fdmod$Site, freq=fdmod$DR30, scale=c(1.5,0.05), random.order=F)
wordcloud(fdmod$Site, freq=fdmod$VSSE, scale=c(1.5,0.05), random.order=F)
wordcloud(fdmod$Site, freq=fdmod$WCS, scale=c(2.5,0.1), random.order=F)


