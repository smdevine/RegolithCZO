#shapiro-wilk normality test: shapiro.test p-value > 0.05 indicates normality
#variables extracted from rasters to soaproot_pts_terrain end in "N" for Neon, "R" for Ryan, or "M" for Mike, showing source of data
library(raster)
library(gstat)
library(spdep)
resolution <- '10m'
#resolution <- '5m'
if(resolution == '10m') {NEONterrainDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/NEON 10m/terrain characteristics'
} else if(resolution == '5m') {NEONterrainDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/5m terrain characteristics'}
modelResults <- 'C:/Users/smdevine/Desktop/post doc/czo work/model_results'
RyanGeospatialDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/RYAN GEOSPATIAL'
DSDdir <- 'C:/Users/smdevine/Desktop/post doc/czo work/DSD Mike'
dataDir <- 'C:/Users/smdevine/Desktop/post doc/czo work'
landsat8Dir <- 'C:/Users/smdevine/Desktop/post doc/czo work/landsat8/summaries/finals'
list.files(dataDir)
soaproot_pts <- read.csv(file.path(dataDir, 'Soaproot points RF.csv'), stringsAsFactors = FALSE)
#CRS("+init=epsg:4326") #this is geographic coordinates using WGS84 datum
#CRS("+init=epsg:4269") #this is geographic coordinates using NAD83 datum
soaproot_pts_WGS84 <- SpatialPointsDataFrame(coords=soaproot_pts[,c('POINT_X', 'POINT_Y')], data = soaproot_pts['Name'], proj4string = CRS('+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
soaproot_pts_usgs_dem <- spTransform(soaproot_pts_WGS84, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_def")) 
soaproot_UTM11N_shp <- spTransform(soaproot_pts_WGS84, CRS("+proj=utm +zone=11 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) #project from geographic to WGS84 UTM 11N
#soaproot_pts_NAD83 <- SpatialPointsDataFrame(coords=soaproot_pts[,c('POINT_X', 'POINT_Y')], data = soaproot_pts['Name'], proj4string = CRS('+init=epsg:4269 +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))
#soaproot_pts_WGS84 <- spTransform(soaproot_pts_NAD83, CRS('+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
#shapefile(soaproot_UTM11N_shp, file.path(dataDir, 'soaproot_pts_UTM11N.shp'))
#soaproot_80pts <- soaproot_UTM11N_shp[c(1:68, 70, 72:75, 79, 81, 86, 88, 92, 96:97),] #SROC01, SROC03, SROC05, SROC06, SROC07, SROC08, SROC12, SROC14, SROC19, SROC21, SROC25, SROC29, SROC30
#soaproot_80pts$Name
#plot(soaproot_80pts)
#dem_USGS <- raster(file.path(RyanGeospatialDir, 'soaproot_dem.tif'))
#stream_distance_R <- raster(file.path(RyanGeospatialDir, 'strdist_10m.tif'))
#stream_distance_N <- raster(file.path(NEONterrainDir, 'strdist_400.tif'))
#DSD <- raster(file.path(DSDdir, 'DSD_V1_2.tif')) #DSD_V1_2.tif'))
DSD_v2 <- raster(file.path(DSDdir, 'DSDout_V2_1_cropped.tif')) #cropped version derived from DSDout_V2_1.tif'
#crs(DSD) #+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#crs(DSD_v2)
#DSD_v2 <- crop(DSD_v2, DSD, filename=file.path(DSDdir, 'DSDout_V2_1_cropped.tif'))
NDVI_landsat <- stack(list.files(file.path(RyanGeospatialDir, 'NDVI landsat'), full.names = TRUE))
names(NDVI_landsat)

NEON_terrain <- stack(list.files(NEONterrainDir, full.names = TRUE))
if(resolution == '10m') {
  NEON_terrain$annsolrad_10m <- NEON_terrain$annsolrad_10m / 1000
  names(NEON_terrain)
  names(NEON_terrain) <- c('solrad_N', 'aspect_N', 'CTI_N', 'curv_mean_N', 'curv_plan_N', 'curv_prof_N', 'elev_N', 'elev_above_str_150', 'EVI_2017_N', 'EVI_2018_N', 'flowacc_N', 'NDVI_2017_N', 'NDVI_2018_N', 'SEI_N', 'slope_N', 'stream_dist_N_100', 'stream_dist_N_150', 'stream_dist_N_200', 'stream_dist_N_300', 'stream_dist_N_400', 'TCI_N')
} else if(resolution == '5m') {
    NEON_terrain$solrad_5m <- NEON_terrain$solrad_5m / 1000
    names(NEON_terrain)
    names(NEON_terrain) <- c('aspect_N', 'CTI_N', 'curv_mean_N', 'curv_plan_N', 'curv_prof_N', 'elev_N', 'elev_above_str_N', 'EVI_2017_N', 'EVI_2018_N', 'IMI_N', 'NDVI_2017_N', 'NDVI_2018_N', 'SEI_N', 'slope_N', 'solrad_N', 'stream_dist_N')
  }
NEON_terrain$TPI_N <- terrain(NEON_terrain$elev_N, opt = 'TPI', neighbors = 8)
NEON_terrain$TRI_N <- terrain(NEON_terrain$elev_N, opt = 'TRI', neighbors = 8)
#USGS_TPI <- terrain(dem_USGS, opt = 'TPI', neighbors = 8)
NEON_terrain$curv_mean_filt <- raster(file.path(dataDir, 'NEON 10m', 'terrain chars filtered', 'curv_mean.tif'))
NEON_terrain$curv_mean_filt_true <- raster(file.path(dataDir, 'NEON 10m', 'terrain chars filtered', 'curv_mean_filtered.tif'))
soaproot_pts_terrain <- extract(NEON_terrain, soaproot_UTM11N_shp, df=TRUE)
#colnames(soaproot_pts_terrain)
soaproot_pts_terrain$ID <- NULL
#lapply(soaproot_pts_terrain, summary)
soaproot_pts_terrain <- cbind(soaproot_UTM11N_shp$Name, soaproot_pts_terrain)
colnames(soaproot_pts_terrain)[1] <- 'Site'
#lapply(soaproot_pts_terrain, class)
soaproot_pts_terrain$Site <- as.character(soaproot_pts_terrain$Site)
soaproot_pts_terrain <- cbind(soaproot_pts_terrain, extract(NDVI_landsat, soaproot_UTM11N_shp, df=TRUE)[ ,2:(nlayers(NDVI_landsat)+1)])
soaproot_pts_terrain$ndvi2006_2009avg <- apply(soaproot_pts_terrain[,which(colnames(soaproot_pts_terrain)=='ndvi2006'):which(colnames(soaproot_pts_terrain)=='ndvi2009')], 1, mean)
soaproot_pts_terrain$ndvi2012_2015avg <- apply(soaproot_pts_terrain[,which(colnames(soaproot_pts_terrain)=='ndvi2012'):which(colnames(soaproot_pts_terrain)=='ndvi2015')], 1, mean)
#soaproot_pts_terrain$stream_dist_R <- extract(stream_distance_R, soaproot_UTM11N_shp)
#soaproot_pts_terrain$DSD_M <- extract(DSD, soaproot_pts_WGS84) / 10 #because DSD is in units 10 * mm yr^-1; buffer=15, fun=mean improves variance explained only slightly with old estimates of DSD but slope still 1.05
#soaproot_pts_terrain$TPI_USGS <- extract(USGS_TPI, soaproot_pts_usgs_dem)
soaproot_pts_terrain$DSD_v2_M <- extract(DSD_v2, soaproot_pts_WGS84) / 10 #latest file from 4/9/19 cropped to previous version extent
#soaproot_pts_terrain$elev_above_str_N <- extract(raster(file.path(NEONterrainDir, 'elev_above_str_150.tif')), soaproot_UTM11N_shp)
df_master <- read.csv(file.path(dataDir, 'Master Data Set_with rock outcrop.csv'), stringsAsFactors = FALSE) #removed biological data from this dataset but it has duplicate rows where multiple biological observations were made at each point; also removed 0's from outcrop (OC) site codes 01-09 to match points dataset labeling
#head(df_master)
#tapply(df_master$Depth, df_master$Site, summary)
#tapply(df_master$Latitude, df_master$Site, summary)
df_sites <- df_master[match(unique(df_master$Site), df_master$Site),]
#dim(df_sites) #these leaves out unique tree data which is not going to be used for this analysis
#colnames(df_sites)
#hist(df_sites$Depth)

#add depth variable
#soaproot_pts_terrain$Site
#df_sites$Site
#soaproot_pts_terrain$DSD_original <- df_sites$DSD[match(soaproot_pts_terrain$Site, df_sites$Site)]
soaproot_pts_terrain$Rdist_R <- df_sites$Rdist[match(soaproot_pts_terrain$Site, df_sites$Site)]
summary(soaproot_pts_terrain$Rdist_R)
hist(soaproot_pts_terrain$Rdist_R)
soaproot_pts_terrain$OCdist_R <- df_sites$Ocdist[match(soaproot_pts_terrain$Site, df_sites$Site)]
summary(soaproot_pts_terrain$OCdist_R)
hist(soaproot_pts_terrain$OCdist_R)
soaproot_pts_terrain$Depth <- df_sites$Depth[match(soaproot_pts_terrain$Site, df_sites$Site)]
#plot(soaproot_pts_terrain$DSD_v2_M, soaproot_pts_terrain$DSD_original)
#summary(lm(DSD_v2_M ~ DSD_original, data = soaproot_pts_terrain)) #DSDout2.tif: r2=0.96, slope=1.08; #DSD_V1_2.tif: r2=0.96; slope=1.05; #DSDout.tif: r2=0.84; slope = 0.67; #DSD_V1_2_5x6.tif: r2=0.91; slope 1.05; #DSD_v2_M: r2=0.84 slope=0.68
#plot(soaproot_pts_terrain$DSD_v2_M, soaproot_pts_terrain$DSD_M)
summary(soaproot_pts_terrain$Depth)
soaproot_pts_terrain$Depth[is.na(soaproot_pts_terrain$Depth) & grepl('OC', soaproot_pts_terrain$Site)] <- 0
soaproot_pts_original <- merge(soaproot_pts_terrain, df_sites, by='Site') #67 augered; 13 OCs hand-picked by Ryan
#dim(soaproot_pts_original)
#soaproot_pts_original <- soaproot_pts_original[!soaproot_pts_original$Depth.x==0,]
#write.csv(soaproot_pts_terrain, file.path(dataDir, 'fit data distribution', 'soaproot_pts100_terrain.csv'), row.names = FALSE)

#inspect time Landsat 8 time series for all 100 points, symbolizing by regolith depth
#doy_20160121_ndvi is wonky
include_OC <- FALSE
NDVI_landsat8 <- read.csv(file.path(landsat8Dir, 'NDVI_2013_2017_final.csv'), row.names = 1)
NDVI_landsat8$doy_20160121_ndvi <- NULL
NDVI_landsat8$Site <- row.names(NDVI_landsat8)
NDVI_landsat8$Depth <- soaproot_pts_terrain$Depth[match(NDVI_landsat8$Site, soaproot_pts_terrain$Site)] #they are both in same order but this is safer nonetheless
NDVI_landsat8 <- NDVI_landsat8[!is.na(NDVI_landsat8$Depth), ]
if (include_OC) {} else {
  NDVI_landsat8 <- NDVI_landsat8[NDVI_landsat8$Depth!=0,]
}
dim(NDVI_landsat8)   
dates_raw <- colnames(NDVI_landsat8)[1:(ncol(NDVI_landsat8)-2)]
dates <- sapply(strsplit(dates_raw, ''), function(x) paste0(x[5:12], collapse=''))
dates <- as.Date(dates, '%Y%m%d')
colnames(NDVI_landsat8)
date1 <- '20130418'
date2 <- '20160715'
indice1 <- which(colnames(NDVI_landsat8)==paste0('doy_', date1, '_ndvi'))
indice2 <- which(colnames(NDVI_landsat8)==paste0('doy_', date2, '_ndvi'))
  
NDVI_landsat8$Depth_color <- ifelse(NDVI_landsat8$Depth==7.56, 'red3', ifelse(NDVI_landsat8$Depth>3, 'orange2', ifelse(NDVI_landsat8$Depth>0, 'grey', ifelse(NDVI_landsat8$Depth==0, 'white', 'pink'))))
NDVI_landsat8$Depth_color
for (i in seq_along(NDVI_landsat8$Depth)) {
  if (i == 1) {
    plot(dates[indice1:indice2], NDVI_landsat8[i,indice1:indice2], type='l', col=NDVI_landsat8$Depth_color[i], ylim=c(0.3, 0.8), xaxt='n', xlab='', yaxt = 'n', ylab = 'NDVI')
  } else {lines(dates[indice1:indice2], NDVI_landsat8[i,indice1:indice2], col=NDVI_landsat8$Depth_color[i])}
}
axis.Date(side = 1, at=seq.Date(from = as.Date(date1, '%Y%m%d'), to = as.Date(date2, '%Y%m%d'), by='months'), format = '%m/%d/%y')

#do the same with EVI
include_OC <- FALSE
EVI_landsat8 <- read.csv(file.path(landsat8Dir, 'EVI_2013_2017_final.csv'), row.names = 1)
EVI_landsat8$Site <- row.names(EVI_landsat8)
EVI_landsat8$Depth <- soaproot_pts_terrain$Depth[match(EVI_landsat8$Site, soaproot_pts_terrain$Site)] #they are both in same order but this is safer nonetheless
EVI_landsat8 <- EVI_landsat8[!is.na(EVI_landsat8$Depth), ]
if (include_OC) {} else {
  EVI_landsat8 <- EVI_landsat8[EVI_landsat8$Depth!=0,]
}
dates_raw <- colnames(EVI_landsat8)[1:(ncol(EVI_landsat8)-2)]
dates <- sapply(strsplit(dates_raw, ''), function(x) paste0(x[5:12], collapse=''))
dates <- as.Date(dates, '%Y%m%d')
EVI_landsat8$Depth_color <- ifelse(EVI_landsat8$Depth==7.56, 'darkred', ifelse(EVI_landsat8$Depth>3, 'orange4', ifelse(EVI_landsat8$Depth>0, 'yellow4', ifelse(EVI_landsat8$Depth==0, 'white', 'pink'))))
for (i in seq_along(EVI_landsat8$Depth)) {
  if (i == 1) {
    plot(dates, EVI_landsat8[i,1:which(colnames(EVI_landsat8)=='doy_20171225_evi')], type='l', col=EVI_landsat8$Depth_color[i], ylim=c(0, 0.6), xaxt='n', xlab='', yaxt = 'n', ylab = 'EVI', xlim = as.Date(c('2013-04-15', '2017-12-30')))
  } else {lines(dates, EVI_landsat8[i,1:which(colnames(EVI_landsat8)=='doy_20171225_evi')], col=EVI_landsat8$Depth_color[i])}
}
axis.Date(side = 1, at=seq.Date(from = as.Date('2013-04-15'), to = as.Date('2017-12-30'), by='months'), format = '%m/%d/%y')


#original model verification (n=80(67 auger; 13 rock outcrop), 67 all auguer, and 40 all auger and depth determinable)
library(car)
summary(lm(Depth.x ~ DSD + TPI, data=soaproot_pts_original)) #r2=0.49 for n=80 (including zeroes)
summary(lm(Depth.x ~ DSD + TPI, data=soaproot_pts_original[!soaproot_pts_original$Depth.x==0, ])) #r2=0.19 for n=67 (no rock outcrop)
summary(lm(Depth.x ~ DSD + TPI, data=soaproot_pts_original[soaproot_pts_original$Depth.x > 0 & soaproot_pts_original$Depth.x < 7.56, ])) #NS
vif(lm(Depth.x ~ DSD + TPI, data=soaproot_pts_original))
plot(soaproot_pts_original$TPI, soaproot_pts_original$TPI_N)
plot(soaproot_pts_original$TPI, soaproot_pts_original$TPI_USGS)
summary(lm(TPI ~ TPI_USGS, data=soaproot_pts_original))
plot(soaproot_pts_original$TPI_N ~ soaproot_pts_original$TPI_USGS)
#compared to new estimate of TPI

#tests with synthetic data for points > 7.5 m
test <- rlnorm(10000, meanlog=1.705, sdlog=1.123) #see CZO regolith data lognnormal fit.xlsx for how meanlog and sdlog were estimated
hist(test)
sum(test > 7.56) #3858 (38.6%)
percentile
sum(test < 1) #664 < 1 (6.6%)
hist(test[test<7.56], breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8))
summary(test)
mean(test) #mean is 10.16 m
median(test)
quantile(test)   #0%         25%         50%         75%        100%
#0%          25%          50%          75%         100% 
#0.08661674   2.49139009   5.37799880  11.62770371 338.33007580
mean(log(test)) #1.687
sd(log(test)) #1.125

#gamma distribution test
qgamma(0.5, shape = 1.915, scale=1/0.273) #this is median
qgamma(0.99, shape = 1.915, scale = 1/0.273) #this is 99th percentile
qgamma(0.999, shape = 1.915, scale = 1/0.273) #this is the 99.9th percentile
#regression test replacing 7.56 m with synthetic data
synthetic_7.56m <- function(meanlog, sdlog) {
  success <- FALSE
  while (!success) {
    x <- rlnorm(1, meanlog, sdlog)
    # check for success
    success <- x > 7.56
  }
  return(x)
}
synthetic_7.56m(meanlog=1.705, sdlog=1.123) #lognormal parameters derived from original data including 4 zeroes (OCs)

synthetic_7.56m_gamma <- function(shape, scale) {
  success <- FALSE
  while (!success) {
    x <- rgamma(1, shape = shape, scale = scale)
    # check for success
    success <- x > 7.56
  }
  return(x)
}


#set.seed(80140)
#OCs_to_keep <- sample(1:33, 4) #27, 14, 12, 4
#OCs_to_keep <- NA  #c(27, 14, 12, 4)
#OCs_to_keep <- paste0('SROC', OCs_to_keep)
#soaproot_pts_analysis <- soaproot_pts_terrain[soaproot_pts_terrain$Site %in% OCs_to_keep | grepl('SR.A.', soaproot_pts_terrain$Site), ]
#soaproot_pts_analysis <- soaproot_pts_analysis[soaproot_pts_analysis$Depth < 7.56,]
soaproot_pts_analysis <- soaproot_pts_terrain[grepl('SR.A.', soaproot_pts_terrain$Site), ] #leaves out all rock outcrop points
soaproot_pts_analysis <- soaproot_pts_analysis[!is.na(soaproot_pts_analysis$Depth),] #one point had a NA for depth

#compare mean curvatures
plot(soaproot_pts_analysis$curv_mean_N, soaproot_pts_analysis$curv_mean_filt)
plot(soaproot_pts_analysis$curv_mean_N, soaproot_pts_analysis$curv_mean_filt_true)
summary(lm(soaproot_pts_analysis$curv_mean_N ~ soaproot_pts_analysis$curv_mean_filt))
hist(soaproot_pts_analysis$curv_mean_filt_true)
hist(soaproot_pts_analysis$curv_mean_filt)
summary(lm(Depth ~ curv_mean_filt_true, data = soaproot_pts_analysis))
summary(lm(Depth ~ curv_mean_filt, data = soaproot_pts_analysis))
summary(lm(Depth ~ curv_mean_N, data = soaproot_pts_analysis))
soaproot_pts_analysis$Depth[soaproot_pts_analysis$Depth > 7.56] <- 7.56
soaproot_pts_analysis$Depth
#soaproot_pts_analysis$Depth[soaproot_pts_analysis$Depth==0] <- 0.01 #assume depth is 0.01 where there is rock outcrop
hist(soaproot_pts_analysis$Depth)
for(i in 1:length(soaproot_pts_analysis$Depth)) {
  if(soaproot_pts_analysis$Depth[i] == 7.56) {
    soaproot_pts_analysis$Depth[i] <- synthetic_7.56m_gamma(shape = 1.915, scale=1/0.273) #when no OCs are included for lognorm: meanlog=1.840, sdlog=0.878; when four OCs with depth =0.01 are included: meanlog=1.705, sdlog=1.123; see excel file
  } else {next}
}
hist(soaproot_pts_analysis$Depth)
soaproot_pts_analysis$Depth_log <- log(soaproot_pts_analysis$Depth)
summary(lm(Depth_log ~ curv_mean_filt_true, data = soaproot_pts_analysis))
summary(lm(Depth_log ~ elev_above_str_150, data = soaproot_pts_analysis))
summary(lm(Depth_log ~ stream_dist_N_150, data = soaproot_pts_analysis))
summary(lm(Depth_log ~ stream_dist_N_150 + curv_mean_filt_true, data = soaproot_pts_analysis))
summary(lm(Depth ~ stream_dist_N_150 + curv_mean_filt_true, data = soaproot_pts_analysis))
soaproot_pts_analysis$depth_class <- ifelse(soaproot_pts_analysis$Depth < 3, 'shallow', ifelse(soaproot_pts_analysis$Depth < 5, 'moderate', 'deep'))
soaproot_pts_analysis$depth_class <- ifelse(soaproot_pts_analysis$Depth < 5.84, 'moderate', 'deep')
table(soaproot_pts_analysis$depth_class)
tapply(soaproot_pts_analysis$curv_mean_filt_true, soaproot_pts_analysis$depth_class, boxplot)

boxplot(curv_mean_filt_true ~ depth_class, data = soaproot_pts_analysis)
t.test(curv_mean_filt_true ~ depth_class, data = soaproot_pts_analysis)
boxplot(stream_dist_N_150 ~ depth_class, data = soaproot_pts_analysis)
t.test(stream_dist_N_150 ~ depth_class, data = soaproot_pts_analysis)
boxplot(slope_N ~ depth_class, data = soaproot_pts_analysis)
t.test(slope_N ~ depth_class, data = soaproot_pts_analysis)
#soaproot_pts_analysis$elev_USGS <- soaproot_pts_original$Elev[match(soaproot_pts_analysis$Site, soaproot_pts_original$Site)]
#plot(soaproot_pts_analysis$elev_N, soaproot_pts_analysis$elev_USGS)
soaproot_pts_analysis$Landscape.indicator <- soaproot_pts_original$Landscape.indicator[match(soaproot_pts_analysis$Site, soaproot_pts_original$Site)]
#soaproot_pts_analysis$Depth.check <- soaproot_pts_original$Depth.x[match(soaproot_pts_analysis$Site, soaproot_pts_original$Site)]
#plot(soaproot_pts_analysis$Depth, soaproot_pts_analysis$Depth.check)
#abline(0, 1, lty=2)
#soaproot_pts_analysis$stream_dist_check <- soaproot_pts_original$Sdist[match(soaproot_pts_analysis$Site, soaproot_pts_original$Site)]
#plot(soaproot_pts_analysis$stream_dist_R, soaproot_pts_analysis$stream_dist_check)
#abline(0, 1, lty=2)
#plot(soaproot_pts_analysis$stream_dist_N_150, soaproot_pts_analysis$stream_dist_R)
#abline(0, 1, lty=2)
#plot(soaproot_pts_analysis$stream_dist_N_100, soaproot_pts_analysis$stream_dist_R)
#abline(0, 1, lty=2)
#plot(soaproot_pts_analysis$stream_dist_N_100, soaproot_pts_analysis$stream_dist_N_150)
#abline(0, 1, lty=2)
#soaproot_pts_analysis_no_OC <- soaproot_pts_analysis[!soaproot_pts_analysis$Depth==0, ]
#dim(soaproot_pts_analysis_no_OC)
#dim(soaproot_pts_analysis) #dim is 44
colnames(soaproot_pts_analysis)
hist(soaproot_pts_analysis$Depth)
hist(soaproot_pts_analysis$Depth_log)
hist(soaproot_pts_analysis$elev_above_str_N)
summary(soaproot_pts_analysis$elev_above_str_N[soaproot_pts_analysis$Depth < 7.56])
summary(soaproot_pts_analysis$elev_above_str_N[soaproot_pts_analysis$Depth > 7.56])
soaproot_pts_analysis$Site[soaproot_pts_analysis$elev_above_str_N > 30]
soaproot_pts_analysis[,c('Site', 'elev_above_str_N')]
mapply(function(x,y,z='Depth_log') 
  {lm_result <- lm(soaproot_pts_analysis[[z]] ~ x)
  plot(x, soaproot_pts_analysis[[z]], main=paste(y, 'r2 = ', round(summary(lm_result)$r.squared, 2)))
  abline(lm_result, lty=2)}, x=soaproot_pts_analysis[,2:32], y=colnames(soaproot_pts_analysis)[2:32])

#do the same for the landsat8 spectral indices time-series
NDVI_landsat8$Depth[NDVI_landsat8$Depth > 7.56] <- 7.56
NDVI_landsat8$Depth
for(i in 1:length(NDVI_landsat8$Depth)) {
  if(NDVI_landsat8$Depth[i] == 7.56) {
    NDVI_landsat8$Depth[i] <- synthetic_7.56m_gamma(shape = 1.915, scale=1/0.273) #when no OCs are included for lognorm: meanlog=1.840, sdlog=0.878; when four OCs with depth =0.01 are included: meanlog=1.705, sdlog=1.123; see excel file
  } else {next}
}
hist(NDVI_landsat8$Depth)
NDVI_landsat8$Depth_log <- log(NDVI_landsat8$Depth) 
mapply(function(x,y,z='Depth') 
{lm_result <- lm(NDVI_landsat8[[z]] ~ x)
plot(x, NDVI_landsat8[[z]], main=paste(y, 'r2 = ', round(summary(lm_result)$r.squared, 2)))
abline(lm_result, lty=2)}, x=NDVI_landsat8[,1:which(colnames(NDVI_landsat8)=='doy_20171225_ndvi')], y=colnames(NDVI_landsat8)[1:which(colnames(NDVI_landsat8)=='doy_20171225_ndvi')])

#check a 2014 Apr-Oct mean vs. Depth
hist(apply(NDVI_landsat8[,which(colnames(NDVI_landsat8)=='doy_20140523_ndvi'):which(colnames(NDVI_landsat8)=='doy_20141030_ndvi')], 1, mean))
summary(lm(NDVI_landsat8$Depth_log ~ apply(NDVI_landsat8[,which(colnames(NDVI_landsat8)=='doy_20140523_ndvi'):which(colnames(NDVI_landsat8)=='doy_20141030_ndvi')], 1, mean)))

#check a 2015 Apr-Oct mean vs. Depth
colnames(NDVI_landsat8)
hist(apply(NDVI_landsat8[,which(colnames(NDVI_landsat8)=='doy_20150510_ndvi'):which(colnames(NDVI_landsat8)=='doy_20151118_ndvi')], 1, mean))
summary(lm(NDVI_landsat8$Depth_log ~ apply(NDVI_landsat8[,which(colnames(NDVI_landsat8)=='doy_20150510_ndvi'):which(colnames(NDVI_landsat8)=='doy_20151118_ndvi')], 1, mean)))
#check 2015 - 2014 
delta2015_2014 <- apply(NDVI_landsat8[,which(colnames(NDVI_landsat8)=='doy_20140216_ndvi'):which(colnames(NDVI_landsat8)=='doy_20141030_ndvi')], 1, mean) - apply(NDVI_landsat8[,which(colnames(NDVI_landsat8)=='doy_20150118_ndvi'):which(colnames(NDVI_landsat8)=='doy_20151118_ndvi')], 1, mean)
delta2016_2013 <- apply(NDVI_landsat8[,which(colnames(NDVI_landsat8)=='doy_20130418_ndvi'):which(colnames(NDVI_landsat8)=='doy_20131027_ndvi')], 1, mean) - apply(NDVI_landsat8[,which(colnames(NDVI_landsat8)=='doy_20160426_ndvi'):which(colnames(NDVI_landsat8)=='doy_20161019_ndvi')], 1, mean)
summary(lm(NDVI_landsat8$Depth_log ~ delta2016_2013))
plot(delta2016_2013, NDVI_landsat8$Depth_log)
plot(delta2016_2013, NDVI_landsat8$Depth)

#EVI time series
EVI_landsat8$Depth[EVI_landsat8$Depth > 7.56] <- 7.56
EVI_landsat8$Depth
for(i in 1:length(EVI_landsat8$Depth)) {
  if(EVI_landsat8$Depth[i] == 7.56) {
    EVI_landsat8$Depth[i] <- synthetic_7.56m_gamma(shape = 1.915, scale=1/0.273) #when no OCs are included for lognorm: meanlog=1.840, sdlog=0.878; when four OCs with depth =0.01 are included: meanlog=1.705, sdlog=1.123; see excel file
  } else {next}
}
hist(EVI_landsat8$Depth)
EVI_landsat8$Depth_log <- log(EVI_landsat8$Depth) 
mapply(function(x,y,z='Depth_log') 
{lm_result <- lm(EVI_landsat8[[z]] ~ x)
plot(x, EVI_landsat8[[z]], main=paste(y, 'r2 = ', round(summary(lm_result)$r.squared, 2)))
abline(lm_result, lty=2)}, x=EVI_landsat8[,1:which(colnames(EVI_landsat8)=='doy_20171225_evi')], y=colnames(EVI_landsat8)[1:which(colnames(EVI_landsat8)=='doy_20171225_evi')])

#check a 2014 mean vs. Depth
hist(apply(EVI_landsat8[,which(colnames(EVI_landsat8)=='doy_20140216_evi'):which(colnames(EVI_landsat8)=='doy_20141030_evi')], 1, mean))
summary(lm(EVI_landsat8$Depth_log ~ apply(EVI_landsat8[,which(colnames(EVI_landsat8)=='doy_20140216_evi'):which(colnames(EVI_landsat8)=='doy_20141030_evi')], 1, mean)))
#check a 2015 mean vs. Depth
hist(apply(EVI_landsat8[,which(colnames(EVI_landsat8)=='doy_20140216_evi'):which(colnames(EVI_landsat8)=='doy_20150118_evi')], 1, mean))
summary(lm(EVI_landsat8$Depth_log ~ apply(EVI_landsat8[,which(colnames(EVI_landsat8)=='doy_20150118_evi'):which(colnames(EVI_landsat8)=='doy_20151118_evi')], 1, mean)))



#look at time-series of NDVIs (landsat Sept from Ryan) by point
soaproot_pts_analysis$Site[soaproot_pts_analysis$ndvi2006_2009avg < 0.2]
lapply(soaproot_pts_analysis[,which(colnames(soaproot_pts_analysis)=='ndvi2006'):which(colnames(soaproot_pts_analysis)=='ndvi2015')], summary)
for (i in 1:66) {
  if (i == 1) {
    plot(1:nlayers(NDVI_landsat), soaproot_pts_analysis[i,which(colnames(soaproot_pts_analysis)=='ndvi2006'):which(colnames(soaproot_pts_analysis)=='ndvi2015')], type='l', ylim=c(-0.2, 0.55), xaxt='n', xlab='', ylab='NDVI landsat')
  } else {lines(1:nlayers(NDVI_landsat), soaproot_pts_analysis[i,which(colnames(soaproot_pts_analysis)=='ndvi2006'):which(colnames(soaproot_pts_analysis)=='ndvi2015')])}
}
axis(side = 1, at=1:nlayers(NDVI_landsat), labels = c(2006, 2007, 2008, 2009, 2012, 2013, 2014, 2015))
#pre-drought NDVI: 2007-2009

#look at non-linear relationship with CTI (compound topographic index)
summary(lm(Depth_log ~ stream_dist_N_150, data=soaproot_pts_analysis))
summary(lm(Depth_log ~ CTI_N, data=soaproot_pts_analysis)) #p=0.11
summary(lm(Depth_log ~ CTI_N + I(CTI_N^2), data=soaproot_pts_analysis)) #significant
summary(lm(Depth_log ~ CTI_N + I(CTI_N^2) + stream_dist_N_150, data=soaproot_pts_analysis))
vif(lm(Depth_log ~ CTI_N + I(CTI_N^2) + stream_dist_N_150, data=soaproot_pts_analysis))
plot(soaproot_pts_analysis$CTI_N, soaproot_pts_analysis$stream_dist_N_150)
plot(lm(Depth_log ~ CTI_N + I(CTI_N^2) + stream_dist_N_150, data=soaproot_pts_analysis))
summary(lm(Depth_log ~ exp(CTI_N), data=soaproot_pts_analysis)) #

#look at Depth by landscape.indicator and as factor
tapply(soaproot_pts_analysis$Depth, soaproot_pts_analysis$Landscape.indicator, summary)
summary(lm(soaproot_pts_analysis$Depth_log ~ as.factor(soaproot_pts_analysis$Landscape.indicator)))

#check stream dist_R
soaproot_pts_analysis$Site[is.na(soaproot_pts_analysis$stream_dist_N)]
soaproot_pts_analysis$Site[is.na(soaproot_pts_analysis$stream_dist_R)] #"SR.A.04" should be around 80 m according to Ryan's original estimate (Sdist) and "SR.A.56" should be around 224.7 m
plot(soaproot_pts_analysis$stream_dist_R, soaproot_pts_analysis$Depth_log)
#points(soaproot_pts_analysis$stream_dist_R, soaproot_pts_analysis$Depth_log, col=if(soaproot))
plot(soaproot_pts_analysis$stream_dist_R, soaproot_pts_analysis$Depth)
sum(soaproot_pts_analysis$stream_dist_R < 100, na.rm = TRUE) #n=42
sum(soaproot_pts_analysis$stream_dist_R < 125, na.rm = TRUE) #n=48
sum(soaproot_pts_analysis$stream_dist_R < 150, na.rm = TRUE) #n=57
sum(soaproot_pts_analysis$stream_dist_R < 175, na.rm = TRUE) #n=62
sum(soaproot_pts_analysis$stream_dist_R < 200, na.rm = TRUE) #n=64
sum(soaproot_pts_analysis$stream_dist_R < 250, na.rm = TRUE) #n=67
summary(lm(Depth ~ stream_dist_R, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_R < 75,])) #p=0.11
summary(lm(Depth_log ~ stream_dist_R, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_R < 75,]))
summary(lm(Depth ~ stream_dist_R, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_R < 100,])) #p=0.11
summary(lm(Depth_log ~ stream_dist_R, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_R < 100,]))
summary(lm(Depth ~ stream_dist_R, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_R < 125,])) #p=0.12
summary(lm(Depth_log ~ stream_dist_R, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_R < 125,])) #p=0.01
summary(lm(Depth ~ stream_dist_R, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_R < 150,])) #p=0.008
summary(lm(Depth_log ~ stream_dist_R, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_R < 150,])) #r2=0.14; p=0.04 n=
summary(lm(Depth ~ stream_dist_R, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_R < 200,])) 
summary(lm(Depth_log ~ stream_dist_R, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_R < 200,])) #p=0.17

#check stream_distance_N (derived from NEON data and stream network formed where flowacc > 400)
soaproot_pts_analysis$Site[is.na(soaproot_pts_analysis$stream_dist_N)]
plot(soaproot_pts_analysis$stream_dist_N, soaproot_pts_analysis$Depth_log)
points(soaproot_pts_analysis$stream_dist_N, soaproot_pts_analysis$Depth_log, col=ifelse(soaproot_pts_analysis$Landscape.indicator %in% c(1, 2), 'red', ifelse(soaproot_pts_analysis$Landscape.indicator %in% c(3), 'orange', 'black')))
abline(lm(Depth_log ~ stream_dist_N, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_N < 200,]), lty=2)
abline(v=200, lty=2, col='grey')

plot(soaproot_pts_analysis$stream_dist_N, soaproot_pts_analysis$Depth)
sum(soaproot_pts_analysis$stream_dist_N < 100, na.rm = TRUE) #n=40
sum(soaproot_pts_analysis$stream_dist_N < 125, na.rm = TRUE) #n=49
sum(soaproot_pts_analysis$stream_dist_N < 150, na.rm = TRUE) #n=54
sum(soaproot_pts_analysis$stream_dist_N < 175, na.rm = TRUE) #n=58
sum(soaproot_pts_analysis$stream_dist_N < 200, na.rm = TRUE) #n=61
sum(soaproot_pts_analysis$stream_dist_N < 250, na.rm = TRUE) #n=65
sum(soaproot_pts_analysis$stream_dist_N < 300, na.rm = TRUE) #n=66

summary(lm(Depth ~ stream_dist_N, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_N < 100,])) #r2=0.29
summary(lm(Depth_log ~ stream_dist_N, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_N < 100,])) #r2=0.26
summary(lm(Depth ~ stream_dist_N, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_N < 125,])) #r2=0.2
summary(lm(Depth_log ~ stream_dist_N, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_N < 125,])) #r2=0.19
summary(lm(Depth ~ stream_dist_N, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_N < 150,])) #r2=0.26
summary(lm(Depth_log ~ stream_dist_N, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_N < 150,])) #r2=0.23
summary(lm(Depth ~ stream_dist_N, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_N < 200,])) #r2=0.26
summary(lm(Depth_log ~ stream_dist_N, data=soaproot_pts_analysis[soaproot_pts_analysis$stream_dist_N < 200,]))  #r2=0.27

#check stream_distance_N (derived from NEON data and stream network formed where flowacc > 150)
soaproot_pts_analysis$Site[is.na(soaproot_pts_analysis$stream_dist_N_150)]
plot(soaproot_pts_analysis$stream_dist_N_100, soaproot_pts_analysis$Depth_log)
points(soaproot_pts_analysis$stream_dist_N_100, soaproot_pts_analysis$Depth_log, col=ifelse(soaproot_pts_analysis$Landscape.indicator %in% c(1, 2), 'red', ifelse(soaproot_pts_analysis$Landscape.indicator %in% c(3), 'orange', 'black')))
abline(lm(Depth_log ~ stream_dist_N_150, data=soaproot_pts_analysis), lty=2) #[soaproot_pts_analysis$stream_dist_N_150 < 200,]
#abline(v=200, lty=2, col='grey')
summary(lm(Depth_log ~ stream_dist_N_400, data=soaproot_pts_analysis))
summary(lm(Depth_log ~ stream_dist_N_300, data=soaproot_pts_analysis))
summary(lm(Depth_log ~ stream_dist_N_200, data=soaproot_pts_analysis))
summary(lm(Depth_log ~ stream_dist_N_150, data=soaproot_pts_analysis))
summary(lm(Depth_log ~ stream_dist_N_100, data=soaproot_pts_analysis))

#quick misc. checks
plot(soaproot_pts_analysis$TPI_N, soaproot_pts_analysis$Depth)
plot(soaproot_pts_analysis$TPI_N, soaproot_pts_analysis$Depth_log)
plot(soaproot_pts_analysis$curv_prof_N, soaproot_pts_analysis$Depth)
plot(soaproot_pts_analysis$curv_prof_N, soaproot_pts_analysis$Depth_log)
plot(soaproot_pts_analysis$stream_dist_R, soaproot_pts_analysis$Depth)
plot(soaproot_pts_analysis$stream_dist_R, soaproot_pts_analysis$Depth_log)
summary(lm(Depth ~ stream_dist_R + I(stream_dist_R^2), data=soaproot_pts_analysis_nozeroes))#NS
summary(lm(Depth ~ stream_dist_R + I(stream_dist_R^2), data=soaproot_pts_analysis))
summary(lm(Depth_log ~ TPI_N + DSD_M, data=soaproot_pts_analysis)) #r2=0.18
summary(lm(Depth ~ curv_prof_N + DSD_M, data = soaproot_pts_analysis_nozeroes)) #r2=0.27 vs. 0.23 for 5 m res
summary(lm(Depth ~ curv_prof_N + DSD_M + NDVI_2017_N, data = soaproot_pts_analysis)) #r2=0.32
summary(lm(Depth ~ curv_prof_N + DSD_M + NDVI_2017_N + TPI_N, data = soaproot_pts_analysis)) #r2=0.35
lm_4var <- lm(Depth ~ curv_prof_N + DSD_M + NDVI_2017_N + TPI_N, data = soaproot_pts_analysis)
plot(lm_4var$fitted.values, soaproot_pts_analysis$Depth[!is.na(soaproot_pts_analysis$Depth)])
abline(0, 1, lty=2)
plot(lm_4var)
summary(lm(Depth ~ curv_prof_N + DSD_M + NDVI_2017_N + stream_dist_R + I(stream_dist_R^2), data = soaproot_pts_analysis)) #r2=0.38
summary(lm(Depth ~ curv_prof_N + DSD_M + NDVI_2017_N + stream_dist_R + I(stream_dist_R^2) + TPI_N + slope_N + solrad_N +, data = soaproot_pts_analysis))
summary(lm(Depth ~ TPI_N, data = soaproot_pts_analysis)) #r2=0.06
summary(lm(Depth ~ slope_N, data = soaproot_pts_analysis))
summary(lm(Depth ~ NDVI_2018_N, data = soaproot_pts_analysis)) #r2=0.19
summary(lm(Depth ~ NDVI_2017_N, data = soaproot_pts_analysis)) #r2=0.20
summary(lm(Depth ~ EVI_2017_N, data = soaproot_pts_analysis)) #r2=0.14
summary(lm(Depth ~ EVI_2018_N, data = soaproot_pts_analysis)) #r2=0.12
summary(lm(Depth ~ elev_N, data = soaproot_pts_analysis))
summary(lm(Depth ~ solrad_N, data = soaproot_pts_analysis))
summary(lm(Depth ~ curv_mean_N, data = soaproot_pts_analysis))
summary(lm(Depth ~ CTI_N, data = soaproot_pts_analysis))
summary(lm(Depth ~ CTI_N + I(CTI_N^2), data = soaproot_pts_analysis)) #p=0.05
summary(lm(TPI_N ~ curv_mean_N, data = soaproot_pts_analysis)) #r2=0.93
plot(soaproot_pts_analysis$TPI_N, soaproot_pts_analysis$curv_mean_N)
plot()
summary(lm(curv_plan_N ~ curv_mean_N, data = soaproot_pts_analysis))
summary(lm(curv_prof_N ~ curv_mean_N, data = soaproot_pts_analysis))
summary(lm(curv_prof_N ~ curv_plan_N, data = soaproot_pts_analysis))
summary(lm(Depth ~ NDVI_2017_N + TPI_N, data = soaproot_pts_analysis))
summary(lm(Depth ~ NDVI_2017_N + TPI_N + NDVI_2018_N, data=soaproot_pts_analysis))

#get cross correlation matrix
rank_test <- function(x, df, y, mtd) {
  test <- cor.test(x, df[[y]], method = mtd)
  result <- data.frame(col.1=test$p.value, col.2=test$estimate)
  colnames(result) <- c(paste0(y, '.p.val.', mtd), paste0(y, if(mtd=='pearson') {'.cor.'} else {'.rho.'}, mtd))
  result
}
#co-variate cross correlation matrix
colnames(soaproot_pts_analysis)
method_corr <- 'pearson'
Depth_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='Depth_log', mtd=method_corr))
solrad_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='solrad_N', mtd=method_corr))
CTI_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='CTI_N', mtd=method_corr))
curv_mean_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='curv_mean_N', mtd=method_corr))
curv_plan_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='curv_plan_N', mtd=method_corr))
curv_prof_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='curv_prof_N', mtd=method_corr))
elev_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='elev_N', mtd=method_corr))
EVI_2017_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='EVI_2017_N', mtd=method_corr))
EVI_2018_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='EVI_2018_N', mtd=method_corr))
NDVI_2017_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='NDVI_2017_N', mtd=method_corr))
NDVI_2018_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='NDVI_2018_N', mtd=method_corr))
NDVI_2006_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='ndvi2006', mtd=method_corr))
NDVI_2006_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='ndvi2006', mtd=method_corr))
NDVI_2007_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='ndvi2007', mtd=method_corr))
NDVI_2008_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='ndvi2008', mtd=method_corr))
NDVI_2009_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='ndvi2009', mtd=method_corr))
NDVI_2012_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='ndvi2012', mtd=method_corr))
NDVI_2013_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='ndvi2013', mtd=method_corr))
NDVI_2014_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='ndvi2014', mtd=method_corr))
NDVI_2015_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='ndvi2015', mtd=method_corr))
NDVI_2006_2009avg_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='ndvi2006_2009avg', mtd=method_corr))
NDVI_2012_2015avg_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='ndvi2012_2015avg', mtd=method_corr))
SEI_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='SEI_N', mtd=method_corr))
slope_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='slope_N', mtd=method_corr))
TCI_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='TCI_N', mtd=method_corr))
TPI_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='TPI_N', mtd=method_corr))
TRI_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='TRI_N', mtd=method_corr))
stream_dist_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='stream_dist_N_200', mtd=method_corr))
DSD_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='DSD_v2_M', mtd=method_corr))
Rdist_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='Rdist_R', mtd=method_corr))
OCdist_corrs <- do.call(rbind, lapply(as.data.frame(soaproot_pts_analysis[ ,c("Depth_log", "solrad_N", "CTI_N", "curv_mean_N", "curv_plan_N", "curv_prof_N", "elev_N", "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'ndvi2006', 'ndvi2007', 'ndvi2008', 'ndvi2009', 'ndvi2012', 'ndvi2013', 'ndvi2014', 'ndvi2015', 'ndvi2006_2009avg', 'ndvi2012_2015avg', "SEI_N", "slope_N", "TCI_N", "TPI_N", "TRI_N", "stream_dist_N_200", "DSD_v2_M", 'Rdist_R', 'OCdist_R')]), rank_test, df=soaproot_pts_analysis, y='OCdist_R', mtd=method_corr))

correlation_matrix_pvals <- cbind(Depth_corrs[1], solrad_corrs[1], CTI_corrs[1], curv_mean_corrs[1], curv_plan_corrs[1], curv_prof_corrs[1], elev_corrs[1], EVI_2017_corrs[1], EVI_2018_corrs[1], NDVI_2017_corrs[1], NDVI_2018_corrs[1], NDVI_2006_corrs[1], NDVI_2007_corrs[1], NDVI_2008_corrs[1], NDVI_2009_corrs[1], NDVI_2012_corrs[1], NDVI_2013_corrs[1], NDVI_2014_corrs[1], NDVI_2015_corrs[1], NDVI_2006_2009avg_corrs[1], NDVI_2012_2015avg_corrs[1], SEI_corrs[1], slope_corrs[1], TCI_corrs[1], TPI_corrs[1], TRI_corrs[1], stream_dist_corrs[1], DSD_corrs[1], Rdist_corrs[1], OCdist_corrs[1])
write.csv(correlation_matrix_pvals, file = file.path(modelResults, 'correlation_matrices', paste0('covariate_', method_corr, '_pvals_synthetic.csv')), row.names = FALSE)
correlation_matrix_corrs <- cbind(Depth_corrs[2], solrad_corrs[2], CTI_corrs[2], curv_mean_corrs[2], curv_plan_corrs[2], curv_prof_corrs[2], elev_corrs[2], EVI_2017_corrs[2], EVI_2018_corrs[2], NDVI_2017_corrs[2], NDVI_2018_corrs[2], NDVI_2006_corrs[2], NDVI_2007_corrs[2], NDVI_2008_corrs[2], NDVI_2009_corrs[2], NDVI_2012_corrs[2], NDVI_2013_corrs[2], NDVI_2014_corrs[2], NDVI_2015_corrs[2], NDVI_2006_2009avg_corrs[2], NDVI_2012_2015avg_corrs[2], SEI_corrs[2], slope_corrs[2], TCI_corrs[2], TPI_corrs[2], TRI_corrs[2], stream_dist_corrs[2], DSD_corrs[2], Rdist_corrs[2], OCdist_corrs[2])
write.csv(correlation_matrix_corrs, file = file.path(modelResults, 'correlation_matrices', paste0('covariate_', method_corr, '_corrs_synthetic.csv')), row.names = FALSE)
autocorr_test_soil <- function(df_shp, varname, nsim) {
  set.seed(19801976)
  #then, make an inverse distance weighted matrix
  idw <- 1/pointDistance(df_shp, latlon=FALSE)  #equivalent to 1/as.matrix(dist(coordinates(forage_data_sp))), see GEO200CN lab 14
  diag(idw) <- 0 #set Inf back to zero
  idw_list <- mat2listw(idw)
  result <- moran.mc(df_shp[[varname]], idw_list, nsim = nsim)
  print(result)
  result
  results <- cbind(result$statistic, result$p.value)
  results <- as.data.frame(results)
  colnames(results) <- c('Moran I statistic', 'p_value')
  results$n_pts <- nrow(df_shp)
  results$varname <- varname
  results
}

#add depth to pts
soaproot_UTM11N_shp$Depth <- soaproot_pts_analysis$Depth[match(soaproot_UTM11N_shp$Name, soaproot_pts_analysis$Site)]
summary(soaproot_UTM11N_shp$Depth)
soaproot_UTM11N_shp$Depth_log <- soaproot_pts_analysis$Depth_log[match(soaproot_UTM11N_shp$Name, soaproot_pts_analysis$Site)]
soaproot_UTM11N_shp_noNA <- soaproot_UTM11N_shp[!is.na(soaproot_UTM11N_shp$Depth),]
allpts_autocorr <- autocorr_test_soil(soaproot_UTM11N_shp_noNA, 'Depth', nsim = 999) #p=0.61
allpts_Depth_log_autocorr <- autocorr_test_soil(soaproot_UTM11N_shp_noNA, 'Depth_log', nsim = 999) #p-val 0.7

soaproot_UTM11N_shp_clean <- soaproot_UTM11N_shp_noNA[soaproot_UTM11N_shp_noNA$Depth > 0 & soaproot_UTM11N_shp_noNA$Depth < 7.56, ]
soaproot_UTM11N_shp_augered <- soaproot_UTM11N_shp_noNA[soaproot_UTM11N_shp_noNA$Depth > 0, ] 
allpts_depth_determ_autocorr <- autocorr_test_soil(soaproot_UTM11N_shp_clean, 'Depth', nsim = 999) #no autocorrelation: p=0.56; n=39
allpts_autocorr_augered <- autocorr_test_soil(soaproot_UTM11N_shp_augered, 'Depth', nsim = 999) #p=0.55
as.data.frame(soaproot_UTM11N_shp_clean)
soaproot_pts_analysis_no_OC[,c('Site', 'Depth')]
plot(soaproot_UTM11N_shp_clean$Depth, soaproot_pts_analysis_no_OC$Depth[!is.na(soaproot_pts_analysis_no_OC$Depth)])


#cross-validation model selection exercise
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}
library(dismo)
set.seed(80140)
dim(soaproot_pts_analysis)
kf <- kfold(1:66, k=10)
lapply(soaproot_pts_analysis, function(x) {sum(is.na(x))}) #2 stream distance NAs
crossval_lm <- function(df_pts, varname, model='~ curv_prof_N + DSD_M + NDVI_2017_N + slope_N + solrad_N', n) {
  rmse <- rep(NA, length(unique(kf)))
  predictions <- rep(NA, n)
  for (k in 1:length(unique(kf))) {
    tst <- df_pts[kf == k, ]
    trn <- df_pts[kf != k, ]
    varname_lm <- lm(as.formula(paste(varname, model)), data = trn)
    #print(summary(varname_lm))
    varname_tst_pred <- predict.lm(varname_lm, tst)
    rmse[k] <- RMSE(tst[[varname]], varname_tst_pred)
    predictions[kf == k] <- varname_tst_pred
  }
  print(summary(lm(df_pts[[varname]] ~ predictions)))
  list(rmse.kfold=rmse, oob.predictions=predictions)
}
#test <- crossval_lm(soaproot_pts_analysis, 'Depth_log', model ='~slope_N')
#test$rmse.kfold
#mean(test$rmse.kfold)

#MLR model selection
colnames(soaproot_pts_analysis)
subset1 <- expand.grid(elev_N=c(TRUE, FALSE), slope_N=c(TRUE, FALSE), EVI_2017_N=c(TRUE, FALSE), OCdist_R=c(TRUE, FALSE), ndvi2012_2015avg=c(TRUE, FALSE))
subset2 <- expand.grid(stream_dist_N_150=c(TRUE, FALSE), slope_N=c(TRUE, FALSE), EVI_2017_N=c(TRUE, FALSE), OCdist_R=c(TRUE, FALSE), DSD_v2_M=c(TRUE, FALSE))
subset3 <- expand.grid(stream_dist_N_150=c(TRUE, FALSE), EVI_2017_N=c(TRUE, FALSE), curv_mean_N=c(TRUE, FALSE), DSD_v2_M=c(TRUE, FALSE), SEI_N=c(TRUE, FALSE))
subset4 <- expand.grid(stream_dist_N_150=c(TRUE, FALSE), EVI_2017_N=c(TRUE, FALSE), TPI_N=c(TRUE, FALSE), solrad_N=c(TRUE, FALSE), OCdist_R=c(TRUE, FALSE))
subset5 <- expand.grid(CTI_N=c(TRUE, FALSE), curv_prof_N=c(TRUE, FALSE), EVI_2017_N=c(TRUE, FALSE), SEI_N=c(TRUE, FALSE), ndvi2012_2015avg=c(TRUE, FALSE))
# df <- soaproot_pts_analysis
# depth <- 'gamma_syn'
# varname <- 'Depth_log'
# varDir1 <- 'subset1'
# version_no <- 'v1'
# varDir2 <- paste0('synthetic_great7.5m_', version_no)
# n <- 66
# model_df <- 1:5
# var_subset <- subset1
model_selection_MLR <- function(df, varname, depth, varDir1, varDir2, n, model_df, var_subset) {
  if(!dir.exists(file.path(modelResults, 'MLR_model_selection', varDir1))) {
    dir.create(file.path(modelResults, 'MLR_model_selection', varDir1))
  }
  if(!dir.exists(file.path(modelResults, 'MLR_model_selection', varDir1, varDir2))) {
    dir.create(file.path(modelResults, 'MLR_model_selection', varDir1, varDir2))
  }
  models_to_test <- var_subset
  models_to_test <- models_to_test[1:(nrow(models_to_test)-1), ]
  model_selection_results <- vector(mode='list', length=nrow(models_to_test))
  #print(length(model_selection_results[1]))
  for (i in 1:nrow(models_to_test)) {
    print(i)
    model_to_test <- paste(colnames(models_to_test)[unlist(models_to_test[i,])], collapse = ' + ')
    model_selection_results[[i]] <- crossval_lm(df, varname, model = paste('~ ', model_to_test))
    print(mean(model_selection_results[[i]]$rmse.kfold))
  }
  mean_RMSEs <- unlist(lapply(model_selection_results, function(x) mean(x$rmse.kfold))) #mean cross-validated RMSE by n tested models
  mean_RMSEs_all <- cbind(as.data.frame(1:length(unique(kf))), do.call(cbind, lapply(model_selection_results, function(x) x$rmse.kfold))) #mean RMSE for each k-fold iteration of n tested models; cbind when one at least column is a data.frame returns a data.frame
  colnames(mean_RMSEs_all) <- c('kfold', paste0('model_', 1:nrow(models_to_test)))
  oob_predictions_all <- cbind(df['Site'], do.call(cbind, lapply(model_selection_results, function(x) x$oob.predictions)))
  colnames(oob_predictions_all)[2:ncol(oob_predictions_all)] <- paste0('model_', 1:nrow(models_to_test))
  #make summary
  df_model <- apply(models_to_test, 1, sum)
  meanRMSEs <- apply(mean_RMSEs_all[,2:ncol(mean_RMSEs_all)], 2, mean)
  oob_r2s <- apply(oob_predictions_all[,2:ncol(oob_predictions_all)], 2, function(x) summary(lm(df[[varname]] ~ x))$r.squared)
  summary <- data.frame(model=apply(models_to_test, 1, function(x) paste(colnames(models_to_test)[x], collapse = ' + ')), meanRMSE=meanRMSEs, oob_r.squared=oob_r2s, df_model=df_model)
  summary_by_model_df <- split(summary, summary$df_model)
  best_models <- unlist(lapply(summary_by_model_df, function(x) as.character(x$model[which.min(x$meanRMSE)])))
  best_rmses <- unlist(lapply(summary_by_model_df, function(x) min(x$meanRMSE)))
  best_oobs_r2 <- unlist(lapply(summary_by_model_df, function(x) x$oob_r.squared[which.min(x$meanRMSE)]))
  final_summary <- data.frame(model_df=model_df, model_name=best_models, meanRMSE_20foldCV=best_rmses, OOB_r2=best_oobs_r2)
  write.csv(mean_RMSEs_all, file.path(modelResults, 'MLR_model_selection', varDir1, varDir2, paste(varname, '_', depth, '_MLR_selection_CV_RMSEs.csv')), row.names = FALSE)
  write.csv(oob_predictions_all, file.path(modelResults, 'MLR_model_selection', varDir1, varDir2, paste(varname, '_', depth, '_MLR_selection_oob_pred.csv')), row.names = FALSE)
  write.csv(models_to_test, file.path(modelResults, 'MLR_model_selection', varDir1, varDir2, paste(varname, '_', depth, '_MLR_selection_model_test_grid.csv')), row.names = FALSE)
  write.csv(final_summary, file.path(modelResults, 'MLR_model_selection', varDir1, varDir2, paste(varname, '_', depth, '_MLR_selection_BEST_models.csv')), row.names = FALSE)
  final_summary
  #list(RMSEs=mean_RMSEs_all, OOBs=oob_predictions_all, test_grid=models_to_test, best_models=final_summary)
}
#model_selection_no_OC <- model_selection_MLR(df=soaproot_pts_analysis_no_OC, depth = 'No_OC', varname = 'Depth', varDir = 'Depth', n=40)

#run different versions of depths > 7.56 m
run_analysis <- function(version_no, varDir1, var_subset) {
  varDir2 <- paste0('gamma_syn_great7.5m_', version_no)
  soaproot_pts_analysis$Depth[soaproot_pts_analysis$Depth > 7.56] <- 7.56
  for(i in 1:length(soaproot_pts_analysis$Depth)) {
    if(soaproot_pts_analysis$Depth[i] == 7.56) {
      soaproot_pts_analysis$Depth[i] <- synthetic_7.56m_gamma(shape = 1.915, scale=1/0.273) #when four OCs with depth =0.01 are included: meanlog=1.705, sdlog=1.123; see excel file
    } else {next}
  }
  #hist(soaproot_pts_analysis$Depth)
  soaproot_pts_analysis$Depth_log <- log(soaproot_pts_analysis$Depth) 
  result <- model_selection_MLR(df=soaproot_pts_analysis, depth = 'gamma_syn', varname = 'Depth_log', varDir1 = varDir1, varDir2 = varDir2, n=66, model_df = 1:5, var_subset = var_subset)
  write.csv(soaproot_pts_analysis[,c('Site', 'Depth')], file.path(modelResults, 'MLR_model_selection', varDir1, varDir2, paste0('soaproot_pts_syndepths_', version_no, '.csv')), row.names = FALSE)
  result
}
#run_analysis(version_no = 'v1', varDir1 = 'subset_1')
finalize_analysis <- function(runs, varDir1, var_subset) { #runs is a vector how many times we want to repeat the assignment of random depths > 7.56 with MLR testing (eg. 1:30 would be 30 times)
  overall_summary <- do.call(cbind, lapply(paste0('v', runs), function(x) {
    run_analysis(x, varDir1 = varDir1, var_subset = var_subset)
    }))
  write.csv(overall_summary, file.path(modelResults, 'MLR_model_selection', varDir1, 'best_models_summary.csv'), row.names = FALSE)
}
finalize_analysis(runs=1:30, varDir1='subset_1', var_subset = subset1)
finalize_analysis(runs=1:30, varDir1='subset_2', var_subset = subset2)
finalize_analysis(runs=1:30, varDir1='subset_3', var_subset = subset3)
finalize_analysis(runs=1:30, varDir1='subset_4', var_subset = subset4)
finalize_analysis(runs=1:30, varDir1='subset_5', var_subset = subset5)
#best 2-var model for v1 trial
summary(lm(Depth_log ~ EVI_2017_N + stream_dist_N_150, data=soaproot_pts_analysis)) #r2=0.36
vif(lm(Depth_log ~ EVI_2017_N + stream_dist_N_150, data=soaproot_pts_analysis))
plot(lm(Depth_log ~ EVI_2017_N + stream_dist_N_150, data=soaproot_pts_analysis))
summary(lm(Depth_log ~ EVI_2017_N + stream_dist_N_150, data=soaproot_pts_analysis[-c(8, 66), ]))

#best 3-var model
summary(lm(Depth_log ~ EVI_2017_N + stream_dist_N_150 + elev_N, data=soaproot_pts_analysis)) #r2=0.38; elev NS at p=0.13
summary(lm(Depth_log ~ EVI_2017_N + stream_dist_N, data=soaproot_pts_analysis)) #r2=0.2
vif(lm(Depth_log ~ EVI_2017_N + stream_dist_N, data=soaproot_pts_analysis))
plot(lm(Depth_log ~ EVI_2017_N + stream_dist_N, data=soaproot_pts_analysis))
summary(lm(Depth_log ~ EVI_2017_N + stream_dist_N, data=soaproot_pts_analysis[-8,]))
plot(lm(Depth_log ~ EVI_2017_N + stream_dist_N, data=soaproot_pts_analysis[-8,]))

lm(Depth_log ~ EVI_2017_N + stream_dist_N, data=soaproot_pts_analysis)$residuals
summary(lm(Depth ~ EVI_2017_N + stream_dist_N, data=soaproot_pts_analysis))
summary(lm(Depth_log ~ slope_N, data=soaproot_pts_analysis))
summary(lm(Depth ~ slope_N + elev_N + DSD_M + curv_prof_N + NDVI_2017_N, data=soaproot_pts_analysis))
summary(lm(Depth_log ~ slope_N + elev_N + DSD_M + curv_prof_N + NDVI_2017_N, data=soaproot_pts_analysis))



#test with both NEON and USGS terrain chars
df_new <- merge(soaproot_pts_terrain, df_sites, by='Site')
dim(df_new) #retains the 80
df_new <- df_new[!is.na(df_new$Depth),]

#check USGS vs. NEON elevation
colnames(df_new)[1:43]
summary(lm(elev_N ~ Elev, data=df_new)) 
plot(df_new$elev_N, df_new$Elev)

#check slope
summary(lm(slope_N ~ Slope, data=df_new)) 
plot(df_new$slope_N, df_new$Slope)

#univariate check
lapply(df_new[,3:21], function(x) {plot(x, df_new$Depth)})

#trim the dataset to 0 < x < 7.56
df_trim <- df_new[df_new$Depth > 0 & df_new$Depth < 7.56,]
dim(df_trim)
mapply(function(x,y) plot(x, df_trim$Depth, main=y), x=df_trim[,3:21], y=colnames(df_trim)[3:21])
lapply(function(x) summary(lm(x, df_trim$Depth)), x=df_trim[,3:20], y=colnames(df_trim)[3:20])
summary(lm(Depth ~ TPI_N, data = df_trim))
summary(lm(Depth ~ slope_N, data = df_trim))
summary(lm(Depth ~ NDVI_2018_N, data = df_trim))
summary(lm(Depth ~ NDVI_2017_N, data = df_trim))
summary(lm(Depth ~ EVI_2017_N, data = df_trim))
summary(lm(Depth ~ EVI_2018_N, data = df_trim))
summary(lm(Depth ~ elev_N, data = df_trim))
summary(lm(Depth ~ TPI, data = df_trim))
summary(lm(Depth ~ solrad_N, data = df_trim))
summary(lm(Depth ~ curv_mean_N, data = df_trim))

summary(lm(Depth ~ CTI_N, data = df_trim))
summary(lm(Depth ~ CTI_N + I(CTI_N^2), data = df_trim)) #p=0.05
summary(lm(Depth ~ curv_mean_N + CTI_N + I(CTI_N^2), data = df_trim)) #r2=0.235
summary(lm(Depth ~ curv_prof_N + CTI_N + I(CTI_N^2), data = df_trim))
summary(lm(Depth ~ curv_mean_N + CTI_N + I(CTI_N^2) + EVI_2017_N, data = df_trim)) #r2=0.265
summary(lm(Depth ~ curv_mean_N + CTI_N + I(CTI_N^2) + TPI_N, data = df_trim))
summary(lm(Depth ~ curv_mean_N + CTI_N + I(CTI_N^2) + DSD, data = df_trim))
summary(lm(TPI_N ~ curv_mean_N, data = df_trim)) #r2=0.96
summary(lm(TPI ~ curv_mean_N, data = df_trim)) #r2=0.11
summary(lm(curv_plan_N ~ curv_mean_N, data = df_trim))
summary(lm(curv_prof_N ~ curv_mean_N, data = df_trim))
summary(lm(curv_prof_N ~ curv_plan_N, data = df_trim))
summary(lm(Depth ~ curv_mean_N + CTI_N + I(CTI_N^2) + TPI, data = df_trim)) #r2=0.30
summary(lm(Depth ~ curv_prof_N + CTI_N + I(CTI_N^2) + DSD, data = df_trim))
summary(lm(Depth ~ curv_prof_N + CTI_N + I(CTI_N^2) + TPI_N, data = df_trim))
summary(lm(Depth ~ curv_prof_N + CTI_N + I(CTI_N^2) + NDVI_2017_N + I(NDVI_2017_N^2), data = df_trim))
summary(lm(Depth ~ curv_plan_N + CTI_N + I(CTI_N^2) + TPI + curv_prof_N, data = df_trim))
summary(lm(Depth ~ curv_prof_N + CTI_N, data = df_trim))
summary(lm(Depth ~ TPI + DSD, data = df_trim))
summary(lm(Depth ~ Curvature..10.8 + CTI + I(CTI^2) + TPI, data = df_trim))
summary(lm(Depth ~ TPI + DSD, data = df_new))
summary(lm(Depth ~ curv_mean_N + CTI_N + I(CTI_N^2) + DSD, data = df_new))
summary(lm(Depth ~ curv_mean_N + DSD, data = df_new))
summary(lm(Depth ~ curv_mean_N + CTI_N + I(CTI_N^2) + NDVI_2017_N, data = df_new))
summary(lm(Depth ~ NDVI_2017_N, data = df_new))
summary(lm(Depth ~ DSD, data = df_new))

lm_result <- lm(Depth ~ TPI + DSD, data = df_new)
plot(lm_result)
plot(lm_result$fitted.values, df_new$Depth)

summary(df_trim$Depth)
hist(df_trim$Depth)
summary(df_new$Depth)


#initial exploratory
summary(lm(Depth ~ TPI, data = df_sites[df_sites$Depth < 7.56,])) #NS
plot(df_sites$TPI[df_sites$Depth<7.56], df_sites$Depth[df_sites$Depth<7.56])
abline(lm(Depth ~ TPI, data = df_sites[df_sites$Depth < 7.56,]), lty=2)
summary(lm(Depth ~ Curvature..10.8, data = df_sites))
summary(lm(Depth ~ Curvature..10.8, data = df_sites[df_sites$Depth < 7.56,]))
summary(lm(DSD ~ Depth, data = df_sites)) #NS
summary(lm(DSD ~ Depth, data = df_sites[df_sites$Depth < 7.56,])) #NS
plot(df_sites$DSD, df_sites$Depth)

hist(df_sites$DSD)
max(df_sites$DSD)
7560*0.1 #756 mm

