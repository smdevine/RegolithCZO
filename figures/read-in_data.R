#note this is current set of reduced number of variables used from NEON Lidar 1-m dem: 'elev_N', 'solrad_N', 'slope_N', 'curv_mean_N', 'curv_plan_N', 'curv_prof_N', 'TWI_N', 'stream_dist_N_150', 'EVI_2017_N', 'EVI_2018_N'
library(raster)
dataDir <- 'C:/Users/smdevine/Desktop/post doc/czo work'
landsat8Dir <- 'C:/Users/smdevine/Desktop/post doc/czo work/landsat8/summaries/finals'
FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/results/figures'
#read-in and prepare data
TablesDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/results/tables'
DSDdir <- 'C:/Users/smdevine/Desktop/post doc/czo work/DSD Mike'
#merge with depth data
df_master <- read.csv(file.path(dataDir, 'Master Data Set_with rock outcrop.csv'), stringsAsFactors = FALSE) #removed biological data from this dataset but it has duplicate rows where multiple biological observations were made at each point; also removed 0's from outcrop (OC) site codes 01-09 to match points dataset labeling
df_sites <- df_master[match(unique(df_master$Site), df_master$Site), ]
soaproot_pts <- read.csv(file.path(dataDir, 'Soaproot points RF.csv'), stringsAsFactors = FALSE)
soaproot_pts_WGS84 <- SpatialPointsDataFrame(coords=soaproot_pts[,c('POINT_X', 'POINT_Y')], data = soaproot_pts['Name'], proj4string = CRS('+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
soaproot_UTM11N_shp <- spTransform(soaproot_pts_WGS84, CRS("+proj=utm +zone=11 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) #project from geographic to WGS84 UTM 11N
soaproot_UTM11N_shp_export <- soaproot_UTM11N_shp
names(soaproot_UTM11N_shp_export)[1] <- 'Site'
soaproot_UTM11N_shp_export$Depth <- df_sites$Depth[match(soaproot_UTM11N_shp_export$Site, df_sites$Site)]
soaproot_UTM11N_shp_export <- soaproot_UTM11N_shp_export[soaproot_UTM11N_shp_export$Depth > 0 & !is.na(soaproot_UTM11N_shp_export$Depth), ]
# shapefile(soaproot_UTM11N_shp_export, file.path(FiguresDir, 'soaproot_pts_UTM111N_noOC.shp'))
resolution <- '10m'
drop_lyrs <- TRUE
if(resolution == '10m') {NEONterrainDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/NEON 10m/terrain characteristics'
} else if(resolution == '5m') {NEONterrainDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/5m terrain characteristics/5m filtered'} #5m filtered data produced from 7/12/19 arcgis work
NEON_terrain <- stack(list.files(NEONterrainDir, full.names = TRUE))
if(resolution == '10m') {
  NEON_terrain$annsolrad_10m <- NEON_terrain$annsolrad_10m / 1000
  names(NEON_terrain)
  names(NEON_terrain) <- c('solrad_N', 'aspect_N', 'CTI_N', 'curv_mean_N', 'curv_plan_N', 'curv_prof_N', 'elev_N', 'elev_above_str_150', 'EVI_2017_N', 'EVI_2018_N', 'flowacc_N', 'NDVI_2017_N', 'NDVI_2018_N', 'SEI_N', 'slope_N', 'stream_dist_N_100', 'stream_dist_N_150', 'stream_dist_N_200', 'stream_dist_N_300', 'stream_dist_N_400', 'TCI_N', 'TWI_N', 'upslope_area_N')
  if (drop_lyrs) {
    NEON_terrain <- subset(NEON_terrain, c('elev_N', 'solrad_N', 'slope_N', 'curv_mean_N', 'curv_plan_N', 'curv_prof_N', 'TWI_N', 'stream_dist_N_150', 'EVI_2017_N', 'EVI_2018_N'))
    names(NEON_terrain)[8] <- 'str_dist_N'
  }
  
} else if(resolution == '5m') {
  NEON_terrain$solrad_5m <- NEON_terrain$solrad_5m / 1000
  names(NEON_terrain)
  names(NEON_terrain) <- c('aspect_N', 'CTI_N', 'curv_mean_N', 'curv_plan_N', 'curv_prof_N', 'elev_N', 'elev_above_str_N', 'EVI_2017_N', 'EVI_2018_N', 'IMI_N', 'NDVI_2017_N', 'NDVI_2018_N', 'SEI_N', 'slope_N', 'solrad_N', 'stream_dist_N', 'TWI_N', 'upslope_area_5m')
  NEON_terrain <- subset(NEON_terrain, c('CTI_N', 'curv_mean_N', 'curv_plan_N', 'curv_prof_N', 'elev_N', 'elev_above_str_N', 'EVI_2017_N', 'EVI_2018_N', 'NDVI_2017_N', 'NDVI_2018_N',  'slope_N', 'solrad_N', 'stream_dist_N', 'TWI_N'))
}
DSD_v2 <- raster(file.path(DSDdir, 'DSDout_V2_1_cropped.tif')) #cropped version derived from DSDout_V2_1.tif'
soaproot_pts_terrain <- extract(NEON_terrain, soaproot_UTM11N_shp, df=TRUE)
soaproot_pts_terrain$ID <- NULL
soaproot_pts_terrain <- cbind(soaproot_UTM11N_shp$Name, soaproot_pts_terrain)
colnames(soaproot_pts_terrain)[1] <- 'Site'
soaproot_pts_terrain$Site <- as.character(soaproot_pts_terrain$Site)

NDVI_landsat8 <- read.csv(file.path(landsat8Dir, 'NDVI_2013_2017_final.csv'), row.names = 1)
NDVI_landsat8$doy_20160121_ndvi <- NULL
NDVI_landsat8$Site <- row.names(NDVI_landsat8)
NDVI_landsat8$Depth <- df_sites$Depth[match(NDVI_landsat8$Site, df_sites$Site)] #they are both in same order but this is safer nonetheless
include_OC <- FALSE
NDVI_landsat8 <- NDVI_landsat8[!is.na(NDVI_landsat8$Depth), ]
if (include_OC) {} else {
  NDVI_landsat8 <- NDVI_landsat8[NDVI_landsat8$Depth!=0,]
}
NDVI_dates <- as.Date(sapply(colnames(NDVI_landsat8)[1:60], function(x) substr(x, 5, 12)), format = '%Y%m%d')
NDVI_by_year <- apply(NDVI_landsat8[,1:60], 1, function(x) tapply(x, format.Date(NDVI_dates, '%Y'), mean))
NDVI_by_year <- as.data.frame(t(NDVI_by_year))
colnames(NDVI_by_year) <- paste0('NDVI_', colnames(NDVI_by_year))
NDVI_by_year$Site <- row.names(NDVI_by_year)
# NDVI_by_GS <- data.frame(Site=names(apply(NDVI_landsat8[,15:21], 1, mean)), NDVI_summer_2014=apply(NDVI_landsat8[,15:21], 1, mean))
# NDVI_by_GS$NDVI_summer_2015 <- apply(NDVI_landsat8[,27:33], 1, mean)
soaproot_pts_terrain$DSD_v2_M <- extract(DSD_v2, soaproot_pts_WGS84) / 10 #latest file from 4/9/19 cropped to previous version extent #because DSD is in units 10 * mm yr^-1; buffer=15, fun=mean improves variance explained only slightly with old estimates of DSD but slope still 1.05
soaproot_pts_analysis <- soaproot_pts_terrain[grepl('SR.A.', soaproot_pts_terrain$Site), ] #leaves out all rock outcrop points
#merge with annual NDVI means
soaproot_pts_analysis <- merge(soaproot_pts_analysis, NDVI_by_year, by='Site')
# soaproot_pts_analysis <- merge(soaproot_pts_analysis, NDVI_by_GS, by='Site')

soaproot_pts_analysis$Depth <- df_sites$Depth[match(soaproot_pts_analysis$Site, df_sites$Site)]
soaproot_pts_analysis <- soaproot_pts_analysis[!is.na(soaproot_pts_analysis$Depth),] #one point had a NA for depth

soaproot_pts_analysis$depth_class <- as.factor(ifelse(soaproot_pts_analysis$Depth < 3.3, 1, ifelse(soaproot_pts_analysis$Depth < 7.56, 2, 3))) #1=shallow; 2=moderate; 3=deep
#soaproot_pts_analysis$depth_class_2 <- as.factor(ifelse(soaproot_pts_analysis$Depth < 5.615, 1, 2)) #1=shallow-moderate; 2=deep
colnames(soaproot_pts_analysis)
#write.csv(soaproot_pts_analysis, file.path(dataDir, 'results', 'tables', 'data',))