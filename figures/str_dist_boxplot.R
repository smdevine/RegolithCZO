library(car)
library(vioplot)
library(raster)
dataDir <- 'C:/Users/smdevine/Desktop/post doc/czo work'
resolution <- '5m'
# resolution <- '10m'
if(resolution == '10m') {
  NEONterrainDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/NEON 10m/terrain characteristics'
  FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/results/figures_10m'
  str_dist <- raster(file.path(NEONterrainDir, 'strdist_150.tif'))
} else if(resolution == '5m') {
  NEONterrainDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/5m terrain characteristics/5m filtered'
  FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/results/figures'
  str_dist <- raster(file.path(NEONterrainDir, 'str_dist_5m.tif'))
  curv_mean <- raster(file.path(NEONterrainDir, 'curv_mean_5m.tif'))
  
  } #5m filtered data produced from 7/12/19 arcgis work

library(extrafont)
library(extrafontdb)
#font_import() only needs to be done once
loadfonts()
res_plots <- 800

#read-in and prepare data
soaproot_pts <- read.csv(file.path(dataDir, 'Soaproot points RF.csv'), stringsAsFactors = FALSE)
soaproot_pts_WGS84 <- SpatialPointsDataFrame(coords=soaproot_pts[,c('POINT_X', 'POINT_Y')], data = soaproot_pts['Name'], proj4string = CRS('+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
soaproot_UTM11N_shp <- spTransform(soaproot_pts_WGS84, CRS("+proj=utm +zone=11 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) #project from geographic to WGS84 UTM 11N
soaproot_pts_terrain <- extract(stack(str_dist, curv_mean), soaproot_UTM11N_shp, df=TRUE)
colnames(soaproot_pts_terrain)[2] <- 'str_dist'
soaproot_pts_terrain$ID <- NULL
#lapply(soaproot_pts_terrain, summary)
soaproot_pts_terrain <- cbind(soaproot_UTM11N_shp$Name, soaproot_pts_terrain)
colnames(soaproot_pts_terrain)[1] <- 'Site'
#lapply(soaproot_pts_terrain, class)
soaproot_pts_terrain$Site <- as.character(soaproot_pts_terrain$Site)

df_master <- read.csv(file.path(dataDir, 'Master Data Set_with rock outcrop.csv'), stringsAsFactors = FALSE) #removed biological data from this dataset but it has duplicate rows where multiple biological observations were made at each point; also removed 0's from outcrop (OC) site codes 01-09 to match points dataset labeling
df_sites <- df_master[match(unique(df_master$Site), df_master$Site), ]
soaproot_pts_terrain$Depth <- df_sites$Depth[match(soaproot_pts_terrain$Site, df_sites$Site)]
soaproot_pts_analysis <- soaproot_pts_terrain[grepl('SR.A.', soaproot_pts_terrain$Site), ] #leaves out all rock outcrop points
soaproot_pts_analysis <- soaproot_pts_analysis[!is.na(soaproot_pts_analysis$Depth),] #one point had a NA for depth
soaproot_pts_analysis$depth_class <- as.factor(ifelse(soaproot_pts_analysis$Depth < 3.3, 1, ifelse(soaproot_pts_analysis$Depth < 7.56, 2, 3))) #1=shallow; 2=moderate; 3=deep
table(soaproot_pts_analysis$depth_class)
head(soaproot_pts_analysis)
boxplot(str_dist ~ depth_class, data = soaproot_pts_analysis)
result.stream_dist.aov <- aov(str_dist ~ depth_class, data = soaproot_pts_analysis)
summary(result.stream_dist.aov)
TukeyHSD(result.stream_dist.aov)
plot(result.stream_dist.aov)
hist(soaproot_pts_analysis$str_dist)
hist(log(soaproot_pts_analysis$str_dist))
leveneTest(str_dist ~ as.factor(depth_class), data = soaproot_pts_analysis) #p=0.68, thereby suggesting no evidence that homogeneity of variance across groups is violated (aka, analysis assumption met)

tiff(file = file.path(FiguresDir, 'stream_dist_boxplots.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
boxplot(str_dist ~ depth_class, data = soaproot_pts_analysis, xlab=c('Regolith depth class (m)'), ylab='Distance from stream (m)', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'), boxwex=0.6)
dev.off()


#vioplot(str_dist ~ depth_class, data = soaproot_pts_analysis, xlab=c('Regolith depth class (m)'), ylab='Distance from stream (m)', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5')) #didn't work when depth_class was recoded to a factor 1, 2, or 3
tiff(file = file.path(FiguresDir, 'stream_dist_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
vioplot(soaproot_pts_analysis$str_dist[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$str_dist[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$str_dist[soaproot_pts_analysis$depth_class==3], xlab=c('Regolith depth class (m)'), ylab='Distance from channel (m)', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'))
dev.off()

#profile curvature plot
tiff(file = file.path(FiguresDir, 'prof_curv_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
vioplot(soaproot_pts_analysis$curv_prof_N[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$curv_prof_N[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$curv_prof_N[soaproot_pts_analysis$depth_class==3], xlab=c('Regolith depth class (m)'), ylab='Profile curvature', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'))
dev.off()

#CTI plot
tiff(file = file.path(FiguresDir, 'CTI_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
vioplot(soaproot_pts_analysis$CTI_N[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$CTI_N[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$CTI_N[soaproot_pts_analysis$depth_class==3], xlab=c('Regolith depth class (m)'), ylab='Compound topographic index', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'))
dev.off()

#TWI plot
tiff(file = file.path(FiguresDir, 'TWI_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
vioplot(soaproot_pts_analysis$twi_5m[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$twi_5m[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$twi_5m[soaproot_pts_analysis$depth_class==3], xlab=c('Regolith depth class (m)'), ylab='Topographic wetness index', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'))
dev.off()

vioplot(soaproot_pts_analysis$TWI_N[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$TWI_N[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$TWI_N[soaproot_pts_analysis$depth_class==3], xlab=c('Regolith depth class (m)'), ylab='Topographic wetness index', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'))

vioplot(soaproot_pts_analysis$stream_dist_N_150[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$stream_dist_N_150[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$stream_dist_N_150[soaproot_pts_analysis$depth_class==3], xlab=c('Regolith depth class (m)'), ylab='Topographic wetness index', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'))
