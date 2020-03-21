library(car)
library(extrafont)
library(extrafontdb)
#font_import() #only needs to be done once if R has been updated
loadfonts()
# library(prism)
FiguresDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/Figures'
landsat8Dir <- 'D:/PostDoc/CZO'
downloadDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/prism_annual'
# options(prism.path = downloadDir)
# get_prism_annual(type = 'ppt', years = 2010:2016, keepZip = FALSE)
dataDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO'
NDVI_scale_factor <- 0.0001
res_plots <- 800
library(raster)
soaproot_pts <- read.csv(file.path(dataDir, 'sampling pts', 'Soaproot points RF.csv'), stringsAsFactors = FALSE)
# soaproot_AWC <- read.csv(file.path(dataDir, 'sampling pts', 'site_awc_estimates.csv'), stringsAsFactors = FALSE)
soaproot_terrain_data <- read.csv(file.path(dataDir, 'results', 'tables', 'data', 'terrain_veg_chars_vs Depth_10m_res.csv'), stringsAsFactors = FALSE)
#CRS("+init=epsg:4326") #this is geographic coordinates using WGS84 datum
#CRS("+init=epsg:4269") #this is geographic coordinates using NAD83 datum
soaproot_pts_WGS84 <- SpatialPointsDataFrame(coords=soaproot_pts[,c('POINT_X', 'POINT_Y')], data = soaproot_pts['Name'], proj4string = CRS('+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
names(soaproot_pts_WGS84)

list.files(file.path(landsat8Dir, 'ndmi shared data'))
file_indices <- 26:34
NDMI <- stack(list.files(file.path(landsat8Dir, 'ndmi shared data'), full.names = TRUE)[file_indices])
list.files(file.path(landsat8Dir, 'ndvi shared data'))
NDVI <- stack(list.files(file.path(landsat8Dir, 'ndvi shared data'), full.names = TRUE)[file_indices])

soaproot_pts_NDMI <- extract(NDMI, soaproot_pts_WGS84, df=TRUE)
soaproot_pts_NDMI$ID <- NULL
soaproot_pts_NDVI <- extract(NDVI, soaproot_pts_WGS84, df=TRUE)
soaproot_pts_NDVI$ID <- NULL

soaproot_remote_data_WGS84 <- cbind(soaproot_pts_WGS84$Name, soaproot_pts_NDMI, soaproot_pts_NDVI)
colnames(soaproot_remote_data_WGS84)[1] <- 'Site'
soaproot_remote_data_WGS84$Site <- as.character(soaproot_remote_data_WGS84$Site)

df_master <- read.csv(file.path(dataDir, 'sampling pts', 'Master Data Set_with rock outcrop.csv'), stringsAsFactors = FALSE) #removed biological data from this dataset but it has duplicate rows where multiple biological observations were made at each point; also removed 0's from outcrop (OC) site codes 01-09 to match points dataset labeling
df_sites <- df_master[match(unique(df_master$Site), df_master$Site),]

soaproot_remote_data_WGS84$Depth <- df_sites$Depth[match(soaproot_remote_data_WGS84$Site, df_sites$Site)]
soaproot_remote_data_WGS84$Depth[is.na(soaproot_remote_data_WGS84$Depth) & grepl('OC', soaproot_remote_data_WGS84$Site)] <- 0

soaproot_remote_data_WGS84 <- soaproot_remote_data_WGS84[grepl('SR.A.', soaproot_remote_data_WGS84$Site), ] #leaves out all rock outcrop points
soaproot_remote_data_WGS84 <- soaproot_remote_data_WGS84[!is.na(soaproot_remote_data_WGS84$Depth),]
soaproot_remote_data_WGS84$Depth_class <- ifelse(soaproot_remote_data_WGS84$Depth < 3.3, 1, ifelse(soaproot_remote_data_WGS84$Depth < 7.56, 2, 3)) #1=shallow; 2=moderate; 3=deep

#remote data
deltaNDMI <- function(year) {
  soaproot_remote_data_WGS84[[paste0('deltaNDMI_', year)]] <- soaproot_remote_data_WGS84[[paste0('MeanNDMIyear', year)]] - apply(soaproot_remote_data_WGS84[,c('MeanNDMIyear2009', 'MeanNDMIyear2010', 'MeanNDMIyear2011')], 1, mean)
  soaproot_remote_data_WGS84
}
soaproot_remote_data_WGS84 <- deltaNDMI(2010)
soaproot_remote_data_WGS84 <- deltaNDMI(2011)
soaproot_remote_data_WGS84 <- deltaNDMI(2012)
soaproot_remote_data_WGS84 <- deltaNDMI(2013)
soaproot_remote_data_WGS84 <- deltaNDMI(2014)
soaproot_remote_data_WGS84 <- deltaNDMI(2015)
soaproot_remote_data_WGS84 <- deltaNDMI(2016)
soaproot_remote_data_WGS84 <- deltaNDMI(2017)

#add mean NDVI
soaproot_remote_data_WGS84$MeanNDVIyear2009_2011 <- apply(soaproot_remote_data_WGS84[,c('MeanNDVIyear2009', 'MeanNDVIyear2010', 'MeanNDVIyear2011')], 1, mean)

#MLR model
cbind(soaproot_remote_data_WGS84$Site, soaproot_terrain_data$Site)
all(soaproot_remote_data_WGS84$Site==soaproot_terrain_data$Site)

scaled_deltaNDMI2012 <- soaproot_remote_data_WGS84$deltaNDMI_2012*NDVI_scale_factor
scaled_deltaNDMI2013 <- soaproot_remote_data_WGS84$deltaNDMI_2013*NDVI_scale_factor
scaled_deltaNDMI2014 <- soaproot_remote_data_WGS84$deltaNDMI_2014*NDVI_scale_factor
scaled_deltaNDMI2015 <- soaproot_remote_data_WGS84$deltaNDMI_2015*NDVI_scale_factor
scaled_deltaNDMI2016 <- soaproot_remote_data_WGS84$deltaNDMI_2016*NDVI_scale_factor
scaled_deltaNDMI2017 <- soaproot_remote_data_WGS84$deltaNDMI_2017*NDVI_scale_factor
scaled_meanNDVI2009_2011 <- soaproot_remote_data_WGS84$MeanNDVIyear2009_2011 * NDVI_scale_factor
colnames(soaproot_remote_data_WGS84)
lapply(soaproot_remote_data_WGS84[,22:29], function(x) {
  summary(lm(x ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011 + soaproot_terrain_data$stream_dist_N_150))
})
lapply(soaproot_remote_data_WGS84[,22:29], function(x) {
  vif(lm(x ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011 + soaproot_terrain_data$stream_dist_N_150))
})
lapply(soaproot_remote_data_WGS84[,22:29], function(x) {
  summary(lm(x ~  soaproot_terrain_data$stream_dist_N_150))
})
lapply(soaproot_remote_data_WGS84[,22:29], function(x) {
  summary(lm(x ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011))
})
lapply(soaproot_remote_data_WGS84[,22:29], function(x) {
  summary(lm(x ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011 + soaproot_terrain_data$stream_dist_N_150 + as.factor(soaproot_terrain_data$depth_class)))
})
lapply(soaproot_remote_data_WGS84[,22:29], function(x) {
  summary(lm(x ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011+ as.factor(soaproot_terrain_data$depth_class)))
})

#str dist vs. predrought NDVI
tiff(file = file.path(FiguresDir, 'MLR_Mar2020', '2009_2011NDVI_vs_strDist.tif'), family = 'Times New Roman', pointsize = 11, width = 3.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(soaproot_terrain_data$stream_dist_N_150, soaproot_remote_data_WGS84$MeanNDVIyear2009_2011*NDVI_scale_factor, ylab='mean NDVI (2009-2011), before drought', xlab='Distance from channel (m)')
abline(lm(scaled_meanNDVI2009_2011 ~ soaproot_terrain_data$stream_dist_N_150), lty=2, col='grey', lwd=1.2)
dev.off()

#delta NDMI vs str dist
#2012
tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'deltaNDMI2012_vs_strDist.tif'), family = 'Times New Roman', pointsize = 11, width = 3.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(soaproot_terrain_data$stream_dist_N_150, soaproot_remote_data_WGS84$deltaNDMI_2012*NDVI_scale_factor, ylab='Delta NDMI (2012)', xlab='Distance from channel (m)')
abline(lm(scaled_deltaNDMI2012 ~ soaproot_terrain_data$stream_dist_N_150), lty=2, col='grey', lwd=1.2)
dev.off()

#2013
tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'deltaNDMI2013_vs_strDist.tif'), family = 'Times New Roman', pointsize = 11, width = 3.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(soaproot_terrain_data$stream_dist_N_150, soaproot_remote_data_WGS84$deltaNDMI_2013*NDVI_scale_factor, ylab='Delta NDMI (2013)', xlab='Distance from channel (m)')
abline(lm(scaled_deltaNDMI2013 ~ soaproot_terrain_data$stream_dist_N_150), lty=2, col='grey', lwd=1.2)
dev.off()

#2014
tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'deltaNDMI2014_vs_strDist.tif'), family = 'Times New Roman', pointsize = 11, width = 3.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(soaproot_terrain_data$stream_dist_N_150, soaproot_remote_data_WGS84$deltaNDMI_2014*NDVI_scale_factor, ylab='Delta NDMI (2014)', xlab='Distance from channel (m)')
abline(lm(scaled_deltaNDMI2014 ~ soaproot_terrain_data$stream_dist_N_150), lty=2, col='grey', lwd=1.2)
dev.off()

#2015
tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'deltaNDMI2015_vs_strDist.tif'), family = 'Times New Roman', pointsize = 11, width = 3.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(soaproot_terrain_data$stream_dist_N_150, soaproot_remote_data_WGS84$deltaNDMI_2015*NDVI_scale_factor, ylab='Delta NDMI (2015)', xlab='Distance from channel (m)')
abline(lm(scaled_deltaNDMI2015 ~ soaproot_terrain_data$stream_dist_N_150), lty=2, col='grey', lwd=1.2)
dev.off()

#2016
tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'deltaNDMI2016_vs_strDist.tif'), family = 'Times New Roman', pointsize = 11, width = 3.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(soaproot_terrain_data$stream_dist_N_150, soaproot_remote_data_WGS84$deltaNDMI_2016*NDVI_scale_factor, ylab='Delta NDMI (2016)', xlab='Distance from channel (m)')
abline(lm(scaled_deltaNDMI2016 ~ soaproot_terrain_data$stream_dist_N_150), lty=2, col='grey', lwd=1.2)
dev.off()



#2017
tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'deltaNDMI2017_vs_strDist.tif'), family = 'Times New Roman', pointsize = 11, width = 3.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(soaproot_terrain_data$stream_dist_N_150, soaproot_remote_data_WGS84$deltaNDMI_2017*NDVI_scale_factor, ylab='Delta NDMI (2017)', xlab='Distance from channel (m)')
abline(lm(scaled_deltaNDMI2017 ~ soaproot_terrain_data$stream_dist_N_150), lty=2, col='grey', lwd=1.2)
dev.off()

#partial regression plots for Delta NDMI 2015
lm_noStrDist <- lm(summary(lm(scaled_deltaNDMI2015 ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)))
summary(lm_noStrDist) #r2=0.28
lm_StrDist_vs_X <- lm(soaproot_terrain_data$stream_dist_N_150 ~ scaled_meanNDVI2009_2011)
summary(lm_StrDist_vs_X) #r2=0.04
summary(lm(residuals(lm_noStrDist) ~ residuals(lm_StrDist_vs_X))) #slope is MLR coefficient; #RSE=0.06563; r2=0.1082

tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'strDist_partialregplot.tif', sep = ''), family = 'Times New Roman', width = 3.5, height = 3.5, pointsize = 11, units = 'in', res=800)
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(residuals(lm_StrDist_vs_X), residuals(lm_noStrDist), ylab='', xlab='')
mtext(text=expression('Distance from channel (m) | others'), side=1, line=2.5, at=25)
mtext(text=expression('Delta NDMI (2015) | others'), side=2, line=2.5)
abline(lm(residuals(lm_noStrDist) ~ residuals(lm_StrDist_vs_X)), lty=2, lwd=1.2, col='grey')
#text(x=-125, y=2.0, label='b')
dev.off()

lm_noNDVI <- lm(scaled_deltaNDMI2015 ~ soaproot_terrain_data$stream_dist_N_150)
summary(lm_noNDVI) #r2=0.03
lm_NDVI_vs_X <- lm(scaled_meanNDVI2009_2011 ~ soaproot_terrain_data$stream_dist_N_150)
summary(lm_NDVI_vs_X) #r2=0.04
summary(lm(residuals(lm_noNDVI) ~ residuals(lm_NDVI_vs_X))) #slope is MLR coefficient; #RSE=0.06563; r2=0.34

tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'NDVI2009_2011_partialregplot.tif', sep = ''), family = 'Times New Roman', width = 3.5, height = 3.5, pointsize = 11, units = 'in', res=800)
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(residuals(lm_NDVI_vs_X), residuals(lm_noNDVI), ylab='', xlab='')
mtext(text=expression('Mean NDVI (2009-2011) | others'), side=1, line=2.5)
mtext(text=expression('Delta NDMI (2015) | others'), side=2, line=2.5)
abline(lm(residuals(lm_noNDVI) ~ residuals(lm_NDVI_vs_X)), lty=2, lwd=1.2, col='grey')
#text(x=-125, y=2.0, label='b')
dev.off()

#partial regression plots for 2017 Delta NDMI
lm_noStrDist <- lm(summary(lm(scaled_deltaNDMI2017 ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)))
summary(lm_noStrDist) #r2=0.34
lm_StrDist_vs_X <- lm(soaproot_terrain_data$stream_dist_N_150 ~ scaled_meanNDVI2009_2011)
summary(lm_StrDist_vs_X) #r2=0.04
summary(lm(residuals(lm_noStrDist) ~ residuals(lm_StrDist_vs_X))) #slope is MLR coefficient; #RSE=0.08638 ; r2=0.16

tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'strDist_partialregplot_NDMI2017.tif', sep = ''), family = 'Times New Roman', width = 3.5, height = 3.5, pointsize = 11, units = 'in', res=800)
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(residuals(lm_StrDist_vs_X), residuals(lm_noStrDist), ylab='', xlab='')
mtext(text=expression('Distance from channel (m) | others'), side=1, line=2.5, at=25)
mtext(text=expression('Delta NDMI (2017) | others'), side=2, line=2.5)
abline(lm(residuals(lm_noStrDist) ~ residuals(lm_StrDist_vs_X)), lty=2, lwd=1.2, col='grey')
#text(x=-125, y=2.0, label='b')
dev.off()

lm_noNDVI <- lm(scaled_deltaNDMI2017 ~ soaproot_terrain_data$stream_dist_N_150)
summary(lm_noNDVI) #r2=0.04
lm_NDVI_vs_X <- lm(scaled_meanNDVI2009_2011 ~ soaproot_terrain_data$stream_dist_N_150)
summary(lm_NDVI_vs_X) #r2=0.04
summary(lm(residuals(lm_noNDVI) ~ residuals(lm_NDVI_vs_X))) #slope is MLR coefficient; #RSE=0.08638; r2=0.43

tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'NDVI2009_2011_partialregplot_NDMI2017.tif', sep = ''), family = 'Times New Roman', width = 3.5, height = 3.5, pointsize = 11, units = 'in', res=800)
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(residuals(lm_NDVI_vs_X), residuals(lm_noNDVI), ylab='', xlab='')
mtext(text=expression('Mean NDVI (2009-2011) | others'), side=1, line=2.5)
mtext(text=expression('Delta NDMI (2017) | others'), side=2, line=2.5)
abline(lm(residuals(lm_noNDVI) ~ residuals(lm_NDVI_vs_X)), lty=2, lwd=1.2, col='grey')
#text(x=-125, y=2.0, label='b')
dev.off()

#mean NDVI 2009-2011 vs. Delta NDMI 2012
tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'deltaNDMI2012_vs_NDVI_2009_2011.tif'), family = 'Times New Roman', pointsize = 11, width = 3.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(soaproot_remote_data_WGS84$MeanNDVIyear2009_2011*NDVI_scale_factor, soaproot_remote_data_WGS84$deltaNDMI_2012*NDVI_scale_factor, ylab='Delta NDMI (2012)', xlab='mean NDVI (2009-2011), before drought')
abline(lm(scaled_deltaNDMI2012 ~ scaled_meanNDVI2009_2011), lty=2, col='grey', lwd=1.2)
dev.off()

#mean NDVI 2009-2011 vs. Delta NDMI 2013
tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'deltaNDMI2013_vs_NDVI_2009_2011.tif'), family = 'Times New Roman', pointsize = 11, width = 3.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(soaproot_remote_data_WGS84$MeanNDVIyear2009_2011*NDVI_scale_factor, soaproot_remote_data_WGS84$deltaNDMI_2013*NDVI_scale_factor, ylab='Delta NDMI (2013)', xlab='mean NDVI (2009-2011), before drought')
abline(lm(scaled_deltaNDMI2013 ~ scaled_meanNDVI2009_2011), lty=2, col='grey', lwd=1.2)
dev.off()

#mean NDVI 2009-2011 vs. Delta NDMI 2014
tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'deltaNDMI2014_vs_NDVI_2009_2011.tif'), family = 'Times New Roman', pointsize = 11, width = 3.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(soaproot_remote_data_WGS84$MeanNDVIyear2009_2011*NDVI_scale_factor, soaproot_remote_data_WGS84$deltaNDMI_2014*NDVI_scale_factor, ylab='Delta NDMI (2014)', xlab='mean NDVI (2009-2011), before drought')
abline(lm(scaled_deltaNDMI2014 ~ scaled_meanNDVI2009_2011), lty=2, col='grey', lwd=1.2)
dev.off()

#mean NDVI 2009-2011 vs. Delta NDMI 2015
tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'deltaNDMI2015_vs_NDVI_2009_2011.tif'), family = 'Times New Roman', pointsize = 11, width = 3.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(soaproot_remote_data_WGS84$MeanNDVIyear2009_2011*NDVI_scale_factor, soaproot_remote_data_WGS84$deltaNDMI_2015*NDVI_scale_factor, ylab='Delta NDMI (2015)', xlab='mean NDVI (2009-2011), before drought')
abline(lm(scaled_deltaNDMI2015 ~ scaled_meanNDVI2009_2011), lty=2, col='grey', lwd=1.2)
dev.off()

#mean NDVI 2009-2011 vs. Delta NDMI 2016
tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'deltaNDMI2016_vs_NDVI_2009_2011.tif'), family = 'Times New Roman', pointsize = 11, width = 3.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(soaproot_remote_data_WGS84$MeanNDVIyear2009_2011*NDVI_scale_factor, soaproot_remote_data_WGS84$deltaNDMI_2016*NDVI_scale_factor, ylab='Delta NDMI (2016)', xlab='mean NDVI (2009-2011), before drought')
abline(lm(scaled_deltaNDMI2016 ~ scaled_meanNDVI2009_2011), lty=2, col='grey', lwd=1.2)
dev.off()

#mean NDVI 2009-2011 vs. Delta NDMI 2017
tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'deltaNDMI2017_vs_NDVI_2009_2011.tif'), family = 'Times New Roman', pointsize = 11, width = 3.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(soaproot_remote_data_WGS84$MeanNDVIyear2009_2011*NDVI_scale_factor, soaproot_remote_data_WGS84$deltaNDMI_2017*NDVI_scale_factor, ylab='Delta NDMI (2017)', xlab='mean NDVI (2009-2011), before drought')
abline(lm(scaled_deltaNDMI2017 ~ scaled_meanNDVI2009_2011), lty=2, col='grey', lwd=1.2)
dev.off()

#predicted vs. observed: delta NDMI 2015
lm_2var_2015 <- lm(scaled_deltaNDMI2015 ~ scaled_meanNDVI2009_2011 + soaproot_terrain_data$stream_dist_N_150)
summary(lm_2var_2015) #R2=0.36
plot(lm_2var_2015)
tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'predictedDeltaNDMI2015_MLR2var_vs_observed.tif', sep = ''), family = 'Times New Roman', width = 3.5, height = 3.5, pointsize = 11, units = 'in', res=800)
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(scaled_deltaNDMI2015, lm_2var_2015$fitted.values, ylab='Predicted Delta NDMI (2015)', xlab='Observed Delta NDMI (2015)', yaxt='n', xaxt='n', xlim=c(-0.35, 0.02), ylim=c(-0.35, 0.02))
axis(side=1, at=seq(from=-0.35, to=0, by=0.05), labels = c('', '-0.3', '', '-0.2', '', '-0.1', '', '0'))
axis(side=2, at=seq(from=-0.35, to=0, by=0.05), labels = c('', '-0.3', '', '-0.2', '', '-0.1', '', '0'))
abline(a=0, b=1, lty=2, col='grey', lwd=1.2)
#text(x=-125, y=2.0, label='b')
dev.off()

#predicted vs. observed: delta NDMI 2017
lm_2var_2017 <- lm(scaled_deltaNDMI2017 ~ scaled_meanNDVI2009_2011 + soaproot_terrain_data$stream_dist_N_150)
summary(lm_2var_2017) #R2=0.45
plot(lm_2var_2017)
tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'predictedDeltaNDMI2017_MLR2var_vs_observed.tif', sep = ''), family = 'Times New Roman', width = 3.5, height = 3.5, pointsize = 11, units = 'in', res=800)
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(scaled_deltaNDMI2017, lm_2var_2017$fitted.values, ylab='Predicted Delta NDMI (2017)', xlab='Observed Delta NDMI (2017)', yaxt='n', xaxt='n', xlim=c(-0.35, 0.02), ylim=c(-0.35, 0.02))
axis(side=1, at=seq(from=-0.35, to=0, by=0.05), labels = c('', '-0.3', '', '-0.2', '', '-0.1', '', '0'))
axis(side=2, at=seq(from=-0.35, to=0, by=0.05), labels = c('', '-0.3', '', '-0.2', '', '-0.1', '', '0'))
abline(a=0, b=1, lty=2, col='grey', lwd=1.2)
#text(x=-125, y=2.0, label='b')
dev.off()

hist(soaproot_terrain_data$stream_dist_N_150)
