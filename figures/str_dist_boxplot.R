library(car)
library(vioplot)
library(raster)
dataDir <- 'C:/Users/smdevine/Desktop/post doc/czo work'
# resolution <- '5m'
resolution <- '10m'
if(resolution == '10m') {
  NEONterrainDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/NEON 10m/terrain characteristics'
  FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/results/figures_10m/2021_revisions'
  #str_dist <- raster(file.path(NEONterrainDir, 'strdist_150.tif'))
} else if(resolution == '5m') {
  NEONterrainDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/5m terrain characteristics/5m filtered'
  FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/results/figures'
  #str_dist <- raster(file.path(NEONterrainDir, 'str_dist_5m.tif'))
  #curv_mean <- raster(file.path(NEONterrainDir, 'curv_mean_5m.tif'))
  
  } #5m filtered data produced from 7/12/19 arcgis work

library(extrafont)
library(extrafontdb)
#font_import() only needs to be done once
loadfonts()
res_plots <- 800
mar_settings <- c(3.5, 4, 0.5, 0.5)

#use read-in_data.R as of 8/8/19 to prepare data
boxplot(stream_dist_N_150 ~ depth_class, data = soaproot_pts_analysis)
result.stream_dist.aov <- aov(stream_dist_N_150 ~ depth_class, data = soaproot_pts_analysis)
summary(result.stream_dist.aov)
TukeyHSD(result.stream_dist.aov)
plot(result.stream_dist.aov)
hist(soaproot_pts_analysis$stream_dist_N_150)
hist(log(soaproot_pts_analysis$stream_dist_N_150))
leveneTest(stream_dist_N_150 ~ as.factor(depth_class), data = soaproot_pts_analysis) #p=0.68, thereby suggesting no evidence that homogeneity of variance across groups is violated (aka, analysis assumption met)

# tiff(file = file.path(FiguresDir, 'stream_dist_boxplots.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
# par(mar=mar_settings)
# boxplot(stream_dist_N_150 ~ depth_class, data = soaproot_pts_analysis, xlab=c('Regolith depth class (m)'), ylab='Distance from stream (m)', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'), boxwex=0.6)
# dev.off()


#vioplot(stream_dist_N_150 ~ depth_class, data = soaproot_pts_analysis, xlab=c('Regolith depth class (m)'), ylab='Distance from stream (m)', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5')) #didn't work when depth_class was recoded to a factor 1, 2, or 3
tiff(file = file.path(FiguresDir, 'stream_dist_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 3.75, height = 3.75, units = 'in', res=res_plots, compression = 'lzw')
par(mar=mar_settings)
vioplot(soaproot_pts_analysis$str_dist_N[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$str_dist_N[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$str_dist_N[soaproot_pts_analysis$depth_class==3], xlab='', ylab='', names=c('Shallow', 'Moderate', 'Deep'))
mtext('Regolith depth class', side = 1, line=2.5)
mtext('Distance from stream (m)', side=2, line=2.5)
text('e', x=0.6, y=155)
text('Deep vs. Shallow, p<0.001', x=1.1, y=155, adj=c(0,0.5), cex=0.9)
text('Deep vs. Moderate, p=0.002', x=1.1, y=145, adj=c(0,0.5), cex=0.9)
dev.off()

#profile curvature plot
# tiff(file = file.path(FiguresDir, 'prof_curv_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
# par(mar=c(4.5, 4.5, 0.5, 0.5))
# vioplot(soaproot_pts_analysis$curv_prof_N[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$curv_prof_N[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$curv_prof_N[soaproot_pts_analysis$depth_class==3], xlab=c('Regolith depth class (m)'), ylab='Profile curvature', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'))
# dev.off()

#CTI plot (no longer including)
# tiff(file = file.path(FiguresDir, 'CTI_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
# par(mar=c(4.5, 4.5, 0.5, 0.5))
# vioplot(soaproot_pts_analysis$CTI_N[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$CTI_N[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$CTI_N[soaproot_pts_analysis$depth_class==3], xlab=c('Regolith depth class (m)'), ylab='Compound topographic index', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'))
# dev.off()

#TWI plot
tiff(file = file.path(FiguresDir, 'TWI_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 3.75, height = 3.75, units = 'in', res=res_plots, compression = 'lzw')
par(mar=mar_settings)
vioplot(soaproot_pts_analysis$TWI_N[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$TWI_N[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$TWI_N[soaproot_pts_analysis$depth_class==3], xlab='', ylab='', names=c('Shallow', 'Moderate', 'Deep'))
mtext('Regolith depth class', side = 1, line=2.5)
mtext('Topographic wetness index', side=2, line=2.5)
text('f', x=0.6, y=10.5)
text('Shallow vs. Moderate, p=0.02', x=1.5, y=10.5, adj=c(0,0.5), cex=0.9)
text('Shallow vs. Deep, p=0.003', x=1.5, y=10, adj=c(0,0.5), cex=0.9)
dev.off()

#elev plot
tiff(file = file.path(FiguresDir, 'elevation_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 3.75, height = 3.75, units = 'in', res=res_plots, compression = 'lzw')
par(mar=mar_settings)
vioplot(soaproot_pts_analysis$elev_N[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$elev_N[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$elev_N[soaproot_pts_analysis$depth_class==3], xlab='', ylab='', names=c('Shallow', 'Moderate', 'Deep'))
mtext('Regolith depth class', side = 1, line=2.5)
mtext('Elevation (m)', side=2, line=2.5)
text('a', x=0.6, y=1440)
dev.off()

#slope
tiff(file = file.path(FiguresDir, 'slope_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 3.75, height = 3.75, units = 'in', res=res_plots, compression = 'lzw')
par(mar=mar_settings)
vioplot(soaproot_pts_analysis$slope_N[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$slope_N[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$slope_N[soaproot_pts_analysis$depth_class==3], xlab='', ylab=, names=c('Shallow', 'Moderate', 'Deep'))
mtext('Regolith depth class', side = 1, line=2.5)
mtext(expression('Slope ('*degree*')'), side=2, line=2.5)
text('b', x=0.6, y=34)
dev.off()

#Annual solar radiation
tiff(file = file.path(FiguresDir, 'solrad_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 3.75, height = 3.75, units = 'in', res=res_plots, compression = 'lzw')
par(mar=mar_settings)
vioplot(soaproot_pts_analysis$solrad_N[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$solrad_N[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$solrad_N[soaproot_pts_analysis$depth_class==3], xlab='', ylab='', names=c('Shallow', 'Moderate', 'Deep'))
mtext('Regolith depth class', side = 1, line=2.5)
mtext(expression('Clear sky insolation (kWh'~yr^-1~m^-2*')'), side=2, line=2.5)
text('d', x=0.6, y=1600)
dev.off()

#mean curv
tiff(file = file.path(FiguresDir, 'mean_curv_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 3.75, height = 3.75, units = 'in', res=res_plots, compression = 'lzw')
par(mar=mar_settings)
vioplot(soaproot_pts_analysis$curv_mean_N[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$curv_mean_N[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$curv_mean_N[soaproot_pts_analysis$depth_class==3], xlab='', ylab='', names=c('Shallow', 'Moderate', 'Deep'))
mtext('Regolith depth class', side = 1, line=2.5)
mtext(expression('Mean curvature (100'~m^-1*')'), side=2, line=2.5)
text('c', x=0.6, y=4.5)
dev.off()

#DSD
# tiff(file = file.path(FiguresDir, 'DSD_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
# par(mar=c(4.5, 4.5, 0.5, 0.5))
# vioplot(soaproot_pts_analysis$DSD_v2_M[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$DSD_v2_M[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$DSD_v2_M[soaproot_pts_analysis$depth_class==3], xlab=c('Regolith depth class (m)'), ylab=expression('Dry season drawdown (mm'~yr^-1*')'), names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'))
# dev.off()

#NDVI 2015
# tiff(file = file.path(FiguresDir, 'NDVI_2015_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
# par(mar=c(4.5, 4.5, 0.5, 0.5))
# vioplot(soaproot_pts_analysis$NDVI_2015[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$NDVI_2015[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$NDVI_2015[soaproot_pts_analysis$depth_class==3], xlab=c('Regolith depth class (m)'), ylab='Landsat8 NDVI, 2015', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'))
# dev.off()
# 
# #EVI 2017
# tiff(file = file.path(FiguresDir, 'EVI_2017_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
# par(mar=c(4.5, 4.5, 0.5, 0.5))
# vioplot(soaproot_pts_analysis$EVI_2017_N[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$EVI_2017_N[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$EVI_2017_N[soaproot_pts_analysis$depth_class==3], xlab=c('Regolith depth class (m)'), ylab='NEON EVI, 2017', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'))
# dev.off()
