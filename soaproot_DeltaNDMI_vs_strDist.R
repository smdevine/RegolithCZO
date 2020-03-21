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
#CRS("+init=epsg:4326") #this is geographic coordinates using WGS84 datum
#CRS("+init=epsg:4269") #this is geographic coordinates using NAD83 datum

list.files(file.path(landsat8Dir, 'ndmi shared data'))
file_indices <- 26:34
NDMI <- stack(list.files(file.path(landsat8Dir, 'ndmi shared data'), full.names = TRUE)[file_indices])
list.files(file.path(landsat8Dir, 'ndvi shared data'))
NDVI <- stack(list.files(file.path(landsat8Dir, 'ndvi shared data'), full.names = TRUE)[file_indices])
crs(NDMI) #CRS arguments: +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
crs(NDVI) #CRS arguments: +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
list.files(file.path(dataDir, 'CZO watersheds/CZO'))
soaproot_shp_UTM11N <- shapefile(file.path(dataDir, 'CZO watersheds/CZO/soaproot_utm11N_smoothed.shp'))
soaproot_shp_WGS84 <- spTransform(soaproot_shp_UTM11N, crs(NDMI))
NDMI_soaproot <- crop(NDMI, soaproot_shp_WGS84)
NDVI_soaproot <- crop(NDVI, soaproot_shp_WGS84)

NDMI2009_2011 <- stack(NDMI_soaproot[[1]], NDMI_soaproot[[2]], NDMI_soaproot[[3]])
meanNDMI2009_2011 <- mean(NDMI2009_2011)
deltaNDMI2012 <- NDMI_soaproot$MeanNDMIyear2012 - meanNDMI2009_2011
deltaNDMI2013 <- NDMI_soaproot$MeanNDMIyear2013 - meanNDMI2009_2011
deltaNDMI2014 <- NDMI_soaproot$MeanNDMIyear2014 - meanNDMI2009_2011
deltaNDMI2015 <- NDMI_soaproot$MeanNDMIyear2015 - meanNDMI2009_2011
deltaNDMI2016 <- NDMI_soaproot$MeanNDMIyear2016 - meanNDMI2009_2011
deltaNDMI2017 <- NDMI_soaproot$MeanNDMIyear2017 - meanNDMI2009_2011

NDVI2009_2011 <- stack(NDVI_soaproot[[1]], NDVI_soaproot[[2]], NDVI_soaproot[[3]])
meanNDVI2009_2011 <- mean(NDVI2009_2011)

list.files(file.path(dataDir, 'NEON/terrain characteristics 10m'))
strDist <- raster(file.path(dataDir, 'NEON/terrain characteristics 10m', 'strdist_150.tif'))
dem <- raster(file.path(dataDir, 'NEON/terrain characteristics 10m',"DEM_soaproot_10m.tif"))
strDist_wgs84 <- projectRaster(from=strDist, to=NDMI_soaproot, method = 'bilinear')
dem_wgs84 <- projectRaster(from = dem, to=NDMI_soaproot)
slope_wgs84 <- terrain(dem_wgs84, opt = 'slope', unit='degrees', neighbors=8)

watershed_df <- data.frame(MeanNDVI2009_2011=values(meanNDVI2009_2011)*NDVI_scale_factor, strDist=values(strDist_wgs84), elev=values(dem_wgs84), slope=values(slope_wgs84), deltaNDMI2012=values(deltaNDMI2012)*NDVI_scale_factor, deltaNDMI2013=values(deltaNDMI2013)*NDVI_scale_factor, deltaNDMI2014=values(deltaNDMI2014)*NDVI_scale_factor, deltaNDMI2015=values(deltaNDMI2015)*NDVI_scale_factor, deltaNDMI2016=values(deltaNDMI2016)*NDVI_scale_factor, deltaNDMI2017=values(deltaNDMI2017)*NDVI_scale_factor)
lapply(watershed_df, function(x) sum(is.na(x)))
lapply(watershed_df, function(x) summary(x))
watershed_df_noOC <- watershed_df[watershed_df$MeanNDVI2009_2011>0.55,]
nrow(watershed_df_noOC) / nrow(watershed_df) #eliminated 12.04% of watershed
colnames(watershed_df_noOC)
lapply(watershed_df_noOC[,5:10], function(x) summary(lm(x ~ watershed_df_noOC$MeanNDVI2009_2011 + watershed_df_noOC$strDist + watershed_df_noOC$elev + watershed_df_noOC$slope)))


sum(watershed_df_noOC$strDist < 18.7) #2194
sum(watershed_df_noOC$strDist >= 18.7 & watershed_df_noOC$strDist < 39.7) #2332
sum(watershed_df_noOC$strDist >= 39.7 & watershed_df_noOC$strDist < 64.7) #1847
sum(watershed_df_noOC$strDist >= 64.7 & watershed_df_noOC$strDist < 98.7) #1417
sum(watershed_df_noOC$strDist >= 98.7)#1955
# strDist_breaks <- c(18.7,39.7,64.7,98.7)
strDist_breaks <- c(25,50,100,150)
set.seed(3192020)
rows_to_select <- c(sample(row.names(watershed_df_noOC)[watershed_df_noOC$strDist < strDist_breaks[1]], 200), sample(row.names(watershed_df_noOC)[watershed_df_noOC$strDist >= strDist_breaks[1] & watershed_df_noOC$strDist < strDist_breaks[2]], 200), sample(row.names(watershed_df_noOC)[watershed_df_noOC$strDist >= strDist_breaks[2] & watershed_df_noOC$strDist < strDist_breaks[3]], 200), sample(row.names(watershed_df_noOC)[watershed_df_noOC$strDist >= strDist_breaks[3] & watershed_df_noOC$strDist < strDist_breaks[4]], 200), sample(row.names(watershed_df_noOC)[watershed_df_noOC$strDist >= strDist_breaks[4]], 200))
length(rows_to_select)
length(unique(rows_to_select))
watershed_df_final <- watershed_df_noOC[row.names(watershed_df_noOC) %in% rows_to_select, ]
lapply(watershed_df_final, hist)
MLR_2016 <- lm(deltaNDMI2016 ~ MeanNDVI2009_2011 + strDist + elev + slope, data=watershed_df_final)
summary(MLR_2016)
plot(MLR_2016)
plot(watershed_df_final$deltaNDMI2016, MLR_2016$fitted.values)
abline(a=0, b=1, col='red', lty=2)
plot(watershed_df_final$elev, watershed_df_final$strDist)

#str dist vs. pre-drought NDVI
tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'whole watershed', 'strDis_vs_predroughtNDVI.tif'), family = 'Times New Roman', width = 2.5, height = 2.5, pointsize = 11, units = 'in', res=800, compression='lzw')
par(mar=c(3.5,3.5,0.2,0.2))
plot(watershed_df_final$strDist, watershed_df_final$MeanNDVI2009_2011, xlab='', ylab='', pch=NA_integer_)
points(watershed_df_final$strDist, watershed_df_final$MeanNDVI2009_2011, pch=1, cex=0.8, lwd=0.5)
abline(lm(watershed_df_final$MeanNDVI2009_2011 ~ watershed_df_final$strDist), lty=2, col='gray', lwd=1.5)
mtext(text=paste0('Dist. from channel (m)'), side=1, line=2.5)
mtext(text='Pre-drought NDVI (2009-2011)', side=2, line=2.5, at=0.62)
text(x=160, y=0.71, labels=expression(R^2~"=0.003"), adj=c(0,0))
dev.off()
summary(lm(watershed_df_final$MeanNDVI2009_2011 ~ watershed_df_final$strDist))

predict_vs_observe_plot <- function(yr, lim1, lim2, axis_labels, R2, byval) {
  lm_2var <- lm(watershed_df_final[[paste0('deltaNDMI', yr)]] ~ watershed_df_final$MeanNDVI2009_2011 + watershed_df_final$strDist)
  print(summary(lm_2var))
  print(summary(watershed_df_final[[paste0('deltaNDMI', yr)]]))
  tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'whole watershed', paste0('predictedDeltaNDMI', yr, 'MLR2var_vs_observed.tif')), family = 'Times New Roman', width = 2.5, height = 2.5, pointsize = 11, units = 'in', res=800, compression='lzw')
  par(mar=c(3.5, 3.5, 0.1, 0.1))
  plot(watershed_df_final[[paste0('deltaNDMI', yr)]], lm_2var$fitted.values, ylab='', xlab='', yaxt='n', xaxt='n', xlim=c(lim1, lim2), ylim=c(lim1, lim2), pch=NA_integer_)
  axis(side=1, at=seq(from=lim1, to=lim2, by=byval), labels = axis_labels)
  mtext(side=1, paste0('Observed Delta NDMI (', yr, ')'), line = 2.5)
  axis(side=2, at=seq(from=lim1, to=lim2, by=byval), labels = axis_labels)
  mtext(side=2, paste0('Predicted Delta NDMI (', yr, ')'), line=2.5)
  points(watershed_df_final[[paste0('deltaNDMI', yr)]], lm_2var$fitted.values, pch=1, cex=0.8, lwd=0.5)
  text(x=lim1, y=lim2, label=bquote(R^2~'='~.(R2)), adj=c(0,1))
  abline(a=0, b=1, lty=2, col='grey', lwd=1.5)
#text(x=-125, y=2.0, label='b')
  dev.off()
}
predict_vs_observe_plot(2013, -0.2, 0.1, axis_labels = c('-0.2', '', '-0.1', '', '0', '', '0.1'), R2='0.10', byval=0.05) #R2=0.13
predict_vs_observe_plot(2014, lim1 = -0.5, lim2 = 0.1, axis_labels=c('-0.5', '-0.4', '-0.3', '-0.2', '-0.1', '0', '0.1'), R2=0.20, byval = 0.1) #R2=0.241
predict_vs_observe_plot(yr=2015, lim1 = -0.5, lim2 = 0.1, axis_labels=c('-0.5', '-0.4', '-0.3', '-0.2', '-0.1', '0', '0.1'), R2=0.36, byval = 0.1) #R2=0.3905
predict_vs_observe_plot(yr=2016, lim1 = -0.5, lim2 = 0.1, axis_labels=c('-0.5', '-0.4', '-0.3', '-0.2', '-0.1', '0', '0.1'), R2=0.46, byval = 0.1) #R2=0.49
predict_vs_observe_plot(yr=2017, lim1 = -0.6, lim2 = 0.2, axis_labels=c('-0.6', '', '-0.4', '', '-0.2', '', '0', '', '0.2'), R2=0.42, byval=0.1) #0.4304

#partial regression function
#partial regression plots for Delta NDMI 2017
partial_reg_2MLR <- function(year) {
  lm_noStrDist <- lm(summary(lm(watershed_df_final[[paste0('deltaNDMI', year)]] ~ watershed_df_final$MeanNDVI2009_2011)))
  summary(lm_noStrDist) #r2=0.27
  lm_StrDist_vs_X <- lm(watershed_df_final$strDist ~ watershed_df_final$MeanNDVI2009_2011)
  summary(lm_StrDist_vs_X) #r2=0.03
  summary(lm(residuals(lm_noStrDist) ~ residuals(lm_StrDist_vs_X))) #slope is MLR coefficient; #RSE=0.06563; r2=0.1082
  tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'whole watershed', paste0('strDist_partialregplot', year, '.tif')), family = 'Times New Roman', width = 2.5, height = 2.5, pointsize = 11, units = 'in', res=800, compression='lzw')
  par(mar=c(3.5, 3.5, 0.4, 0.3))
  plot(residuals(lm_StrDist_vs_X), residuals(lm_noStrDist), ylab='', xlab='', pch=NA_integer_, cex.axis=0.85)
  points(residuals(lm_StrDist_vs_X), residuals(lm_noStrDist), pch=1, cex=0.8, lwd=0.5)
  mtext(text=paste0('Dist. from channel (m) | others'), side=1, line=2.5)
  mtext(text=paste0('Delta NDMI (', year, ') | others'), side=2, line=2.5)
  abline(lm(residuals(lm_noStrDist) ~ residuals(lm_StrDist_vs_X)), lty=2, lwd=1.5, col='grey')
  #text(x=-125, y=2.0, label='b')
  dev.off()

  lm_noNDVI <- lm(watershed_df_final[[paste0('deltaNDMI', year)]] ~ watershed_df_final$strDist)
  summary(lm_noNDVI) #r2=0.02
  lm_NDVI_vs_X <- lm(watershed_df_final$MeanNDVI2009_2011 ~ watershed_df_final$strDist)
  summary(lm_NDVI_vs_X) #r2=0.03
  summary(lm(residuals(lm_noNDVI) ~ residuals(lm_NDVI_vs_X))) #slope is MLR coefficient; #RSE=0.06563; r2=0.31

  tiff(file = file.path(FiguresDir, 'MLR_Mar2020', 'whole watershed', paste0('NDVI2009_2011_partialregplot', year, '.tif')), family = 'Times New Roman', width = 2.5, height = 2.5, pointsize = 11, units = 'in', res=800, compression = 'lzw')
  par(mar=c(3.5, 3.5, 0.4, 0.3))
  plot(residuals(lm_NDVI_vs_X), residuals(lm_noNDVI), ylab='', xlab='', pch=NA_integer_, cex.axis=0.85)
  points(residuals(lm_NDVI_vs_X), residuals(lm_noNDVI), pch=1, cex=0.8, lwd=0.5)
  mtext(text='NDVI (2009-2011) | others', side=1, line=2.5)
  mtext(text=paste0('Delta NDMI (', year, ') | others'), side=2, line=2.5)
  abline(lm(residuals(lm_noNDVI) ~ residuals(lm_NDVI_vs_X)), lty=2, lwd=1.5, col='grey')
#text(x=-125, y=2.0, label='b')
  dev.off()
}
partial_reg_2MLR(2013)
partial_reg_2MLR(2014)
partial_reg_2MLR(2015)
partial_reg_2MLR(2016)
partial_reg_2MLR(2017)
