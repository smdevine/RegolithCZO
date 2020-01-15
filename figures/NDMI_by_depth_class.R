#directories refer to lab desktop
# library(corrplot)
library(extrafont)
library(extrafontdb)
#font_import() #only needs to be done once if R has been updated
loadfonts()
library(raster)
# library(prism)
FiguresDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/Figures'
downloadDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/prism_annual'
# options(prism.path = downloadDir)
# get_prism_annual(type = 'ppt', years = 2010:2016, keepZip = FALSE)
landsat8Dir <- 'D:/PostDoc/CZO'
dataDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO'
saprock_awc <- 0.12
soil_awc <- 0.2
NDVI_scale_factor <- 0.0001
res_plots <- 800
NDVI_to_ET <- function(y) {
  sapply(y, function(x) {
    117.16 * exp(2.8025*x)
  })
}

soaproot_pts <- read.csv(file.path(dataDir, 'sampling pts', 'Soaproot points RF.csv'), stringsAsFactors = FALSE)
# soaproot_AWC <- read.csv(file.path(dataDir, 'sampling pts', 'site_awc_estimates.csv'), stringsAsFactors = FALSE)
soaproot_terrain_data <- read.csv(file.path(dataDir, 'results', 'tables', 'data', 'terrain_veg_chars_vs Depth_10m_res.csv'), stringsAsFactors = FALSE)
#CRS("+init=epsg:4326") #this is geographic coordinates using WGS84 datum
#CRS("+init=epsg:4269") #this is geographic coordinates using NAD83 datum
soaproot_pts_WGS84 <- SpatialPointsDataFrame(coords=soaproot_pts[,c('POINT_X', 'POINT_Y')], data = soaproot_pts['Name'], proj4string = CRS('+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
names(soaproot_pts_WGS84)

list.files(file.path(landsat8Dir, 'ndmi shared data'))
NDMI <- stack(list.files(file.path(landsat8Dir, 'ndmi shared data'), full.names = TRUE))
list.files(file.path(landsat8Dir, 'ndvi shared data'))
NDVI <- stack(list.files(file.path(landsat8Dir, 'ndvi shared data'), full.names = TRUE))

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
# soaproot_remote_data_WGS84$Depth_test <- soaproot_AWC$Regolith[match(soaproot_remote_data_WGS84$Site, soaproot_AWC$Site)]
# cbind(soaproot_remote_data_WGS84$Site, soaproot_remote_data_WGS84$Depth - soaproot_remote_data_WGS84$Depth_test ) #SR.A.66 is off
soaproot_remote_data_WGS84 <- soaproot_remote_data_WGS84[!is.na(soaproot_remote_data_WGS84$Depth),]
# soaproot_remote_data_WGS84$Depth_test <- NULL
# soaproot_remote_data_WGS84$Saprock_depth <- soaproot_AWC$Saprock.fraction[match(soaproot_remote_data_WGS84$Site, soaproot_AWC$Site)]
soaproot_remote_data_WGS84$Depth_class <- ifelse(soaproot_remote_data_WGS84$Depth < 3.3, 1, ifelse(soaproot_remote_data_WGS84$Depth < 7.56, 2, 3)) #1=shallow; 2=moderate; 3=deep

#plot NDMI (need to substitute NDVI)
col_indices <- which(grepl('MeanNDMI', colnames(soaproot_remote_data_WGS84)))
yrs_NDMI <- sapply(colnames(soaproot_remote_data_WGS84)[col_indices], function(x) as.integer(paste0(unlist(strsplit(x, ''))[13:16], collapse = '')), USE.NAMES = FALSE)
plot(apply(soaproot_remote_data_WGS84[,col_indices], 2, mean)*0.0001, type='b', ylab='mean NDMI, regolith sampling points', xaxt='n', xlab='Year')
axis(side=1, at=seq_along(col_indices), labels=yrs_NDMI)

NDMI_means_by_depth_cl <- do.call(cbind, lapply(soaproot_remote_data_WGS84[,col_indices], function(x) {tapply(x, soaproot_remote_data_WGS84$Depth_class, mean)}))
NDMI_sd_by_depth_cl <- do.call(cbind, lapply(soaproot_remote_data_WGS84[,col_indices], function(x) {tapply(x,  soaproot_remote_data_WGS84$Depth_class, sd)}))
NDMI_n_by_depth_cl <- do.call(cbind, lapply(soaproot_remote_data_WGS84[,col_indices], function(x) {tapply(x, soaproot_remote_data_WGS84$Depth_class, length)}))
NDMI_se_by_depth_cl <- NDMI_sd_by_depth_cl / sqrt(NDMI_n_by_depth_cl)
NDMI_plus1SE_by_depth_cl <- NDMI_means_by_depth_cl + NDMI_se_by_depth_cl
NDMI_minus1SE_by_depth_cl <- NDMI_means_by_depth_cl - NDMI_se_by_depth_cl
anova_trim <- lapply(soaproot_remote_data_WGS84[,col_indices], function(x) summary(aov(x ~ soaproot_remote_data_WGS84$Depth_class)))
p_values <- sapply(anova_trim, function(x) x[[1]][1,5])
p_values[which(p_values < 0.05)]
gray_colors <- c('gray70', 'gray55', 'gray40')


tiff(file = file.path(FiguresDir, 'NDMI_by_depth_class3_1SE_historical.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.5, 4.5, 0.5, 0.5))
plot(c(1,length(col_indices)), c(min(NDMI_minus1SE_by_depth_cl[c(1,3),]), max(NDMI_plus1SE_by_depth_cl[c(1,3),]))*NDVI_scale_factor, type='n', xlab='', ylab = 'NDMI, regolith sampling points', xaxt='n')
axis(side=1, at=1:length(col_indices), labels=yrs_NDMI)
polygon(x=c(seq_along(col_indices), rev(seq_along(col_indices))), y=c(NDMI_plus1SE_by_depth_cl[1,], rev(NDMI_minus1SE_by_depth_cl[1,]))*NDVI_scale_factor,  border=NA, lwd=0.3, col='gray95')
polygon(x=c(seq_along(col_indices), rev(seq_along(col_indices))), y=c(NDMI_plus1SE_by_depth_cl[2,], rev(NDMI_minus1SE_by_depth_cl[2,]))*NDVI_scale_factor, border=NA, lwd=0.3, col='gray90')
polygon(x=c(seq_along(col_indices), rev(seq_along(col_indices))), y=c(NDMI_plus1SE_by_depth_cl[3,], rev(NDMI_minus1SE_by_depth_cl[3,]))*NDVI_scale_factor, border = NA, lwd=0.3, col='gray85')
for (i in 1:3) {
  if(i==4) {next} else {
    lines(seq_along(col_indices), NDMI_means_by_depth_cl[i,]*NDVI_scale_factor, col=gray_colors[i], lty=i+1)
    points(seq_along(col_indices), NDMI_means_by_depth_cl[i,]*NDVI_scale_factor, col=gray_colors[i], pch=i+16, cex=0.5)
  }
}
if(length(which(p_values < 0.05))==0) {print('No significant contrasts')} else {points(seq_along(col_indices)[which(p_values < 0.05)], NDMI_means_by_depth_cl[3,]*NDVI_scale_factor[which(p_values < 0.05)]-0.01, col=gray_colors[i], pch=8, cex=0.5)}
legend('bottomleft', legend=c('shallow regolith (<3.3m) \u00B1 1 S.E.', 'moderate regolith (3.3-7.5m) \u00B1 1 S.E.', 'deep regolith (>7.5m) \u00B1 1 S.E.'), lty = c(2:4, NA), pch=c(17:19,8), col = c(gray_colors, 'black'), pt.cex=0.6, inset = 0.01) #'significant contrast (p<0.05)'
# text(as.Date('2014-06-01'), 0.61, '2012-2015: historic 4-year drought and forest die-off')
# abline(v=as.Date('2015-10-01'), lty=2, lwd=0.7)
# text(x=as.Date('2016-01-15'), y=0.7, 'average')
# text(x=as.Date('2016-01-15'), y=0.685, 'winter')
# abline(v=as.Date('2016-05-01'), lty=2, lwd=0.7)
# abline(v=as.Date('2016-10-01'), lty=2, lwd=0.7)
# text(x=as.Date('2017-01-15'), y=0.7, 'historic')
# text(x=as.Date('2017-01-15'), y=0.685, 'wet winter')
# abline(v=as.Date('2017-05-01'), lty=2, lwd=0.7)
# text(x=as.Date('2017-02-25'), y=0.55, labels='official end of drought')
dev.off()

#plot NDVI
col_indices <- which(grepl('MeanNDVI', colnames(soaproot_remote_data_WGS84)))
yrs_NDVI <- sapply(colnames(soaproot_remote_data_WGS84)[col_indices], function(x) as.integer(paste0(unlist(strsplit(x, ''))[13:16], collapse = '')), USE.NAMES = FALSE)
plot(apply(soaproot_remote_data_WGS84[,col_indices], 2, mean)*0.0001, type='b', ylab='mean NDVI, regolith sampling points', xaxt='n', xlab='Year')
axis(side=1, at=seq_along(col_indices), labels=yrs_NDVI)

NDVI_means_by_depth_cl <- do.call(cbind, lapply(soaproot_remote_data_WGS84[,col_indices], function(x) {tapply(x, soaproot_remote_data_WGS84$Depth_class, mean)}))
NDVI_sd_by_depth_cl <- do.call(cbind, lapply(soaproot_remote_data_WGS84[,col_indices], function(x) {tapply(x,  soaproot_remote_data_WGS84$Depth_class, sd)}))
NDVI_n_by_depth_cl <- do.call(cbind, lapply(soaproot_remote_data_WGS84[,col_indices], function(x) {tapply(x, soaproot_remote_data_WGS84$Depth_class, length)}))
NDVI_se_by_depth_cl <- NDVI_sd_by_depth_cl / sqrt(NDVI_n_by_depth_cl)
NDVI_plus1SE_by_depth_cl <- NDVI_means_by_depth_cl + NDVI_se_by_depth_cl
NDVI_minus1SE_by_depth_cl <- NDVI_means_by_depth_cl - NDVI_se_by_depth_cl
anova_trim <- lapply(soaproot_remote_data_WGS84[,col_indices], function(x) summary(aov(x ~ soaproot_remote_data_WGS84$Depth_class)))
p_values <- sapply(anova_trim, function(x) x[[1]][1,5])
p_values[which(p_values < 0.05)]
gray_colors <- c('gray70', 'gray55', 'gray40')


tiff(file = file.path(FiguresDir, 'NDVI_by_depth_class3_1SE_historical.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.5, 4.5, 0.5, 0.5))
plot(c(1,length(col_indices)), c(min(NDVI_minus1SE_by_depth_cl[c(1,3),]), max(NDVI_plus1SE_by_depth_cl[c(1,3),]))*NDVI_scale_factor, type='n', xlab='', ylab = 'NDVI, regolith sampling points', xaxt='n')
axis(side=1, at=1:length(col_indices), labels=yrs_NDVI)
polygon(x=c(seq_along(col_indices), rev(seq_along(col_indices))), y=c(NDVI_plus1SE_by_depth_cl[1,], rev(NDVI_minus1SE_by_depth_cl[1,]))*NDVI_scale_factor,  border=NA, lwd=0.3, col='gray95')
polygon(x=c(seq_along(col_indices), rev(seq_along(col_indices))), y=c(NDVI_plus1SE_by_depth_cl[2,], rev(NDVI_minus1SE_by_depth_cl[2,]))*NDVI_scale_factor, border=NA, lwd=0.3, col='gray90')
polygon(x=c(seq_along(col_indices), rev(seq_along(col_indices))), y=c(NDVI_plus1SE_by_depth_cl[3,], rev(NDVI_minus1SE_by_depth_cl[3,]))*NDVI_scale_factor, border = NA, lwd=0.3, col='gray85')
for (i in 1:3) {
  if(i==4) {next} else {
    lines(seq_along(col_indices), NDVI_means_by_depth_cl[i,]*NDVI_scale_factor, col=gray_colors[i], lty=i+1)
    points(seq_along(col_indices), NDVI_means_by_depth_cl[i,]*NDVI_scale_factor, col=gray_colors[i], pch=i+16, cex=0.5)
  }
}
if(length(which(p_values < 0.05))==0) {print('No significant contrasts')} else {points(seq_along(col_indices)[which(p_values < 0.05)], (NDVI_means_by_depth_cl[3,]*NDVI_scale_factor)[which(p_values < 0.05)]-0.01, col=gray_colors[i], pch=8, cex=0.5)}
legend('bottomleft', legend=c('shallow regolith (<3.3m) \u00B1 1 S.E.', 'moderate regolith (3.3-7.5m) \u00B1 1 S.E.', 'deep regolith (>7.5m) \u00B1 1 S.E.'), lty = c(2:4, NA), pch=c(17:19,8), col = c(gray_colors, 'black'), pt.cex=0.6, inset = 0.01) #'significant contrast (p<0.05)'
# text(as.Date('2014-06-01'), 0.61, '2012-2015: historic 4-year drought and forest die-off')
# abline(v=as.Date('2015-10-01'), lty=2, lwd=0.7)
# text(x=as.Date('2016-01-15'), y=0.7, 'average')
# text(x=as.Date('2016-01-15'), y=0.685, 'winter')
# abline(v=as.Date('2016-05-01'), lty=2, lwd=0.7)
# abline(v=as.Date('2016-10-01'), lty=2, lwd=0.7)
# text(x=as.Date('2017-01-15'), y=0.7, 'historic')
# text(x=as.Date('2017-01-15'), y=0.685, 'wet winter')
# abline(v=as.Date('2017-05-01'), lty=2, lwd=0.7)
# text(x=as.Date('2017-02-25'), y=0.55, labels='official end of drought')
dev.off()
