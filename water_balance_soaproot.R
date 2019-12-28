#directories refer to lab desktop
library(prism)
downloadDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/prism_annual'
options(prism.path = downloadDir)
get_prism_annual(type = 'ppt', years = 2010:2016, keepZip = FALSE)
dataDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO'
saprock_awc <- 0.12
soil_awc <- 0.2
NDVI_scale_factor <- 0.0001
NDVI_to_ET <- function(y) {
  sapply(y, function(x) {
    117.16 * exp(2.8025*x)
  })
}
library(raster)
soaproot_pts <- read.csv(file.path(dataDir, 'sampling pts', 'Soaproot points RF.csv'), stringsAsFactors = FALSE)
soaproot_AWC <- read.csv(file.path(dataDir, 'sampling pts', 'site_awc_estimates.csv'), stringsAsFactors = FALSE)
#CRS("+init=epsg:4326") #this is geographic coordinates using WGS84 datum
#CRS("+init=epsg:4269") #this is geographic coordinates using NAD83 datum
soaproot_pts_WGS84 <- SpatialPointsDataFrame(coords=soaproot_pts[,c('POINT_X', 'POINT_Y')], data = soaproot_pts['Name'], proj4string = CRS('+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
names(soaproot_pts_WGS84)

list.files(file.path(dataDir, 'spatial data', 'NDMI'))
NDMI <- stack(list.files(file.path(dataDir, 'spatial data', 'NDMI'), full.names = TRUE))
NDVI <- stack(list.files(file.path(dataDir, 'spatial data', 'NDVI'), full.names = TRUE))

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
soaproot_remote_data_WGS84$Depth_test <- soaproot_AWC$Regolith[match(soaproot_remote_data_WGS84$Site, soaproot_AWC$Site)]
cbind(soaproot_remote_data_WGS84$Site, soaproot_remote_data_WGS84$Depth - soaproot_remote_data_WGS84$Depth_test ) #SR.A.66 is off
soaproot_remote_data_WGS84 <- soaproot_remote_data_WGS84[!is.na(soaproot_remote_data_WGS84$Depth),]
soaproot_remote_data_WGS84$Depth_test <- NULL
soaproot_remote_data_WGS84$Saprock_depth <- soaproot_AWC$Saprock.fraction[match(soaproot_remote_data_WGS84$Site, soaproot_AWC$Site)]
soaproot_remote_data_WGS84$Depth_class <- ifelse(soaproot_remote_data_WGS84$Depth < 3.3, 1, ifelse(soaproot_remote_data_WGS84$Depth < 7.56, 2, 3)) #1=shallow; 2=moderate; 3=deep
table(soaproot_remote_data_WGS84$Depth_class)
lapply(soaproot_remote_data_WGS84[,2:17], function(x) {
  summary(aov(x ~ as.factor(soaproot_remote_data_WGS84$Depth_class)))
})

soaproot_remote_data_WGS84$AWS_mm <- (soaproot_remote_data_WGS84$Depth - soaproot_remote_data_WGS84$Saprock_depth) * soil_awc * 1000 + soaproot_remote_data_WGS84$Saprock_depth * saprock_awc * 1000 #Ryan assumed that all sites had 2 m soil above saprock
summary(soaproot_remote_data_WGS84$AWS_mm)
head(soaproot_remote_data_WGS84[,c('Site', 'AWS_mm')])
soaproot_remote_data_WGS84[soaproot_remote_data_WGS84$Depth < 2, c('Site', 'AWS_mm')]
soaproot_remote_data_WGS84$latitude_WGS84 <- coordinates(soaproot_pts_WGS84)[match(soaproot_remote_data_WGS84$Site, soaproot_pts_WGS84$Name),2]
soaproot_remote_data_WGS84$longitutde_WGS84 <- coordinates(soaproot_pts_WGS84)[match(soaproot_remote_data_WGS84$Site, soaproot_pts_WGS84$Name),1]

soaproot_center <- SpatialPoints(coords=data.frame(longitutde_WGS84=mean(soaproot_remote_data_WGS84$longitutde_WGS84), latitude_WGS84=mean(soaproot_remote_data_WGS84$latitude_WGS84)), proj4string=CRS('+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
plot(soaproot_pts_WGS84[match(soaproot_remote_data_WGS84$Site, soaproot_pts_WGS84$Name), ], col='black')
plot(soaproot_center, col='red', pch=8, add=TRUE)
soaproot_center_NAD83 <- spTransform(soaproot_center, CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))

list.files(downloadDir)
datesequence <- 2010:2017
prism_data <- data.frame(year=datesequence, annual_precip=NA, stringsAsFactors = FALSE)
for (i in 1:length(datesequence)) {
  yr <- datesequence[i]
  PRISM <- raster(file.path(downloadDir, paste0('PRISM_ppt_stable_4kmM3_', yr, '_bil'), paste0('PRISM_ppt_stable_4kmM3_', yr, '_bil.bil')))
  prism_data[i, 'annual_precip'] <- extract(PRISM, soaproot_center_NAD83)
  print(i)
}
prism_data

#conceptually, AWS is end-of-year storage
#for 2010, a wet-year, it is assumed that AWS is at field-capacity at the end-of-year
water_balance_soaproot <- data.frame(Site=soaproot_remote_data_WGS84$Site, AWS_2010_mm=soaproot_remote_data_WGS84$AWS_mm, ET_2011_mm=NDVI_to_ET(soaproot_remote_data_WGS84$MeanNDVIyear2011 * NDVI_scale_factor), P_2011_mm=prism_data[prism_data$year==2011, 'annual_precip'], AWS_2011_mm=NA, ET_2012_mm=NDVI_to_ET(soaproot_remote_data_WGS84$MeanNDVIyear2012 * NDVI_scale_factor), P_2012_mm=prism_data[prism_data$year==2012, 'annual_precip'], AWS_2012_mm=NA, ET_2013_mm=NDVI_to_ET(soaproot_remote_data_WGS84$MeanNDVIyear2013 * NDVI_scale_factor), P_2013_mm=prism_data[prism_data$year==2013, 'annual_precip'], AWS_2013_mm=NA, ET_2014_mm=NDVI_to_ET(soaproot_remote_data_WGS84$MeanNDVIyear2014 * NDVI_scale_factor), P_2014_mm=prism_data[prism_data$year==2014, 'annual_precip'], AWS_2014_mm=NA, ET_2015_mm=NDVI_to_ET(soaproot_remote_data_WGS84$MeanNDVIyear2015 * NDVI_scale_factor), P_2015_mm=prism_data[prism_data$year==2015, 'annual_precip'], AWS_2015_mm=NA, ET_2016_mm=NDVI_to_ET(soaproot_remote_data_WGS84$MeanNDVIyear2016 * NDVI_scale_factor), P_2016_mm=prism_data[prism_data$year==2016, 'annual_precip'], AWS_2016_mm=NA, ET_2017_mm=NDVI_to_ET(soaproot_remote_data_WGS84$MeanNDVIyear2017 * NDVI_scale_factor), P_2017_mm=prism_data[prism_data$year==2017, 'annual_precip'], AWS_2017_mm=NA, stringsAsFactors = FALSE)

water_balance_soaproot$AWS_2011_mm <- ifelse(water_balance_soaproot$AWS_2010_mm - water_balance_soaproot$ET_2011_mm + water_balance_soaproot$P_2011_mm >= water_balance_soaproot$AWS_2010_mm, water_balance_soaproot$AWS_2010_mm, ifelse(water_balance_soaproot$AWS_2010_mm - water_balance_soaproot$ET_2011_mm + water_balance_soaproot$P_2011_mm > 0, water_balance_soaproot$AWS_2010_mm - water_balance_soaproot$ET_2011_mm + water_balance_soaproot$P_2011_mm, 0))

calculate_storage <- function(yr) {
  ifelse(water_balance_soaproot[[paste0('AWS_', yr-1, '_mm')]] - water_balance_soaproot[[paste0('ET_', yr, '_mm')]] + water_balance_soaproot[[paste0('P_', yr, '_mm')]] >= water_balance_soaproot$AWS_2010_mm , water_balance_soaproot$AWS_2010_mm, ifelse(water_balance_soaproot[[paste0('AWS_', yr-1, '_mm')]] - water_balance_soaproot[[paste0('ET_', yr, '_mm')]] + water_balance_soaproot[[paste0('P_', yr, '_mm')]] > 0, water_balance_soaproot[[paste0('AWS_', yr-1, '_mm')]] - water_balance_soaproot[[paste0('ET_', yr, '_mm')]] + water_balance_soaproot[[paste0('P_', yr, '_mm')]], 0))
}
test <- calculate_storage(2011)
sum(test==water_balance_soaproot$AWS_2010_mm)
sum(test < water_balance_soaproot$AWS_2010_mm)
all(water_balance_soaproot$AWS_2011_mm==test)
water_balance_soaproot$AWS_2012_mm <- calculate_storage(2012)
hist(water_balance_soaproot$AWS_2012_mm)
water_balance_soaproot$AWS_2013_mm <- calculate_storage(2013)
hist(water_balance_soaproot$AWS_2013_mm)
water_balance_soaproot$AWS_2014_mm <- calculate_storage(2014)
hist(water_balance_soaproot$AWS_2014_mm)
water_balance_soaproot$AWS_2015_mm <- calculate_storage(2015)
hist(water_balance_soaproot$AWS_2015_mm)
water_balance_soaproot$AWS_2016_mm <- calculate_storage(2016)
hist(water_balance_soaproot$AWS_2016_mm)
water_balance_soaproot$AWS_2017_mm <- calculate_storage(2017)
hist(water_balance_soaproot$AWS_2017_mm)
write.csv(water_balance_soaproot, file.path(dataDir, 'water balance', 'water_balance_calcs.csv'), row.names = FALSE)
write.csv(soaproot_remote_data_WGS84, file.path(dataDir, 'water balance', 'soaproot_remote_data.csv'), row.names = FALSE)

AWS_results <- water_balance_soaproot[ ,c('Site', colnames(water_balance_soaproot)[grepl('AWS', colnames(water_balance_soaproot))])]
lapply(AWS_results[,2:ncol(AWS_results)], summary)
lapply(AWS_results[,2:ncol(AWS_results)], function(x) {summary(100*(x / AWS_results$AWS_2010_mm))})
all(AWS_results$Site==soaproot_remote_data_WGS84$Site) #same order
write.csv(AWS_results, file.path(dataDir, 'water balance', 'soaproot_aws_mm_results.csv'), row.names = FALSE)
AWS_proportion_results <- data.frame(Site=AWS_results$Site, do.call(cbind, lapply(AWS_results[,2:ncol(AWS_results)], function(x) {100*(x / AWS_results$AWS_2010_mm)})), stringsAsFactors = FALSE)
AWS_proportion_results$Depth <- soaproot_remote_data_WGS84$Depth
AWS_proportion_results$Depth_class <- soaproot_remote_data_WGS84$Depth_class
lapply(AWS_proportion_results[,2:9], function(x) tapply(x, AWS_proportion_results$Depth_class, summary))

plot(AWS_proportion_results$AWS_2013_mm, soaproot_remote_data_WGS84$MeanNDMIyear2016*NDVI_scale_factor)
plot(AWS_proportion_results$AWS_2014_mm, soaproot_remote_data_WGS84$MeanNDMIyear2016*NDVI_scale_factor)
plot(AWS_proportion_results$AWS_2015_mm, soaproot_remote_data_WGS84$MeanNDMIyear2016*NDVI_scale_factor)
plot(AWS_proportion_results$AWS_2016_mm, soaproot_remote_data_WGS84$MeanNDMIyear2016*NDVI_scale_factor)
plot(AWS_proportion_results$AWS_2017_mm, soaproot_remote_data_WGS84$MeanNDMIyear2017*NDVI_scale_factor)
summary(lm(soaproot_remote_data_WGS84$MeanNDMIyear2016*NDVI_scale_factor ~ AWS_proportion_results$AWS_2016_mm))
summary(lm(soaproot_remote_data_WGS84$MeanNDMIyear2016 ~ soaproot_remote_data_WGS84$Depth + soaproot_remote_data_WGS84$MeanNDVIyear2014))
plot(AWS_results$AWS_2010_mm, soaproot_remote_data_WGS84$MeanNDMIyear2016*NDVI_scale_factor)

#plot NDMI (need to substitute NDVI)
NDVI_means_by_depth_cl <- do.call(cbind, lapply(NDVI_landsat8[,1:59], function(x) {tapply(x, NDVI_landsat8$Depth_class, mean)}))
NDVI_sd_by_depth_cl <- do.call(cbind, lapply(NDVI_landsat8[,1:59], function(x) {tapply(x, NDVI_landsat8$Depth_class, sd)}))
NDVI_n_by_depth_cl <- do.call(cbind, lapply(NDVI_landsat8[,1:59], function(x) {tapply(x, NDVI_landsat8$Depth_class, length)}))

#1 standard error (S.E) plot for NDMI
gray_colors <- c('gray70', 'gray55', 'gray40')
class(as.POSIXlt(NDVI_dates, tz='PDT'))
tiff(file = file.path(FiguresDir, 'NDMI_by_depth_class_1SE.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.5, 4.5, 0.5, 0.5))
plot(c(min(NDVI_dates), max(NDVI_dates)), c(min(NDVI_minus1SE_by_depth_cl[1,]), max(NDVI_plus1SE_by_depth_cl[1,])), type='n', xlab='', ylab = 'NDVI', xaxt='n')
axis.Date(side = 1, x = NDVI_dates)
polygon(x=c(NDVI_dates, rev(NDVI_dates)), y=c(NDVI_plus1SE_by_depth_cl[1,], rev(NDVI_minus1SE_by_depth_cl[1,])),  border=NA, lwd=0.3, col='gray95')
#polygon(x=c(NDVI_dates, rev(NDVI_dates)), y=c(NDVI_plus1SE_by_depth_cl[2,], rev(NDVI_minus1SE_by_depth_cl[2,])), border=gray_colors[2], lwd=0.3)
polygon(x=c(NDVI_dates, rev(NDVI_dates)), y=c(NDVI_plus1SE_by_depth_cl[3,], rev(NDVI_minus1SE_by_depth_cl[3,])), border = NA, lwd=0.3, col='gray90')
for (i in 1:3) {
  if(i==2) {next}
  lines(NDVI_dates, NDVI_means_by_depth_cl[i,], col=gray_colors[i], lty=i+1)
  points(NDVI_dates, NDVI_means_by_depth_cl[i,], col=gray_colors[i], pch=i+16, cex=0.5)
}
points(NDVI_dates[which(p_values < 0.05)], NDVI_means_by_depth_cl[3,][which(p_values < 0.05)]-0.01, col=gray_colors[i], pch=8, cex=0.5)
legend('bottomleft', legend=c('shallow regolith (<3.3m) mean NDVI \u00B1 1 S.E.', 'deep regolith (>7.5m) mean NDVI \u00B1 1 S.E.', 'significant contrast (p<0.05)'), lty = c(2,4, NA), pch=c(17,19, 8), col = c(gray_colors[c(1,3)], 'black'), pt.cex=0.6, inset = 0.01)
text(as.Date('2014-06-01'), 0.61, '2012-2015: historic 4-year drought and forest die-off')
abline(v=as.Date('2015-10-01'), lty=2, lwd=0.7)
text(x=as.Date('2016-01-15'), y=0.7, 'average')
text(x=as.Date('2016-01-15'), y=0.685, 'winter')
abline(v=as.Date('2016-05-01'), lty=2, lwd=0.7)
abline(v=as.Date('2016-10-01'), lty=2, lwd=0.7)
text(x=as.Date('2017-01-15'), y=0.7, 'historic')
text(x=as.Date('2017-01-15'), y=0.685, 'wet winter')
abline(v=as.Date('2017-05-01'), lty=2, lwd=0.7)
#text(x=as.Date('2017-02-25'), y=0.55, labels='official end of drought')
dev.off()