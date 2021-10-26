#directories refer to lab desktop
library(corrplot)
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
saprock_awc <- 0.074
soil_awc <- 0.135
NDVI_scale_factor <- 0.0001
res_plots <- 800
NDVI_to_ET <- function(y) {
  sapply(y, function(x) {
    117.16 * exp(2.8025*x)
  })
}
library(raster)
soaproot_pts <- read.csv(file.path(dataDir, 'sampling pts', 'Soaproot points RF.csv'), stringsAsFactors = FALSE)
soaproot_AWC <- read.csv(file.path(dataDir, 'sampling pts', 'site_awc_estimates.csv'), stringsAsFactors = FALSE)
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
# soaproot_remote_data_WGS84$Depth_test <- soaproot_AWC$Regolith[match(soaproot_remote_data_WGS84$Site, soaproot_AWC$Site)]
# cbind(soaproot_remote_data_WGS84$Site, soaproot_remote_data_WGS84$Depth - soaproot_remote_data_WGS84$Depth_test ) #SR.A.66 is off
soaproot_remote_data_WGS84 <- soaproot_remote_data_WGS84[!is.na(soaproot_remote_data_WGS84$Depth),]
# soaproot_remote_data_WGS84$Depth_test <- NULL
soaproot_remote_data_WGS84$Saprock_depth <- soaproot_AWC$Saprock.fraction[match(soaproot_remote_data_WGS84$Site, soaproot_AWC$Site)]
soaproot_remote_data_WGS84$Depth_class <- ifelse(soaproot_remote_data_WGS84$Depth < 3.3, 1, ifelse(soaproot_remote_data_WGS84$Depth < 7.56, 2, 3)) #1=shallow; 2=moderate; 3=deep
# table(soaproot_remote_data_WGS84$Depth_class)
lapply(soaproot_remote_data_WGS84[,2:19], function(x) {
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
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) {
  summary(aov(x ~ as.factor(soaproot_remote_data_WGS84$Depth_class)))
})


#add mean NDVI
soaproot_remote_data_WGS84$MeanNDVIyear2009_2011 <- apply(soaproot_remote_data_WGS84[,c('MeanNDVIyear2009', 'MeanNDVIyear2010', 'MeanNDVIyear2011')], 1, mean)

lapply(soaproot_remote_data_WGS84[,which(grepl('delta', colnames(soaproot_remote_data_WGS84)))], hist)
plot(soaproot_remote_data_WGS84$Depth, soaproot_remote_data_WGS84$deltaNDMI_2014)
plot(soaproot_remote_data_WGS84$Depth, soaproot_remote_data_WGS84$deltaNDMI_2015)
plot(soaproot_remote_data_WGS84$Depth, soaproot_remote_data_WGS84$deltaNDMI_2016)
plot(soaproot_remote_data_WGS84$Depth, soaproot_remote_data_WGS84$deltaNDMI_2017)

#get some correlation plots
#amongst annual NDVI and NDMI
colnames(soaproot_remote_data_WGS84) #2-19 are for NDMI and NDVI
cor_result <- cor(soaproot_remote_data_WGS84[,2:19], method = 'pearson')
mag.factor <- 2
cex.before <- par("cex") #saves current cex setting for plotting
par(cex = 0.7)  #set cex for plotting text.  this invisibly affects p-value text
corrplot(cor_result, type = 'lower', diag = TRUE, order = 'AOE', hclust.method = 'ward.D2', tl.col = 'black', tl.srt = 30, cl.ratio = 0.2, cl.align.text = 'c', p.mat = cor.mtest(soaproot_remote_data_WGS84[,2:19], method = 'pearson')[[1]], sig.level = 0.01, insig='p-value', tl.cex = par("cex") * mag.factor, cl.cex = par("cex") * mag.factor)
par(cex = cex.before)

#amongst annual NDVI and delta NDMI
colnames(soaproot_remote_data_WGS84) #11-19 are for NDVI and 26-31 are for delta NDMI
cor_result <- cor(soaproot_remote_data_WGS84[ ,c(11:19,26:31)], method = 'pearson')
mag.factor <- 2
cex.before <- par("cex") #saves current cex setting for plotting
par(cex = 0.7)  #set cex for plotting text.  this invisibly affects p-value text
corrplot(cor_result, type = 'lower', diag = TRUE, order = 'original', tl.col = 'black', tl.srt = 30, cl.ratio = 0.2, cl.align.text = 'c', p.mat = cor.mtest(soaproot_remote_data_WGS84[,c(11:19,26:31)], method = 'pearson')[[1]], sig.level = 0.01, insig='p-value', tl.cex = par("cex") * mag.factor, cl.cex = par("cex") * mag.factor) #hclust.method = 'ward.D2',
par(cex = cex.before)

#read-in PRISM data
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

calculate_storage <- function(yr) {
  ifelse(water_balance_soaproot[[paste0('AWS_', yr-1, '_mm')]] - water_balance_soaproot[[paste0('ET_', yr, '_mm')]] + water_balance_soaproot[[paste0('P_', yr, '_mm')]] >= water_balance_soaproot$AWS_2010_mm , water_balance_soaproot$AWS_2010_mm, ifelse(water_balance_soaproot[[paste0('AWS_', yr-1, '_mm')]] - water_balance_soaproot[[paste0('ET_', yr, '_mm')]] + water_balance_soaproot[[paste0('P_', yr, '_mm')]] > 0, water_balance_soaproot[[paste0('AWS_', yr-1, '_mm')]] - water_balance_soaproot[[paste0('ET_', yr, '_mm')]] + water_balance_soaproot[[paste0('P_', yr, '_mm')]], 0))
}


# test <- calculate_storage(2011)
# sum(test==water_balance_soaproot$AWS_2010_mm)
# sum(test < water_balance_soaproot$AWS_2010_mm)
# all(water_balance_soaproot$AWS_2011_mm==test)
water_balance_soaproot$AWS_2011_mm <- calculate_storage(2011)
mean(water_balance_soaproot$AWS_2011_mm)
hist(water_balance_soaproot$AWS_2011_mm)
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
# write.csv(water_balance_soaproot, file.path(dataDir, 'water balance', 'water_balance_calcs.csv'), row.names = FALSE)
# write.csv(soaproot_remote_data_WGS84, file.path(dataDir, 'water balance', 'soaproot_remote_data.csv'), row.names = FALSE)

AWS_results <- water_balance_soaproot[ ,c('Site', colnames(water_balance_soaproot)[grepl('AWS', colnames(water_balance_soaproot))])]
lapply(AWS_results[,2:ncol(AWS_results)], summary)
lapply(AWS_results[,2:ncol(AWS_results)], function(x) {summary(100*(x / AWS_results$AWS_2010_mm))})
all(AWS_results$Site==soaproot_remote_data_WGS84$Site) #same order
# write.csv(AWS_results, file.path(dataDir, 'water balance', 'soaproot_aws_mm_results.csv'), row.names = FALSE)
AWS_proportion_results <- data.frame(Site=AWS_results$Site, do.call(cbind, lapply(AWS_results[,2:ncol(AWS_results)], function(x) {100*(x / AWS_results$AWS_2010_mm)})), stringsAsFactors = FALSE)
AWS_proportion_results$Depth <- soaproot_remote_data_WGS84$Depth
AWS_proportion_results$Depth_class <- soaproot_remote_data_WGS84$Depth_class
lapply(AWS_proportion_results[,2:9], function(x) tapply(x, AWS_proportion_results$Depth_class, summary))

plot(AWS_proportion_results$AWS_2012_mm, soaproot_remote_data_WGS84$MeanNDMIyear2013*NDVI_scale_factor)
plot(AWS_proportion_results$AWS_2013_mm, soaproot_remote_data_WGS84$MeanNDMIyear2013*NDVI_scale_factor)
plot(AWS_proportion_results$AWS_2014_mm, soaproot_remote_data_WGS84$MeanNDMIyear2014*NDVI_scale_factor)
plot(AWS_proportion_results$AWS_2015_mm, soaproot_remote_data_WGS84$MeanNDMIyear2015*NDVI_scale_factor)
plot(AWS_proportion_results$AWS_2016_mm, soaproot_remote_data_WGS84$MeanNDMIyear2016*NDVI_scale_factor)
plot(AWS_proportion_results$AWS_2017_mm, soaproot_remote_data_WGS84$MeanNDMIyear2017*NDVI_scale_factor)


#make some comparisions with terrain data
all(soaproot_remote_data_WGS84$Site==soaproot_terrain_data$Site)
summary(lm(soaproot_remote_data_WGS84$MeanNDMIyear2016 ~ soaproot_remote_data_WGS84$Depth + soaproot_remote_data_WGS84$MeanNDVIyear2012 + soaproot_terrain_data$stream_dist_N_150))
summary(lm(soaproot_remote_data_WGS84$MeanNDMIyear2016 ~ soaproot_terrain_data$stream_dist_N_150))
summary(lm(soaproot_remote_data_WGS84$MeanNDMIyear2015 ~ soaproot_terrain_data$stream_dist_N_150 + AWS_proportion_results$AWS_2015_mm))
summary(lm(soaproot_remote_data_WGS84$MeanNDMIyear2014 ~ soaproot_terrain_data$stream_dist_N_150 + AWS_proportion_results$AWS_2014_mm))

#corrplot of delta NDMI
#get some correlation plots
colnames(soaproot_remote_data_WGS84) #26-31 are for delta NDMI only
cor_result <- cor(cbind(soaproot_remote_data_WGS84[,26:31], AWS_proportion_results[,3:8]), method = 'pearson')
mag.factor <- 2
cex.before <- par("cex") #saves current cex setting for plotting
par(cex = 0.7)  #set cex for plotting text.  this invisibly affects p-value text
corrplot(cor_result, type = 'lower', diag = TRUE, order = 'original', tl.col = 'black', tl.srt = 30, cl.ratio = 0.2, cl.align.text = 'c', p.mat = cor.mtest(cbind(soaproot_remote_data_WGS84[,26:31], AWS_proportion_results[,3:8]), method = 'pearson')[[1]], sig.level = 0.01, insig='p-value', tl.cex = par("cex") * mag.factor, cl.cex = par("cex") * mag.factor) #when order is AOE hclust.method = 'ward.D2'
par(cex = cex.before)

#corrplot of AWS_results and delta NDMI
#get some correlation plots
colnames(soaproot_remote_data_WGS84) #26-31 are for delta NDMI only
colnames(AWS_proportion_results) #3-8 are 2011-2016 results
cor_result <- cor(cbind(soaproot_remote_data_WGS84[,26:31], AWS_proportion_results[,3:8]), method = 'pearson')
mag.factor <- 2
cex.before <- par("cex") #saves current cex setting for plotting
par(cex = 0.7)  #set cex for plotting text.  this invisibly affects p-value text
corrplot(cor_result, type = 'lower', diag = TRUE, order = 'original', tl.col = 'black', tl.srt = 30, cl.ratio = 0.2, cl.align.text = 'c', p.mat = cor.mtest(cbind(soaproot_remote_data_WGS84[,26:31], AWS_proportion_results[,3:8]), method = 'pearson')[[1]], sig.level = 0.01, insig='p-value', tl.cex = par("cex") * mag.factor, cl.cex = par("cex") * mag.factor) #when order is AOE hclust.method = 'ward.D2'
par(cex = cex.before)

plot(apply(soaproot_remote_data_WGS84[,2:10], 2, mean)*0.0001, type='b', ylab='mean NDMI, regolith sampling points', xaxt='n', xlab='Year')
axis(side=1, at=1:9, labels=2009:2017)

plot(apply(soaproot_remote_data_WGS84[,11:19], 2, mean)*0.0001, type='b', ylab='mean NDVI, regolith sampling points', xaxt='n', xlab='Year')
axis(side=1, at=1:9, labels=2009:2017)


lapply(AWS_results[,2:ncol(AWS_results)], function(x) sum(x==0)) #21, 31, and 36 from 2013-2015
lapply(AWS_proportion_results[,2:9], function(x) sum(x<50))
lapply(AWS_proportion_results[,2:9], function(x) sum(x<25))
lapply(AWS_proportion_results[,2:9], function(x) sum(x<20)) #27, 38, and 48
soaproot_remote_data_WGS84$time_to_depletion <- ifelse(water_balance_soaproot$AWS_2013_mm==0, 1, ifelse(water_balance_soaproot$AWS_2014_mm==0, 2, ifelse(water_balance_soaproot$AWS_2015_mm==0, 3, 4)))
soaproot_remote_data_WGS84$time_to_0.2depletion <- ifelse(AWS_proportion_results$AWS_2013_mm<20, 1, ifelse(AWS_proportion_results$AWS_2014_mm<20, 2, ifelse(AWS_proportion_results$AWS_2015_mm<20, 3, 4)))
table(soaproot_remote_data_WGS84$time_to_depletion)
table(soaproot_remote_data_WGS84$time_to_0.2depletion)
tapply(soaproot_remote_data_WGS84$deltaNDMI_2016, soaproot_remote_data_WGS84$time_to_depletion, summary)
summary(aov(soaproot_remote_data_WGS84$deltaNDMI_2016 ~ soaproot_remote_data_WGS84$time_to_depletion))
tapply(soaproot_remote_data_WGS84$deltaNDMI_2017, soaproot_remote_data_WGS84$time_to_depletion, summary)
summary(aov(soaproot_remote_data_WGS84$deltaNDMI_2017 ~ soaproot_remote_data_WGS84$time_to_depletion))
tapply(soaproot_remote_data_WGS84$deltaNDMI_2016, soaproot_remote_data_WGS84$time_to_0.2depletion, summary)
summary(aov(soaproot_remote_data_WGS84$deltaNDMI_2016 ~ as.factor(soaproot_remote_data_WGS84$time_to_0.2depletion)))
tapply(soaproot_remote_data_WGS84$deltaNDMI_2017, soaproot_remote_data_WGS84$time_to_0.2depletion, summary)
summary(aov(soaproot_remote_data_WGS84$deltaNDMI_2017 ~ soaproot_remote_data_WGS84$time_to_0.2depletion))


lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_terrain_data$stream_dist_N_150)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_terrain_data$elev_N)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) plot(soaproot_terrain_data$elev_N, x))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_terrain_data$curv_mean_N)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_terrain_data$elev_N + soaproot_terrain_data$stream_dist_N_150)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_terrain_data$elev_N + soaproot_terrain_data$stream_dist_N_150+soaproot_remote_data_WGS84$MeanNDVIyear2009_2011))) #R^2=0.46 for 2016 & 2017
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_terrain_data$stream_dist_N_150+soaproot_remote_data_WGS84$MeanNDVIyear2009_2011))) #R2=0.43 for 2016 & 2017
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ AWS_proportion_results$AWS_2013_mm + soaproot_terrain_data$stream_dist_N_150+soaproot_remote_data_WGS84$MeanNDVIyear2009_2011))) #AWS non-sig
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ AWS_proportion_results$AWS_2014_mm + soaproot_terrain_data$stream_dist_N_150+soaproot_remote_data_WGS84$MeanNDVIyear2009_2011))) #AWS non-sig
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_terrain_data$elev_N + soaproot_terrain_data$stream_dist_N_150 + soaproot_terrain_data$curv_mean_N)))
summary(lm(soaproot_remote_data_WGS84$MeanNDVIyear2009_2011 ~ soaproot_terrain_data$elev_N + soaproot_terrain_data$stream_dist_N_150))
summary(lm(soaproot_remote_data_WGS84$MeanNDVIyear2009_2011 ~ soaproot_remote_data_WGS84$Depth))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_terrain_data$elev_N + soaproot_terrain_data$stream_dist_N_150 + AWS_proportion_results$AWS_2013_mm)))

lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_terrain_data$elev_N + soaproot_terrain_data$stream_dist_N_150 + AWS_results$AWS_2013_mm)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_terrain_data$elev_N + soaproot_terrain_data$stream_dist_N_150 + AWS_proportion_results$AWS_2014_mm)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_terrain_data$elev_N + soaproot_terrain_data$stream_dist_N_150 + AWS_results$AWS_2014_mm)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_terrain_data$elev_N + soaproot_terrain_data$stream_dist_N_150 + AWS_proportion_results$AWS_2015_mm)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_terrain_data$elev_N + soaproot_terrain_data$stream_dist_N_150 + AWS_results$AWS_2015_mm)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ as.factor(soaproot_remote_data_WGS84$Depth_class))))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_terrain_data$stream_dist_N_150 + as.factor(soaproot_remote_data_WGS84$Depth_class))))


#corrplot of AWS_results and NDMI
#get some correlation plots
colnames(soaproot_remote_data_WGS84) #2-9 are for NDMI only
colnames(AWS_proportion_results) #3-8 are 2011-2016 results
cor_result <- cor(cbind(soaproot_remote_data_WGS84[,2:9], AWS_proportion_results[,3:8]), method = 'pearson')
mag.factor <- 2
cex.before <- par("cex") #saves current cex setting for plotting
par(cex = 0.7)  #set cex for plotting text.  this invisibly affects p-value text
corrplot(cor_result, type = 'lower', diag = TRUE, order = 'original', tl.col = 'black', tl.srt = 30, cl.ratio = 0.2, cl.align.text = 'c', p.mat = cor.mtest(cbind(soaproot_remote_data_WGS84[,2:9], AWS_proportion_results[,3:8]), method = 'pearson')[[1]], sig.level = 0.01, insig='p-value', tl.cex = par("cex") * mag.factor, cl.cex = par("cex") * mag.factor) #when order is AOE hclust.method = 'ward.D2'
par(cex = cex.before)
plot(soaproot_remote_data_WGS84$MeanNDVIyear2015, AWS_proportion_results$AWS_2015_mm, col=ifelse(AWS_proportion_results$Depth_class==3, 'red', 'black'))
plot(soaproot_remote_data_WGS84$MeanNDMIyear2015, AWS_proportion_results$AWS_2015_mm, col=ifelse(AWS_proportion_results$Depth_class==3, 'red', 'black'))
plot(AWS_proportion_results$Depth, soaproot_remote_data_WGS84$MeanNDMIyear2010)
plot(AWS_proportion_results$Depth, soaproot_remote_data_WGS84$MeanNDMIyear2016)

#corrplot of AWS_results_proportion and NDMI
colnames(soaproot_remote_data_WGS84) #2-9 are for NDMI only
colnames(AWS_proportion_results) #3-8 are 2011-2016 results
cor_result <- cor(cbind(soaproot_remote_data_WGS84[,2:9], AWS_proportion_results[,3:8]), method = 'pearson')
mag.factor <- 2
cex.before <- par("cex") #saves current cex setting for plotting
par(cex = 0.7)  #set cex for plotting text.  this invisibly affects p-value text
corrplot(cor_result, type = 'lower', diag = TRUE, order = 'original', tl.col = 'black', tl.srt = 30, cl.ratio = 0.2, cl.align.text = 'c', p.mat = cor.mtest(cbind(soaproot_remote_data_WGS84[,2:9], AWS_proportion_results[,3:8]), method = 'pearson')[[1]], sig.level = 0.01, insig='p-value', tl.cex = par("cex") * mag.factor, cl.cex = par("cex") * mag.factor) #when order is AOE hclust.method = 'ward.D2'
par(cex = cex.before)

#corrplot of AWS_results_proportion and NDVI
colnames(soaproot_remote_data_WGS84) #10-17 are for NDVI only
colnames(AWS_proportion_results) #3-8 are 2011-2016 results
cor_result <- cor(cbind(soaproot_remote_data_WGS84[,10:17], AWS_proportion_results[,3:8]), method = 'pearson')
mag.factor <- 2
cex.before <- par("cex") #saves current cex setting for plotting
par(cex = 0.7)  #set cex for plotting text.  this invisibly affects p-value text
corrplot(cor_result, type = 'lower', diag = TRUE, order = 'original', tl.col = 'black', tl.srt = 30, cl.ratio = 0.2, cl.align.text = 'c', p.mat = cor.mtest(cbind(soaproot_remote_data_WGS84[,10:17], AWS_proportion_results[,3:8]), method = 'pearson')[[1]], sig.level = 0.01, insig='p-value', tl.cex = par("cex") * mag.factor, cl.cex = par("cex") * mag.factor) #when order is AOE hclust.method = 'ward.D2'
par(cex = cex.before)

#corrplot of AWS_results (absolute) and NDMI
colnames(soaproot_remote_data_WGS84) #2-9 are for NDMI only
colnames(AWS_results) #3-8 are 2011-2016 results
cor_result <- cor(cbind(soaproot_remote_data_WGS84[,2:9], AWS_results[,3:8]), method = 'pearson')
mag.factor <- 2
cex.before <- par("cex") #saves current cex setting for plotting
par(cex = 0.7)  #set cex for plotting text.  this invisibly affects p-value text
corrplot(cor_result, type = 'lower', diag = TRUE, order = 'original', tl.col = 'black', tl.srt = 30, cl.ratio = 0.2, cl.align.text = 'c', p.mat = cor.mtest(cbind(soaproot_remote_data_WGS84[,2:9], AWS_results[,3:8]), method = 'pearson')[[1]], sig.level = 0.01, insig='p-value', tl.cex = par("cex") * mag.factor, cl.cex = par("cex") * mag.factor) #when order is AOE hclust.method = 'ward.D2'
par(cex = cex.before)


#calculate storage as a deficit
calculate_surdef <- function(yr) {
  ifelse(surdef_soaproot[[paste0('AWS_', yr-1, '_mm')]] - surdef_soaproot[[paste0('ET_', yr, '_mm')]] + surdef_soaproot[[paste0('P_', yr, '_mm')]] >= surdef_soaproot$AWS_2010_mm, surdef_soaproot$AWS_2010_mm, surdef_soaproot[[paste0('AWS_', yr-1, '_mm')]] - surdef_soaproot[[paste0('ET_', yr, '_mm')]] + surdef_soaproot[[paste0('P_', yr, '_mm')]])
} #allows for accumulation of 'deficit' as negative surplus

#conceptually, AWS is end-of-year storage or deficit in this case
#for 2010, a wet-year, it is assumed that AWS is at field-capacity at the end-of-year
surdef_soaproot <- data.frame(Site=soaproot_remote_data_WGS84$Site, AWS_2010_mm=soaproot_remote_data_WGS84$AWS_mm, ET_2011_mm=NDVI_to_ET(soaproot_remote_data_WGS84$MeanNDVIyear2011 * NDVI_scale_factor), P_2011_mm=prism_data[prism_data$year==2011, 'annual_precip'], AWS_2011_mm=NA, ET_2012_mm=NDVI_to_ET(soaproot_remote_data_WGS84$MeanNDVIyear2012 * NDVI_scale_factor), P_2012_mm=prism_data[prism_data$year==2012, 'annual_precip'], AWS_2012_mm=NA, ET_2013_mm=NDVI_to_ET(soaproot_remote_data_WGS84$MeanNDVIyear2013 * NDVI_scale_factor), P_2013_mm=prism_data[prism_data$year==2013, 'annual_precip'], AWS_2013_mm=NA, ET_2014_mm=NDVI_to_ET(soaproot_remote_data_WGS84$MeanNDVIyear2014 * NDVI_scale_factor), P_2014_mm=prism_data[prism_data$year==2014, 'annual_precip'], AWS_2014_mm=NA, ET_2015_mm=NDVI_to_ET(soaproot_remote_data_WGS84$MeanNDVIyear2015 * NDVI_scale_factor), P_2015_mm=prism_data[prism_data$year==2015, 'annual_precip'], AWS_2015_mm=NA, ET_2016_mm=NDVI_to_ET(soaproot_remote_data_WGS84$MeanNDVIyear2016 * NDVI_scale_factor), P_2016_mm=prism_data[prism_data$year==2016, 'annual_precip'], AWS_2016_mm=NA, ET_2017_mm=NDVI_to_ET(soaproot_remote_data_WGS84$MeanNDVIyear2017 * NDVI_scale_factor), P_2017_mm=prism_data[prism_data$year==2017, 'annual_precip'], AWS_2017_mm=NA, stringsAsFactors = FALSE)

surdef_soaproot$AWS_2011_mm <- calculate_surdef(2011)
mean(surdef_soaproot$AWS_2011_mm)
hist(surdef_soaproot$AWS_2011_mm)
surdef_soaproot$AWS_2012_mm <- calculate_surdef(2012)
hist(surdef_soaproot$AWS_2012_mm)
surdef_soaproot$AWS_2013_mm <- calculate_surdef(2013)
hist(surdef_soaproot$AWS_2013_mm)
surdef_soaproot$AWS_2014_mm <- calculate_surdef(2014)
hist(surdef_soaproot$AWS_2014_mm)
surdef_soaproot$AWS_2015_mm <- calculate_surdef(2015)
hist(surdef_soaproot$AWS_2015_mm)
surdef_soaproot$AWS_2016_mm <- calculate_surdef(2016)
hist(surdef_soaproot$AWS_2016_mm)
surdef_soaproot$AWS_2017_mm <- calculate_surdef(2017)
hist(surdef_soaproot$AWS_2017_mm)

surdef_results <- surdef_soaproot[ ,c('Site', colnames(surdef_soaproot)[grepl('AWS', colnames(surdef_soaproot))])]
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ surdef_results$AWS_2013_mm))) #AWS non-sig
plot(surdef_results$AWS_2013_mm, soaproot_remote_data_WGS84$deltaNDMI_2014)
plot(surdef_results$AWS_2013_mm, soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ surdef_results$AWS_2014_mm))) #AWS sig in 2014
plot(surdef_results$AWS_2014_mm, soaproot_remote_data_WGS84$deltaNDMI_2014)
plot(surdef_results$AWS_2014_mm, soaproot_remote_data_WGS84$deltaNDMI_2016)
plot(surdef_results$AWS_2014_mm, soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)

lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ surdef_results$AWS_2015_mm))) #AWS sig in 2014 & 2015
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)))

lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ surdef_results$AWS_2013_mm + soaproot_terrain_data$stream_dist_N_150+soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ surdef_results$AWS_2014_mm + soaproot_terrain_data$stream_dist_N_150+soaproot_remote_data_WGS84$MeanNDVIyear2009_2011))) #AWS non-sig
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ surdef_results$AWS_2015_mm + soaproot_terrain_data$stream_dist_N_150+soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ surdef_results$AWS_2013_mm +soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ surdef_results$AWS_2014_mm +soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ surdef_results$AWS_2015_mm +soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ surdef_results$AWS_2014_mm +soaproot_remote_data_WGS84$MeanNDVIyear2009_2011+soaproot_terrain_data$elev_N)))

lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ surdef_results$AWS_2015_mm +soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ surdef_results$AWS_2015_mm +soaproot_remote_data_WGS84$MeanNDVIyear2009_2011+soaproot_terrain_data$elev_N)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ surdef_results$AWS_2014_mm*soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_remote_data_WGS84$AWS_mm*soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_remote_data_WGS84$AWS_mm+soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_remote_data_WGS84$Depth*soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)))
lapply(soaproot_remote_data_WGS84[,grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84))], function(x) summary(lm(x ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011 + soaproot_terrain_data$elev_N + soaproot_terrain_data$solrad_N + soaproot_terrain_data$slope_N + soaproot_terrain_data$curv_mean_N + surdef_results$AWS_2015_mm + surdef_results$AWS_2014_mm + surdef_results$AWS_2013_mm)))

#linear models by year
#2015
scaled_deltaNDMI2015 <- soaproot_remote_data_WGS84$deltaNDMI_2015*NDVI_scale_factor
scaled_meanNDVI2009_2011 <- soaproot_remote_data_WGS84$MeanNDVIyear2009_2011 * NDVI_scale_factor
summary(lm(scaled_deltaNDMI2015 ~ scaled_meanNDVI2009_2011 + surdef_results$AWS_2015_mm)) #R2=0.54
summary(lm(soaproot_remote_data_WGS84$deltaNDMI_2015 ~ surdef_results$AWS_2015_mm)) #R2=0.05
summary(lm(soaproot_remote_data_WGS84$deltaNDMI_2015 ~ soaproot_terrain_data$stream_dist_N_150)) #non-sig
summary(lm(soaproot_remote_data_WGS84$deltaNDMI_2015 ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)) #R2=0.28
summary(lm(surdef_results$AWS_2015_mm ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)) #R2=0.19
plot(soaproot_remote_data_WGS84$MeanNDVIyear2009_2011, surdef_results$AWS_2015_mm)
vif(lm(soaproot_remote_data_WGS84$deltaNDMI_2015 ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011 + surdef_results$AWS_2015_mm))
summary(lm(surdef_results$AWS_2015_mm ~ soaproot_terrain_data$stream_dist_N_150)) #R2=0.27
summary(lm(soaproot_remote_data_WGS84$deltaNDMI_2015 ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011 + soaproot_terrain_data$stream_dist_N_150)) #R2=0.36
summary(lm(soaproot_remote_data_WGS84$deltaNDMI_2015 ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011 + soaproot_terrain_data$stream_dist_N_150 + surdef_results$AWS_2015_mm)) #R2=0.54 (distance from stream non-sig)


tiff(file = file.path(FiguresDir, 'deltaNDMI2015_vs_surdef2015.tif'), family = 'Times New Roman', pointsize = 11, width = 3.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(surdef_results$AWS_2015_mm, soaproot_remote_data_WGS84$deltaNDMI_2015*NDVI_scale_factor, ylab='Delta NDMI (2015)', xlab='Plant available water -or- deficit, 2015 (mm) ')
abline(lm(scaled_delta2015 ~ surdef_results$AWS_2015_mm), lty=2, col='grey', lwd=1.2)
dev.off()


tiff(file = file.path(FiguresDir, 'deltaNDMI2015_vs_NDVI_2009_2011.tif'), family = 'Times New Roman', pointsize = 11, width = 3.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(soaproot_remote_data_WGS84$MeanNDVIyear2009_2011*NDVI_scale_factor, soaproot_remote_data_WGS84$deltaNDMI_2015*NDVI_scale_factor, ylab='Delta NDMI (2015)', xlab='mean NDVI (2009-2011), before drought')
abline(lm(scaled_delta2015 ~ scaled_meanNDVI2009_2011), lty=2, col='grey', lwd=1.2)
dev.off()

tiff(file = file.path(FiguresDir, 'surdef2015_vs_str_dist.tif'), family = 'Times New Roman', pointsize = 11, width = 3.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(soaproot_terrain_data$stream_dist_N_150, surdef_results$AWS_2015_mm, ylab='Plant available water -or- deficit, 2015 (mm)', xlab= 'Distance from stream (m)')
abline(lm(surdef_results$AWS_2015_mm ~ soaproot_terrain_data$stream_dist_N_150), lty=2, lwd=1.2, col='grey')
dev.off()

#partial regression plots
lm_noSurdef <- lm(summary(lm(scaled_delta2015 ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011)))
summary(lm_noSurdef) #r2=0.28
lm_Surdef_vs_X <- lm(surdef_results$AWS_2015_mm ~ scaled_meanNDVI2009_2011)
summary(lm_Surdef_vs_X) #r2=0.19
summary(lm(residuals(lm_noSurdef) ~ residuals(lm_Surdef_vs_X))) #slope is MLR coefficient; #RSE=553.9; r2=0.36

tiff(file = file.path(FiguresDir, 'surdef2015_partialregplot.tif', sep = ''), family = 'Times New Roman', width = 3.5, height = 3.5, pointsize = 11, units = 'in', res=800)
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(residuals(lm_Surdef_vs_X), residuals(lm_noSurdef), ylab='', xlab='')
mtext(text=expression('Soil water surplus -or- deficit, 2015 (mm) | others'), side=1, line=2.5, at=-50)
mtext(text=expression('Delta NDMI (2015) | others'), side=2, line=2.5)
abline(lm(residuals(lm_noSurdef) ~ residuals(lm_Surdef_vs_X)), lty=2, lwd=1.2, col='grey')
#text(x=-125, y=2.0, label='b')
dev.off()

lm_noNDVI <- lm(scaled_delta2015 ~ surdef_results$AWS_2015_mm)
summary(lm_noNDVI) #r2=0.06
lm_NDVI_vs_X <- lm(scaled_meanNDVI2009_2011 ~ surdef_results$AWS_2015_mm)
summary(lm_NDVI_vs_X) #r2=0.19
summary(lm(residuals(lm_noNDVI) ~ residuals(lm_NDVI_vs_X))) #slope is MLR coefficient; #RSE=553.9; r2=0.52

tiff(file = file.path(FiguresDir, 'NDVI2009_2011_partialregplot.tif', sep = ''), family = 'Times New Roman', width = 3.5, height = 3.5, pointsize = 11, units = 'in', res=800)
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(residuals(lm_NDVI_vs_X), residuals(lm_noNDVI), ylab='', xlab='')
mtext(text=expression('Mean NDVI (2009-2011) | others'), side=1, line=2.5)
mtext(text=expression('Delta NDMI (2015) | others'), side=2, line=2.5)
abline(lm(residuals(lm_noNDVI) ~ residuals(lm_NDVI_vs_X)), lty=2, lwd=1.2, col='grey')
#text(x=-125, y=2.0, label='b')
dev.off()

#predicted vs. observed
lm_2var <- lm(scaled_delta2015 ~ scaled_meanNDVI2009_2011 + surdef_results$AWS_2015_mm)
summary(lm_2var) #R2=0.54
plot(lm_2var)
tiff(file = file.path(FiguresDir, 'predictedDeltaNDMI2015_2var_vs_observed.tif', sep = ''), family = 'Times New Roman', width = 3.5, height = 3.5, pointsize = 11, units = 'in', res=800)
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(scaled_delta2015, lm_2var$fitted.values, ylab='Predicted Delta NDMI (2015)', xlab='Observed Delta NDMI (2015)', yaxt='n', xaxt='n', xlim=c(-0.35, 0.02), ylim=c(-0.35, 0.02))
axis(side=1, at=seq(from=-0.35, to=0, by=0.05), labels = c('', '0.3', '', '0.2', '', '0.1', '', '0'))
axis(side=2, at=seq(from=-0.35, to=0, by=0.05), labels = c('', '0.3', '', '0.2', '', '0.1', '', '0'))
abline(a=0, b=1, lty=2, col='grey', lwd=1.2)
#text(x=-125, y=2.0, label='b')
dev.off()
table(soaproot_remote_data_WGS84$Depth_class[scaled_delta2015 > -0.1])
table(soaproot_remote_data_WGS84$Depth_class[soaproot_remote_data_WGS84$deltaNDMI_2017*NDVI_scale_factor > -0.15])
summary(scaled_meanNDVI2009_2011[soaproot_remote_data_WGS84$deltaNDMI_2017*NDVI_scale_factor > -0.15])
lapply(soaproot_terrain_data[,2:9], function(x) summary(x[soaproot_remote_data_WGS84$deltaNDMI_2017*NDVI_scale_factor > -0.15]))
lapply(soaproot_terrain_data[,2:9], function(x) t.test(x[soaproot_remote_data_WGS84$deltaNDMI_2017*NDVI_scale_factor > -0.15], x[soaproot_remote_data_WGS84$deltaNDMI_2017*NDVI_scale_factor <= -0.15]))
sum(soaproot_remote_data_WGS84$deltaNDMI_2017*NDVI_scale_factor <= -0.15)
sum(soaproot_remote_data_WGS84$deltaNDMI_2017*NDVI_scale_factor > -0.15)
summary(soaproot_remote_data_WGS84$deltaNDMI_2017*NDVI_scale_factor)
lapply(soaproot_terrain_data[,2:9], function(x) t.test(x[soaproot_remote_data_WGS84$deltaNDMI_2017*NDVI_scale_factor > -0.2], x[soaproot_remote_data_WGS84$deltaNDMI_2017*NDVI_scale_factor <= -0.2]))

colnames(soaproot_terrain_data)
lapply(soaproot_terrain_data[,2:9], function(x) summary(lm(lm_2var$residuals ~ x)))
plot(soaproot_terrain_data$slope_N, lm_2var$residuals)
plot(soaproot_terrain_data$curv_mean_N, lm_2var$residuals)
summary(lm(scaled_delta2015 ~ scaled_meanNDVI2009_2011 + surdef_results$AWS_2015_mm + soaproot_terrain_data$slope_N))
summary(lm(scaled_delta2015 ~ scaled_meanNDVI2009_2011 + surdef_results$AWS_2015_mm + soaproot_terrain_data$curv_mean_N))
plot(lm(scaled_delta2015 ~ scaled_meanNDVI2009_2011 + surdef_results$AWS_2015_mm + soaproot_terrain_data$curv_mean_N))
plot(scaled_delta2015, lm(scaled_delta2015 ~ scaled_meanNDVI2009_2011 + surdef_results$AWS_2015_mm + soaproot_terrain_data$curv_mean_N)$fitted.values)
abline(a=0, b=1, lty=2, col='grey')
lm_3var <- lm(scaled_delta2015 ~ scaled_meanNDVI2009_2011 + surdef_results$AWS_2015_mm + soaproot_terrain_data$curv_mean_N)
lapply(soaproot_terrain_data[,2:9], function(x) summary(lm(lm_3var$residuals ~ x)))
summary(lm(scaled_delta2015 ~ scaled_meanNDVI2009_2011 + surdef_results$AWS_2015_mm + soaproot_terrain_data$curv_mean_N+soaproot_terrain_data$slope_N+soaproot_terrain_data$elev_N))

#2014
summary(lm(soaproot_remote_data_WGS84$deltaNDMI_2014 ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011 + surdef_results$AWS_2014_mm))
summary(lm(soaproot_remote_data_WGS84$deltaNDMI_2014 ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011))
summary(lm(soaproot_remote_data_WGS84$deltaNDMI_2014 ~ surdef_results$AWS_2014_mm))
vif(lm(soaproot_remote_data_WGS84$deltaNDMI_2014 ~ soaproot_remote_data_WGS84$MeanNDVIyear2009_2011 + surdef_results$AWS_2014_mm))

#TO-DO
#1 standard error (S.E) plot for NDMI
#plot NDMI
plot(apply(soaproot_remote_data_WGS84[,2:10], 2, mean)*0.0001, type='b', ylab='mean NDMI, regolith sampling points', xaxt='n', xlab='Year')
axis(side=1, at=1:9, labels=2009:2017)
col_indices <- which(grepl('MeanNDMI', colnames(soaproot_remote_data_WGS84)))

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


tiff(file = file.path(FiguresDir, 'NDMI_by_depth_class_1SE.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.5, 4.5, 0.5, 0.5))
plot(c(1,length(col_indices)), c(min(NDMI_minus1SE_by_depth_cl[c(1,3),]), max(NDMI_plus1SE_by_depth_cl[c(1,3),]))*NDVI_scale_factor, type='n', xlab='', ylab = 'NDMI, regolith sampling points', xaxt='n')
axis(side=1, at=1:length(col_indices), labels=2009:2017)
polygon(x=c(seq_along(col_indices), rev(seq_along(col_indices))), y=c(NDMI_plus1SE_by_depth_cl[1,], rev(NDMI_minus1SE_by_depth_cl[1,]))*NDVI_scale_factor,  border=NA, lwd=0.3, col='gray95')
#polygon(x=c(NDVI_dates, rev(NDVI_dates)), y=c(NDVI_plus1SE_by_depth_cl[2,], rev(NDVI_minus1SE_by_depth_cl[2,])), border=gray_colors[2], lwd=0.3)
polygon(x=c(seq_along(col_indices), rev(seq_along(col_indices))), y=c(NDMI_plus1SE_by_depth_cl[3,], rev(NDMI_minus1SE_by_depth_cl[3,]))*NDVI_scale_factor, border = NA, lwd=0.3, col='gray90')
for (i in 1:3) {
  if(i==2) {next} else {
    lines(seq_along(col_indices), NDMI_means_by_depth_cl[i,]*NDVI_scale_factor, col=gray_colors[i], lty=i+1)
    points(seq_along(col_indices), NDMI_means_by_depth_cl[i,]*NDVI_scale_factor, col=gray_colors[i], pch=i+16, cex=0.5)
  }
}
if(length(which(p_values < 0.05))==0) {print('No significant contrasts')} else {points(seq_along(col_indices)[which(p_values < 0.05)], NDMI_means_by_depth_cl[3,]*NDVI_scale_factor[which(p_values < 0.05)]-0.01, col=gray_colors[i], pch=8, cex=0.5)}
legend('bottomleft', legend=c('shallow regolith (<3.3m) \u00B1 1 S.E.', 'deep regolith (>7.5m) \u00B1 1 S.E.'), lty = c(2,4, NA), pch=c(17,19,8), col = c(gray_colors[c(1,3)], 'black'), pt.cex=0.6, inset = 0.01) #'significant contrast (p<0.05)'
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

#delta NDMI plot
col_indices <- which(grepl('deltaNDMI', colnames(soaproot_remote_data_WGS84)))
yrs_deltaNDMI <- sapply(colnames(soaproot_remote_data_WGS84)[col_indices], function(x) as.integer(paste0(unlist(strsplit(x, ''))[11:14], collapse = '')), USE.NAMES = FALSE)
plot(apply(soaproot_remote_data_WGS84[,col_indices], 2, mean)*0.0001, type='b', ylab='delta NDMI, regolith sampling points', xaxt='n', xlab='Year')
axis(side=1, at=seq_along(col_indices), labels=yrs_deltaNDMI)


deltaNDMI_means_by_depth_cl <- do.call(cbind, lapply(soaproot_remote_data_WGS84[,col_indices], function(x) {tapply(x, soaproot_remote_data_WGS84$Depth_class, mean)}))
deltaNDMI_sd_by_depth_cl <- do.call(cbind, lapply(soaproot_remote_data_WGS84[,col_indices], function(x) {tapply(x,  soaproot_remote_data_WGS84$Depth_class, sd)}))
deltaNDMI_n_by_depth_cl <- do.call(cbind, lapply(soaproot_remote_data_WGS84[,col_indices], function(x) {tapply(x, soaproot_remote_data_WGS84$Depth_class, length)}))
deltaNDMI_se_by_depth_cl <- deltaNDMI_sd_by_depth_cl / sqrt(deltaNDMI_n_by_depth_cl)
deltaNDMI_plus1SE_by_depth_cl <- deltaNDMI_means_by_depth_cl + deltaNDMI_se_by_depth_cl
deltaNDMI_minus1SE_by_depth_cl <- deltaNDMI_means_by_depth_cl - deltaNDMI_se_by_depth_cl
anova_trim <- lapply(soaproot_remote_data_WGS84[,col_indices], function(x) summary(aov(x ~ soaproot_remote_data_WGS84$Depth_class)))
p_values <- sapply(anova_trim, function(x) x[[1]][1,5])
p_values[which(p_values < 0.05)]
gray_colors <- c('gray70', 'gray55', 'gray40')


tiff(file = file.path(FiguresDir, 'deltaNDMI_by_depth_class3_1SE.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.5, 4.5, 0.5, 0.5))
plot(c(1,length(col_indices)), c(min(deltaNDMI_minus1SE_by_depth_cl[c(1,3),]), max(deltaNDMI_plus1SE_by_depth_cl[c(1,3),]))*NDVI_scale_factor, type='n', xlab='', ylab = 'delta NDMI, regolith sampling points', xaxt='n')
axis(side=1, at=1:length(col_indices), labels=yrs_deltaNDMI)
polygon(x=c(seq_along(col_indices), rev(seq_along(col_indices))), y=c(deltaNDMI_plus1SE_by_depth_cl[1,], rev(deltaNDMI_minus1SE_by_depth_cl[1,]))*NDVI_scale_factor,  border=NA, lwd=0.3, col='gray95')
polygon(x=c(seq_along(col_indices), rev(seq_along(col_indices))), y=c(deltaNDMI_plus1SE_by_depth_cl[2,], rev(deltaNDMI_minus1SE_by_depth_cl[2,]))*NDVI_scale_factor, border=NA, lwd=0.3, col='gray90')
polygon(x=c(seq_along(col_indices), rev(seq_along(col_indices))), y=c(deltaNDMI_plus1SE_by_depth_cl[3,], rev(deltaNDMI_minus1SE_by_depth_cl[3,]))*NDVI_scale_factor, border = NA, lwd=0.3, col='gray85')
for (i in 1:3) {
  if(i==4) {next} else {
    lines(seq_along(col_indices), deltaNDMI_means_by_depth_cl[i,]*NDVI_scale_factor, col=gray_colors[i], lty=i+1)
    points(seq_along(col_indices), deltaNDMI_means_by_depth_cl[i,]*NDVI_scale_factor, col=gray_colors[i], pch=i+16, cex=0.5)
  }
}
if(length(which(p_values < 0.05))==0) {print('No significant contrasts')} else {points(seq_along(col_indices)[which(p_values < 0.05)], (deltaNDMI_means_by_depth_cl[3,]*NDVI_scale_factor)[which(p_values < 0.05)]-0.01, col=gray_colors[i], pch=8, cex=0.5)}
legend('bottomleft', legend=c('shallow regolith (<3.3m) \u00B1 1 S.E.', 'moderate regolith (3.3-7.5m) \u00B1 1 S.E.', 'deep regolith (>7.5m) \u00B1 1 S.E.', 'significant contrast (p<0.05)'), lty = c(2:4, NA), pch=c(17:19,8), col = c(gray_colors, 'black'), pt.cex=0.6, inset = 0.01) #
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

summary(aov(soaproot_remote_data_WGS84$MeanNDVIyear2009_2011 ~ as.factor(soaproot_remote_data_WGS84$Depth_class)))
meanNDMI_2009_2011 <- apply(soaproot_remote_data_WGS84[,c('MeanNDMIyear2009', 'MeanNDMIyear2010', 'MeanNDMIyear2011')], 1, mean)
summary(aov(meanNDMI_2009_2011 ~ as.factor(soaproot_remote_data_WGS84$Depth_class)))

#cumulative deficit plot
col_indices <- which(grepl('AWS', colnames(surdef_results)))
yrs_surdef <- sapply(colnames(surdef_results)[col_indices], function(x) as.integer(paste0(unlist(strsplit(x, ''))[5:8], collapse = '')), USE.NAMES = FALSE)
plot(apply(surdef_results[,col_indices], 2, mean), type='b', ylab='plant available water/water deficit, mm', xaxt='n', xlab='Year')
axis(side=1, at=seq_along(col_indices), labels=yrs_surdef)


surdef_means_by_depth_cl <- do.call(cbind, lapply(surdef_results[,col_indices], function(x) {tapply(x, soaproot_remote_data_WGS84$Depth_class, mean)}))
surdef_sd_by_depth_cl <- do.call(cbind, lapply(surdef_results[,col_indices], function(x) {tapply(x, soaproot_remote_data_WGS84$Depth_class, sd)}))
surdef_n_by_depth_cl <- do.call(cbind, lapply(surdef_results[,col_indices], function(x) {tapply(x, soaproot_remote_data_WGS84$Depth_class, length)}))
surdef_se_by_depth_cl <- surdef_sd_by_depth_cl / sqrt(surdef_n_by_depth_cl)
surdef_plus1SE_by_depth_cl <- surdef_means_by_depth_cl + surdef_se_by_depth_cl
surdef_minus1SE_by_depth_cl <- surdef_means_by_depth_cl - surdef_se_by_depth_cl
anova_trim <- lapply(surdef_results[,col_indices], function(x) summary(aov(x ~ soaproot_remote_data_WGS84$Depth_class)))
p_values <- sapply(anova_trim, function(x) x[[1]][1,5])
p_values[which(p_values < 0.05)]
gray_colors <- c('gray70', 'gray55', 'gray40')


tiff(file = file.path(FiguresDir, 'surdef_by_depth_class_1SE.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.5, 4.5, 0.5, 0.5))
plot(c(1,length(col_indices)), c(min(surdef_minus1SE_by_depth_cl[c(1,3),]), max(surdef_plus1SE_by_depth_cl[c(1,3),])), type='n', xlab='', ylab = 'Plant available water -or- water deficit, mm', xaxt='n')
axis(side=1, at=1:length(col_indices), labels=yrs_surdef)
polygon(x=c(seq_along(col_indices), rev(seq_along(col_indices))), y=c(surdef_plus1SE_by_depth_cl[1,], rev(surdef_minus1SE_by_depth_cl[1,])),  border=NA, lwd=0.3, col='gray95')
polygon(x=c(seq_along(col_indices), rev(seq_along(col_indices))), y=c(surdef_plus1SE_by_depth_cl[2,], rev(surdef_minus1SE_by_depth_cl[2,])), border=NA, lwd=0.3, col='gray90')
polygon(x=c(seq_along(col_indices), rev(seq_along(col_indices))), y=c(surdef_plus1SE_by_depth_cl[3,], rev(surdef_minus1SE_by_depth_cl[3,])), border = NA, lwd=0.3, col='gray85')
for (i in 1:3) {
  #if(i==2) {next} else {
  lines(seq_along(col_indices), surdef_means_by_depth_cl[i,], col=gray_colors[i], lty=i+1)
  points(seq_along(col_indices), surdef_means_by_depth_cl[i,], col=gray_colors[i], pch=i+16, cex=0.5)
  #}
}
abline(h=0, lty=2)
#if(length(which(p_values < 0.05))==0) {print('No significant contrasts')} else {points(seq_along(col_indices)[which(p_values < 0.05)], (surdef_means_by_depth_cl[3,]*NDVI_scale_factor)[which(p_values < 0.05)]-0.01, col=gray_colors[i], pch=8, cex=0.5)}
legend('bottomleft', legend=c('shallow regolith (<3.3m) \u00B1 1 S.E.', 'moderate regolith (3.3-7.5m) \u00B1 1 S.E.', 'deep regolith (>7.5m) \u00B1 1 S.E.'), lty = c(2:4), pch=c(17:19), col = gray_colors, pt.cex=0.6, inset = 0.01) #'significant contrast (p<0.05)'
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

climate_data <- data.frame(years=2011:2017, ET_annual=apply(water_balance_soaproot[,grepl('ET', colnames(water_balance_soaproot))], 2, mean), P_annual=prism_data$annual_precip[2:8])
write.csv(climate_data, file.path(dataDir, 'water balance', 'climate_data.csv'), row.names = FALSE)
summary(soaproot_remote_data_WGS84$AWS_mm)
