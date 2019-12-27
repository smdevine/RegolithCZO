landsat8Dir <- 'C:/Users/smdevine/Desktop/post doc/czo work/landsat8/summaries/finals'
#inspect time Landsat 8 time series for all 100 points, symbolizing by regolith depth
#doy_20160121_NDVI is wonky
dataDir <- 'C:/Users/smdevine/Desktop/post doc/czo work'
FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/results/figures'
library(itsadug)
library(extrafont)
library(extrafontdb)
#font_import() only needs to be done once
loadfonts()
res_plots <- 800
df_master <- read.csv(file.path(dataDir, 'Master Data Set_with rock outcrop.csv'), stringsAsFactors = FALSE) #removed biological data from this dataset but it has duplicate rows where multiple biological observations were made at each point; also removed 0's from outcrop (OC) site codes 01-09 to match points dataset labeling
df_sites <- df_master[match(unique(df_master$Site), df_master$Site), ]
include_OC <- FALSE
NDVI_landsat8 <- read.csv(file.path(landsat8Dir, 'NDVI_2013_2017_final.csv'), row.names = 1)
NDVI_landsat8$doy_20160121_ndvi <- NULL
NDVI_landsat8$Site <- row.names(NDVI_landsat8)
NDVI_landsat8$Depth <- df_sites$Depth[match(NDVI_landsat8$Site, df_sites$Site)] #they are both in same order but this is safer nonetheless
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

#plot mean and variance for different depth classes (defined the same in str_dist_boxplot.R)
NDVI_landsat8$Depth_class <- ifelse(NDVI_landsat8$Depth < 3.3, 1, ifelse(NDVI_landsat8$Depth < 7.56, 2, 3)) #1=shallow; 2=moderate; 3=deep
table(NDVI_landsat8$Depth_class)
dim(NDVI_landsat8)
NDVI_means_by_depth_cl <- do.call(cbind, lapply(NDVI_landsat8[,1:59], function(x) {tapply(x, NDVI_landsat8$Depth_class, mean)}))
NDVI_sd_by_depth_cl <- do.call(cbind, lapply(NDVI_landsat8[,1:59], function(x) {tapply(x, NDVI_landsat8$Depth_class, sd)}))
NDVI_n_by_depth_cl <- do.call(cbind, lapply(NDVI_landsat8[,1:59], function(x) {tapply(x, NDVI_landsat8$Depth_class, length)}))
NDVI_75q_by_depth_cl <- do.call(cbind, lapply(NDVI_landsat8[,1:59], function(x) {tapply(x, NDVI_landsat8$Depth_class, function(y) {quantile(y, probs = 0.75)})}))
NDVI_25q_by_depth_cl <- do.call(cbind, lapply(NDVI_landsat8[,1:59], function(x) {tapply(x, NDVI_landsat8$Depth_class, function(y) {quantile(y, probs = 0.25)})}))
NDVI_CI95_by_depth_cl <- 1.96 * NDVI_sd_by_depth_cl / sqrt(NDVI_n_by_depth_cl)
NDVI_se_by_depth_cl <- NDVI_sd_by_depth_cl / sqrt(NDVI_n_by_depth_cl)
NDVI_plus1SE_by_depth_cl <- NDVI_means_by_depth_cl + NDVI_se_by_depth_cl
NDVI_minus1SE_by_depth_cl <- NDVI_means_by_depth_cl - NDVI_se_by_depth_cl
NDVI_dates <- as.Date(sapply(colnames(NDVI_means_by_depth_cl), function(x) substr(x, 5, 12)), format = '%Y%m%d')

#analysis
anova_full <- lapply(NDVI_landsat8[,1:59], function(x) aov(x ~ as.factor(NDVI_landsat8$Depth_class)))
anova_trim <- lapply(NDVI_landsat8[,1:59], function(x) summary(aov(x ~ NDVI_landsat8$Depth_class)))
p_values <- sapply(anova_trim, function(x) x[[1]][1,5])
p_values[which(p_values < 0.05)]
sig_indices <- which(p_values < 0.05)
tukey_results <- lapply(anova_full, TukeyHSD)
tukey_results[sig_indices]

#1 standard error (S.E) plot
gray_colors <- c('gray70', 'gray55', 'gray40')
class(as.POSIXlt(NDVI_dates, tz='PDT'))
tiff(file = file.path(FiguresDir, 'NDVI_by_depth_class_1SE.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
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

#smooth polygon example
tiff(file = file.path(FiguresDir, 'NDVI_by_depth_class_1SE_spline.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.5, 4.5, 0.5, 0.5))
plot(as.POSIXlt(c(min(NDVI_dates), max(NDVI_dates))), c(min(NDVI_minus1SE_by_depth_cl[1,])-0.01, max(NDVI_plus1SE_by_depth_cl[1,])+0.03), type='n', xlab='', ylab = 'NDVI', xaxt='n')
axis.POSIXct(side = 1, x = as.POSIXlt(NDVI_dates))
polygon_shallow <- rbind(as.data.frame(spline(NDVI_dates, NDVI_plus1SE_by_depth_cl[1,], method = 'natural')), as.data.frame(spline(NDVI_dates, NDVI_minus1SE_by_depth_cl[1,], method = 'natural'))[length(spline(NDVI_dates, NDVI_minus1SE_by_depth_cl[1,], method = 'natural')[[1]]):1, ])
polygon_deep <- rbind(as.data.frame(spline(NDVI_dates, NDVI_plus1SE_by_depth_cl[3,], method = 'natural')), as.data.frame(spline(NDVI_dates, NDVI_minus1SE_by_depth_cl[3,], method = 'natural'))[length(spline(NDVI_dates, NDVI_minus1SE_by_depth_cl[3,], method = 'natural')[[1]]):1, ])
polygon(x=as.POSIXlt(polygon_shallow$x * 86400, origin = '1970-01-01'), y=polygon_shallow$y, border=NA, lwd=0.3, col='gray95')
#polygon(x=c(NDVI_dates, rev(NDVI_dates)), y=c(NDVI_plus1SE_by_depth_cl[2,], rev(NDVI_minus1SE_by_depth_cl[2,])), border=gray_colors[2], lwd=0.3)
polygon(x=as.POSIXlt(polygon_deep$x * 86400, origin = '1970-01-01'), y=polygon_deep$y, border = NA, lwd=0.3, col='gray90')
for (i in 1:3) {
  if(i==2) {next}
  lines(as.POSIXlt(NDVI_dates), NDVI_means_by_depth_cl[i,], col=gray_colors[i], lty=i+1)
  points(as.POSIXlt(NDVI_dates), NDVI_means_by_depth_cl[i,], col=gray_colors[i], pch=i+16, cex=0.5)
}
legend('bottomleft', legend=c('shallow regolith (<3.3m) mean NDVI \u00B1 1 S.E.', 'deep regolith (>7.5m) mean NDVI \u00B1 1 S.E.'), lty = c(2,4), pch=c(17,19), col = gray_colors[c(1,3)], pt.cex=0.6, inset = 0.02)
text(as.Date('2014-06-01'), 0.6, '2012-2015: historic 4-year drought and forest die-off')
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

#IQR plot
tiff(file = file.path(FiguresDir, 'NDVI_by_depth_class_IQR.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.5, 4.5, 0.5, 0.5))
plot(c(min(NDVI_dates), max(NDVI_dates)), c(min(NDVI_25q_by_depth_cl[1,]), max(NDVI_75q_by_depth_cl[1,])), type='n', xlab='', ylab = 'NDVI', xaxt='n')
polygon(x=c(NDVI_dates, rev(NDVI_dates)), y=c(NDVI_75q_by_depth_cl[1,], rev(NDVI_25q_by_depth_cl[1,])),  border=NA, lwd=0.3, col='gray95')
#polygon(x=c(NDVI_dates, rev(NDVI_dates)), y=c(NDVI_75q_by_depth_cl[2,], rev(NDVI_25q_by_depth_cl[2,])), border=gray_colors[2], lwd=0.3)
polygon(x=c(NDVI_dates, rev(NDVI_dates)), y=c(NDVI_75q_by_depth_cl[3,], rev(NDVI_25q_by_depth_cl[3,])), border = NA, lwd=0.3, col='gray90')
for (i in 1:3) {
  if(i==2) {next}
  lines(NDVI_dates, NDVI_means_by_depth_cl[i,], col=gray_colors[i], lty=i+1)
  points(NDVI_dates, NDVI_means_by_depth_cl[i,], col=gray_colors[i], pch=i+16, cex=0.5)
}
axis.Date(side = 1, x = NDVI_dates)
#legend('bottomleft', legend=c('shallow (<3.3m)', 'moderate (3.3-7.5m)', 'deep (>7.5m)'), lty = 1:3, col = gray_colors, inset = 0.02)
legend('bottomleft', legend=c('shallow (<3.3m) mean NDVI \u00B1 1 S.E.', 'deep (>7.5m) mean NDVI \u00B1 1 S.E.'), lty = c(2,4), pch=c(17,19), col = gray_colors[c(1,3)], pt.cex=0.5, inset = 0.02)
dev.off()


#transform NDVI to ET via this equation:
#ET (mm) = 117.16 * exp(2.8025*NDVI))
colnames(NDVI_landsat8)
dim(NDVI_landsat8)

ET_est_landsat8 <- as.data.frame(do.call(cbind, lapply(NDVI_landsat8[,1:59], function(y) {
  sapply(y, function(x) {
    117.16 * exp(2.8025*x)
  })
})
))
dim(ET_est_landsat8)
ET_est_landsat8$Depth_class <- NDVI_landsat8$Depth_class

ET_est_means_by_depth_cl <- do.call(cbind, lapply(ET_est_landsat8[,1:59], function(x) {tapply(x, ET_est_landsat8$Depth_class, mean)}))
ET_est_sd_by_depth_cl <- do.call(cbind, lapply(ET_est_landsat8[,1:59], function(x) {tapply(x, ET_est_landsat8$Depth_class, sd)}))
ET_est_n_by_depth_cl <- do.call(cbind, lapply(ET_est_landsat8[,1:59], function(x) {tapply(x, ET_est_landsat8$Depth_class, length)}))
ET_est_75q_by_depth_cl <- do.call(cbind, lapply(ET_est_landsat8[,1:59], function(x) {tapply(x, ET_est_landsat8$Depth_class, function(y) {quantile(y, probs = 0.75)})}))
ET_est_25q_by_depth_cl <- do.call(cbind, lapply(ET_est_landsat8[,1:59], function(x) {tapply(x, ET_est_landsat8$Depth_class, function(y) {quantile(y, probs = 0.25)})}))
ET_est_CI95_by_depth_cl <- 1.96 * ET_est_sd_by_depth_cl / sqrt(ET_est_n_by_depth_cl)
ET_est_se_by_depth_cl <- ET_est_sd_by_depth_cl / sqrt(ET_est_n_by_depth_cl)
ET_est_plus1SE_by_depth_cl <- ET_est_means_by_depth_cl + ET_est_se_by_depth_cl
ET_est_minus1SE_by_depth_cl <- ET_est_means_by_depth_cl - ET_est_se_by_depth_cl
ET_est_dates <- as.Date(sapply(colnames(ET_est_means_by_depth_cl), function(x) substr(x, 5, 12)), format = '%Y%m%d')

#analysis
anova_full <- lapply(ET_est_landsat8[,1:59], function(x) aov(x ~ as.factor(ET_est_landsat8$Depth_class)))
anova_trim <- lapply(ET_est_landsat8[,1:59], function(x) summary(aov(x ~ ET_est_landsat8$Depth_class)))
p_values_ET <- sapply(anova_trim, function(x) x[[1]][1,5])
p_values_ET[which(p_values_ET < 0.05)]
sig_indices <- which(p_values_ET < 0.05)
tukey_results <- lapply(anova_full, TukeyHSD)
tukey_results[sig_indices]

#1 standard error (S.E) plot
gray_colors <- c('gray70', 'gray55', 'gray40')
class(as.POSIXlt(ET_est_dates, tz='PDT'))
tiff(file = file.path(FiguresDir, 'ET_est_by_depth_class_1SE.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.5, 4.5, 0.5, 0.5))
plot(c(min(ET_est_dates), max(ET_est_dates)), c(min(ET_est_minus1SE_by_depth_cl[1,]), max(ET_est_plus1SE_by_depth_cl[1,])), type='n', xlab='', ylab = 'Evapotranspiration (mm)', xaxt='n')
axis.Date(side = 1, x = ET_est_dates)
polygon(x=c(ET_est_dates, rev(ET_est_dates)), y=c(ET_est_plus1SE_by_depth_cl[1,], rev(ET_est_minus1SE_by_depth_cl[1,])),  border=NA, lwd=0.3, col='gray95')
#polygon(x=c(ET_est_dates, rev(ET_est_dates)), y=c(ET_est_plus1SE_by_depth_cl[2,], rev(ET_est_minus1SE_by_depth_cl[2,])), border=gray_colors[2], lwd=0.3)
polygon(x=c(ET_est_dates, rev(ET_est_dates)), y=c(ET_est_plus1SE_by_depth_cl[3,], rev(ET_est_minus1SE_by_depth_cl[3,])), border = NA, lwd=0.3, col='gray90')
for (i in 1:3) {
  if(i==2) {next}
  lines(ET_est_dates, ET_est_means_by_depth_cl[i,], col=gray_colors[i], lty=i+1)
  points(ET_est_dates, ET_est_means_by_depth_cl[i,], col=gray_colors[i], pch=i+16, cex=0.5)
}
points(ET_est_dates[which(p_values_ET < 0.05)], ET_est_means_by_depth_cl[3,][which(p_values_ET < 0.05)]-25, col=gray_colors[i], pch=8, cex=0.5)
legend('bottomleft', legend=c('shallow regolith (<3.3m) mean ET_est \u00B1 1 S.E.', 'deep regolith (>7.5m) mean ET_est \u00B1 1 S.E.', 'significant contrast (p<0.05)'), lty = c(2,4, NA), pch=c(17,19, 8), col = c(gray_colors[c(1,3)], 'black'), pt.cex=0.6, inset = 0.01)
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



##ggplot trial
library(ggplot2)
NDVI_means_by_depth_cl_tr <- as.data.frame(t(NDVI_means_by_depth_cl))
colnames(NDVI_means_by_depth_cl_tr) <- c('Shallow', 'Moderate', 'Deep')
NDVI_means_by_depth_cl_tr$Date <- NDVI_dates

NDVI_means_by_depth_cl_tr <- rbind(data.frame(NDVI=NDVI_means_by_depth_cl_tr[,1], Date=NDVI_means_by_depth_cl_tr[,4], Depth_class= 'Shallow', row.names=NULL, stringsAsFactors=FALSE), data.frame(NDVI=NDVI_means_by_depth_cl_tr[,2], Date=NDVI_means_by_depth_cl_tr[,4], Depth_class = 'Moderate', row.names=NULL, stringsAsFactors=FALSE), data.frame(NDVI=NDVI_means_by_depth_cl_tr[,3], Date=NDVI_means_by_depth_cl_tr[,4], Depth_class= 'Deep', row.names=NULL, stringsAsFactors=FALSE), stringsAsFactors=FALSE)

tiff(file = file.path(FiguresDir, 'NDVI_by_depth_class_ggplot.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.5, 4.5, 0.5, 0.5))
ggplot(NDVI_means_by_depth_cl_tr, aes(x=Date, y=NDVI, color=Depth_class)) +
  #geom_point() +
  geom_smooth(method='loess', span=0.1, se=TRUE, level=0.68, fill='lightgrey') +
  scale_color_grey(start=0.2833, end = 0.75) +
  theme_classic() +
  xlab("") +
  scale_fill_discrete(name="Ya Ya")
dev.off()

#do the same with EVI
include_OC <- FALSE
EVI_landsat8 <- read.csv(file.path(landsat8Dir, 'EVI_2013_2017_final.csv'), row.names = 1)
EVI_landsat8$Site <- row.names(EVI_landsat8)
EVI_landsat8$Depth <- df_sites$Depth[match(EVI_landsat8$Site, df_sites$Site)] #they are both in same order but this is safer nonetheless
EVI_landsat8 <- EVI_landsat8[!is.na(EVI_landsat8$Depth), ]
if (include_OC) {} else {
  EVI_landsat8 <- EVI_landsat8[EVI_landsat8$Depth!=0,]
}
dates_raw <- colnames(EVI_landsat8)[1:(ncol(EVI_landsat8)-2)]
dates <- sapply(strsplit(dates_raw, ''), function(x) paste0(x[5:12], collapse=''))
dates <- as.Date(dates, '%Y%m%d')

EVI_landsat8$Depth_class <- ifelse(EVI_landsat8$Depth < 3.3, 1, ifelse(EVI_landsat8$Depth < 7.56, 2, 3)) #1=shallow; 2=moderate; 3=deep
table(EVI_landsat8$Depth_class)
dim(EVI_landsat8)
EVI_means_by_depth_cl <- do.call(cbind, lapply(EVI_landsat8[,1:59], function(x) {tapply(x, EVI_landsat8$Depth_class, mean)}))
EVI_sd_by_depth_cl <- do.call(cbind, lapply(EVI_landsat8[,1:59], function(x) {tapply(x, EVI_landsat8$Depth_class, sd)}))
EVI_n_by_depth_cl <- do.call(cbind, lapply(EVI_landsat8[,1:59], function(x) {tapply(x, EVI_landsat8$Depth_class, length)}))
EVI_75q_by_depth_cl <- do.call(cbind, lapply(EVI_landsat8[,1:59], function(x) {tapply(x, EVI_landsat8$Depth_class, function(y) {quantile(y, probs = 0.75)})}))
EVI_25q_by_depth_cl <- do.call(cbind, lapply(EVI_landsat8[,1:59], function(x) {tapply(x, EVI_landsat8$Depth_class, function(y) {quantile(y, probs = 0.25)})}))
EVI_se_by_depth_cl <- EVI_sd_by_depth_cl / sqrt(EVI_n_by_depth_cl)
EVI_plus1SE_by_depth_cl <- EVI_means_by_depth_cl + EVI_se_by_depth_cl
EVI_minus1SE_by_depth_cl <- EVI_means_by_depth_cl - EVI_se_by_depth_cl
EVI_dates <- as.Date(sapply(colnames(EVI_means_by_depth_cl), function(x) substr(x, 5, 12)), format = '%Y%m%d')

tiff(file = file.path(FiguresDir, 'EVI_by_depth_class_1SE.tif'), family = 'Times New Roman', pointsize = 11, width = 9, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(2.5, 4.5, 0.5, 0.5))
plot(c(min(EVI_dates), max(EVI_dates)), c(min(EVI_minus1SE_by_depth_cl[1,]), max(EVI_plus1SE_by_depth_cl[1,])), type='n', xlab='', ylab = 'EVI', xaxt='n')
polygon(x=c(EVI_dates, rev(EVI_dates)), y=c(EVI_plus1SE_by_depth_cl[1,], rev(EVI_minus1SE_by_depth_cl[1,])),  border=NA, lwd=0.3, col='gray95')
#polygon(x=c(EVI_dates, rev(EVI_dates)), y=c(EVI_plus1SE_by_depth_cl[2,], rev(EVI_minus1SE_by_depth_cl[2,])), border=gray_colors[2], lwd=0.3)
polygon(x=c(EVI_dates, rev(EVI_dates)), y=c(EVI_plus1SE_by_depth_cl[3,], rev(EVI_minus1SE_by_depth_cl[3,])), border = NA, lwd=0.3, col='gray90')
for (i in 1:3) {
  if(i==2) {next}
  lines(EVI_dates, EVI_means_by_depth_cl[i,], col=gray_colors[i], lty=i+1)
  points(EVI_dates, EVI_means_by_depth_cl[i,], col=gray_colors[i], pch=i+16, cex=0.5)
}
axis.Date(side = 1, x = EVI_dates)
#legend('bottomleft', legend=c('shallow (<3.3m)', 'moderate (3.3-7.5m)', 'deep (>7.5m)'), lty = 1:3, col = gray_colors, inset = 0.02)
legend('bottomleft', legend=c('shallow (<3.3m) mean EVI \u00B1 1 S.E.', 'deep (>7.5m) mean EVI \u00B1 1 S.E.'), lty = c(2,4), pch=c(17,19), col = gray_colors[c(1,3)], pt.cex=0.5, inset = 0.02)
dev.off()