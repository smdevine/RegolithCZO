###use read-in_data.R
library(raster)
dataDir <- 'C:/Users/smdevine/Desktop/post doc/czo work'
landsat8Dir <- 'C:/Users/smdevine/Desktop/post doc/czo work/landsat8/summaries/finals'
DSDdir <- 'C:/Users/smdevine/Desktop/post doc/czo work/DSD Mike'
FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/results/figures'
TablesDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/results/tables'
library(extrafont)
library(extrafontdb)
#font_import() only needs to be done once
loadfonts()
res_plots <- 800
synthetic_7.56m_log <- function(meanlog, sdlog) {
  success <- FALSE
  while (!success) {
    x <- rlnorm(1, meanlog, sdlog)
    # check for success
    success <- x > 7.56
  }
  return(x)
}
synthetic_7.56m_log(meanlog=1.773, sdlog=0.904) #lognormal parameters derived from original data not including zeroes

synthetic_7.56m_gamma <- function(shape, scale) {
  success <- FALSE
  while (!success) {
    x <- rgamma(1, shape = shape, scale = scale)
    # check for success
    success <- x > 7.56
  }
  return(x)
}
# #read-in and prepare data
colnames(soaproot_pts_analysis) #check columns

# if (resolution=='5m') {
#   soaproot_pts_matrix <- soaproot_pts_analysis[,c('CTI_N', 'curv_mean_N', 'curv_plan_N', 'curv_prof_N', 'elev_N', "EVI_2017_N", "EVI_2018_N", "NDVI_2017_N", "NDVI_2018_N", 'NDVI_LS8_2013', 'NDVI_LS8_2014', 'NDVI_LS8_2015', 'NDVI_LS8_2016', 'NDVI_LS8_2017', 'slope_N', 'solrad_N', 'stream_dist_N')]
# }
length(c(2:5,8:10,12,15))
soaproot_df <- soaproot_pts_analysis[,c(2:5,8:10,12,15)]
colnames(soaproot_df) <- c('Elevation', 'Insolation', 'Slope', 'Curvature', 'Terrain wetness index', 'Distance from channel', 'EVI, 2017', 'Dry season drawdown', 'NDVI, 2015')
cor_result <- cor(soaproot_df, method = 'pearson')
# write.csv(cor_result, file.path(TablesDir, paste0('corrs_pearson_', resolution, '_terrain_9vars.csv')))
library(corrplot)
tiff(file = file.path(FiguresDir, 'corrplot_9vars_10m_res_hclust_.tif'), family = 'Times New Roman', pointsize = 11, width = 6.5, height = 5.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(0.5, 0.5, 0.5, 0.5))
mag.factor <- 2
cex.before <- par("cex") #saves current cex setting for plotting
par(cex = 0.7)  #set cex for plotting text.  this invisibly affects p-value text
corrplot(cor_result, type = 'lower', diag = TRUE, order = 'AOE', hclust.method = 'ward.D2', tl.col = 'black', tl.srt = 30, cl.ratio = 0.2, cl.align.text = 'c', p.mat = cor.mtest(soaproot_df, method = 'pearson')[[1]], sig.level = 0.01, insig='p-value', tl.cex = par("cex") * mag.factor, cl.cex = par("cex") * mag.factor)#, cl.pos = 'b', cl.align.text = 'r', cl.ratio = 0.2)
par(cex = cex.before)
dev.off()

cor_pvalues <- cor.mtest(soaproot_df, method = 'pearson')[[1]]
write.csv(cor_pvalues, file.path(TablesDir, paste0('cor_pvals_pearson_', resolution, '_terrain_9vars.csv')))

#corrs with synthetic data
depths <- soaproot_pts_analysis$Depth
set.seed(8092019)
gamma_dist <- replicate(10000, sapply(depths, function(y) {if (y == 7.56) {synthetic_7.56m_gamma(shape = 1.915, scale = 1/0.273)} else{y}}))
log_dist <- replicate(10000, sapply(depths, function(y) {if (y == 7.56) {synthetic_7.56m_log(meanlog=1.773, sdlog=0.904)} else{y}}))

log_depth_corrs 