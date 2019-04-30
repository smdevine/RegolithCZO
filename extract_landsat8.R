library(raster)
pointsDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/sampling pts'
downloadfileDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/landsat8'
summaryDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/landsat8/summaries'

#check some point values
soaproot_pts <- shapefile(file.path(pointsDir, 'soaproot_pts.shp'))
subdir <- file.path('LC080420342013060501T1-SC20190426180641.tar', 'LC080420342013060501T1-SC20190426180641')
fnames <- list.files(file.path(downloadfileDir, '2013', subdir))
evi <- raster(file.path(downloadfileDir, '2013', subdir, fnames[5]))
ndvi <- raster(file.path(downloadfileDir, '2013', subdir, fnames[6]))
qc <- raster(file.path(downloadfileDir, '2013', subdir, fnames[4]))
evi_values <- extract(evi, soaproot_pts)
summary(evi_values) / 10000
ndvi_values <- extract(ndvi, soaproot_pts)
summary(ndvi_values) / 10000
qc_values <- extract(qc, soaproot_pts)
qc_values
soaproot_pts$evi_Jun2013 <- evi_values
as.data.frame(soaproot_pts)[15,]

#formalize data extraction process
yr <- '2013'
scale_factor <- 0.0001
dirnames <- list.files(file.path(downloadfileDir, yr))
column_dates <- sapply(strsplit(dirnames, split = ''), function(x) paste0(x[11:18], collapse = ''))
column_dates_ndvi <- paste0('doy_', column_dates, '_ndvi')
column_dates_evi <- paste0('doy_', column_dates, '_evi')
column_dates_qc <- paste0('doy_', column_dates, '_qc')
NDVI_results <- data.frame(matrix(data = NA, nrow=length(soaproot_pts$Name), ncol = length(dirnames)))
row.names(NDVI_results) <- soaproot_pts$Name
colnames(NDVI_results) <- column_dates_ndvi
EVI_results <- NDVI_results
colnames(EVI_results) <- column_dates_evi
qc_results <- NDVI_results
colnames(qc_results) <- column_dates_qc 
for (i in seq_along(dirnames)) {
  subdir <- list.files(file.path(downloadfileDir, yr, dirnames[i]))
  fnames <- list.files(file.path(downloadfileDir, yr, dirnames[i], subdir))
  #print(fnames)
  NDVI_raster <- raster(file.path(downloadfileDir, yr, dirnames[i], subdir, fnames[6]))
  EVI_raster <- raster(file.path(downloadfileDir, yr, dirnames[i], subdir, fnames[5]))
  QC_raster <- raster(file.path(downloadfileDir, yr, dirnames[i], subdir, fnames[4]))
  NDVI_results[,i] <- extract(NDVI_raster, soaproot_pts) * scale_factor
  EVI_results[,i] <- extract(EVI_raster, soaproot_pts) * scale_factor
  qc_results[,i] <- extract(QC_raster, soaproot_pts)
}
lapply(NDVI_results, summary)
lapply(EVI_results, summary)
lapply(qc_results, unique)
write.csv(NDVI_results, file.path(summaryDir, 'NDVI', paste0('NDVI_', yr, '_summary.csv')), row.names = TRUE)
write.csv(EVI_results, file.path(summaryDir, 'EVI', paste0('EVI_', yr, '_summary.csv')), row.names = TRUE)
write.csv(qc_results, file.path(summaryDir, 'QA', paste0('QA_', yr, '_summary.csv')), row.names = TRUE)
