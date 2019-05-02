#note that year and yr variables were changed manually, as data was downloaded by year for 2013-2017
library(raster)
pointsDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/sampling pts'
downloadfileDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/landsat8'
summaryDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/landsat8/summaries'

#unpack the tar.gz files after downloading
year <- '2017'
fnames_gz <- list.files(file.path(downloadfileDir, year))
fnames_gz_full <- list.files(file.path(downloadfileDir, year), full.names = TRUE)
for (i in seq_along(fnames_gz)) {
  date <- paste0(unlist(strsplit(fnames_gz[i], split = ''))[11:18], collapse = '')
  untar(fnames_gz_full[i], exdir = file.path(downloadfileDir, year, date))
}

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

#formalize data extraction process--only applied to 2013 data
# yr <- '2013'
# scale_factor <- 0.0001
# dirnames <- list.files(file.path(downloadfileDir, yr))
# column_dates <- sapply(strsplit(dirnames, split = ''), function(x) paste0(x[11:18], collapse = ''))
# column_dates_ndvi <- paste0('doy_', column_dates, '_ndvi')
# column_dates_evi <- paste0('doy_', column_dates, '_evi')
# column_dates_qc <- paste0('doy_', column_dates, '_qc')
# NDVI_results <- data.frame(matrix(data = NA, nrow=length(soaproot_pts$Name), ncol = length(dirnames)))
# row.names(NDVI_results) <- soaproot_pts$Name
# colnames(NDVI_results) <- column_dates_ndvi
# EVI_results <- NDVI_results
# colnames(EVI_results) <- column_dates_evi
# qc_results <- NDVI_results
# colnames(qc_results) <- column_dates_qc 
# for (i in seq_along(dirnames)) {
#   subdir <- list.files(file.path(downloadfileDir, yr, dirnames[i]))
#   fnames <- list.files(file.path(downloadfileDir, yr, dirnames[i], subdir))
#   #print(fnames)
#   NDVI_raster <- raster(file.path(downloadfileDir, yr, dirnames[i], subdir, fnames[6]))
#   EVI_raster <- raster(file.path(downloadfileDir, yr, dirnames[i], subdir, fnames[5]))
#   QC_raster <- raster(file.path(downloadfileDir, yr, dirnames[i], subdir, fnames[4]))
#   NDVI_results[,i] <- extract(NDVI_raster, soaproot_pts) * scale_factor
#   EVI_results[,i] <- extract(EVI_raster, soaproot_pts) * scale_factor
#   qc_results[,i] <- extract(QC_raster, soaproot_pts)
# }
# lapply(NDVI_results, summary)
# lapply(EVI_results, summary)
# lapply(qc_results, unique)
# write.csv(NDVI_results, file.path(summaryDir, 'NDVI', paste0('NDVI_', yr, '_summary.csv')), row.names = TRUE)
# write.csv(EVI_results, file.path(summaryDir, 'EVI', paste0('EVI_', yr, '_summary.csv')), row.names = TRUE)
# write.csv(qc_results, file.path(summaryDir, 'QA', paste0('QA_', yr, '_summary.csv')), row.names = TRUE)

#used different directory naming convention for 2014-2017 as untar R function allowed for that as opposed to manual file unzipping using 7-zip
#formalize data extraction process
scale_factor <- 0.0001 #in USGS metadata

yr <- '2017'
dirnames <- list.files(file.path(downloadfileDir, yr))
#column_dates <- sapply(strsplit(dirnames, split = ''), function(x) paste0(x[11:18], collapse = ''))
column_dates_ndvi <- paste0('doy_', dirnames, '_ndvi')
column_dates_evi <- paste0('doy_', dirnames, '_evi')
column_dates_qc <- paste0('doy_', dirnames, '_qc')
NDVI_results <- data.frame(matrix(data = NA, nrow=length(soaproot_pts$Name), ncol = length(dirnames)))
row.names(NDVI_results) <- soaproot_pts$Name
colnames(NDVI_results) <- column_dates_ndvi
EVI_results <- NDVI_results
colnames(EVI_results) <- column_dates_evi
qc_results <- NDVI_results
colnames(qc_results) <- column_dates_qc 
for (i in seq_along(dirnames)) {
  fnames <- list.files(file.path(downloadfileDir, yr, dirnames[i]))
  #print(fnames)
  NDVI_raster <- raster(file.path(downloadfileDir, yr, dirnames[i], fnames[6]))
  EVI_raster <- raster(file.path(downloadfileDir, yr, dirnames[i], fnames[5]))
  QC_raster <- raster(file.path(downloadfileDir, yr, dirnames[i], fnames[4]))
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

#now get into one master file, eliminating columns when not cloud-free (i.e. when qc not 322)
NDVI_summaries <- lapply(list.files(file.path(summaryDir, 'NDVI'), full.names = TRUE), read.csv, stringsAsFactors=FALSE, row.names = 1)
#names(NDVI_summaries) <- list.files(file.path(summaryDir, 'NDVI')) #this is fine but ends up getting appended to each column name when running do.call(cbind...below
#colnames(NDVI_summaries$NDVI_2014_summary.csv)
#row.names(NDVI_summaries$NDVI_2014_summary.csv)
EVI_summaries <- lapply(list.files(file.path(summaryDir, 'EVI'), full.names = TRUE), read.csv, stringsAsFactors=FALSE, row.names = 1)
QA_summaries <- lapply(list.files(file.path(summaryDir, 'QA'), full.names = TRUE), read.csv, stringsAsFactors=FALSE, row.names=1)
#names(QA_summaries) <- list.files(file.path(summaryDir, 'QA'))

NDVI_master <- do.call(cbind, NDVI_summaries)
EVI_master <- do.call(cbind, EVI_summaries)
QA_master <- do.call(cbind, QA_summaries)
write.csv(QA_master, file.path(summaryDir, 'finals', 'QA_2013_2017_final.csv'), row.names = TRUE)
#these QA values all mean clear: 322, 386, 834, 898, 1346, according to LSDS-1368_ L8_Surface-Reflectance-Code-LASRC-Product-Guide.pdf
QA_indices <- lapply(QA_master, function(x) {
  sum(!(x%in%c(322, 386, 834, 898, 1346))) #thus a zero means that all points were under clear sky and snow/ice-free based on QA codes 
  #y <- as.numeric(paste0(unique(x), collapse = ''))
  #print(y)
  })
QA_indices
dim(NDVI_master) #100 109
NDVI_master <- NDVI_master[ ,which(QA_indices==0)]
dim(NDVI_master) #100  61 [got two more columns]
write.csv(NDVI_master, file.path(summaryDir, 'finals', 'NDVI_2013_2017_final.csv'), row.names = TRUE)

dim(EVI_master)
EVI_master <- EVI_master[ ,which(QA_indices==0)]
dim(EVI_master)
write.csv(EVI_master, file.path(summaryDir, 'finals', 'EVI_2013_2017_final.csv'), row.names = TRUE)

#merge with depth

