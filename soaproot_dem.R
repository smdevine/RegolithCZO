library(raster)
library(SpaDES)
resultsDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/NEON/DEM'
VegResultsDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/NEON/VegIndices'
demDir <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/NEON/NEON_lidar-elev/2018/FullSite/D17/2018_SOAP_3/L3/DiscreteLidar/DTMGtif'
VegIndicesDir_2017 <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/NEON/NEON_indices-veg-spectrometer-mosaic/2017/FullSite/D17/2017_SOAP_2/L3/Spectrometer/VegIndices'
VegIndicesDir_2018 <- 'C:/Users/smdevine/Desktop/PostDoc/CZO/NEON/NEON_indices-veg-spectrometer-mosaic/2018/FullSite/D17/2018_SOAP_3/L3/Spectrometer/VegIndices'
list.files(demDir)
list.files(VegIndicesDir_2017)
tags_of_interest <- c('297000_4102000', '297000_4101000', '297000_4100000', '298000_4102000', '298000_4101000', '298000_4100000', '299000_4102000', '299000_4101000', '299000_4100000', '300000_4102000', '300000_4101000', '300000_4100000')

#mosaic DEMs
filenames_of_interest <- unlist(lapply(tags_of_interest, function(x) {list.files(demDir, full.names = TRUE)[grepl(x, list.files(demDir))]}))
rasters_of_interest <- lapply(filenames_of_interest, function(x) raster(x))
rasters_of_interest
DEM_soaproot <- mergeRaster(rasters_of_interest, fun = mean)
plot(DEM_soaproot)
writeRaster(DEM_soaproot, format='GTiff', filename = file.path(resultsDir, 'DEM_soaproot_1m.tif'))

#mosaic NDVI 2017
tags_of_interest_NDVI <- paste0(tags_of_interest, '_NDVI')
filenames_of_interest <- unlist(lapply(tags_of_interest_NDVI, function(x) {list.files(VegIndicesDir_2017, full.names = TRUE)[grepl(x, list.files(VegIndicesDir_2017))]}))
rasters_of_interest <- lapply(filenames_of_interest, function(x) raster(x))
rasters_of_interest
NDVI_soaproot_2017 <- mergeRaster(rasters_of_interest, fun = mean)
plot(NDVI_soaproot_2017)
writeRaster(NDVI_soaproot_2017, format='GTiff', filename = file.path(VegResultsDir, 'NDVI_2017_1m.tif'))

#mosaic NDVI 2018
tags_of_interest_NDVI <- paste0(tags_of_interest, '_NDVI')
filenames_of_interest <- unlist(lapply(tags_of_interest_NDVI, function(x) {list.files(VegIndicesDir_2018, full.names = TRUE)[grepl(x, list.files(VegIndicesDir_2018))]}))
rasters_of_interest <- lapply(filenames_of_interest, function(x) raster(x))
rasters_of_interest
NDVI_soaproot_2018 <- mergeRaster(rasters_of_interest, fun = mean)
plot(NDVI_soaproot_2018)
writeRaster(NDVI_soaproot_2018, format='GTiff', filename = file.path(VegResultsDir, 'NDVI_2018_1m.tif'))

#mosaic EVI 2017
tags_of_interest_EVI <- paste0(tags_of_interest, '_EVI')
filenames_of_interest <- unlist(lapply(tags_of_interest_EVI, function(x) {list.files(VegIndicesDir_2017, full.names = TRUE)[grepl(x, list.files(VegIndicesDir_2017))]}))
filenames_of_interest
rasters_of_interest <- lapply(filenames_of_interest, function(x) raster(x))
rasters_of_interest
EVI_soaproot_2017 <- mergeRaster(rasters_of_interest, fun = mean)
plot(EVI_soaproot_2017)
writeRaster(EVI_soaproot_2017, format='GTiff', filename = file.path(VegResultsDir, 'EVI_2017_1m.tif'))

#mosaic EVI 2018
tags_of_interest_EVI <- paste0(tags_of_interest, '_EVI')
filenames_of_interest <- unlist(lapply(tags_of_interest_EVI, function(x) {list.files(VegIndicesDir_2018, full.names = TRUE)[grepl(x, list.files(VegIndicesDir_2018))]}))
rasters_of_interest <- lapply(filenames_of_interest, function(x) raster(x))
rasters_of_interest
EVI_soaproot_2018 <- mergeRaster(rasters_of_interest, fun = mean)
plot(EVI_soaproot_2018)
writeRaster(EVI_soaproot_2018, format='GTiff', filename = file.path(VegResultsDir, 'EVI_2018_1m.tif'))
