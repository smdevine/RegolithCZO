library(raster)
library(dynatopmodel)
NEONterrainDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/5m terrain characteristics/5m filtered'
NEONterrain10mDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/NEON 10m/terrain characteristics'
dem_5m_soaproot <- raster(file.path(NEONterrainDir, 'elev_5m_filtered.tif'))
twi_soaproot <- upslope.area(dem_5m_soaproot, atb = TRUE)
class(twi_soaproot)
plot(twi_soaproot$atb)
writeRaster(twi_soaproot$a, filename = file.path(NEONterrainDir, 'upslope_area_5m.tif'))
writeRaster(twi_soaproot$atb, filename = file.path(NEONterrainDir, 'twi_5m.tif'))
dem_10m_soaproot <- raster(file.path(NEONterrain10mDir, 'DEM_soaproot_10m.tif'))
twi10m_soaproot <- upslope.area(dem_10m_soaproot, atb = TRUE)
plot(twi10m_soaproot$atb)
plot(twi10m_soaproot$a)
writeRaster(twi10m_soaproot$a, filename = file.path(NEONterrain10mDir, 'upslope_area_10m.tif'))
writeRaster(twi10m_soaproot$atb, filename = file.path(NEONterrain10mDir, 'twi_10m.tif'))
