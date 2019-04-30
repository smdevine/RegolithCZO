RyanGeospatialDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/RYAN GEOSPATIAL'
library(raster)
list.files(file.path(RyanGeospatialDir, 'CZO watersheds', 'CZO'))
soaproot_watershed <- shapefile(file.path(RyanGeospatialDir, 'CZO watersheds', 'CZO', 'Soaproot_integrated.shp'))
soaproot_watershed_wgs84 <- spTransform(soaproot_watershed, CRS('+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
plot(soaproot_watershed_wgs84)
shapefile(soaproot_watershed_wgs84, file.path(RyanGeospatialDir, 'CZO watersheds', 'CZO', 'Soaproot_integrated_wgs84.shp'))
