library(raster)
#flowacc <- raster('C:/Users/smdevine/Desktop/PostDoc/CZO/NEON/DEM/10m_filled/flowacc_10m.tif')
flowacc <- raster('C:/Users/smdevine/Desktop/PostDoc/CZO/NEON/DEM/5m_filled/flowacc_5m.tif')
flowacc
hist(flowacc$flowacc_5m[flowacc$flowacc_5m<1000])
freq_table <- freq(flowacc)
dim(freq_table)
tail(freq_table, 100)
head(freq_table, 200)
freq_table[400,]
sum(freq_table[1:400,2]) #116439 out of 120000 are <400
sum(freq_table[1:150,2]) #114206 out of 120000 are <150
sum(freq_table[1:100,2]) #112964 out of 120000 are <100
