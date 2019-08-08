library(raster)
dataDir <- 'C:/Users/smdevine/Desktop/post doc/czo work'
landsat8Dir <- 'C:/Users/smdevine/Desktop/post doc/czo work/landsat8/summaries/finals'
DSDdir <- 'C:/Users/smdevine/Desktop/post doc/czo work/DSD Mike'
df_master <- read.csv(file.path(dataDir, 'Master Data Set_with rock outcrop.csv'), stringsAsFactors = FALSE) #removed biological data from this dataset but it has duplicate rows where multiple biological observations were made at each point; also removed 0's from outcrop (OC) site codes 01-09 to match points dataset labeling
df_sites <- df_master[match(unique(df_master$Site), df_master$Site), ]
soaproot_pts <- read.csv(file.path(dataDir, 'Soaproot points RF.csv'), stringsAsFactors = FALSE)
soaproot_pts_WGS84 <- SpatialPointsDataFrame(coords=soaproot_pts[,c('POINT_X', 'POINT_Y')], data = soaproot_pts['Name'], proj4string = CRS('+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
soaproot_UTM11N_shp <- spTransform(soaproot_pts_WGS84, CRS("+proj=utm +zone=11 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) #project from geographic to WGS84 UTM 11N
resolution <- '5m'
if(resolution == '10m') {NEONterrainDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/NEON 10m/terrain characteristics'
} else if(resolution == '5m') {NEONterrainDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/5m terrain characteristics/5m filtered'} #5m filtered data produced from 7/12/19 arcgis work
NEON_terrain <- stack(list.files(NEONterrainDir, full.names = TRUE))
if(resolution == '10m') {
  NEON_terrain$annsolrad_10m <- NEON_terrain$annsolrad_10m / 1000
  names(NEON_terrain)
  names(NEON_terrain) <- c('solrad_N', 'aspect_N', 'CTI_N', 'curv_mean_N', 'curv_plan_N', 'curv_prof_N', 'elev_N', 'elev_above_str_150', 'EVI_2017_N', 'EVI_2018_N', 'flowacc_N', 'NDVI_2017_N', 'NDVI_2018_N', 'SEI_N', 'slope_N', 'stream_dist_N_100', 'stream_dist_N_150', 'stream_dist_N_200', 'stream_dist_N_300', 'stream_dist_N_400', 'TCI_N')
} else if(resolution == '5m') {
  NEON_terrain$solrad_5m <- NEON_terrain$solrad_5m / 1000
  names(NEON_terrain)
  names(NEON_terrain) <- c('aspect_N', 'CTI_N', 'curv_mean_N', 'curv_plan_N', 'curv_prof_N', 'elev_N', 'elev_above_str_N', 'EVI_2017_N', 'EVI_2018_N', 'IMI_N', 'NDVI_2017_N', 'NDVI_2018_N', 'SEI_N', 'slope_N', 'solrad_N', 'stream_dist_N')
}
NEON_terrain$TPI_N <- terrain(NEON_terrain$elev_N, opt = 'TPI', neighbors = 8)
NEON_terrain$TRI_N <- terrain(NEON_terrain$elev_N, opt = 'TRI', neighbors = 8)
DSD_v2 <- raster(file.path(DSDdir, 'DSDout_V2_1_cropped.tif')) #cropped version derived from DSDout_V2_1.tif'

soaproot_pts_terrain <- extract(NEON_terrain, soaproot_UTM11N_shp, df=TRUE)
soaproot_pts_terrain$ID <- NULL
soaproot_pts_terrain <- cbind(soaproot_UTM11N_shp$Name, soaproot_pts_terrain)
colnames(soaproot_pts_terrain)[1] <- 'Site'
soaproot_pts_terrain$Site <- as.character(soaproot_pts_terrain$Site)
soaproot_pts_terrain$DSD_v2_M <- extract(DSD_v2, soaproot_pts_WGS84) / 10
soaproot_pts_analysis <- soaproot_pts_terrain[grepl('SR.A.', soaproot_pts_terrain$Site), ] #leaves out all rock outcrop points
soaproot_pts_analysis$Depth <- df_sites$Depth[match(soaproot_pts_analysis$Site, df_sites$Site)]
soaproot_pts_analysis <- soaproot_pts_analysis[!is.na(soaproot_pts_analysis$Depth),] #one point had a NA for depth
soaproot_pts_analysis$depth_class <- as.factor(ifelse(soaproot_pts_analysis$Depth < 3.3, 1, ifelse(soaproot_pts_analysis$Depth < 7.56, 2, 3))) #1=shallow; 2=moderate; 3=deep
soaproot_pts_analysis$depth_class_2 <- as.factor(ifelse(soaproot_pts_analysis$Depth < 5.615, 1, 2)) #1=shallow-moderate; 2=deep
colnames(soaproot_pts_analysis)

list.files(file.path(dataDir, 'tina paper'))
k2o_df <- read.csv(file.path(dataDir, 'tina paper', 'k2o_data.csv'), stringsAsFactors = FALSE, na.strings = 'L.N.R.')

dim(k2o_df)
head(k2o_df)
length(unique(k2o_df$Site)) #58
class(k2o_df$K2O_.)
# k2o_df$K2O_. <- as.numeric(k2o_df$K2O_.)
tapply(k2o_df$K2O_., k2o_df$Site, mean, na.rm = TRUE)
k2o_site.means <- as.data.frame(tapply(k2o_df$K2O_., k2o_df$Site, mean, na.rm = TRUE))
k2o_site.means <- aggregate(k2o_df$K2O_. ~ k2o_df$Site, FUN = mean, na.rm = TRUE)
dim(k2o_site.means)
k2o_site.means$`k2o_df$Site`
colnames(k2o_site.means) <- c('Site', 'k2o_mean')
k2o_site.means$Reg_depth <- soaproot_pts_analysis$Depth[match(k2o_site.means$Site, soaproot_pts_analysis$Site)]
plot(k2o_site.means$k2o_mean, k2o_site.means$Reg_depth)
summary(lm(Reg_depth ~ k2o_mean, data = k2o_site.means))

tot_elem_df <- read.csv(file.path(dataDir, 'tina paper', 'Total Element Analysis.csv'), stringsAsFactors = FALSE)
colnames(tot_elem_df)
lapply(tot_elem_df, class)
tot_elem_df$regDepth <- soaproot_pts_analysis$Depth[match(tot_elem_df$Site, soaproot_pts_analysis$Site)]
summary(lm(Depth ~ regDepth, data = tot_elem_df))
summary(lm(CaO ~ Depth + regDepth, data = tot_elem_df))
summary(lm(CaO ~ Depth, data = tot_elem_df))
summary(lm(CaO ~ regDepth, data = tot_elem_df))
plot(tot_elem_df$Depth[tot_elem_df$Site == 'SR.A.39'], tot_elem_df$CaO[tot_elem_df$Site == 'SR.A.39'])
plot(tot_elem_df$Depth[tot_elem_df$Site == 'SR.A.53'], tot_elem_df$CaO[tot_elem_df$Site == 'SR.A.53'])
plot(tot_elem_df$Depth, tot_elem_df$CaO)
plot(tot_elem_df$regDepth, tot_elem_df$CaO)
plot(tot_elem_df$Depth, tot_elem_df$regDepth)
plot(tot_elem_df$Depth, tot_elem_df$Na2O)
plot(tot_elem_df$regDepth, tot_elem_df$Na2O)
summary(lm(Na2O ~ Depth + regDepth, data = tot_elem_df))
summary(lm(Na2O ~ Depth, data = tot_elem_df))
summary(lm(Na2O ~ regDepth, data = tot_elem_df))

aggregate_andsome <- function(x, y) {
  result <- aggregate(y[[x]] ~ y[['Site']], FUN = mean, na.rm = TRUE)
  colnames(result) <- c('Site', x)
  result$Depth <- soaproot_pts_analysis$Depth[match(result$Site, soaproot_pts_analysis$Site)]
  result
}
si_to_al_means <- aggregate_andsome('Si.Al', tot_elem_df)
plot(si_to_al_means$Depth, si_to_al_means$Si.Al)
summary(lm(Si.Al ~ Depth, data = si_to_al_means))

k_means <- aggregate_andsome('K2O', tot_elem_df)
k_means
plot(k_means$Depth, k_means$K2O)
summary(lm(K2O ~ Depth, data = k_means)) #positive but not sig

al_means <- aggregate_andsome('Al2O3', tot_elem_df)
plot(al_means$Depth, al_means$Al2O3)
summary(lm(Al2O3 ~ Depth, data = al_means)) #pos, nearly sig

si_means <- aggregate_andsome('SiO2', tot_elem_df)
plot(si_means$Depth, si_means$SiO2)
summary(lm(SiO2 ~ Depth, data = si_means))

ca_means <- aggregate_andsome('CaO', tot_elem_df)
plot(ca_means$Depth, ca_means$CaO)
summary(lm(CaO ~ Depth, data = ca_means))
tiff(file = file.path(FiguresDir, 'stream_dist_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
vioplot(soaproot_pts_analysis$str_dist[soaproot_pts_analysis$depth_class==1], soaproot_pts_analysis$str_dist[soaproot_pts_analysis$depth_class==2], soaproot_pts_analysis$str_dist[soaproot_pts_analysis$depth_class==3], xlab=c('Regolith depth class (m)'), ylab='Distance from channel (m)', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'))
dev.off()


mg_means <- aggregate_andsome('MgO', tot_elem_df)
plot(mg_means$Depth, mg_means$MgO)
text(mg_means$Depth, mg_means$MgO, mg_means$Site, pos = 1, offset = 0.5)
summary(lm(MgO ~ Depth, data = mg_means[mg_means$Site != 'SR.A.51' & mg_means$Site != 'SR.A.55',])) #2 outliers: 51 and 55

na_means <- aggregate_andsome('Na2O', tot_elem_df)
plot(na_means$Depth, na_means$Na2O)
summary(lm(Na2O ~ Depth, data = na_means))

elem_by_site <- data.frame(Site=ca_means$Site, Depth=ca_means$Depth, CaO=ca_means$CaO, MgO=mg_means$MgO, K2O=k_means$K2O, Na2O=na_means$Na2O, Al2O3=al_means$Al2O3, SiO2=si_means$SiO2)

