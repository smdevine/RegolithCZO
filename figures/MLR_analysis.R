library(raster)
dataDir <- 'C:/Users/smdevine/Desktop/post doc/czo work'
landsat8Dir <- 'C:/Users/smdevine/Desktop/post doc/czo work/landsat8/summaries/finals'
FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/results/figures'
#read-in and prepare data
TablesDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/results/tables'
#merge with depth data
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
soaproot_pts_terrain <- extract(NEON_terrain, soaproot_UTM11N_shp, df=TRUE)
soaproot_pts_terrain$ID <- NULL
soaproot_pts_terrain <- cbind(soaproot_UTM11N_shp$Name, soaproot_pts_terrain)
colnames(soaproot_pts_terrain)[1] <- 'Site'
soaproot_pts_terrain$Site <- as.character(soaproot_pts_terrain$Site)

NDVI_landsat8 <- read.csv(file.path(landsat8Dir, 'NDVI_2013_2017_final.csv'), row.names = 1)
NDVI_landsat8$doy_20160121_ndvi <- NULL
NDVI_landsat8$Site <- row.names(NDVI_landsat8)
NDVI_landsat8$Depth <- df_sites$Depth[match(NDVI_landsat8$Site, df_sites$Site)] #they are both in same order but this is safer nonetheless
include_OC <- FALSE
NDVI_landsat8 <- NDVI_landsat8[!is.na(NDVI_landsat8$Depth), ]
if (include_OC) {} else {
  NDVI_landsat8 <- NDVI_landsat8[NDVI_landsat8$Depth!=0,]
}
NDVI_dates <- as.Date(sapply(colnames(NDVI_landsat8)[1:60], function(x) substr(x, 5, 12)), format = '%Y%m%d')
NDVI_by_year <- apply(NDVI_landsat8[,1:60], 1, function(x) tapply(x, format.Date(NDVI_dates, '%Y'), mean))
NDVI_by_year <- as.data.frame(t(NDVI_by_year))
colnames(NDVI_by_year) <- paste0('NDVI_', colnames(NDVI_by_year))
NDVI_by_year$Site <- row.names(NDVI_by_year)

soaproot_pts_analysis <- soaproot_pts_terrain[grepl('SR.A.', soaproot_pts_terrain$Site), ] #leaves out all rock outcrop points
#merge with annual NDVI means
soaproot_pts_analysis <- merge(soaproot_pts_analysis, NDVI_by_year, by='Site')
soaproot_pts_analysis$Depth <- df_sites$Depth[match(soaproot_pts_analysis$Site, df_sites$Site)]
soaproot_pts_analysis <- soaproot_pts_analysis[!is.na(soaproot_pts_analysis$Depth),] #one point had a NA for depth

soaproot_pts_analysis$depth_class <- as.factor(ifelse(soaproot_pts_analysis$Depth < 3.3, 1, ifelse(soaproot_pts_analysis$Depth < 7.56, 2, 3))) #1=shallow; 2=moderate; 3=deep
soaproot_pts_analysis$depth_class_2 <- as.factor(ifelse(soaproot_pts_analysis$Depth < 5.615, 1, 2)) #1=shallow-moderate; 2=deep
colnames(soaproot_pts_analysis)
write.csv(soaproot_pts_analysis, file = file.path(TablesDir, 'terrain_veg_chars_vs Depth_5m_res.csv'), row.names = FALSE)


if (resolution=='5m') {
  lapply(soaproot_pts_analysis[,2:24], function(x) {summary(aov(x ~ soaproot_pts_analysis$depth_class))}) } else if (resolution=='10m') {
  lapply(soaproot_pts_analysis[,2:30], function(x) {summary(aov(x ~ soaproot_pts_analysis$depth_class))})
}
if (resolution=='5m') {
  lapply(soaproot_pts_analysis[,2:24], function(x) {summary(lm(x ~ soaproot_pts_analysis$Depth))})
} else if (resolution=='10m') {
  lapply(soaproot_pts_analysis[,2:30], function(x) {summary(lm(x ~ soaproot_pts_analysis$Depth))})
}
mapply(function(x,y,z='Depth') 
{lm_result <- lm(soaproot_pts_analysis[[z]] ~ x)
plot(x, soaproot_pts_analysis[[z]], main=paste(y, 'r2 = ', round(summary(lm_result)$r.squared, 2)))
abline(lm_result, lty=2)}, x=soaproot_pts_analysis[,2:25], y=colnames(soaproot_pts_analysis)[2:25])

lapply(soaproot_pts_analysis[soaproot_pts_analysis$Depth < 7.56, 2:25], function(x) {summary(aov(x ~ soaproot_pts_analysis$depth_class[soaproot_pts_analysis$Depth < 7.56]))})
mapply(function(x,y,z='Depth') 
{lm_result <- lm(soaproot_pts_analysis[[z]][soaproot_pts_analysis$Depth < 7.56] ~ x)
plot(x, soaproot_pts_analysis[[z]][soaproot_pts_analysis$Depth < 7.56], main=paste(y, 'r2 = ', round(summary(lm_result)$r.squared, 2)))
abline(lm_result, lty=2)}, x=soaproot_pts_analysis[soaproot_pts_analysis$Depth < 7.56, 2:19], y=colnames(soaproot_pts_analysis)[2:19])

#MLR model selection exercise
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}
library(dismo)
set.seed(80140)
dim(soaproot_pts_analysis)
kf <- kfold(1:nrow(soaproot_pts_analysis), k=10)
colnames(soaproot_pts_analysis)
crossval_lm <- function(df_pts, varname, model='~ stream_dist_N + CTI_N + curv_prof_N + NDVI_2014 + NDVI_2015 + NDVI_2016 + NDVI_2017 + EVI_2017_N + EVI_2018_N + elev_N + slope_N + solrad_N', n) {
  rmse <- rep(NA, length(unique(kf)))
  predictions <- rep(NA, n)
  for (k in 1:length(unique(kf))) {
    tst <- df_pts[kf == k, ]
    trn <- df_pts[kf != k, ]
    varname_lm <- lm(as.formula(paste(varname, model)), data = trn)
    #print(summary(varname_lm))
    varname_tst_pred <- predict.lm(varname_lm, tst)
    rmse[k] <- RMSE(tst[[varname]], varname_tst_pred)
    predictions[kf == k] <- varname_tst_pred
  }
  print(summary(lm(df_pts[[varname]] ~ predictions)))
  list(rmse.kfold=rmse, oob.predictions=predictions)
}
#need to update subsets based on 5m varnames
if (resolution=='10m') {
  subset1 <- expand.grid(elev_N=c(TRUE, FALSE), slope_N=c(TRUE, FALSE), EVI_2017_N=c(TRUE, FALSE), OCdist_R=c(TRUE, FALSE), ndvi2012_2015avg=c(TRUE, FALSE))
  subset2 <- expand.grid(stream_dist_N_150=c(TRUE, FALSE), slope_N=c(TRUE, FALSE), EVI_2017_N=c(TRUE, FALSE), OCdist_R=c(TRUE, FALSE), DSD_v2_M=c(TRUE, FALSE))
  subset3 <- expand.grid(stream_dist_N_150=c(TRUE, FALSE), EVI_2017_N=c(TRUE, FALSE), curv_mean_N=c(TRUE, FALSE), DSD_v2_M=c(TRUE, FALSE), SEI_N=c(TRUE, FALSE))
  subset4 <- expand.grid(stream_dist_N_150=c(TRUE, FALSE), EVI_2017_N=c(TRUE, FALSE), TPI_N=c(TRUE, FALSE), solrad_N=c(TRUE, FALSE), OCdist_R=c(TRUE, FALSE))
  subset5 <- expand.grid(CTI_N=c(TRUE, FALSE), curv_prof_N=c(TRUE, FALSE), EVI_2017_N=c(TRUE, FALSE), SEI_N=c(TRUE, FALSE), ndvi2012_2015avg=c(TRUE, FALSE))
} else if {
  
}

model_selection_MLR <- function(df, varname, depth, varDir1, varDir2, n, model_df, var_subset) {
  if(!dir.exists(file.path(modelResults, 'MLR_model_selection', varDir1))) {
    dir.create(file.path(modelResults, 'MLR_model_selection', varDir1))
  }
  if(!dir.exists(file.path(modelResults, 'MLR_model_selection', varDir1, varDir2))) {
    dir.create(file.path(modelResults, 'MLR_model_selection', varDir1, varDir2))
  }
  models_to_test <- var_subset
  models_to_test <- models_to_test[1:(nrow(models_to_test)-1), ]
  model_selection_results <- vector(mode='list', length=nrow(models_to_test))
  #print(length(model_selection_results[1]))
  for (i in 1:nrow(models_to_test)) {
    print(i)
    model_to_test <- paste(colnames(models_to_test)[unlist(models_to_test[i,])], collapse = ' + ')
    model_selection_results[[i]] <- crossval_lm(df, varname, model = paste('~ ', model_to_test))
    print(mean(model_selection_results[[i]]$rmse.kfold))
  }
  mean_RMSEs <- unlist(lapply(model_selection_results, function(x) mean(x$rmse.kfold))) #mean cross-validated RMSE by n tested models
  mean_RMSEs_all <- cbind(as.data.frame(1:length(unique(kf))), do.call(cbind, lapply(model_selection_results, function(x) x$rmse.kfold))) #mean RMSE for each k-fold iteration of n tested models; cbind when one at least column is a data.frame returns a data.frame
  colnames(mean_RMSEs_all) <- c('kfold', paste0('model_', 1:nrow(models_to_test)))
  oob_predictions_all <- cbind(df['Site'], do.call(cbind, lapply(model_selection_results, function(x) x$oob.predictions)))
  colnames(oob_predictions_all)[2:ncol(oob_predictions_all)] <- paste0('model_', 1:nrow(models_to_test))
  #make summary
  df_model <- apply(models_to_test, 1, sum)
  meanRMSEs <- apply(mean_RMSEs_all[,2:ncol(mean_RMSEs_all)], 2, mean)
  oob_r2s <- apply(oob_predictions_all[,2:ncol(oob_predictions_all)], 2, function(x) summary(lm(df[[varname]] ~ x))$r.squared)
  summary <- data.frame(model=apply(models_to_test, 1, function(x) paste(colnames(models_to_test)[x], collapse = ' + ')), meanRMSE=meanRMSEs, oob_r.squared=oob_r2s, df_model=df_model)
  summary_by_model_df <- split(summary, summary$df_model)
  best_models <- unlist(lapply(summary_by_model_df, function(x) as.character(x$model[which.min(x$meanRMSE)])))
  best_rmses <- unlist(lapply(summary_by_model_df, function(x) min(x$meanRMSE)))
  best_oobs_r2 <- unlist(lapply(summary_by_model_df, function(x) x$oob_r.squared[which.min(x$meanRMSE)]))
  final_summary <- data.frame(model_df=model_df, model_name=best_models, meanRMSE_20foldCV=best_rmses, OOB_r2=best_oobs_r2)
  write.csv(mean_RMSEs_all, file.path(modelResults, 'MLR_model_selection', varDir1, varDir2, paste(varname, '_', depth, '_MLR_selection_CV_RMSEs.csv')), row.names = FALSE)
  write.csv(oob_predictions_all, file.path(modelResults, 'MLR_model_selection', varDir1, varDir2, paste(varname, '_', depth, '_MLR_selection_oob_pred.csv')), row.names = FALSE)
  write.csv(models_to_test, file.path(modelResults, 'MLR_model_selection', varDir1, varDir2, paste(varname, '_', depth, '_MLR_selection_model_test_grid.csv')), row.names = FALSE)
  write.csv(final_summary, file.path(modelResults, 'MLR_model_selection', varDir1, varDir2, paste(varname, '_', depth, '_MLR_selection_BEST_models.csv')), row.names = FALSE)
  final_summary
  #list(RMSEs=mean_RMSEs_all, OOBs=oob_predictions_all, test_grid=models_to_test, best_models=final_summary)
}
#model_selection_no_OC <- model_selection_MLR(df=soaproot_pts_analysis_no_OC, depth = 'No_OC', varname = 'Depth', varDir = 'Depth', n=40)

#run different versions of depths > 7.56 m
run_analysis <- function(version_no, varDir1, var_subset) {
  varDir2 <- paste0('gamma_syn_great7.5m_', version_no)
  soaproot_pts_analysis$Depth[soaproot_pts_analysis$Depth > 7.56] <- 7.56
  for(i in 1:length(soaproot_pts_analysis$Depth)) {
    if(soaproot_pts_analysis$Depth[i] == 7.56) {
      soaproot_pts_analysis$Depth[i] <- synthetic_7.56m_gamma(shape = 1.915, scale=1/0.273) #when four OCs with depth =0.01 are included: meanlog=1.705, sdlog=1.123; see excel file
    } else {next}
  }
  #hist(soaproot_pts_analysis$Depth)
  soaproot_pts_analysis$Depth_log <- log(soaproot_pts_analysis$Depth) 
  result <- model_selection_MLR(df=soaproot_pts_analysis, depth = 'gamma_syn', varname = 'Depth_log', varDir1 = varDir1, varDir2 = varDir2, n=66, model_df = 1:5, var_subset = var_subset)
  write.csv(soaproot_pts_analysis[,c('Site', 'Depth')], file.path(modelResults, 'MLR_model_selection', varDir1, varDir2, paste0('soaproot_pts_syndepths_', version_no, '.csv')), row.names = FALSE)
  result
}
#run_analysis(version_no = 'v1', varDir1 = 'subset_1')
finalize_analysis <- function(runs, varDir1, var_subset) { #runs is a vector how many times we want to repeat the assignment of random depths > 7.56 with MLR testing (eg. 1:30 would be 30 times)
  overall_summary <- do.call(cbind, lapply(paste0('v', runs), function(x) {
    run_analysis(x, varDir1 = varDir1, var_subset = var_subset)
  }))
  write.csv(overall_summary, file.path(modelResults, 'MLR_model_selection', varDir1, 'best_models_summary.csv'), row.names = FALSE)
}
finalize_analysis(runs=1:30, varDir1='subset_1', var_subset = subset1)
finalize_analysis(runs=1:30, varDir1='subset_2', var_subset = subset2)
finalize_analysis(runs=1:30, varDir1='subset_3', var_subset = subset3)
finalize_analysis(runs=1:30, varDir1='subset_4', var_subset = subset4)
finalize_analysis(runs=1:30, varDir1='subset_5', var_subset = subset5)

library(randomForest)
tuneRF(x=soaproot_pts_analysis[ ,3:19], y=soaproot_pts_analysis$depth_class, ntreeTry = 100, stepFactor = 1, improve = 0.02, trace = TRUE)
rf_all <- randomForest(x=soaproot_pts_analysis[ ,3:19], y=soaproot_pts_analysis$depth_class, mtry = 4, ntree = 500)
which(rf_all$importance > 2)
indices_cols <- c(3, 6:10, 12:13, 17)
tuneRF(x=soaproot_pts_analysis[ ,indices_cols], y=soaproot_pts_analysis$depth_class, ntreeTry = 100, stepFactor = 1, improve = 0.02, trace = TRUE)
rf_subset <- randomForest(x=soaproot_pts_analysis[ ,indices_cols], y=soaproot_pts_analysis$depth_class, mtry = 4, ntree = 150)
rf_subset
rf_subset$importance
tuneRF(x=soaproot_pts_analysis[ ,indices_cols], y=soaproot_pts_analysis$depth_class_2, ntreeTry = 200, stepFactor = 1, improve = 0.02, trace = TRUE)
rf_subset_2class <- randomForest(x=soaproot_pts_analysis[ ,indices_cols], y=soaproot_pts_analysis$depth_class_2, mtry = 4, ntree = 100)
rf_subset_2class
rf_all_2class <- randomForest(x=soaproot_pts_analysis[ ,3:19], y=soaproot_pts_analysis$depth_class_2, mtry = 4, ntree = 100)
rf_all_2class

#Random Forest cross-validation exercise

crossval_RF <- function(df_pts, varname, ntree=75, model='~ curvature_mean + slope + annual_kwh.m2 + elevation') { #varnames=c("curvature_mean", "slope", "annual_kwh.m2", "elevation")
  rmse <- rep(NA, 20) 
  predictions <- rep(NA, 105)
  for (k in 1:20) {
    tst <- df_pts[kf == k, ]
    trn <- df_pts[kf != k, ]
    model_vector <- gsub('[+]', '', model)
    model_vector <- gsub('~ ', '', model_vector)
    model_vector <- unlist(strsplit(model_vector, '  '))
    mtry_tuned <- tuneRF(x=as.data.frame(df_pts)[ ,model_vector], df_pts[[varname]], ntreeTry = 100, stepFactor = 1, improve = 0.02, trace = FALSE)[1]
    #print(mtry_tuned)
    RF_varname <- randomForest(as.formula(paste(varname, model)), data = trn, mtry=mtry_tuned, ntree=ntree)
    varname_tst_pred <- predict(RF_varname, tst)
    rmse[k] <- RMSE(tst[[varname]], varname_tst_pred)
    predictions[kf == k] <- varname_tst_pred
    #plot(RF_varname)
  }
  print(summary(lm(df_pts[[varname]] ~ predictions)))
  list(rmse.kfold=rmse, oob.predictions=predictions)
}
