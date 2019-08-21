#read-in and prepare data and define directories, now using read-in_data.R first
modelResults <- 'C:/Users/smdevine/Desktop/post doc/czo work/model_results'
if (resolution=='5m') {
  lapply(soaproot_pts_analysis[,2:22], function(x) {summary(aov(x ~ soaproot_pts_analysis$depth_class))}) 
} else if (resolution=='10m') {
  lapply(soaproot_pts_analysis[,2:17], function(x) {summary(aov(x ~ soaproot_pts_analysis$depth_class))})
}
if (resolution=='5m') {
  lapply(soaproot_pts_analysis[,2:22], function(x) {summary(lm(x ~ soaproot_pts_analysis$Depth))})
} else if (resolution=='10m') {
  lapply(soaproot_pts_analysis[,2:17], function(x) {summary(lm(x ~ soaproot_pts_analysis$Depth))})
}
plot(aov(CTI_N ~ depth_class, data = soaproot_pts_analysis))
boxplot(CTI_N ~ depth_class, data = soaproot_pts_analysis)

mapply(function(x,y,z='Depth')
{lm_result <- lm(soaproot_pts_analysis[[z]] ~ x)
plot(x, soaproot_pts_analysis[[z]], main=paste(y, 'r2 = ', round(summary(lm_result)$r.squared, 2)))
abline(lm_result, lty=2)}, x=soaproot_pts_analysis[,2:22], y=colnames(soaproot_pts_analysis)[2:22])
summary(lm(Depth ~ poly(TWI_N, 2), data = soaproot_pts_analysis))
summary(lm(log(Depth) ~ TWI_N, data = soaproot_pts_analysis))
lapply(soaproot_pts_analysis[soaproot_pts_analysis$Depth < 7.56, 2:26], function(x) {summary(aov(x ~ soaproot_pts_analysis$depth_class[soaproot_pts_analysis$Depth < 7.56]))})

#tukey test
aov_dist_twi <- aov(TWI_N ~ depth_class, data = soaproot_pts_analysis)
TukeyHSD(aov_dist_twi)

aov_dist_channel <- aov(str_dist_N ~ depth_class, data = soaproot_pts_analysis)
TukeyHSD(aov_dist_channel)

#this more applicable to <7.56 m dataset
lapply(soaproot_pts_analysis[soaproot_pts_analysis$Depth < 7.56, 2:22], function(x) {summary(lm(x ~ soaproot_pts_analysis$depth_class[soaproot_pts_analysis$Depth < 7.56]))})
mapply(function(x,y,z='Depth') 
{lm_result <- lm(soaproot_pts_analysis[[z]][soaproot_pts_analysis$Depth < 7.56] ~ x)
plot(x, soaproot_pts_analysis[[z]][soaproot_pts_analysis$Depth < 7.56], main=paste(y, 'r2 = ', round(summary(lm_result)$r.squared, 2)))
abline(lm_result, lty=2)}, x=soaproot_pts_analysis[soaproot_pts_analysis$Depth < 7.56, 2:19], y=colnames(soaproot_pts_analysis)[2:19])

#MLR model selection exercise
synthetic_7.56m_gamma <- function(shape, scale) {
  success <- FALSE
  while (!success) {
    x <- rgamma(1, shape = shape, scale = scale)
    # check for success
    success <- x > 7.56
  }
  return(x)
}
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}
library(dismo)
set.seed(80140)
dim(soaproot_pts_analysis)
kf <- kfold(1:nrow(soaproot_pts_analysis), k=10)
colnames(soaproot_pts_analysis)[c(2:5,8:10,12,15)]
crossval_lm <- function(df_pts, varname, model=' "elev_N" + "solrad_N" + "slope_N" + "curv_mean_N" + "TWI_N" + "str_dist_N" + "EVI_2017_N" + "DSD_v2_M" + "NDVI_2015"', n) {
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
if (resolution=='5m') {
  subset1 <- expand.grid(elev_N=c(TRUE, FALSE), slope_N=c(TRUE, FALSE), EVI_2017_N=c(TRUE, FALSE), OCdist_R=c(TRUE, FALSE), ndvi2012_2015avg=c(TRUE, FALSE))
  subset2 <- expand.grid(stream_dist_N_150=c(TRUE, FALSE), slope_N=c(TRUE, FALSE), EVI_2017_N=c(TRUE, FALSE), OCdist_R=c(TRUE, FALSE), DSD_v2_M=c(TRUE, FALSE))
  subset3 <- expand.grid(stream_dist_N_150=c(TRUE, FALSE), EVI_2017_N=c(TRUE, FALSE), curv_mean_N=c(TRUE, FALSE), DSD_v2_M=c(TRUE, FALSE), SEI_N=c(TRUE, FALSE))
  subset4 <- expand.grid(stream_dist_N_150=c(TRUE, FALSE), EVI_2017_N=c(TRUE, FALSE), TPI_N=c(TRUE, FALSE), solrad_N=c(TRUE, FALSE), OCdist_R=c(TRUE, FALSE))
  subset5 <- expand.grid(CTI_N=c(TRUE, FALSE), curv_prof_N=c(TRUE, FALSE), EVI_2017_N=c(TRUE, FALSE), SEI_N=c(TRUE, FALSE), ndvi2012_2015avg=c(TRUE, FALSE))
} else if (resolution=='10m') {
  subset1 <- expand.grid(elev_N=c(TRUE, FALSE), solrad_N=c(TRUE, FALSE), slope_N=c(TRUE, FALSE), curv_mean_N=c(TRUE, FALSE), TWI_N=c(TRUE, FALSE), str_dist_N=c(TRUE, FALSE), EVI_2017_N=c(TRUE, FALSE), DSD_v2_M=c(TRUE, FALSE), NDVI_2015=c(TRUE, FALSE))
}
colnames(soaproot_pts_analysis)
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
  result <- model_selection_MLR(df=soaproot_pts_analysis, depth = 'gamma_syn', varname = 'Depth_log', varDir1 = varDir1, varDir2 = varDir2, n=66, model_df = 1:9, var_subset = var_subset)
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
finalize_analysis(runs=1:1000, varDir1='10m_res_9var', var_subset = subset1)
# finalize_analysis(runs=1:30, varDir1='subset_2', var_subset = subset2)
# finalize_analysis(runs=1:30, varDir1='subset_3', var_subset = subset3)
# finalize_analysis(runs=1:30, varDir1='subset_4', var_subset = subset4)
# finalize_analysis(runs=1:30, varDir1='subset_5', var_subset = subset5)

library(randomForest)
tuneRF(x=soaproot_pts_analysis[ , colnames(subset1)], y=soaproot_pts_analysis$depth_class, ntreeTry = 400, stepFactor = 1, improve = 0.02, trace = TRUE)
rf_all <- randomForest(x=soaproot_pts_analysis[ ,colnames(subset1)], y=soaproot_pts_analysis$depth_class, mtry = 3, ntree = 200)
rf_all$importance
rf_all$confusion
1 - (rf_all$confusion[1,1] + rf_all$confusion[2,2] + rf_all$confusion[3,3]) / 66 
avg_error <- vector(mode = 'numeric', length = 1000)
confusion_results <- vector(mode = 'list', length = 1000)

for (i in 1:1000) {
  rf_all <- randomForest(x=soaproot_pts_analysis[ ,colnames(subset1)], y=soaproot_pts_analysis$depth_class, mtry = 3, ntree = 200)
  avg_error[i] <- 1 - (rf_all$confusion[1,1] + rf_all$confusion[2,2] + rf_all$confusion[3,3]) / 66
  confusion_results[[i]] <- rf_all$confusion[,1:3]
}

mat_1_1 <- sapply(confusion_results, function(x) {
  x[1,1]
})
mat_1_2 <- sapply(confusion_results, function(x) {
  x[1,2]
})
mat_1_3 <- sapply(confusion_results, function(x) {
  x[1,3]
})
mat_2_1 <- sapply(confusion_results, function(x) {
  x[2,1]
})
mat_2_2 <- sapply(confusion_results, function(x) {
  x[2,2]
})
mat_2_3 <- sapply(confusion_results, function(x) {
  x[2,3]
})
mat_3_1 <- sapply(confusion_results, function(x) {
  x[3,1]
})
mat_3_2 <- sapply(confusion_results, function(x) {
  x[3,2]
})
mat_3_3 <- sapply(confusion_results, function(x) {
  x[3,3]
})
mean_confusion_matrix <- matrix(data = c(mean(mat_1_1), mean(mat_2_1), mean(mat_3_1), mean(mat_1_2), mean(mat_2_2), mean(mat_3_2), mean(mat_1_3), mean(mat_2_3), mean(mat_3_3)), nrow = 3, ncol = 3)
sd_confusion_matrix <- matrix(data = c(sd(mat_1_1), sd(mat_2_1), sd(mat_3_1), sd(mat_1_2), sd(mat_2_2), sd(mat_3_2), sd(mat_1_3), sd(mat_2_3), sd(mat_3_3)), nrow = 3, ncol = 3)
range_confusion_matrix <- matrix(data = c(paste(range(mat_1_1), collapse = '_'), paste(range(mat_2_1), collapse = '_'), paste(range(mat_3_1), collapse = '_'), paste(range(mat_1_2), collapse = '_'), paste(range(mat_2_2), collapse = '_'), paste(range(mat_3_2), collapse = '_'), paste(range(mat_1_3), collapse = '_'), paste(range(mat_2_3), collapse = '_'), paste(range(mat_3_3), collapse = '_')), nrow = 3, ncol = 3)
write.csv(mean_confusion_matrix, file.path(TablesDir, '10 m analysis', 'rf_9vars_mean_confusion_matrix.csv'))
write.csv(sd_confusion_matrix, file.path(TablesDir, '10 m analysis', 'rf_9vars_sd_confusion_matrix.csv'))
write.csv(range_confusion_matrix, file.path(TablesDir, '10 m analysis', 'rf_9vars_range_confusion_matrix.csv'))
range(avg_error) #40.9 - 62.1
mean(avg_error) #52.5%
range(1 - mat_1_1/19) #shallow: 42.1 - 73.4%
range(1 - mat_2_2/20) #moderate: 45 - 90%
range(1- mat_3_3/27) #deep: 18.5% - 51.9%

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
