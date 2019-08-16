library(raster)
dataDir <- 'C:/Users/smdevine/Desktop/post doc/czo work'
landsat8Dir <- 'C:/Users/smdevine/Desktop/post doc/czo work/landsat8/summaries/finals'
DSDdir <- 'C:/Users/smdevine/Desktop/post doc/czo work/DSD Mike'
FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/results/figures'
#use read-in_data.R to get other necessary soaproot data
list.files(file.path(dataDir, 'tina paper'))
k2o_df <- read.csv(file.path(dataDir, 'tina paper', 'k2o_data.csv'), stringsAsFactors = FALSE, na.strings = 'L.N.R.')

dim(k2o_df)
head(k2o_df)
length(unique(k2o_df$Site)) #58
table(k2o_df$Site)
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
  result$depth_class <- soaproot_pts_analysis$depth_class[match(result$Site, soaproot_pts_analysis$Site)]
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

mg_means <- aggregate_andsome('MgO', tot_elem_df)
plot(mg_means$Depth, mg_means$MgO)
text(mg_means$Depth, mg_means$MgO, mg_means$Site, pos = 1, offset = 0.5)
summary(lm(MgO ~ Depth, data = mg_means[mg_means$Site != 'SR.A.51' & mg_means$Site != 'SR.A.55',])) #2 outliers: 51 and 55
mg_means <- mg_means[mg_means$Site != 'SR.A.51' & mg_means$Site != 'SR.A.55',]

na_means <- aggregate_andsome('Na2O', tot_elem_df)
plot(na_means$Depth, na_means$Na2O)
summary(lm(Na2O ~ Depth, data = na_means))

elem_by_site <- data.frame(Site=ca_means$Site, Depth=ca_means$Depth, CaO=ca_means$CaO, MgO=mg_means$MgO, K2O=k_means$K2O, Na2O=na_means$Na2O, Al2O3=al_means$Al2O3, SiO2=si_means$SiO2)


tiff(file = file.path(FiguresDir, 'total chemistry', 'Ca_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
vioplot(ca_means$CaO[ca_means$depth_class==1], ca_means$CaO[ca_means$depth_class==2], ca_means$CaO[ca_means$depth_class==3], xlab='Regolith depth class (m)', ylab='Calcium oxide (%)', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'))
dev.off()

plot_the_vioplot <- function(fname, df, varname, ylabel) {
  tiff(file = file.path(FiguresDir, 'total chemistry', fname), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
  par(mar=c(4.5, 4.5, 0.5, 0.5))
  vioplot(df[[varname]][df$depth_class==1], df[[varname]][df$depth_class==2], df[[varname]][df$depth_class==3], xlab='Regolith depth class (m)', ylab=ylabel, names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'))
  dev.off()
}
#expression('title'[2]
plot_the_vioplot('CaO_vioplots.tif', ca_means, 'CaO', 'CaO (%)')
plot_the_vioplot('K2O_vioplots.tif', k_means, 'K2O', expression('K'[2]*'O (%)'))
plot_the_vioplot('MgO_vioplots.tif', mg_means, 'MgO', 'MgO (%)')
plot_the_vioplot('Na2O_vioplots.tif', na_means, 'Na2O', expression('Na'[2]*'O (%)'))
plot_the_vioplot('Al2O3_vioplots.tif', al_means, 'Al2O3', expression('Al'[2]*'O'[3]~'(%)'))