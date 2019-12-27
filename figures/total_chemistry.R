library(raster)
dataDir <- 'C:/Users/smdevine/Desktop/post doc/czo work'
FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/results/figures'
library(vioplot)
library(extrafont)
library(extrafontdb)
#font_import() only needs to be done once
loadfonts()
res_plots <- 800
#use read-in_data.R to get other necessary soaproot data
list.files(file.path(dataDir, 'tina paper'))
tot_elem_df <- read.csv(file.path(dataDir, 'tina paper', 'Total Element Analysis.csv'), stringsAsFactors = FALSE)
colnames(tot_elem_df)
lapply(tot_elem_df, class)
tot_elem_df$regDepth <- soaproot_pts_analysis$Depth[match(tot_elem_df$Site, soaproot_pts_analysis$Site)]

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
plot(k_means$Depth, k_means$K2O)
text(k_means$Depth, k_means$K2O, k_means$Site)
k_means <- k_means[k_means$Site != 'SR.A.02',]
summary(lm(K2O ~ Depth, data = k_means)) #positive but not sig

al_means <- aggregate_andsome('Al2O3', tot_elem_df)
plot(al_means$Depth, al_means$Al2O3)
text(al_means$Depth, al_means$Al2O3, al_means$Site) #SR.A.41 is clearly outlier
al_means <- al_means[al_means$Site != 'SR.A.41',]
summary(lm(Al2O3 ~ Depth, data = al_means)) #pos, nearly sig

si_means <- aggregate_andsome('SiO2', tot_elem_df)
plot(si_means$Depth, si_means$SiO2)
text(si_means$Depth, si_means$SiO2, si_means$Site)
si_means <- si_means[si_means$Site != 'SR.A.51' & si_means$Site != 'SR.A.55', ]
summary(lm(SiO2 ~ Depth, data = si_means))

ca_means <- aggregate_andsome('CaO', tot_elem_df)
plot(ca_means$Depth, ca_means$CaO)
text(ca_means$Depth, ca_means$CaO, ca_means$Site)
ca_means <- ca_means[ca_means$Site != 'SR.A.51' & ca_means$Site != 'SR.A.55', ]
summary(lm(CaO ~ Depth, data = ca_means))

mg_means <- aggregate_andsome('MgO', tot_elem_df)
plot(mg_means$Depth, mg_means$MgO)
text(mg_means$Depth, mg_means$MgO, mg_means$Site, pos = 1, offset = 0.5)
summary(lm(MgO ~ Depth, data = mg_means[mg_means$Site != 'SR.A.51' & mg_means$Site != 'SR.A.55',])) #2 outliers: 51 and 55
mg_means <- mg_means[mg_means$Site != 'SR.A.51' & mg_means$Site != 'SR.A.55', ]

na_means <- aggregate_andsome('Na2O', tot_elem_df)
plot(na_means$Depth, na_means$Na2O)
summary(lm(Na2O ~ Depth, data = na_means))

fe_means <- aggregate_andsome('Fe2O3', tot_elem_df)
plot(fe_means$Depth, fe_means$Fe2O3)
text(fe_means$Depth, fe_means$Fe2O3, fe_means$Site)
fe_means <- fe_means[fe_means$Site != 'SR.A.51' & fe_means$Site != 'SR.A.55', ]

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
plot_the_vioplot('MgO_vioplots_v2.tif', mg_means, 'MgO', 'MgO (%)')
plot_the_vioplot('Na2O_vioplots.tif', na_means, 'Na2O', expression('Na'[2]*'O (%)'))
plot_the_vioplot('Al2O3_vioplots.tif', al_means, 'Al2O3', expression('Al'[2]*'O'[3]~'(%)'))
plot_the_vioplot('Fe2O3_vioplots.tif', fe_means, 'Fe2O3', expression('Fe'[2]*'O'[3]~'(%)'))
plot_the_vioplot('SiO2_vioplots.tif', si_means, 'SiO2', expression('SiO'[2]~'(%)'))
