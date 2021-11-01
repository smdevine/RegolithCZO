library(car)
library(onewaytests)
library(userfriendlyscience)
#use read-in data first
#run levene Test
if (resolution=='5m') {
  lapply(soaproot_pts_analysis[,2:22], function(x) {leveneTest(x ~ soaproot_pts_analysis$depth_class)}) 
} else if (resolution=='10m') {
  lapply(soaproot_pts_analysis[,2:17], function(x) {leveneTest(x ~ soaproot_pts_analysis$depth_class)})
}

#anova
if (resolution=='5m') {
  lapply(soaproot_pts_analysis[,2:22], function(x) {summary(aov(x ~ soaproot_pts_analysis$depth_class))}) 
} else if (resolution=='10m') {
  lapply(soaproot_pts_analysis[,2:17], function(x) {summary(aov(x ~ soaproot_pts_analysis$depth_class))})
}

#take closer look at TWI
qqPlot(soaproot_pts_analysis$TWI_N[soaproot_pts_analysis$depth_class==1])
qqPlot(soaproot_pts_analysis$TWI_N[soaproot_pts_analysis$depth_class==2]) #close to not normal
qqPlot(soaproot_pts_analysis$TWI_N[soaproot_pts_analysis$depth_class==3])
leveneTest(TWI_N ~ depth_class, data = soaproot_pts_analysis) #p=0.001
welch.test(TWI_N ~ depth_class, data = soaproot_pts_analysis) #Welch's Heteroscedastic F Test (alpha = 0.05); p=0.03
aov_twi <- aov(TWI_N ~ depth_class, data = soaproot_pts_analysis)
oneway(y=soaproot_pts_analysis$TWI_N, x=soaproot_pts_analysis$depth_class, posthoc = 'games-howell') #3 vs. 1 significant (p=0.02); 2 vs. 1 notable (p=0.06)
#summary(aov_twi) #p=0.003
TukeyHSD(aov_twi)

#stream distance
qqPlot(soaproot_pts_analysis$str_dist_N[soaproot_pts_analysis$depth_class==1]) #ok
qqPlot(soaproot_pts_analysis$str_dist_N[soaproot_pts_analysis$depth_class==2]) #ok
qqPlot(soaproot_pts_analysis$str_dist_N[soaproot_pts_analysis$depth_class==3]) #ok
leveneTest(str_dist_N ~ depth_class, data = soaproot_pts_analysis)
aov_dist_channel <- aov(str_dist_N ~ depth_class, data = soaproot_pts_analysis)
summary(aov_dist_channel) #p < 0.001

#--or--
lm_dist_channel <- lm(str_dist_N ~ depth_class, data = soaproot_pts_analysis)
anova(lm_dist_channel)
#which gives the exact same answer
TukeyHSD(aov_dist_channel) #3 vs. 1 and 3 vs. 2 are different

#use total_chemistry.R first to prepare data for analysis
#CaO
summary(ca_means$CaO)
hist(ca_means$CaO)
boxplot(ca_means$CaO)
ca_means <- ca_means[!is.na(ca_means$Depth),]
table(ca_means$depth_class) #13 shallow; 16 moderate; 26 deep, because 2 outliers were removed
leveneTest(CaO ~ depth_class, data = ca_means) # 0.10
ca_aov <- aov(CaO ~ depth_class, data = ca_means)
summary(ca_aov) #p val = 0.04
TukeyHSD(ca_aov) #3 vs. 1 = 0.04;

#MgO
summary(mg_means$MgO)
hist(mg_means$MgO)
boxplot(mg_means$MgO)
mg_means <- mg_means[!is.na(mg_means$Depth),]
table(mg_means$depth_class) #13 shallow; 16 moderate; 26 deep, because 2 outliers were removed
leveneTest(MgO ~ depth_class, data = mg_means) #p=0.66
mg_aov <- aov(MgO ~ depth_class, data = mg_means)
summary(mg_aov) #0.998 if 2 extreme values removed from shallow regolith sites; p=0.1 if all data kept in

#K2O
summary(k_means$K2O)
hist(k_means$K2O)
k_means <- k_means[!is.na(k_means$Depth),]
table(k_means$depth_class) #14 shallow; 16 moderate; 26 deep because 1 outlier removed
leveneTest(K2O ~ depth_class, data = k_means) #p=0.60
k_aov <- aov(K2O ~ depth_class, data = k_means)
summary(k_aov) #p val = 0.69
#TukeyHSD(k_aov)

#NaO
summary(na_means$Na2O)
hist(na_means$Na2O)
na_means <- na_means[!is.na(na_means$Depth),]
table(na_means$depth_class) #15 shallow; 16 moderate; 26 deep
leveneTest(Na2O ~ depth_class, data = na_means) #0.09
na_aov <- aov(Na2O ~ depth_class, data = na_means)
summary(na_aov) #p val = 0.0141
TukeyHSD(na_aov) #3 vs. 1 = 0.01;

#al2o3
summary(al_means$Al2O3)
hist(al_means$Al2O3)
boxplot(al_means$Al2O3)
al_means <- al_means[!is.na(al_means$Depth),]
table(al_means$depth_class) #15 shallow; 15 moderate; 26 deep, because one outlier removed
leveneTest(Al2O3 ~ depth_class, data = al_means) #p=0.27
al_aov <- aov(Al2O3 ~ depth_class, data = al_means)
summary(al_aov) #p val = 0.10
#TukeyHSD(al_aov)

#si means
summary(si_means$SiO2)
hist(si_means$SiO2)
si_means <- si_means[!is.na(si_means$Depth),]
table(si_means$depth_class) #13 shallow; 16 moderate; 26 deep, because 2 outliers removed
leveneTest(SiO2 ~ depth_class, data = si_means) #p=0.95
si_aov <- aov(SiO2 ~ depth_class, data = si_means)
summary(si_aov) #p val = 0.71
#TukeyHSD(si_aov)

#fe means
summary(fe_means$Fe2O3)
hist(fe_means$Fe2O3)
fe_means <- fe_means[!is.na(fe_means$Depth),]
table(fe_means$depth_class) #13 shallow; 16 moderate; 26 deep, because 2 outliers were removed
leveneTest(Fe2O3 ~ depth_class, data = fe_means) #0.33, ok when outliers removed
# welch.test(Fe2O3 ~ depth_class, data = fe_means) #p=0.53
fe_aov <- aov(Fe2O3 ~ depth_class, data = fe_means)
summary(fe_aov) #p val = 0.25
#TukeyHSD(fe_aov)