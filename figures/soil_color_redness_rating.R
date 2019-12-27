#run read-in_data.R first
library(vioplot)
library(extrafont)
library(extrafontdb)
#font_import() only needs to be done once
loadfonts()
res_plots <- 800
list.files(file.path(dataDir, 'RYAN GEOSPATIAL'))
soil_color <- read.csv(file.path(dataDir, 'RYAN GEOSPATIAL', 'soaproot_soil_color.csv'), stringsAsFactors = FALSE) #this is csv version of 'Soaproot_soilColor_RF.xlsx' created by Ryan F
dim(soil_color)
colnames(soil_color)
soil_color$H <- paste(soil_color$Hue, soil_color$Hue_2)
unique(soil_color$H)
soil_color$Hue <- NULL
soil_color$Hue_2 <- NULL
soil_color$H[soil_color$H=='NA NA'] <- NA
table(soil_color$H)
sum(is.na(soil_color$H))
summary(soil_color$Value)
summary(soil_color$Chroma)
soil_color <- soil_color[!is.na(soil_color$H), ]
#using Hurst, 1977
#soil_color$H_prime <- ifelse(soil_color$H=='2.5 YR', 12.5, ifelse(soil_color$H=='5 YR', 15, ifelse(soil_color$H=='7.5 YR', 17.5, ifelse(soil_color$H=='10 YR', 20, ifelse(soil_color$H=='2.5 Y', 22.5, ifelse(soil_color$H=='5 Y', 25, NA))))))
#soil_color$RR <- soil_color$H_prime * (soil_color$Value / soil_color$Chroma)

#using Torrent, 1980
soil_color$H_prime <- ifelse(soil_color$H=='2.5 YR', 0, ifelse(soil_color$H=='5 YR', 2, ifelse(soil_color$H=='7.5 YR', 4, ifelse(soil_color$H=='10 YR', 6, ifelse(soil_color$H=='2.5 Y', 8, ifelse(soil_color$H=='5 Y', 10, NA))))))
summary(soil_color$H_prime)
soil_color$RR <- ((10 - soil_color$H_prime) * soil_color$Chroma) / soil_color$Value

hist(soil_color$RR)
summary(soil_color$RR)
soil_color$RR_log <- log(soil_color$RR)
hist(soil_color$RR_log)
soil_color$depth_check <- soaproot_pts_analysis$Depth[match(soil_color$Site, soaproot_pts_analysis$Site)]
soil_color$Depth - soil_color$depth_check #all good
soil_color$depth_class <- as.factor(ifelse(soil_color$Depth < 3.3, 1, ifelse(soil_color$Depth < 7.56, 2, 3)))
plot(soil_color$Depth, soil_color$RR)

tiff(file = file.path(FiguresDir, 'redness_vioplots.tif'), family = 'Times New Roman', pointsize = 11, width = 4.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
vioplot(soil_color$RR[soil_color$depth_class==1], soil_color$RR[soil_color$depth_class==2], soil_color$RR[soil_color$depth_class==3], xlab=c('Regolith depth class (m)'), ylab='Redness rating', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'))
dev.off()

#test for statistically different means
table(soil_color$depth_class) #9 shallow; 15 moderate; 25 deep
leveneTest(RR ~ depth_class, data = soil_color) #0.12
rr_aov <- aov(RR ~ depth_class, data = soil_color)
summary(rr_aov) #p val = 0.05
TukeyHSD(rr_aov)

#log scale plot
vioplot(soil_color$RR_log[soil_color$depth_class==1], soil_color$RR_log[soil_color$depth_class==2], soil_color$RR_log[soil_color$depth_class==3], xlab=c('Regolith depth class (m)'), ylab='Redness rating', names=c('Shallow <3.3', 'Moderate 3.3-7.5', 'Deep >7.5'))
