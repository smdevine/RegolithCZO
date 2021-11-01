DataFitDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/fit data distribution'
FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/results/figures'
library(extrafont)
library(extrafontdb)
#font_import() only needs to be done once
loadfonts()
list.files(DataFitDir)
data_fits <- read.csv(file.path(DataFitDir, 'czo regolith data dist fits_revised.csv'), stringsAsFactors = FALSE) #this analysis was completed in " .xlsx"
head(data_fits)
res_plots <- 800
gray_colors <- gray.colors(n=4, start = 0.05, end = 0.75)
lwd_setting <- 2

tiff(file = file.path(FiguresDir, 'cdf0_8_data_distributions_revised.tif'), family = 'Times New Roman', pointsize = 11, width = 3.25, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(data_fits$Depth, data_fits$observed.plotting.position, type='p', pch=16, col=gray_colors[1], xlim=c(0,8), ylim=c(0,0.58), ylab='Cumulative probability', xlab='Regolith depth (m)')
lines(data_fits$Gamma.prediction, data_fits$probability, lty=2, lwd=lwd_setting, col=gray_colors[2])
lines(data_fits$Lognormal.prediction, data_fits$probability, lty=3, lwd=lwd_setting, col=gray_colors[3])
lines(data_fits$Normal, data_fits$probability, lty=5, lwd=lwd_setting, col=gray_colors[4])
#legend('bottomright', legend = c('actual samples', '', 'gamma', 'lognormal', 'normal'), pch=c(16,rep(NA,4)), pt.bg=c(gray_colors[1], rep(NA,4)), lty=c(NA,NA,2,3,5), lwd=c(NA,NA,rep(lwd_setting,3)), col=c('black', NA, gray_colors[2:4]), y.intersp = 0.8, x.intersp = 0.8, inset = 0.01)
#text(4.75, 0.17, 'fitted distributions', adj=c(0,0))
text(0.16,0.55, 'b', adj=c(0,0))
dev.off()

tiff(file = file.path(FiguresDir, 'cdf0_40_data_distributions_revised.tif'), family = 'Times New Roman', pointsize = 11, width = 3.25, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(data_fits$Depth, data_fits$observed.plotting.position, type='p', pch=16, cex=0.6, col=gray_colors[1], xlim=c(0,40), ylim=c(0,0.99), ylab='Cumulative probability', xlab='Regolith depth (m)')
lines(data_fits$Gamma.prediction, data_fits$probability, lty=2, lwd=lwd_setting, col=gray_colors[2])
lines(data_fits$Lognormal.prediction, data_fits$probability, lty=3, lwd=lwd_setting, col=gray_colors[3])
lines(data_fits$Normal, data_fits$probability, lty=5, lwd=lwd_setting, col=gray_colors[4])
legend(x=15, y=0.8, legend = c('actual samples', '', 'normal', 'gamma', 'lognormal'), pch=c(16,rep(NA,4)), pt.bg=c(gray_colors[1], rep(NA,4)), pt.cex=c(0.6, rep(NA, 4)), lty=c(NA,NA,5,2,3), lwd=c(NA,NA,rep(lwd_setting,3)), col=c(gray_colors[1], NA, gray_colors[c(4,2,3)]), y.intersp = 0.9, x.intersp = 0.9)
text(18, 0.64, 'fitted distributions', adj=c(0,0))
rect(xleft = 0, ybottom = 0, xright = 8, ytop = 0.6, lty = 4, lwd = 1, col=NA, border = 'black')
text(9, 0.15, 'see Figure b', adj=c(0,0))
text(1, 0.95, 'a', adj=c(0,0))
dev.off()
