dataDir <- 'C:/Users/smdevine/Desktop/post doc/czo work'
FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/czo work/results/figures'
library(extrafont)
library(extrafontdb)
#font_import() only needs to be done once
loadfonts()
res_plots <- 800
gray_colors <- gray.colors(n=4, start = 0.05, end = 0.75)
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

synthetic_7.56m_norm <- function(mean_sample, sd_sample) {
  success <- FALSE
  while (!success) {
    x <- rnorm(1, mean_sample, sd_sample)
    # check for success
    success <- x > 7.56
  }
  return(x)
}
synthetic_7.56m_log <- function(meanlog, sdlog) {
  success <- FALSE
  while (!success) {
    x <- rlnorm(1, meanlog, sdlog)
    # check for success
    success <- x > 7.56
  }
  return(x)
}
synthetic_7.56m_log(meanlog=1.773, sdlog=0.904) #lognormal parameters derived from original data not including zeroes

synthetic_7.56m_gamma <- function(shape, scale) {
  success <- FALSE
  while (!success) {
    x <- rgamma(1, shape = shape, scale = scale)
    # check for success
    success <- x > 7.56
  }
  return(x)
}
df_master <- read.csv(file.path(dataDir, 'Master Data Set_with rock outcrop.csv'), stringsAsFactors = FALSE) #removed biological data from this dataset but it has duplicate rows where multiple biological observations were made at each point; also removed 0's from outcrop (OC) site codes 01-09 to match points dataset labeling
df_sites <- df_master[match(unique(df_master$Site), df_master$Site),]
depths <- df_sites$Depth[df_sites$Depth != 0 & !is.na(df_sites$Depth)]


synthetic_7.56m_gamma(shape = 1.915, scale = 1/0.273)


#do.call(cbind, 
gamma_dist <- replicate(10000, sapply(depths, function(y) {if (y == 7.56) {synthetic_7.56m_gamma(shape = 1.915, scale = 1/0.273)} else{y}}))
log_dist <- replicate(10000, sapply(depths, function(y) {if (y == 7.56) {synthetic_7.56m_log(meanlog=1.773, sdlog=0.904)} else{y}}))
norm_dist <- replicate(10000, sapply(depths, function(y) {if (y == 7.56) {synthetic_7.56m_norm(mean_sample = 5.591, sd_sample = 2.581)} else{y}}))

#entirely synthetic approach
gamma_dist_syn <- replicate(10000, rgamma(66, shape = 1.915, scale = 1/0.273))
gamma_dist_syn <- as.data.frame(gamma_dist_syn)
log_dist_syn <- replicate(10000, rlnorm(66, meanlog=1.773, sdlog=0.904))
log_dist_syn <- as.data.frame(log_dist_syn)
norm_dist_syn <- replicate(10000, rnorm(66, mean = 5.591, sd = 2.581))
norm_dist_syn <- as.data.frame(norm_dist_syn)

mean(sapply(gamma_dist_syn, function(x) quantile(x, 0.9))) #13.55043 for gamma
mean(sapply(log_dist_syn, function(x) quantile(x, 0.9))) #18.41627 for lognormal
mean(sapply(norm_dist_syn, function(x) quantile(x, 0.9))) #8.795558 for norm


result_gamma_syn <- data.frame(less_than_2.5=sapply(gamma_dist_syn, function(x) sum(x < 2.5)), from_2.5_to_5=sapply(gamma_dist_syn, function(x) sum(x >= 2.5 & x < 5)), from_5_to_7.5=sapply(gamma_dist_syn, function(x) sum(x >= 5 & x < 7.5)), from_7.5_to_10=sapply(gamma_dist_syn, function(x) sum(x >= 7.5 & x < 10)), from_10_to_12.5=sapply(gamma_dist_syn, function(x) sum(x >= 10 & x < 12.5)), from_12.5_to_15=sapply(gamma_dist_syn, function(x) sum(x >= 12.5 & x < 15)), great_than_15=sapply(gamma_dist_syn, function(x) sum(x >= 15)))

result_log_syn <- data.frame(less_than_2.5=sapply(log_dist_syn, function(x) sum(x < 2.5)), from_2.5_to_5=sapply(log_dist_syn, function(x) sum(x >= 2.5 & x < 5)), from_5_to_7.5=sapply(log_dist_syn, function(x) sum(x >= 5 & x < 7.5)), from_7.5_to_10=sapply(log_dist_syn, function(x) sum(x >= 7.5 & x < 10)), from_10_to_12.5=sapply(log_dist_syn, function(x) sum(x >= 10 & x < 12.5)), from_12.5_to_15=sapply(log_dist_syn, function(x) sum(x >= 12.5 & x < 15)), great_than_15=sapply(log_dist_syn, function(x) sum(x >= 15)))

result_norm_syn <- data.frame(less_than_2.5=sapply(norm_dist_syn, function(x) sum(x < 2.5)), from_2.5_to_5=sapply(norm_dist_syn, function(x) sum(x >= 2.5 & x < 5)), from_5_to_7.5=sapply(norm_dist_syn, function(x) sum(x >= 5 & x < 7.5)), from_7.5_to_10=sapply(norm_dist_syn, function(x) sum(x >= 7.5 & x < 10)), from_10_to_12.5=sapply(norm_dist_syn, function(x) sum(x >= 10 & x < 12.5)), from_12.5_to_15=sapply(norm_dist_syn, function(x) sum(x >= 12.5 & x < 15)), great_than_15=sapply(norm_dist_syn, function(x) sum(x >= 15)))

result_act_syn <- c(sum(depths < 2.5), sum(depths >= 2.5 & depths < 5), sum(depths >= 5 & depths < 7.5), sum(depths >= 7.5 & depths < 10), sum(depths >= 10 & depths < 12.5), sum(depths >= 12.5 & depths < 15), sum(depths >= 15))

result_act_syn[4:length(result_act_syn)] <- NA

barplot_matrix <- 100 * rbind(result_act_syn, sapply(result_gamma_syn, mean), sapply(result_log_syn, mean), sapply(result_norm_syn, mean)) / 66 #, sapply(result_norm))
barplot_sds <-rbind(rep(NA, 7), sapply(result_gamma_syn, sd), sapply(result_log_syn, sd), sapply(result_norm_syn, sd))
barplot_CI99 <- 3.291 * barplot_sds / sqrt(10000) #

tiff(file = file.path(FiguresDir, 'hist_data_distributions.tif'), family = 'Times New Roman', pointsize = 11, width = 6.5, height = 3.5, units = 'in', res=res_plots, compression = 'lzw')
par(mar=c(3.75, 4.5, 0.5, 0.5))
bar_stats <- barplot(height = barplot_matrix, space = c(0,0.5), xaxt = 'n', xlab='', ylab='Soaproot watershed area (%)', beside=TRUE, axes=TRUE, col = gray_colors) #make a matrix where each row represents different distribution
mtext(text = c('<2.5', '2.5-5', '5-7.5', '7.5-10', '10-12.5', '12.5-15', '>15'), side = 1, line = 0.75, at = c(2.5, 7, 11.5, 16.5, 21, 25.5, 30))
mtext(text = 'Depth classes (m)', side = 1, line = 2.5, at = 16.5)
lines(x=c(15, 15), y=c(15, 20), lty=2)
arrows(x0=15, y0=17.5, x1=17, length = 0.05)
text(x=17.5, y=18.5, labels='41% of locations sampled had', adj=c(0,0))
text(x=17.5, y=15.5, labels='indeterminable depth > 7.56 m', adj=c(0,0))
legend_colors <- gray_colors
legend_colors <- c(legend_colors[1], NA, legend_colors[2:4])
legend(x = 23, y=39, legend = c('actual samples', '', 'gamma', 'lognormal', 'normal'), pch=c(22,NA,rep(22, 3)), pt.bg=legend_colors)
text(23.4, y=32.8, 'fitted distributions', adj=c(0,0))
#error.bar(bar_stats, barplot_matrix,  barplot_CI99)
dev.off()

#previous version
bar_stats <- barplot(height = barplot_matrix, space = c(0,0.5), xaxt = 'n', xlab='', ylab='Soaproot watershed area (%)', beside=TRUE, legend.text=c('actual samples', 'fitted gamma', 'fitted lognormal', 'fitted normal'), axes=TRUE)