
conus.names <- scan('z:/lulc/legend_conus.txt',what=character(), sep=',')
conus.names <- conus.names[seq(2,34,2)]

lulc.2005 <- raster('z:/lulc/historical/conus_historical_y2005.img')
lulc.1966 <- raster('z:/lulc/gp_backcast_1938_1992/gp_lcyear_1966.tif')
lulc.1966[lulc.1966==0] <- NA
lulc.2005 <- crop(lulc.2005,lulc.1966)
lulc.2005 <- mask(lulc.2005,mask=lulc.1966)

cat('2005 freq\n')
f.2005 <- data.frame(freq(lulc.2005))
f.2005$proportion <- round(f.2005$count/sum(f.2005$count),4)
print(f.2005)
cat('1966 freq\n')
f.1966 <- data.frame(freq(lulc.1966))
f.1966$proportion <- round(f.1966$count/sum(f.1966$count),4)
print(f.1966)

change <- data.frame(value=seq(1,17,1))
change <- merge(change,f.2005,all.x=TRUE)
change <- merge(change,f.1966,by='value',all.x=TRUE)
print(head(change))
change$change <- change$proportion.x - change$proportion.y
change$names <- conus.names
print(change)

temp <- read.csv('z:/lulc/plots/gp.lulc.change.250m.cbc.r12070m.csv')
change$cbc <- temp[,2]
write.csv(change,'z:/lulc/plots/gp.lulc.all.change.csv')
