
library(raster)

# mask the strongholds map with the LULC map
s.bbs <- raster('d:/strongholds/ensemble_te_in_bbs.img')

lulc <- raster('d:/strongholds/lulc.strong.a2.y2080.delta.dev.tif')
s.bbs <- mask(s.bbs,lulc)
admin <- raster('d:/strongholds/states_ras')
state <- admin
state[state!=48] <- NA # TX, Need to look at the states_ras and tl_2013_us_state attributes tables to figure this out.
# state[state!=27] <- NA # MN
# state[state!=53] <- NA # WA
s.bbs <- mask(s.bbs, state)
lulc <- mask(lulc, state)
lulc[lulc<0] <- 0 # Want to only characterize threats (removal of natural habitat)
plot(s.bbs)
# zoom(s.bbs)
plot(lulc)
# zoom(lulc)

# ratio <- s.bbs/lulc
# plot(ratio)
# subtract <- s.bbs - lulc
# plot(subtract)
# zoom(subtract)

# writeRaster(s.bbs, 'd:/strongholds/tx_bbs_strong.tif', overwrite=TRUE)
# writeRaster(lulc, 'd:/strongholds/tx_a2_ag.tif')

# convert to vectors
s.bbs.v <- as.vector(s.bbs)
lulc.v <- as.vector(lulc)

# plot
par(mar=c(4,5,1,1))
# plot(lulc.v ~ s.bbs.v, pch=20, xlab='STRONGHOLDS RANKING', ylab='% CHANGE IN DEVELOPED AREA', main='STRONGHOLDS RANKING AND FUTURE\nAGRICULTURAL EXPANSION\nA2 EMISSIONS SCENARIO')
plot(lulc.v ~ s.bbs.v, pch=20, xlab='CLIMATE PRIORITIZATION RANKING', ylab='% CHANGE IN DEVELOPED AREA',cex.axis=2,cex.lab=2)
