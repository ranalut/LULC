

strong.table <- read.csv('d:/strongholds/lulc.strong.2005.full.csv', row.names=1)
legend.conus <- read.csv('d:/lulc/legend_conus.txt', header=FALSE, stringsAsFactors=FALSE)
colnames(legend.conus) <- c('index','lulc')

sink('d:/strongholds/cor.strong.2005.txt')
cat('\n\nCorrelations with Strongholds Score and % LULC\n')
for (i in 1:17)
{
	correlation <- cor(strong.table[,c((i+2),20)],use="pairwise.complete.obs")
	cat(legend.conus$lulc[i],' ',correlation[2],'\n')
}
sink()

sink('d:/strongholds/prop.95.strong.2005.txt')
# quantile(strong.table$score,probs=c(0.9,0.95,0.98,1),na.rm=TRUE)

st.95 <- strong.table[strong.table$score >= 0.95 & is.na(strong.table$zone)==FALSE,]
st.95.means <- apply(st.95[,3:19],2,mean, na.rm=TRUE)
names(st.95.means) <- legend.conus$lulc
cat('\n\nMean LULC proportion in top 5% Strongholds\n')
for (i in 1:17)
{
	cat(legend.conus$lulc[i],' ',st.95.means[i],'\n')
}
# print(st.95.means)
sink()

# Calculate percentage of each LULC type across the landscape to test whether a LULC type is over-represented.
