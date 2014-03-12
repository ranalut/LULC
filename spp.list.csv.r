
spp.list.csv <- function(sci.name, code, outputFile)
{
	sci.name <- paste("ref_species.sci_name = '",sci.name,"'",sep='')
	output <- data.frame(ScientificName=sci.name,Code=code)
	write.csv(output,outputFile)
}

spp <- read.csv('z:/lulc/gp_focal_spp_list.csv')
spp.list.csv(
	sci.name=spp$AOU54_SPECIES,
	code=spp$BBL_ABBREV,
	outputFile='z:/CBC-Data/gp.focal.spp.csv'
	)
