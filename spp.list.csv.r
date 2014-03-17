
spp.list.csv <- function(code, code2name, outputFile)
{
	output <- read.csv(code2name)
	output <- output[output$Code %in% code,]
	write.csv(output,outputFile,row.names=FALSE)
}

spp <- read.csv('z:/lulc/gp_focal_spp_list.csv')
spp.list.csv(
	code=spp$BBL_ABBREV,
	code2name="z:/CBC-Data/CBC Database Query/SpList_ScientificNames.csv",
	outputFile='z:/CBC-Data/gp.focal.spp.csv'
	)
