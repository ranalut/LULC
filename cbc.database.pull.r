#Connect to the CBC database

workspace <- 'z:/CBC-Data/'

#Install the required package
library(RODBC)

#Load a list of species to call
SpTable=read.csv(paste(workspace,"gp.focal.spp.csv",sep=''))

for (s in 16:length(SpTable$Code)){

#Load a list of species to call
# SpTable=read.csv("SpList_Arid.csv")

#Load the hexgrid conversion table
CircleYear=read.csv(paste(workspace,"CircleStrataInfo.csv",sep=''))
  
#Establish a connection
channel = odbcConnect("CBC Database", uid = "CBCReportingP", pwd = "vq5m7t9k")

CBC <- sqlQuery(channel, paste("SELECT loc_circle.abbrev, loc_circle.subnational_code, loc_circle.name, cnt_submission.count_yr, ref_species.com_name,
cnt_observation.how_many, ref_transportation.description, cnt_effort_time_distance.distance, ref_dist_unit.description, cnt_effort_time_distance.hours,
cnt_effort_feeder_night.feeder_hrs, cnt_effort_feeder_night.nocturnal_hrs, cnt_effort_feeder_night.nocturnal_distance FROM cnt_submission FULL JOIN loc_circle ON
loc_circle.circle_id = cnt_submission.circle_id FULL JOIN cnt_observation ON cnt_submission.submission_id = cnt_observation.submission_id FULL JOIN ref_species ON
cnt_observation.species_id = ref_species.species_id FULL JOIN cnt_effort_time_distance ON cnt_submission.submission_id = cnt_effort_time_distance.submission_id
FULL JOIN cnt_effort_feeder_night ON cnt_submission.submission_id = cnt_effort_feeder_night.submission_id FULL JOIN ref_transportation ON
cnt_effort_time_distance.trans_id = ref_transportation.trans_id FULL JOIN ref_dist_unit  ON cnt_effort_time_distance.distance_unit_id = ref_dist_unit.unit_id WHERE",
paste(SpTable[s,'ScientificName']), "AND CNT_SUBMISSION.COUNT_YR BETWEEN '66' AND '113'"))
cat('complete sql...')

CBC7=subset(CBC,CBC$subnational_code != "NA")
sub <- data.frame(do.call('rbind', strsplit(as.character(CBC7$subnational_code),'-',fixed=TRUE)))
colnames(sub)<-c("Country","State")
CBC6=cbind(CBC7,sub)
CBC5=subset(CBC6,CBC6$Country == "US" | CBC6$Country == "CA" | CBC6$Country == "us" | CBC6$Country == "ca")
CBC4=subset(CBC5,CBC5$hours != "NA" & CBC5$hours != 0)
CBC3a <- aggregate(CBC4[6], by=list(abbrev=CBC4$abbrev, subnational_code=CBC4$subnational_code, name=CBC4$name, count_yr=CBC4$count_yr, com_name=CBC4$com_name, country=CBC4$Country, state=CBC4$State), FUN=mean)
CBC2a <- aggregate(CBC4[10], by=list(abbrev=CBC4$abbrev, subnational_code=CBC4$subnational_code, name=CBC4$name, count_yr=CBC4$count_yr, com_name=CBC4$com_name), FUN=sum)
CBC3 <- aggregate(CBC3a[8], by=list(abbrev=CBC3a$abbrev, subnational_code=CBC3a$subnational_code, name=CBC3a$name, count_yr=CBC3a$count_yr, country=CBC3a$country, state=CBC3a$state), FUN=sum)
CBC2 <- aggregate(CBC2a[6], by=list(abbrev=CBC2a$abbrev, subnational_code=CBC2a$subnational_code, name=CBC2a$name, count_yr=CBC2a$count_yr), FUN=mean)
CBC3=CBC3[order(CBC3$abbrev,CBC3$count_yr),]
CBC2=CBC2[order(CBC2$abbrev,CBC2$count_yr),]
CBC1a <- cbind(CBC3,CBC2$hours)
CBC1a$ID<-CBC1a$abbrev

#Replace DC with MD (treats DC as part of Maryland for analytical purposes)
CBC1a$subnational_code=as.factor(gsub("US-DC","US-MD",CBC1a$subnational_code))

#Merge the database output with the strata file
CBC1b=merge(CBC1a,CircleYear, by=c("abbrev","count_yr"), all.y=TRUE)

#Extract the relevant columns
AllCY=CBC1b[,c(1:2,5:7,9:20)]
colnames(AllCY)=c("abbrev", "count_yr", "country", "state", "how_many", "ID", "strata", "subnational_code","circle_name", "hours", 
"distance", "longitude", "latitude", "hexid", "bcr", "lcc",  "Area")

circ=as.vector(AllCY$ID)
found2=unique(circ, na.rm=T)
found=found2[!is.na(found2)]

RCY=NULL

for (i in 1:length(found))
{
  test=subset(AllCY, abbrev == paste(found[i]))
  RCY=rbind(RCY,test)
}

RCY$how_many[is.na(RCY$how_many)]<-0

CBC1=RCY

#Order the data 
CBC1=CBC1[ order(CBC1$subnational_code,CBC1$abbrev),]

#Identify those counts where the species was detected
CBC1$detect=NA
CBC1$detect=as.numeric(CBC1$detect)
for (i in 1:length(CBC1$how_many)){
   if (CBC1$how_many[i] > 0)
      {CBC1$detect[i]=1}
   else
      {CBC1$detect[i]=0}
}


#Determine whether any circles had no detections over the years
ZeroCircles <- aggregate(CBC1$detect, by=list(abbrev=CBC1$abbrev), FUN=sum)
colnames(ZeroCircles) <- c("abbrev","DetectionsPerCircle")

#Caculate number of circles with detections per stratum and ratio of circles with detections to those without
NZC <- aggregate(CBC1$detect, by=list(abbrev=CBC1$abbrev, strata=CBC1$strata), FUN=max)
NZC2 <- aggregate(NZC$x, by=list(strata=NZC$strata), FUN = sum)
NZC3 <- aggregate(NZC$x, by=list(strata=NZC$strata), FUN = length)
colnames(NZC2) <- c("strata","NonzeroCircles")
colnames(NZC3) <- c("strata","CirclesPerStratum")

#Sum the number of detections and the total number of counts and calculate the ratio, by stratum
NumD <- aggregate(CBC1$detect, by=list(strata=CBC1$strata), FUN=sum)
NumC <- aggregate(CBC1$detect, by=list(strata=CBC1$strata), FUN=length)
NumI <- aggregate(CBC1$how_many, by=list(strata=CBC1$strata), FUN=sum)
colnames(NumD) <- c("strata","NumDetections")
colnames(NumC) <- c("strata","TotalCounts")
colnames(NumI) <- c("strata","TotalIndividuals")
StrataData=cbind(NumC,NumD$NumDetections,NumI$TotalIndividuals, NZC2$NonzeroCircles, NZC3$CirclesPerStratum)
colnames(StrataData) <- c("strata","TotalCounts", "NumDetections", "TotalIndividuals", "NonzeroCircles", "CirclesPerStratum")
StrataData$CircleDetectionRatio = StrataData$NonzeroCircles/StrataData$CirclesPerStratum

#Combine the detection info with the raw data
CBC_r0=merge(CBC1, ZeroCircles, by = "abbrev")
CBC_r1=merge(CBC_r0, StrataData, by = "strata")

#Print a table with the raw data
write.csv(CBC_r1, paste(workspace,SpTable[s,'Code'],".csv",sep=""),row.names=FALSE)
cat('file written',SpTable[s,'Code'],'\n')

#Close the connection
odbcClose(channel)

#rm(list=ls())

}
