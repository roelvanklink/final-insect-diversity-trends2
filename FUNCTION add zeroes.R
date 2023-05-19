# this function adds zeroes for all species that were not observed in a 
# certain year, but were seen in other years. 
# if a dataset has more than one sampling time ('period'), it will throw a warning

addZeroes<- function (myDf) {

completeData<- NULL


	for(i in 1:length(unique(myDf$Plot_ID))){
		
		plt<- sort(unique(myDf$Plot_ID))[i]
		myData<- myDf[myDf$Plot_ID == plt , ]
		
		# throw errow if has multiple periods
		if(length(unique(myData$Period))>1){
			print ("WARNING: more than one period in data")
		}
		
		
				# add up males and females and different dates
		taxonCounts<- aggregate(Number~Taxon + Year , myData, sum)
		
		
		constantData <- unique(myData[,c("Plot_ID","Datasource_ID", "Taxon", "Unit_in_data", "Taxonomic_precision", "Realm")])#these are defo unique
		#expand grid to include NAs  # note that the 'date' variable is removed here. 
		# Date plays no role in the analysis, 
		allgrid <- expand.grid(Plot_ID = unique(myData$Plot_ID),
													 Year= unique(myData$Year),
													 Taxon = unique(myData$Taxon))
		
		allgrid <- merge(constantData, allgrid,all.x=T)
		allgrid <- merge(allgrid,taxonCounts, all.x=T)
		
		allgrid$Number[is.na(allgrid$Number)]<- 0
		
		
		completeData<-rbind (completeData, allgrid)

	}
	

return(completeData)
}



