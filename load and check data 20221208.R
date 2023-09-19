# this script builds the final dataframes used for all analyses from compponent datasets
# some of the datasets only provide abundance data, some pre-calcualted diversity metrics, 
# and some only raw data standardized per year.  


# load all datasets ##### 
rm(list=ls()) 

library(data.table)
library(reshape2)
library(tidyverse)
library(beepr)
source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/calculate metrics.R")
source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/effort_rarefaction.R")
source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/calculate expected beta diversity.R")
source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/function_cleaning_taxon_names.R")
source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/FUNCTION_manual_taxon_cleaning.R")
source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/FUNCTION add zeroes.R")





# make alternative color schemes
col.scheme.realm<-c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "peru")

columnsProvidedData<- c("Datasource_name" ,   "Plot_ID", "Plot_name", "Sample_ID", "Year", "Period", "Date", "Taxon", "Sex", 
												"Unit",    "Original_number", "Transformed_number", "Number", "Error", "ExpectedBeta",  "SDexpectedBeta" )


# load sheets of original database. These are all available at the KNB repository: 
# https://knb.ecoinformatics.org/view/urn%3Auuid%3Ab338c276-1d3f-4cc7-a192-cad846083455
setwd("/csvs") # work 

taxa<-read.csv( file = "taxa5.2.csv"); dim(taxa)
plots<-as.data.frame(fread( file = "PlotData 5.0.csv")); dim(plots)

samples <-read.csv( file = "Sample_Info 5.3.csv"); dim(samples)
database <-read.csv( file = "Data 5.3.csv"); str(database) # this is the main database
database<- subset(database, Note != "remove");dim(database); is.numeric(database$Number)
unique(database$Datasource_name)
studies<-read.csv(file = "studies 5.2.csv", header = T); dim(studies)
studies<- studies %>% select(-starts_with('X')) ; dim(studies)

#Add taxonomic level to Taxon table
taxa<- taxa[, 1:14] 
taxa$Level<- NA
taxa$Level[taxa$Phylum!= ""]<- "Phylum"
taxa$Level[taxa$Class!= ""]<- "Class"
taxa$Level[taxa$Subclass!= ""]<- "Subclass"
taxa$Level[taxa$Order!= ""]<- "Order"
taxa$Level[taxa$Suborder!= ""]<- "Suborder"
taxa$Level[taxa$Family!= ""]<- "Family"
taxa$Level[taxa$Subfamily!= ""]<- "Subfamily"
taxa$Level[taxa$Genus!= ""]<- "Genus"
taxa$Level[taxa$Species!= ""]<- "Species"
taxa$Level <- factor(taxa$Level, ordered = TRUE, 
										 levels = c("Phylum",  "Class", "Subclass", "Order","Suborder",  "Family",
										 					 "Subfamily","Genus" ,"Species" ))
taxa$Rank<-as.numeric(taxa$Level)        
write.csv(taxa, file = "C:\\Dropbox\\Insect Biomass Trends/csvs/taxa5.2.csv", row.names = F )  

# some changes to groupings. not used in this paper
studies$Continent[studies$Continent == "South America"]  <- "Latin America"
studies$Continent[studies$Continent == "Central America"]  <- "Latin America"
studies$Region[studies$Region == "Russia Volga"]  <- "Russia Central & Volga"
studies$Region[studies$Region == "Russia Central"]  <- "Russia Central & Volga"
studies$Region[studies$Region == "Russia Ural"]  <- "Russia Ural & Siberia"
studies$Region[studies$Region== "Russia Siberia"]  <- "Russia Ural & Siberia"
studies$Region[studies$Region== "Russia Far East"]  <- "Asia East"

# manual groupings of some datasets
studies$Region[(studies$Region == "Germany" & studies$Realm == "Freshwater" ) ] <- "Europe rest West"
studies$Region[(studies$Region == "United Kingdom" & studies$Realm == "Freshwater" ) ] <- "Europe rest West"
studies$Region[(studies$Region == "Russia Northwest" & studies$Realm == "Terrestrial" ) ] <- "Europe rest North"



# remove repeated column names
names(studies) # no redundancy
names(database) # remove redundant columns later
names(samples) # remove redundant columns
samples<- samples[, c("Sample_ID", "Datasource_ID", "Datasource_nameREDUNDANT", "Data_source", "Extraction_method", "Sampling_method", "Stratum", 
											"Sample_area", "Ref.to.methods", "Number_of_replicates", "Aggregation_of_replicates", "Taxon_in_Data",            
											"original_abundance_analysis.Science.", "NEWabundance_analysis.erratum." ,"PurifiedAnalysis.Science_reply.", "Biomass_Abundance_analysis", "taxonomic_paper", "non.insects" , 
											"taxon_comparison", "order_comparison", "family_comparison", "Populations",
											"non.insect_proportion", "Original_unit", "Calculations", "Unit", "Taxonomic_precision" , "Error_unit", "Flag_taxonomy"  )   ]
names(plots) # remove redundant columns
plots<- plots[, c("Plot_ID", "Datasource_ID", "Location", "Plot_name", "Details_plots",  "Experimental_Treatment", "Details_expt_trt",
									"Process_of_change", "notes_change", "invasives", "Coord_system", "Original_Latitude", "Original_Longitude", "Latitude",
									"Longitude", "Elevation", "Source_geogr_data")]
names(taxa) # no redundancy
taxa<-taxa[, c("ID","Phylum", "Class", "Subclass", "Suborder",  "Order", "Family","Subfamily", "Genus",     "Species",   "Taxon", "Level", "Rank", "Note")]

# clean some datasets from the raw database: 
# remove extra months from harvard forest
database<- subset(database, Datasource_name != "LTER Harvard forest" | Year != 2006 | Period != 8 ) # remove august in 2006
database<- subset(database, Datasource_name != "LTER Harvard forest" | Year != 2005 | Period <7  ) # remove  july and august in 2005
# remove summer sampling Brazil freshwater 2 to retain the 10 years of sampling (only winter samples in Year 1) 
database<- subset(database,  Datasource_name != "Brazil Freshwater 2" | Period != "summer")  
dim(database)

# remove hubbard brook caterpillars and replace with year totals 
database<- subset(database, !Plot_ID %in% c(639, 640, 641, 642))


# remove old portal ant data
database<- subset(database, !Plot_ID %in% c(663:668) ) 

# remove double taxa from Russian island fauna data: 
database<- subset(database, !Taxon %in% c('All_chortobionts', 'Carabids_larval_overwinter',   'Chrysomelids_larval_overwinter',   'Curculionids_larval_overwinter'))




# Load rest of data
Biotime <- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/BioTIMEstd2023.csv"); unique(Biotime$Datasource_name)
bt249.2023<- read.csv ("Copenhagen lighttrap reprocessed 2023.csv")
HubbardbrookLeps<- read.csv( 'C:/Dropbox/Insect Biomass Trends/csvs/Hubbard brook lepidoptera aggregated per year.csv')
LTERportalAnts <- 	read.csv("C:\\Dropbox\\Insect Biomass Trends/csvs/LTER portal ant nests 2023.csv")				 
LTERportalAnts$Taxon <- gsub(" ", "_", LTERportalAnts$Taxon)
hov.std<- read.csv( file = "Owen hoverflies standardized.csv")
schuch<- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/Schuch data for richness 2023.csv"); dim(schuch)
schuch<- subset(schuch, Number >=0) # exclude sight observations
chek<- schuch %>% 
	group_by(Plot_ID, Year) %>%
	summarise(  NUMBER_OF_DATES =  length(unique(Date))    ) # good 
print(chek, n = Inf)
sum(duplicated(schuch[, 2:10]))# males/females will be merged
sum(duplicated(schuch[, 2:11]))
GPDD.std <- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/GPDD data_standardized.csv"); unique(GPDD.std$Datasource_name)
Hungary <- read.csv (file = "Valtonen_long.csv"); #head(Hungary)
Lauwersmeer<- read.csv( file = "lauwersmeer final.csv", header = T)
Wijster <- read.csv( file = "Netherlands Ground beetles 2021.csv", header = T)
MWsuction<- read.csv(file = "Suction trap data formatted.csv", header = T)
PanamaLeafhoppers<- read.csv(file = "C:\\Dropbox\\Insect Biomass Trends/csvs/Panama leafhoppers full dataset.csv", header = T)   
Harvard_forest<- read.csv ( file = "hemlock expt final 2023.csv", header = T)
HubbardBrookBeetles<- read.csv( file = "c:\\Dropbox\\Insect Biomass Trends/csvs/HubbardBrookBeetles.csv")
Panamabutt <- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends\\csvs/Panamabutt.csv")
KoreaMoths <- read.csv( file = "C:/Dropbox/Insect Biomass Trends/csvs/KoreaMoths.csv")	
walesMoths<- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/wales moths.csv")


# load data with metrics already calculated
CzbeetlesStd<- read.csv(file = "Czech beetles standardized 20230801.csv") # no beta calculated! 
ECNbuttStd<- read.csv( file = "ECN butterflies standardized 20210715.csv")
ECNmothsStd<- read.csv(file = "ECN moths standardized 20210718.csv")
ECNgbStd<- read.csv( file = "ECN ground beetles standardized 20210718.csv")
IRbuttStd<- read.csv( file = "IR butterflies standardized 20210715.csv")
Luquillo1<- read.csv(file = "Luquillo canopy rarefied 20210803.csv", header = T)
SwengelButterfliesRar<- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/Swengel butterflies rarefied20221204.csv", header = T)
BEx <- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/GermanyBiodiversityExploratories.csv"); dim(BEx)
BEx$ExpectedBeta <- NA;  BEx$SDexpectedBeta<- NA
Greenland<- read.csv( file = "C:/Dropbox/Insect Biomass Trends/csvs/greenlandRarefied 20210804.csv")        # checked 19-12-19
SpaindungbeetlesStd<- read.csv(file = "C:\\Dropbox\\Insect Biomass Trends\\csvs/Spain Dungbeetles standardized 20210718.csv", header = T) 
AustrAntsRarefied<-read.csv( file = "C:\\Dropbox\\Insect Biomass Trends\\csvs/Australia ants3 rarefied 20210804.csv", header = T)
KonzaStd<- read.csv(file = "C:\\Dropbox\\Insect Biomass Trends\\csvs/Konza updated and standardized 20210715.csv")
TDbuttStd<- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends\\csvs/Tam dao Butterflies standardized 20210719.csv")
FinlandMoths<- read.csv(file = "FinlandMoths.csv")
AZ<- read.csv( file = "c:\\Dropbox\\Insect Biomass Trends/csvs/LTERarizona pitfalls rarefied 20230808.csv", header = T)
brazilbees3<- read.csv(file = "C:\\Dropbox\\Insect Biomass Trends\\csvs/Brazilbees3.csv")






# FILE 1 ALL BIODIVERSITY METRICS #####

# split database between raw data and provided data, and remove data of insufficiant quality and datasets that were replaced #####
table(samples$taxonomic_paper)   #!! this is not the number of datasets, just the number of sample IDs
# "" means drop data, 
selectedAbundanceDataIDs<- c(27, 2,3,378,333,399) # these are the Sample_IDs of the pure insect abundance data of: ColoradoFW, ArkansasFW, IdahoFW, NorthCaro FW and Australia fw + Costa Rica
samplesMeans<- subset(samples, taxonomic_paper == "addMeans"); dim(samplesMeans); unique(samplesMeans$Datasource_nameREDUNDANT)
samplesCalc<- subset(samples, taxonomic_paper == "calculate"); dim(samplesCalc);  unique(samplesCalc$Datasource_nameREDUNDANT)
samplesProv<- subset(samples, taxonomic_paper == "provided");  dim(samplesProv);  unique(samplesProv$Datasource_nameREDUNDANT)
samplesAbunOnly<- subset(samples, taxonomic_paper == "abundOnly");  dim(samplesAbunOnly);  unique(samplesAbunOnly$Datasource_nameREDUNDANT)  #61 datasets


# test if the data in databse are all either calculate or provided
unique(merge(database, samples, by = "Sample_ID")$taxonomic_paper ) # good. no 'addMeans' 

databaseRaw<-database[database$Sample_ID %in% samplesCalc$Sample_ID  , ]; dim(databaseRaw) #12260 
table(databaseRaw$Unit) # good: only abundance or density


databaseProv<- database[database$Sample_ID %in% samplesProv$Sample_ID  , ]; dim(databaseProv) #4218
table(databaseProv$Unit) # various metrics 
table(databaseProv$Datasource_name,  databaseProv$Unit) # various metrics 
subset(databaseProv, Unit == "biomass")$Datasource_name

# extract datasets where abundance must first be calclated
databaseAbundOnly<-  database[database$Sample_ID %in% samplesAbunOnly$Sample_ID  , ]; dim(databaseAbundOnly)   
table(databaseAbundOnly$Datasource_name,  databaseAbundOnly$Unit) # 12 richness values from russia soil fauna that need to be added up after selection. same procedure as abundance data 











# metadata per dataset (aggregated over plots) - this can go into Studies file

descriptors<- plots %>% 
	group_by(Datasource_ID) %>%
	summarise(#Duration = (max(Year) - min(Year))+1, 
		mean_lat = mean(Latitude, na.rm = T),
		mean_long = mean(Longitude, na.rm = T),
		NUMBER_OF_PLOTS =  length(unique(Plot_ID)),
		NUMBER_OF_LOCATIONS = length(unique(Location))#,
		#  NUMBER_OF_SAMPLES = (),
		# NUMBER_OF_YEARS = length(unique(Year))
		# NUMBER_OF_TAXA = length(unique(Taxon),
		#  N = sum(Number)
	)
(descriptors)
save(descriptors, file = "Descriptors.RData")



# Step 1: abundance-only data, which need to be aggregated to form one value ####

#  1) remove non-insects , non-springtails and non-arachnids from abundance data 
dim(databaseAbundOnly)
temp<- merge(databaseAbundOnly, taxa, by = "Taxon"); dim(temp)
unique(temp$Phylum)


# make pure dataset of only insects, entognatha and arachnids:
tempPure<- subset(temp, Phylum == "Arthropoda" ); dim(tempPure)
sort(unique(tempPure$Datasource_name))
tempPure<- subset(tempPure, Class == "Insecta" | Class == "Arachnida" | Class == "Entognatha" |  Class == ""); dim(tempPure) # empty arthropod class is combined insects and arachnids, possibly some myriapods and terrestrial crustaceans, 
sort(unique(tempPure$Datasource_name))

setdiff(unique(temp$Datasource_name), unique(tempPure$Datasource_name)) # there are noninsects in these datasets, with unknown abundances
setdiff(unique(temp$Taxon), unique(tempPure$Taxon)) #looks like only non-insects
unique(tempPure$Phylum) # only arthropoda in tempPure


tempPure<- merge(tempPure, plots[, 1:2]);    dim(tempPure)
tempPure<- merge(tempPure, studies); dim(tempPure)

subset(unique(tempPure[, c("Invertebrate_group_Scientific_name", "Taxon")]), Invertebrate_group_Scientific_name == "All_invertebrates")
# only insects and arachnids









# 2) aggregate over all taxa in abundance data
abundDfPure<- dcast(tempPure,  Datasource_name + Plot_ID + Plot_name +   Year + Period + 
											Date  + Invertebrate_group_Scientific_name + Unit ~ "Number", value.var = "Number", sum) ;  dim(abundDfPure); length(unique(abundDfPure$Datasource_name)) # 400 difference
table(abundDfPure$Datasource_name, abundDfPure$Unit) # richness values in russia can stay

# 3) add columns 
names(abundDfPure)
names(abundDfPure)[names(abundDfPure) == "Invertebrate_group_Scientific_name"]<- "Taxon"
unique(abundDfPure$Taxon)
# rename all_invertebrates to insects, since all non insects and non arachnids were excluded
abundDfPure$Taxon[abundDfPure$Taxon == "All_invertebrates"]<- "Insecta"



# add missing columns
addColumns<- function(myData){  
	myData$Sample_ID<- 0 ; myData$Year_orig <- "" 
	myData$Sex<- "" ; myData$Original_number <- NA  ; myData$Transformed_number<- NA
	myData$Error <- NA ;   myData$ExpectedBeta<- NA  ; myData$SDexpectedBeta <- NA
	myData<- myData[,  columnsProvidedData]
	return(myData)}
abundDfPure<- addColumns(abundDfPure)


#check for duplicates()  
dups<-  abundDfPure[duplicated(abundDfPure[, c("Plot_ID", "Year", "Period", "Unit")]) ,]
unique(dups$Datasource_name)
# correct dups:   "Brazil Dungbeetles" "Israel butterflies" "Utah freshwater" , russia parasitoids
# because of multiple samplings per period 


# Step 2: combine provided and processed data #####
table(databaseProv$Period) # there are some not ideal, but possible to deal with in the statistical models

databaseProv<- databaseProv[, names(databaseProv) %in% columnsProvidedData]
databaseProv$ExpectedBeta<- NA ;   databaseProv$SDexpectedBeta <- NA


#check if there is only one value per plot / year / period / unit (/ date) combination
checkValues<-  rbind( abundDfPure, databaseProv) 
checker<- paste(checkValues$Datasource_name,  checkValues$Plot_ID, checkValues$Unit, checkValues$Year, checkValues$Period, checkValues$Date)
length(checker) # should be the same as 
length(unique(checker)) # -1
checker[duplicated(checker)]
# correct duplicates are: "Brazil Dungbeetles 137 abundance 2000 1 "

checkTaxa<- unique(databaseProv[, c(  "Datasource_name", "Taxon" )]); checkTaxa # if analysis is resticted to only purest insect data, we lose another 9 datasets, or they are demoted to abundance only:
#Arkansas freshwater (demoted), Idaho freshwater (demoted), Idaho freshwater 2 (excluded) , pennsylvania fw (excluded), georgia fw (excluded), Costa Rica freshwater (demoted), Italy freshwater (excl), Australia fw (demoted)
checktaxaPure <- subset(checkTaxa, Taxon != "All_invertebrates" & Taxon != "Macroinvertebrate" & Taxon != "Freshwater_invertebrates" & 
													Taxon != "Freshwater_fauna" & Taxon != "All_macroinvertebrates" & Taxon != "All_invertebrates" & 
													Taxon !="freshwater_invertebrates"  )
dim(checkTaxa)
dim(checktaxaPure)
anti_join(checkTaxa, checktaxaPure)

# select the pure datasets 
dim(databaseProv)

databaseProvPure<- databaseProv[databaseProv$Datasource_name %in% checktaxaPure$Datasource_name, ] ; dim(databaseProvPure) # lost 1885 datapoints but gained 1000 in abundance only 
setdiff(unique(databaseProvPure$Datasource_name), unique(databaseProv$Datasource_name))

# combine with other data, where values were either provided in the paper, or where we did rarefaction to obtain biodiversity data
allProvidedData<- rbind(
	HubbardbrookLeps[, -(1)],
	TDbuttStd,
	KonzaStd , 
	ECNbuttStd,
	ECNmothsStd,
	ECNgbStd,
	AZ, 
	CzbeetlesStd,
	IRbuttStd,
	Greenland,
	Luquillo1,
	SwengelButterfliesRar[, -(1)] ,
	SpaindungbeetlesStd,
	FinlandMoths,
	AustrAntsRarefied,
	BEx[, -(1:2)],
	brazilbees3[, -(1)]
); dim(allProvidedData) # 265566      rows



# combine abundance only data and provided data
allProvidedDataPure<- rbind(databaseProvPure, # all with richness/ diversity values
														abundDfPure,
														allProvidedData)






length(unique(allProvidedDataPure$Datasource_name)) #106 studies
dim(allProvidedDataPure)

#fix and add some columns
addColumns2<- function(myData){
	myData$Unit[myData$Unit == "density"]<- "abundance"
	myData<- merge(myData, samples[, c("Sample_ID", "Taxonomic_precision" )], all.x = T) # merge in taxonomic  flags 
	myData$source<- "Complex raw data"
	myData$source[myData$Plot_ID %in% databaseAbundOnly$Plot_ID ] <- "Provided or summed from data in publication" # for metadata file 
	myData$source[myData$Plot_ID %in% databaseProv$Plot_ID ] <- "Provided in publication" # for metadata file 
	myData$source[myData$Plot_ID %in% FinlandMoths$Plot_ID ] <- "Provided in publication"
	myData$source[myData$Plot_ID %in% BEx$Plot_ID ] <- "Provided in publication"
	
	return (myData)
}

#allProvidedData<- addColumns2(allProvidedData); dim(allProvidedData)
allProvidedDataPure<- addColumns2(allProvidedDataPure); dim(allProvidedDataPure)
head(allProvidedDataPure)

table(allProvidedDataPure$source)
unique(allProvidedDataPure$Taxon)
subset(allProvidedDataPure, Taxon == "All_invertebrates") # should be none

# each datasource_ID should have only 1 taxon here (except colorado)
pvt<- dcast(allProvidedDataPure, Datasource_name ~ Unit, value.var = "Taxon", function(x){length(unique(x))}) # the NA doesn't seem to exist 
apply(pvt[, -1], 1, function(r) any(r >1))    # none have more than 1 taxon

table(allProvidedDataPure$Datasource_name, allProvidedDataPure$Unit)
tail(table(allProvidedDataPure$Datasource_name, allProvidedDataPure$Unit), 7)
length(unique(allProvidedDataPure$Datasource_name)) # 119 studies
length(unique(allProvidedDataPure$Plot_name)) # 1139 plots 

saveRDS(allProvidedDataPure, file = "allProvidedDataPure20230817.RDS")

old<- (read_rds('allprovidedDataPure.RDS'))    ; dim(old)
dim(allProvidedDataPure)

anti_join(old, allProvidedDataPure)
setdiff(unique(old$Datasource_name), allProvidedDataPure$Datasource_name)
setdiff(unique(allProvidedDataPure$Datasource_name), old$Datasource_name)

setdiff(unique(old$Plot_name), allProvidedDataPure$Plot_name) # 3 plots from arizona fell out, and 1 plot in Russia was renamed
setdiff(unique(allProvidedDataPure$Plot_name), unique(old$Plot_name))








# Step 3  load raw data for processing   #####


allRawData <- rbind(
	Biotime %>%select(-Note),
	bt249.2023,
	MWsuction, 
	GPDD.std %>% select(-Datasource_ID),
	Wijster[, -(1)],
	PanamaLeafhoppers [, -(1)],
	hov.std[, -(1)],
	Hungary[, -(1:2)] ,
	schuch[, -c(1,6 )],
	Harvard_forest, #,
	HubbardBrookBeetles,
	Panamabutt[, -c(1, 16,17)] ,
	KoreaMoths[, -(1)] , 
	walesMoths[, -c(1, 6, 17,18)],
	LTERportalAnts[,-(1)],
	Lauwersmeer[,-(1)],
	databaseRaw[, -c(1,6, 17:20)]  ) ; dim(allRawData)# 189367          



length(unique(allRawData$Datasource_name)) # 46 studies 
length(unique(allRawData$Plot_name)) # 327 plots 
length(unique(allRawData$Sample_ID)) # 53
dcast(allRawData, Datasource_name + Sample_ID ~ "n", value.var = "Number", length)

allRawData$Unit[allRawData$Unit == "density"]<- "abundance"
allRawData$Unit[allRawData$Unit == "Abundance"]<- "abundance"
unique(allRawData$Unit)
sum(duplicated(allRawData)) # == 4 # something's wrong here 

dups<- allRawData[ (duplicated(allRawData)), ]
table(dups$Datasource_name)
table(dups$Number)
# # accepted duplicates: 
# 4 in sev ants; correct. date has been changed for some plots, these numbers are from different plots that were sampled on different dates. 
# are merged in our analysis.


# can the data here all be merged to give one value per plot per year? 
# in other words: is sampling effort consistent each year? 

plts<- unique(allRawData$Plot_ID)
allRes<- NULL
for (i in 1: length(plts)){
	plt<- plts[i]
	dat<-  subset(allRawData, Plot_ID == plt)
	dat$Period[is.na(dat$Period)]<- 1
	periods<- aggregate( Period~  Year  ,data = dat,  function(x){length(unique(x))})$Period
	
	res<- data.frame(Plot_name =  unique(dat$Plot_name), 
									 Plot_ID   =  unique(dat$Plot_ID), 
									 periods = mean(unique(periods)), 
									 Flag = !length(unique(periods))==1)
	allRes <- rbind(allRes, res)
}
subset(allRes, Flag == T )  
subset(allRes, periods >1 )  #  Schuch data still needs to be merged per year



metadata_per_dataset<-  allRawData %>% 
	group_by( Datasource_name, Unit) %>%
	summarise(
		Start = min(Year, na.rm = T),
		End = max(Year, na.rm = T),
		Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
		NUMBER_OF_PLOTS =  length(unique(Plot_ID)), 
		NUMBER_OF_YEARS = length(unique(Year)), 
		NUMBER_OF_TAXA = length(unique(Taxon))
	)
print(subset(metadata_per_dataset, NUMBER_OF_YEARS >19 & NUMBER_OF_TAXA > 4 & Unit == "abundance"), n= Inf)


metadata_per_plot<-  selRawData %>% 
	group_by(Datasource_name, Plot_ID, Unit) %>%
	summarise(
		Start = min(Year, na.rm = T),
		End = max(Year, na.rm = T),
		Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
		NUMBER_OF_YEARS = length(unique(Year)), 
		NUMBER_OF_TAXA = length(unique(Taxon))
	)
print(subset(metadata_per_plot, NUMBER_OF_YEARS >19 & Unit == "abundance"), n= Inf)






# Step 4 merge raw data files with plot, sample and taxonomic data #####

#Test all merges and links
# test if plot and study files correspond
studies[duplicated(studies$Datasource_ID), ] # only empty lines

dim(allRawData)
test1<-merge(allRawData, taxa) # 
dim(test1)  


dim(plots)
test2<-merge(plots, studies, by = "Datasource_ID") # 
dim(test2) # correct, because black rock forest is not in studies

plots[!plots$Datasource_ID %in% studies$Datasource_ID, ] # black rock forest and donana were disqualified 


# observations and plots
dim(allRawData)
test2<-(merge(allRawData, plots, by = "Plot_ID"))
dim(test2) # all there
unique(allRawData$Plot_ID)[! unique(allRawData$Plot_ID) %in% plots$Plot_ID ]



# samples and observations
test3<-(merge(test2, samples, by = "Sample_ID"))
dim(test3) 
unique(test2$Sample_ID)[! unique(test2$Sample_ID) %in% test3$Sample_ID ]
unique(test2$Sample_ID)[! unique(test2$Sample_ID) %in% samples$Sample_ID ]


# which are the missing sample IDs?
samples[samples$Sample_ID %in% setdiff(samples$Sample_ID, unique(test2$Sample_ID)),] #
setdiff( unique(test2$Sample_ID), samples$Sample_ID)

unique(test2$Sample_ID) [!unique(test2$Sample_ID)  %in%  samples$Sample_ID] # 0
samples[duplicated(samples$Sample_ID)] #no duplicates


# make sure NA's in number are recognized as na's
is.numeric(test3$Number) # is ok 
mf<- as.numeric(test3$Number)
wrong<-(is.na(mf))
sum(wrong)
test4<- subset(test3, is.na(Number))[, c('Datasource_name', 'Taxon' )]
unique(test4$Datasource_name) # none of these are used in current analysis














# step 5 Select data for analysis #####

# remove duplicate columns 
names(allRawData)
names(allRawData)[names(allRawData) == "Sex"]<-"Withintaxon_group"
names(allRawData)[names(allRawData) == "Unit"]<-"Unit_in_data"
allRawData$Period[is.na(allRawData$Period)]<-1  # replace missing Period data with 1
allRawData$Period[allRawData$Period == ""]<-1  # replace missing Period data with 1
allRawData<- allRawData[, c("Plot_ID", "Sample_ID",  "Year", "Period", "Date", "Taxon", "Withintaxon_group",  
														"Unit_in_data",  "Original_number", "Transformed_number", "Number",  "Error")] 


# merge all tables into 1 big object 

# merge with taxon
dim(allRawData)
merge1<-merge(allRawData, taxa, by = "Taxon")
dim(merge1) # all there
unique(allRawData$Taxon)[!unique(allRawData$Taxon) %in% taxa$Taxon]

# merge with samples
merge2<-(merge(merge1, samples, by = "Sample_ID"))
dim(merge2) # all there. 
length(unique(merge2$Datasource_ID)) #53

# merge with plot # mind that column 'Datasource ID is in both 
merge3<- merge(merge2, plots , by = c("Plot_ID", "Datasource_ID") )#
dim(merge3) # all there 
merge3.1<- merge(merge2, plots , all.x = T)#, by = c("Plot_ID", "Datasource_ID", "Plot_name") 
missing<- anti_join(merge3.1, merge3)
unique(missing$Plot_ID)
names(merge3)
unique(missing$Datasource_ID)

# merge with studies 
merge4<- merge(merge3, studies, by = "Datasource_ID")
#names(merge4)[order(names(merge4))]
dim(merge4)

beep(2)

# check for complete data of all important variables 
sum(is.na(merge4$Taxon))
sum(is.na(merge4$Latitude))
sum(is.na(merge4$Longitude))
sum(is.na(merge4$Stratum))
sum(is.na(merge4$Continent))
sum(is.na(merge4$Biome))
sum(is.na(merge4$Sample_ID))
sum(is.na(merge4$Location))
sum(is.na(merge4$Datasource_ID))
sum(is.na(merge4$Period))
sum(is.na(merge4$Plot_ID))
sum(is.na(merge4$Number)) #
sum(is.na(merge4$Year ))








# Selection 1:  Duration of timeseries: #####

metadata_per_plot<-  merge4 %>% 
	group_by(Plot_ID) %>%
	summarise(
		Plot_name = length(unique(Plot_name)), 
		Datasource_ID = unique(Datasource_ID),
		Datasource_name = unique(Datasource_name), 
		Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T),
		Country_State = unique(Country_State),
		Country = unique(Country),
		Region = unique(Region),
		Realm = unique(Realm),
		#Stratum = length(unique(Stratum)),
		Longitude = unique(Longitude),
		Latitude = unique(Latitude),
		AbundanceBiomass = unique(Abundance.Biomass),
		NUMBER_OF_PLOTS =  length(unique(Plot_ID)), # should be 1
		NUMBER_OF_SAMPLES = length(unique(paste(Year, Period))),
		NUMBER_OF_YEARS = length(unique(Year)),
		NUMBER_OF_TAXA = length(unique(Taxon)),
		TOTAL_N = sum(Number, na.rm = T))

dim(metadata_per_plot) # 

metadata_per_dataset<-  merge4 %>% 
	group_by(Datasource_ID) %>%
	summarise(
		Datasource_ID = unique(Datasource_ID),
		Datasource_name = unique(Datasource_name), 
		Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
		Number_of_years_data = length(unique(Year)), 
		Taxon = unique(Invertebrate_group), 
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T),
		Country_State = unique(Country_State),
		Country = unique(Country),
		Region = unique(Region),
		Realm = unique(Realm),
		Longitude = mean(unique(Longitude)),
		Latitude = mean(unique(Latitude)),
		#  AbundanceBiomass = unique(Abundance.Biomass),
		NUMBER_OF_PLOTS =  length(unique(Plot_ID)), # should be 1
		NUMBER_OF_SAMPLES = length(unique(paste(Year, Period))),
		NUMBER_OF_YEARS = length(unique(Year)),
		NUMBER_OF_TAXA = length(unique(Taxon)), 
		Number_of_Orders = length(unique(Order)),
		Orders = paste(unique(Order), collapse = " "), 
		Number_of_families = length(unique(Family)),
		# Families = paste(unique(merge4$Family), collapse = " "),
		TOTAL_N = sum(Number, na.rm = T)
	)

as.data.frame(subset(metadata_per_dataset, Number_of_Orders> 1 ) )







# which plots and datasets  have less that 10 years data? 
# and Israel (all plots)  and Ukraine beetles (1 plot) 
# which  datasets have short plots?
unique(subset(metadata_per_plot, Duration < 9)$Datasource_name) # 

subset(metadata_per_plot, Duration < 10) # 27 plots
subset(metadata_per_plot, Duration < 9) # 6 plots

# remove all plots < 9 years 
# bad plots: 
short.plots<- subset(metadata_per_plot, Duration < 9)$Plot_ID
# plots with 9 yrs  within datasets with >10 years are allowed to stay 

# only select plots that have sufficient duration
merge4.1<- merge4[! merge4$Plot_ID %in% short.plots, ]
dim(merge4.1)#


# remove plots with 0 observations 
empty.plots<- subset(metadata_per_plot, TOTAL_N == 0)$Plot_ID # one georgia coast plot
merge4.1<- merge4.1[! merge4.1$Plot_ID %in% empty.plots, ]



#Selection  2: replace NA in Number with 0 ####
nas<-subset(merge4.1, is.na(Number) ) ;dim(nas) #
merge4.1$Number[is.na(merge4.1$Number)] <- 0 
dim(merge4.1) # 
length(unique(merge4.1$Datasource_ID)) # 46
saveRDS(merge4.1, file = "all invertebrates unmerged.rds")


# for all inverts: merge dates and periods and sexes to give 1 value per year
rawallinv<- dcast(merge4.1, Datasource_ID+Plot_ID+ Realm + Plot_name+Year+Unit_in_data+ Taxon+ Phylum + Class + 
										Subclass +Suborder+Order+Family+Subfamily+Genus+Species+Level+Rank+ Flag_taxonomy ~ "Number", value.var = "Number" , sum)
dim(rawallinv)# 
head(rawallinv)

write.csv(rawallinv, file = "all invertebrates std raw data.csv")







# Selection 3: remove non arthropoda and crustacea####
dim(merge4.1) #   
others<-subset(merge4.1, Phylum !=  "Arthropoda" & Phylum != "Invertebrata" ); dim(others)
raw1<-subset(merge4.1, Phylum ==  "Arthropoda" |  Phylum == "Invertebrata")
dim(raw1) #       
length(unique(raw1$Datasource_ID)) #   


# only keep insects, arachnids and springtails
unique(raw1$Class)
raw2<-subset(raw1, Class ==  "Insecta" | Class == "Arachnida" | Class == "Entognatha" )
unique(raw2$Class)
dim(raw2) #
length(unique(raw2$Datasource_ID))
dim(subset(raw2, Number == 0)) # 0's still there


length(unique(raw2$Plot_ID)) # 789


# Step 6 aggregate all dates, sexes and periods to get one value per year #####
raw3<- dcast(raw2, Datasource_ID+Plot_ID+ Realm + Plot_name+Year+Unit_in_data+ Taxon+ Phylum + Class + 
						 	Subclass +Suborder+Order+Family+Subfamily+Genus+Species+Level+Rank+ Taxonomic_precision ~ "Number", value.var = "Number" , sum)
dim(raw3) #     
head(raw3)

saveRDS(raw3, file = "rawInsectsForRichnessAggregatedPerYear.RDS")




# Step 7 calculate all biodiversity metrics on raw data #####

raw3<- readRDS(file = "rawInsectsForRichnessAggregatedPerYear.RDS")

raw3ab<- subset(raw3, Unit_in_data == "abundance")
raw3bm<- subset(raw3, Unit_in_data == "biomass")
unique(raw3ab$Datasource_ID)

# Loop to calculate metrics on raw data 
beta_randomizations<- 100 
smpls<- 100
all.metrics<- NULL
all.densities<- NULL
sites<- sort(unique(subset(raw3ab, Realm == 'Terrestrial')  $Plot_ID)) 
pb <- txtProgressBar(min = 0, max = length(sites), style = 3) # progressbar


# note that sites with only 1 species are removed from the data 
for(i in 1:length(sites)){#   #
	#print(i)  
	pltID<- sites [i]
	dat<- subset(raw3ab, Plot_ID == pltID)  
	plt<- unique(dat$Plot_name)
	
	#print (dat$Plot_name[1])
	taxPrec<- unique(dat$Taxonomic_precision)
	
	if(unique(dat$Taxonomic_precision) == "species"){ # clean taxonomy if taxonomic resolution is good
		dat1<- cleaning_dataset (dat, mode =  'beta', add_unique_sample_ID = TRUE) 
		pvt<- dcast(dat1,  Year ~ Taxon , value.var = "Number", sum)
		
		alpha<- cbind(Plot_name = plt,
									Plot_ID = unique(dat1$Plot_ID),
									calculate_alpha_metrics(pvt) )
		
		
		beta<- cbind(Plot_name = plt,
								 Plot_ID = unique(dat1$Plot_ID),
								 merge(calculate_beta(pvt),
								 			calculate_expected_beta(pvt, beta_randomizations), all.x = T ),
								 Taxonomic_precision = taxPrec)
		if(all( vegan::specnumber(as.data.frame(pvt[, -(1)]), MARGIN = 1)<2 )) next # if all years have only one sp, skip
		densities<- cbind(Plot_ID = unique(dat$Plot_ID),
											calculate_density(pvt, BW = 0.3) )
		
		
		
	}else{ #don't clean taxonomy if taxonomic resolution is shitty anyway
		pvt<- reshape2::dcast(dat,  Year ~ Taxon , value.var = "Number", sum)
		
		alpha<- cbind(Plot_name = plt,
									Plot_ID = unique(dat$Plot_ID),
									calculate_alpha_metrics(pvt)  )
		
		
		# beta<- cbind(Plot_name = plt, # beta diversity not used in paper. Also takes long to calculate
		# 						 Plot_ID = unique(dat$Plot_ID),
		# 						 merge(calculate_beta(pvt),
		# 						 			calculate_expected_beta(pvt, beta_randomizations), all.x = T ),
		# 						 Taxonomic_precision = taxPrec)
		# 
		if(all( vegan::specnumber(as.data.frame(pvt[, -(1)]), MARGIN = 1)<2 )) next # if all years have only one sp, skip
		# densities<- cbind(Plot_ID = unique(dat$Plot_ID), # kernel densities no longer in paper
		# 									calculate_density(pvt, BW = 0.3) )
		# 
	}
	
	
	metrics<- alpha#merge(alpha, beta, all.x = T, all.y = T )     
	all.metrics<- rbind(all.metrics, metrics)  
	
	#	all.densities<- rbind(all.densities, densities)
	
	
	
	warning(paste("!!! plot", pltID, "not included in all.metrics!"), call. = (pltID %in% all.metrics$Plot_ID == F) )
	
	
	#print (dat$Datasource_name[1])
	setTxtProgressBar(pb, i)
	
}


dim(all.metrics)#129488
dim(all.densities)
table(all.metrics$Unit_in_data)
# lost quite some data for rarefied richness and coverage (i.e. non integers)
mis<- (sites)[!sites %in% unique(all.metrics$Plot_ID)] # 16 missing. Why? 
mis
arrange(subset(raw3ab, Plot_ID %in% mis)[, c("Datasource_ID", "Plot_ID", "Realm", "Plot_name", "Year", "Taxon", "Number" )], Plot_ID, Year)
# 632 1 species in 1 year  
# 634 1 species in most years
# 1634  1 sp in all years
# 1635  1 sp in all years 
# 1636   1 sp in all years
# 1637    1 sp in all years
# 1647    1 sp in all years
# 1646    1 sp in all years
# 1656    1 sp in all years
# 923  1 species in some years
#  
# 1405 1 sp in all years
#  
# 2218 1 sp in first year 
# 2223 
# 2228
# 2229 1 sp in first and 1 sp in last yr
# 2230 




saveRDS(all.metrics, file = "C:\\Dropbox\\Insect Biomass Trends\\csvs/all  metrics calculated from database 20230822.rds ")


ll.metrics   <- readRDS( file = "all  metrics calculated from database 20230822.rds ")
raw3<- readRDS(file = "rawInsectsForRichnessAggregatedPerYear.RDS")
raw3ab<- subset(raw3, Unit_in_data == "abundance")

dim(all.metrics)
all.metrics<- merge(all.metrics, plots); dim(all.metrics)
all.metrics<- merge(all.metrics, studies, by = "Datasource_ID"); dim(all.metrics)


# put in right format 
allCalculatedData<- data.frame(
	Datasource_name = all.metrics$Datasource_name, 
	Plot_ID = all.metrics$Plot_ID, 
	Plot_name = all.metrics$Plot_name, 
	Sample_ID = NA, 
	Year = all.metrics$Year,
	Period = "",
	Date = "",
	Taxon = all.metrics$Invertebrate_group, 
	Sex = "", 
	Unit =all.metrics$Unit_in_data, 
	Original_number  = all.metrics$Number, 
	Transformed_number = "", 
	Number = all.metrics$Number, 
	Error = NA,
	ExpectedBeta = NA, 
	SDexpectedBeta = NA, 
	source =  "Simple raw data"
)
# merge n flags
taxPrec<- unique(raw3ab[, c("Plot_ID", "Taxonomic_precision")])
#allCalculatedData$Flag_taxonomy[allCalculatedData$Datasource_name == "Sweden freshwater"] <- "FLAG" # patch until the dataframe is rebuilt from scratch 
dim(allCalculatedData)
allCalculatedData<- merge(allCalculatedData, taxPrec); dim(allCalculatedData)
allCalculatedData$source<- "Simple raw data"
saveRDS(allCalculatedData, file = "C:\\Dropbox\\Insect Biomass Trends/csvs/allCalculatedData.rds")







# 




# Step 8 final data: bind provided and calculated data together #####
allCalculatedData<- readRDS("C:\\Dropbox\\Insect Biomass Trends/csvs/allCalculatedData.rds")

allProvidedDataPure<- readRDS("C:\\Dropbox\\Insect Biomass Trends/csvs/allProvidedDataPure20230822.RDS")

head(allCalculatedData)
head(allProvidedDataPure)


unique(allProvidedDataPure$Taxon)
length(unique(allCalculatedData$Datasource_name)) # 40 datasets (excludes fw)
length(unique(allProvidedDataPure$Datasource_name)) # 119 datasets
length(unique(allCalculatedData$Plot_ID)) # 285 plots
length(unique(allProvidedDataPure$Plot_ID)) # 1149 plots

# bind together
all.resultsPure<- rbind(allCalculatedData,  allProvidedDataPure ); dim(all.resultsPure) #606687     18
all.resultsPure <- merge(all.resultsPure[ , -c(which (names(all.resultsPure) %in% c( "Plot_name", "Datasource_name") ))], plots); dim(all.resultsPure) # merge in plot data
all.resultsPure <- merge(all.resultsPure, studies[,1:30]); dim(all.resultsPure)  # merge in study data
# 
length(unique(all.resultsPure$Datasource_name)) # 160 datasets

length(unique(all.resultsPure$Plot_ID)) # 1439 plots
test<- table(all.resultsPure$Datasource_name, all.resultsPure$Unit)
test
tail(test, 68)
tail(test, 22)
tail(test, 2)

dim(all.resultsPure)#606687      



metadata_per_plot<-  all.resultsPure %>% 
	group_by(Plot_ID) %>%
	summarise(
		Datasource_ID = unique(Datasource_ID),
		Datasource_name = unique(Datasource_name), 
		nPlot_names = length(unique(Plot_name)), 
		Realm = unique(Realm),
		Taxonomic_scope =  (unique(Invertebrate_group)),
		Taxonomic_precision =  paste(unique(Taxonomic_precision), collapse = " "),
		Source_of_values = unique(source),
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T),
		Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
		Country_State = unique(Country_State),
		Country = unique(Country),
		Region = unique(Region),
		#Stratum = length(unique(Stratum)),
		Longitude = unique(Longitude),
		Latitude = unique(Latitude),
		AbundanceBiomass = unique(Abundance.Biomass),
		Number_of_samples = length(unique(paste(Year, Period))),
		Number_of_years = length(unique(Year))
	)
dim(metadata_per_plot) # 1439
metadata_per_plot
str(metadata_per_plot)





# Step 9 Prepare data for INLA: #####

# again check to remove short plots (being very strict now on the 10 years. we only keep Israel and Ukraine, Panama and Brazil
too.short<- subset(metadata_per_plot, Duration<10)
print(too.short, n = Inf)
too.short.sel<- subset(too.short, !Datasource_ID %in% c(1471, 1481, 1541, 1530))
print(too.short.sel, n=Inf) # 
dim(all.resultsPure)
all.resultsPure<- subset(all.resultsPure, !Plot_ID %in% too.short.sel$Plot_ID )
length(unique(all.resultsPure$Datasource_ID)) # 161


dim(all.resultsPure) #     
subset(all.resultsPure, Number < 0) # only skewness
dim(subset(all.resultsPure, Number == 0)) # still there



addIndicies <- function(myData){
	
	#year covariates
	myData$cYear <- myData$Year - floor(median(myData$Year))
	myData$iYear <- myData$Year - min(myData$Year) + 1
	myData$rYear <- myData$iYear
	myData$rYear2 <- myData$iYear
	
	#random intercept indices (these are nested)
	myData$Plot_ID_4INLA <- interaction(myData$Datasource_ID,myData$Plot_ID)
	myData$Plot_ID_4INLA <- as.numeric(factor(myData$Plot_ID_4INLA))   
	myData$Datasource_ID_4INLA <- as.numeric(factor(myData$Datasource_ID))
	myData$Country_State_4INLA <- as.numeric(factor(myData$Country_State))
	
	# This is now a crossed random effect: accounting for datasets that were collected at the same location
	#  myData$Location[is.na(myData$Location)] <- 1#dummy value
	# myData$Location_4INLA <- interaction(myData$Datasource_ID,myData$Location) # this is not necessary anymore
	myData$Location_4INLA <- as.numeric(factor(myData$Location))
	
	myData$Period_4INLA <- interaction(myData$Datasource_ID,myData$Period)
	myData$Period_4INLA <- as.numeric(factor(myData$Period_4INLA))
	
	#random slope indices
	myData$Plot_ID_4INLAs <- myData$Plot_ID_4INLA+max(myData$Plot_ID_4INLA)
	myData$Datasource_ID_4INLAs <- myData$Datasource_ID_4INLA+max(myData$Datasource_ID_4INLA)
	myData$Location_4INLAs <- myData$Location_4INLA+max(myData$Location_4INLA)
	myData$Country_State_4INLAs <- myData$Country_State_4INLA+max(myData$Country_State_4INLA)
	
	# add indices for Inla. VEry important to have different indices for the biomass and abundance data in the same dataset! 
	# otherwise Inla thinks these different metrics are drawn from the same distribution! 
	myData$DSunit_4INLA <- interaction(myData$Datasource_ID,myData$Unit)
	myData$DSunit_4INLA <- as.numeric(factor(myData$DSunit_4INLA))
	myData$Locunit_4INLA <- interaction(myData$Location,myData$Unit)
	myData$Locunit_4INLA <- as.numeric(factor(myData$Locunit_4INLA))
	myData$Plotunit_4INLA <- interaction(myData$Plot_ID, myData$Unit)
	myData$Plotunit_4INLA <- as.numeric(factor(myData$Plotunit_4INLA))
	# random slopes
	myData$Plotunit_4INLAs <- myData$Plotunit_4INLA+max(myData$Plotunit_4INLA)
	myData$DSunit_4INLAs   <- myData$DSunit_4INLA+max(myData$DSunit_4INLA)
	myData$Locunit_4INLAs <- myData$Locunit_4INLA+max(myData$Locunit_4INLA)
	
	unique(myData$Unit)
	myData$Unit<- droplevels(myData$Unit)
	
	
	
	
	return(myData)
}

completeData2023pure <- addIndicies(all.resultsPure); dim(completeData2023pure)

length(unique(completeData2023pure$Datasource_ID)) # 158





save(completeData2023pure, file = "completeData2023pure.RData")

unique(completeData2023pure$Period)
unique(completeData2023pure$Datasource_ID)

load("completeData2022pure.RData")
setdiff(unique(completeData2022pure$Datasource_ID),unique(completeData2023pure$Datasource_ID) )
#freshwater:  63  478 1347 1353 1376 1388  1408 1418 1433 1488 1444 1491 1542 1551
# portal ant bait 1364 

setdiff(unique(completeData2023pure$Datasource_ID),unique(completeData2022pure$Datasource_ID) )

#completeData2021$Period[is.na(completeData2021$Period)]<- ""




load( file = "completeData2023pure.RData")

# make final metadata files" 

metadata_per_plot<-  all.resultsPure %>% 
	group_by(Plot_ID) %>%
	summarise(
		Datasource_ID = unique(Datasource_ID),
		Datasource_name = unique(Datasource_name), 
		nPlot_names = length(unique(Plot_name)), 
		Realm = unique(Realm),
		Taxonomic_scope =  (unique(Invertebrate_group)),
		Taxonomic_precision =  paste(unique(Taxonomic_precision), collapse = " "), 
		Source_of_values = unique(source),
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T),
		Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
		Country_State = unique(Country_State),
		Country = unique(Country),
		Region = unique(Region),
		#Stratum = length(unique(Stratum)),
		Longitude = unique(Longitude),
		Latitude = unique(Latitude),
		AbundanceBiomass = unique(Abundance.Biomass),
		Number_of_samples = length(unique(paste(Year, Period))),
		Number_of_years = length(unique(Year))
	)
dim(metadata_per_plot) # 2435
str(metadata_per_plot)
length(unique(completeData2022pure$Plot_ID))# same

setdiff(unique(metadata_per_plot$Plot_ID), unique(completeData2022pure$Plot_ID))
setdiff(unique(completeData2022pure$Plot_ID), unique(metadata_per_plot$Plot_ID))



sample_n(metadata_per_plot, 20)
save(metadata_per_plot, file ="metadata_per_plot richness 20220818.RData")


metadata_per_dataset<-  all.resultsPure %>% 
	group_by(Datasource_ID) %>%
	summarise(
		Datasource_ID = unique(Datasource_ID),
		Datasource_name = unique(Datasource_name), 
		Number_of_metrics = length(unique(Unit)),
		Realm = unique(Realm),
		Taxonomic_scope =  (unique(Invertebrate_group)),
		Taxonomic_precision =  paste(unique(Taxonomic_precision), collapse = " "), 
		Source_of_values = unique(source),
		Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
		Number_of_years_data = length(unique(Year)), 
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T),
		Number_of_plots =  length(unique(Plot_ID)), # should be 1
		Number_of_samples = length(unique(paste(Year, Period))),
		Spatial_extent_degrees = max( abs(max(Latitude)) - abs(min(Latitude)), abs(max(Longitude)) - abs(min(Longitude)) ),
		Mean_longitude = mean(unique(Longitude)),
		Mean_latitude = mean(unique(Latitude)),
		Country_State = unique(Country_State),
		Country = unique(Country),
		Region = unique(Region),
		Realm = unique(Realm)
		#  AbundanceBiomass = unique(Abundance.Biomass),
	) ; nrow(metadata_per_dataset) # 176 incl. australia double

save(metadata_per_dataset, file ="C:/Dropbox/Insect Biomass Trends/csvs/metadata_per_dataset richness 20230818.RData")



metadata_per_dataset <- load( file ="C:/Dropbox/Insect Biomass Trends/csvs/metadata_per_dataset richness 20221208.RData")















# FILE 2: POPULATION DATA FILE #####
raw3<- readRDS(file = "C:\\Dropbox\\Insect Biomass Trends/csvs/rawInsectsForRichnessAggregatedPerYear.RDS") # continue with file raw 3

# Step 1 load population means (over all iterations)#####

popmedian<- readRDS( "C:/Dropbox/Insect Biomass Trends/csvs//allPopMedians.rds")
dim(popmedian)
length(unique(popmedian$Plot_ID)) # fewer plots




# check if there are no years with decimals
unique(popmedian$Year)[unique(popmedian$Year) != floor(unique(popmedian$Year))]
wrongYrs<- unique(popmedian$Year)[unique(popmedian$Year) != floor(unique(popmedian$Year))]
allpltyrs<- unique(popmedian[, c(1,4)])
allpltyrs[allpltyrs$Year %in% wrongYrs, ]


popmedian<- merge(popmedian, taxa); dim(popmedian)
test<- merge(popmedian, plots, all.x = T); dim(test)
test2<- merge(popmedian, plots); dim(test2)
anti_join(test, test2)[, 1:4] # 

popmedian<-merge(popmedian, plots)
popmedian<- merge(popmedian, studies, by = "Datasource_ID"); dim(popmedian)


# add taxonomic precision at dataset level
samplesSubset<- subset(samples, taxonomic_paper == 'addMeans')
samplesSubset<- unique(samplesSubset[, c('Datasource_ID', 'Taxonomic_precision')])
popmedian<- merge(popmedian, samplesSubset  ); dim(popmedian)








# any columns missing? 
setdiff( names(raw3), names(popmedian)) #  Unit is not necessary
popmedian$Unit_in_data <- "abundance"



# Step 2 combine raw population data and means from randomizations####
raw3ab<- subset(raw3, Unit_in_data == "abundance")

dim(raw3ab)
rawPops<- addZeroes(raw3ab) # this could be done faster by making an object to fill...
dim(rawPops)#768556      
head(rawPops)



# random checks
sample(unique(rawPops$Plot_ID), 20)
dcast(subset(rawPops, Plot_ID == 1883), Taxon~Year)




allPops<- rbind(rawPops, popmedian[, names(rawPops)]); dim(allPops) #      
table(allPops$Taxonomic_precision, useNA = 'ifany')


#round non-integers
sum(allPops$Number != floor(allPops$Number))# non-integers
sum(allPops$Number == floor(allPops$Number))

#test what it looks like
test<- dcast(subset(allPops, Plot_ID == 100), Taxon~Year)
test[, 4:5]<- round(test[2:3])# LOOKS ACCEPTABLE. sTILL, IT'S NOT IDEAL
test# will get lots of zeroes

hist(log10(allPops$Number))
allPops$Number<- round(allPops$Number)


# remove populations with only zero counts 
allPops$plotSP<- paste(allPops$Plot_ID, allPops$Taxon, sep = '_')# unique species-plot ID

plotTotals<-dcast(allPops, plotSP~"total", value.var = 'Number', sum) 
dim(plotTotals) #64640 populations
sum(plotTotals$total==0)# only 5911 populations with 0 observations
zeroCounts<- subset(plotTotals, total == 0); dim(zeroCounts)

checkzeroes<- subset(allPops, plotSP %in% zeroCounts$plotSP)
sum(checkzeroes$Number)# = 0

allPops<- subset(allPops, !plotSP %in% zeroCounts$plotSP); dim(allPops)






dim(allPops) #        lines
length(unique(allPops$Taxon)) #  taxa
length(unique(allPops$Plot_ID)) #  plots
length(unique(allPops$Datasource_ID)) # 69 studies
dim(unique(allPops[, c("Taxon", "Plot_ID")])) # 58729 unique time series of species (but some plots will still be excluded) 

saveRDS(allPops, file = "C:/Dropbox/Insect Biomass Trends/csvs/allpops before processing 2023.rds")



allPops<- readRDS(file = "C:/Dropbox/Insect Biomass Trends/csvs/allpops before processing 2023.rds")
dim(allPops)



# assign SAD intervals to all species in all plots
allPopsT<- subset(allPops, Realm == 'Terrestrial')

plts<- sort(as.numeric(unique(allPopsT$Plot_ID))); length(plts)# 605


















#Step 3 assign commonness groups #####

dim(allPopsT) #594250      
allPopsT<- allPopsT %>% select(-plotSP)
# assign all species the SAD interval of the start of the time series
# and remove all species with 0 individuals 
allPopsPrepd<- NULL
noInsectsYr1<- NULL
discardedPlots <- NULL
startTime<- Sys.time()
allSpDoms<- NULL
short<- NULL
for(i in 1:length(plts)){  #length(plts) # 
	
	plt<- plts[i]
	print(paste(i, "plot" , plt))
	dat<- subset(allPopsT, Plot_ID == plt)	; dim(dat)
	dim(dat)
	length(unique(dat$Year))
	
	# skip for short timeseries
	if(max(dat$Year)- min(dat$Year)<8){print('too short')
		short<- rbind(short, paste(i, "plot" , plt))
		next} # remove some 9yr data later 
	
	
	# clean up taxonomy 
	#merge some species aggregates
	#	dat0<- manual_taxon_cleaning(dat)
	#	dim(dat0)
	dat0<-dat	
	
	dat0.5<- merge(dat0, taxa[, -(which(names(taxa) %in% c("ID", "validTaxon", "Note", "FLAG")))])
	dim(dat0.5)
	if(nrow(dat0) != nrow(dat0.5)){print("WARNING, merge gone wrong")}
	
	
	
	# only for datasets with good taxonomy: 
	# clean up genus level id's by proportionally allocating them to underlying species
	if(length(unique(dat0.5$Taxonomic_precision)) >1 ){ print("WARNING Taxonomic precision not unique")}
	
	if(unique(dat0.5$Taxonomic_precision) == "species"){
		dat1<- as.data.frame(cleaning_dataset (dat0.5, mode =  'beta', add_unique_sample_ID = FALSE, keep_null_abundances = TRUE) )
	}	 else{
		dat1 <- dat0.5
	}	
	# fix up missing columns
	dat1<- arrange(dat1, Taxon)
	
	dat1[is.na(dat1)]<- ""
	
	dat1$Unit_in_data[(dat1$Unit_in_data)==""]<- unique(dat0.5$Unit_in_data)
	dat1$Realm[(dat1$Realm)==""]<- unique(dat0.5$Realm)
	dat1$Taxonomic_precision[(dat1$Taxonomic_precision)==""]<- unique(dat0.5$Taxonomic_precision)
	
	dat1<- (aggregate(dat1, Number ~., sum)) # aggregate the rows where some species may have been added
	dim(dat1)
	#	remove taxa with 0 (remaining) individuals in plot
	means<- aggregate(dat1, Number~Taxon, mean); dim(means)
	means<- subset(means, Number != 0); dim(means)		
	dat1<- subset(dat1, Taxon %in% means$Taxon) # to remove absent species			
	dim(dat1)
	
	# add Year indices to plot. This will conflict with the indices for Inla
	dat1$cYear<- scale(dat1$Year, scale = F)
	dat1$indexYear <- dat1$Year - min(dat1$Year) + 1 # relative year for censoring
	dat1$fYear <- as.factor(dat1$Year)	
	dat1$negIndex<- dat1$Year - max(dat1$Year) -1
	
	arrange(unique(dat1[, c("Year","indexYear", "negIndex")]), Year)
	reshape2::dcast(dat1, Taxon~Year, value.var = "Number", sum)
	
	
	# some tests on data 
	if(sum(subset(dat1, indexYear == 1)$Number) == 0){ # if no insetcs in yr 1
		print("WARNING: No insects observed in Year 1. Skipped")
		noInsectsYr1<-rbind(noInsectsYr1, c(dat1$Datasource_ID[1], dat1$Plot_ID[1]))
		next}
	
	if(length(unique(subset(dat1, Number >0)$Taxon))== 1){ # if only 1 sp
		print("WARNING: only 1 species in plot")}	
	
	
	if(length(unique(subset(dat1, indexYear == 1 & Number > 0)$Taxon))==1){
		print("WARNING: only 1 species in year 1")	}
	
	
	
	
	#CLASSIFICATIONS
	
	# classification on yr 1 in relation to highest observed value ever 
	yr1<- subset(dat1, indexYear == 1)
	
	# rescale if any non-zero <1
	if (min(dat1$Number[dat1$Number>0]) <1 ){ # if the smallest non-zero number < 1
		maxSAD<- log10(max(dat1$Number* 1/(min(dat1$Number[dat1$Number>0]))))
		yr1$NrScaled<- yr1$Number* 1/(min(dat1$Number[dat1$Number>0]))	# rescale to prevent abundances <1
		yr1$SADpropYr1 <- log10(yr1$NrScaled)/maxSAD 
	} else {
		maxSAD<- log10(max(dat1$Number))
		yr1$SADpropYr1 <- log10(yr1$Number)/maxSAD 
	}
	
	
	# maximum number ever observed as maximum dominance
	
	yr1$CGYr1 <- NA
	
	#yr1$CGYr1 [yr1$SADpropYr1 < 0] <- 1 # logged numbers <1 will be negative numbers. These go in group 1, since they were present, but very rare
	yr1$CGYr1 [yr1$SADpropYr1  >= 0   & yr1$SADpropYr1 <=  0.2] <- 1
	yr1$CGYr1 [yr1$SADpropYr1  >  0.2 & yr1$SADpropYr1 <=  0.4] <- 2
	yr1$CGYr1 [yr1$SADpropYr1  >  0.4 & yr1$SADpropYr1 <=  0.6] <- 3
	yr1$CGYr1 [yr1$SADpropYr1  >  0.6 & yr1$SADpropYr1 <=  0.8] <- 4
	yr1$CGYr1 [yr1$SADpropYr1  >  0.8 & yr1$SADpropYr1 <=  1] <- 5
	yr1$CGYr1 [yr1$SADpropYr1 ==  -Inf] <- 0
	yr1$CGYr1 [yr1$SADpropYr1 ==  Inf] <- 0 # for dividing by negative number
	
	if(any(is.na(yr1$CGYr1))){ 
		yr1$CGYr1 <- NA
		print("Commonness group 1 has NAs")}
	##	if(any(yr1$SADpropYr1 >  1)){ print("SADprop 1 has numbers >1")}
	
	
	# in relation to highest value in yr 1	
	if (min(dat1$Number[dat1$Number>0]) <1 ){ # if the smallest non-zero number < 1
		maxSAD<- log10(max(yr1$NrScaled))
		yr1$SADpropYr1.0 <- log10(yr1$NrScaled)/maxSAD 
	} else {
		maxSAD<- log10(max(yr1$Number))
		yr1$SADpropYr1.0 <- log10(yr1$Number)/maxSAD 
	}
	
	
	yr1$CGYr1.0 <- NA
	
	#	yr1$CGYr1.0 [yr1$SADpropYr1.0 >  1] <- 1 # logged numbers <1 will be negative numbers. These go in group 1, since they were present, but very rare
	yr1$CGYr1.0 [yr1$SADpropYr1.0 >= 0   & yr1$SADpropYr1.0<= 0.2 ] <- 1
	yr1$CGYr1.0 [yr1$SADpropYr1.0  >  0.2 & yr1$SADpropYr1.0 <=  0.4] <- 2
	yr1$CGYr1.0 [yr1$SADpropYr1.0  >  0.4 & yr1$SADpropYr1.0 <=  0.6] <- 3
	yr1$CGYr1.0 [yr1$SADpropYr1.0  >  0.6 & yr1$SADpropYr1.0 <=  0.8] <- 4
	yr1$CGYr1.0 [yr1$SADpropYr1.0 >  0.8 & yr1$SADpropYr1.0 <=  1] <- 5
	yr1$CGYr1.0 [yr1$SADpropYr1.0 ==  -Inf] <- 0
	yr1$CGYr1.0 [yr1$SADpropYr1.0 ==  Inf] <- 0 # for dividing by negative number
	
	if(any(is.na(yr1$CGYr1.0))){ 
		yr1$CGYr1.0 <- NA
		print("Commonnessgroup 1.0 had NAs")
		discardedPlots <- rbind(discardedPlots, c('CG1.0', dat1$Datasource_ID[1], dat1$Plot_ID[1]))
	}
	
	
	dat1<- merge(dat1, yr1[, c("Taxon", "SADpropYr1", "CGYr1", "SADpropYr1.0", "CGYr1.0" )],all.x = T)
	dim(dat1)
	
	
	
	
	# classification based on mean of yr 1 and 2	
	yr1.2<- subset(dat1, indexYear <=2)
	means1.2<- aggregate(yr1.2, Number~Taxon, mean)
	
	
	# rescale to prevent abundances <1
	means1.2$NrScaled<- means1.2$Number* 1/(min(means1.2$Number[means1.2$Number>0]))
	
	
	maxSAD<- log10(max(means1.2$NrScaled))
	
	means1.2$CG1.2 <- NA
	means1.2$SADpropYr1.2 <- log10(means1.2$NrScaled)/maxSAD 
	
	means1.2$CG1.2 [means1.2$SADpropYr1.2  >= 0   & means1.2$SADpropYr1.2<= 0.2 ] <- 1
	means1.2$CG1.2 [means1.2$SADpropYr1.2  >  0.2 & means1.2$SADpropYr1.2 <=  0.4] <- 2
	means1.2$CG1.2 [means1.2$SADpropYr1.2  >  0.4 & means1.2$SADpropYr1.2 <=  0.6] <- 3
	means1.2$CG1.2 [means1.2$SADpropYr1.2  >  0.6 & means1.2$SADpropYr1.2 <=  0.8] <- 4
	means1.2$CG1.2 [means1.2$SADpropYr1.2  >  0.8 & means1.2$SADpropYr1.2 <=  1] <- 5
	means1.2$CG1.2 [means1.2$SADpropYr1.2 ==  Inf] <- 0
	means1.2$CG1.2 [means1.2$SADpropYr1.2 ==  -Inf] <- 0
	
	if(any(is.na(means1.2$CG1.2))){ # if cg can't be calculated -> replace with NA's (i.e. discard plot)
		means1.2$CG1.2 <- NA 
		print("Commonnessgroup 1.2 had NAs")
		
		discardedPlots <- rbind(discardedPlots, c('CG1.2', dat1$Datasource_ID[1], dat1$Plot_ID[1]))
	}
	
	noInsectsYr1
	
	table(discardedPlots[,1], discardedPlots[,3])
	table(discardedPlots[,1], discardedPlots[,2])
	# nb datasets with discarded plots: 
	union(noInsectsYr1[,1], discardedPlots[,2])
	
	
	dat1<- merge(dat1, means1.2[, c("Taxon", "SADpropYr1.2", "CG1.2")], all.x = T)
	dim(dat1)
	# year 1-5
	yr1.5<- subset(dat1, indexYear <=5)
	means1.5<- aggregate(yr1.5, Number~Taxon, mean)
	
	# rescale to prevent abundances <1
	means1.5$NrScaled<-means1.5$Number* 1/(min(means1.5$Number[means1.5$Number>0]))
	
	
	
	maxSAD<- log10(max(means1.5$NrScaled))
	
	means1.5$logNr <- log10(means1.5$NrScaled)
	means1.5$CG1.5 <- NA
	means1.5$SADpropYr1.5 <- log10(means1.5$NrScaled)/maxSAD 
	
	means1.5$CG1.5 [means1.5$SADpropYr1.5  >= 0   & means1.5$SADpropYr1.5<= 0.2  ] <- 1
	means1.5$CG1.5 [means1.5$SADpropYr1.5  >  0.2 & means1.5$SADpropYr1.5 <=  0.4] <- 2
	means1.5$CG1.5 [means1.5$SADpropYr1.5  >  0.4 & means1.5$SADpropYr1.5 <=  0.6] <- 3
	means1.5$CG1.5 [means1.5$SADpropYr1.5  >  0.6 & means1.5$SADpropYr1.5 <=  0.8] <- 4
	means1.5$CG1.5 [means1.5$SADpropYr1.5  >  0.8 & means1.5$SADpropYr1.5 <=  1  ] <- 5
	means1.5$CG1.5 [means1.5$SADpropYr1.5 ==  Inf] <- 0
	means1.5$CG1.5 [means1.5$SADpropYr1.5 == -Inf] <- 0
	
	if(any(is.na(means1.5$CG1.5))){ 
		print("Commonnessgroup 1.5 had NAs")
		means1.5$CG1.5 <- NA 
		discardedPlots <- rbind(discardedPlots, c('CG1.5', dat1$Datasource_ID[1], dat1$Plot_ID[1]))
	}
	
	dat1<- merge(dat1, means1.5[, c("Taxon", "SADpropYr1.5", "CG1.5")], all.x = T)
	
	
	# mean over all years
	means<- aggregate(dat1, Number~Taxon, mean); dim(means)
	
	# rescale to prevent abundances <1
	means$NrScaled<-means$Number* 1/(min(means$Number[means$Number>0]))
	
	means$CGallYrs <- NA
	maxSAD<- log10(max(means$NrScaled))
	
	means$SADpropAll <- log10(means$NrScaled)/maxSAD 
	
	means$CGallYrs [means$SADpropAll  >= 0   & means$SADpropAll <= 0.2] <- 1
	means$CGallYrs [means$SADpropAll  >  0.2 & means$SADpropAll <=  0.4] <- 2
	means$CGallYrs [means$SADpropAll  >  0.4 & means$SADpropAll <=  0.6] <- 3
	means$CGallYrs [means$SADpropAll  >  0.6 & means$SADpropAll <=  0.8] <- 4
	means$CGallYrs [means$SADpropAll  >  0.8 & means$SADpropAll <=  1  ] <- 5
	means$CGallYrs [means$SADpropAll  <  0  ] <- 1 # should change this to 1
	
	if(any(is.na(means$CGallYrs))){ 
		means$CGallYrs <- NA 
		print("Commonnessgroup 'all years' had NAs")
		discardedPlots <- rbind(discardedPlots, c('CGallYrs', dat1$Datasource_ID[1], dat1$Plot_ID[1]))
	}
	
	dat1<- merge(dat1, means[, c("Taxon", "SADpropAll", "CGallYrs")], all = T)
	dim(dat1)
	
	# # check how similar the classificaions are 
	spdoms <-as.data.frame(unique(dat1[, c('Plot_ID', 'Taxon', 'SADpropYr1', 'SADpropYr1.0','SADpropYr1.2', 'SADpropYr1.5' ,  'SADpropAll','CGYr1','CGYr1.0','CG1.2', 'CG1.5', 'CGallYrs') ])		)
	#ggpairs(dat1[, c(  'CGYr1','CGYr1','CG1.2', 'CG1.5', 'CGallYrs') ])
	# length(unique(dat1$Year))
	# dcast(dat1, Family + Subfamily + Genus + Taxon ~ Year, value.var = "Number")
	# ggpairs(dat1[, c( 'SADpropYr1', 'SADpropYr1.2', 'SADpropYr1.5',  'SADpropAll')])
	
	
	allPopsPrepd<- rbind(allPopsPrepd, dat1)
	
}
Sys.time() - startTime # 10 mins
short
# NA's are present in the data when there were too few species in the classification years to allocate classification.
# in other classifications the may be present. 


# tests:
dcast(subset(allPopsPrepd, Plot_ID == 6), Taxon ~ Year, value.var = 'Number')

subset(allPopsPrepd, Plot_ID == 494 & Taxon == 'Ribautodelphax_pungens')
subset(allPopsPrepd, plotSP == "")
arrange(subset(allPopsPrepd, Plot_ID == 133 & Taxon == 'Entomobrya_lanuginosa'), Year)

dcast(subset(allPopsPrepd, Plot_ID == 2224), Taxon ~ Year, value.var = 'Number')

plt<- sample(unique(allPopsPrepd$Plot_ID), 1)
print(plt)
dcast(subset(allPopsPrepd, Plot_ID == plt), Taxon ~ Year, value.var = 'Number')



#no duplicates it seems

# Step 4 remove too short plots #####
metadata_per_plotPop<-  allPopsPrepd %>% 
	group_by(Plot_ID) %>%
	summarise(
		Datasource_ID = unique(Datasource_ID),
		#	Datasource_name = unique(Datasource_name), 
		#	nPlot_names = length(unique(Plot_name)), 
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T),
		Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1
	)
too.short<- subset(metadata_per_plotPop, Duration<10)
print(too.short, n = Inf)
too.short.sel<- subset(too.short, !Datasource_ID %in% c(1471, 1481, 1541, 1530))
print(too.short.sel, n=Inf) # 27 plots
dim(allPopsPrepd)
all.resultsPure<- subset(allPopsPrepd, !Plot_ID %in% too.short.sel$Plot_ID )
length(unique(allPopsPrepd$Datasource_ID)) # 56






str(allPopsPrepd)
unique(allPopsPrepd$Realm)
names(allPopsPrepd)
uniqPlts<- (unique(allPopsPrepd[, c("Plot_ID", "Realm")])) # 
table(uniqPlts$Realm)# 584

table(allPopsPrepd$CGYr1, useNA = 'ifany')
table(allPopsPrepd$CGYr1.0, useNA = 'ifany')
table(allPopsPrepd$CG1.2, useNA = 'ifany')
table(allPopsPrepd$CG1.5, useNA = 'ifany')
table(allPopsPrepd$CGallYrs, useNA = 'ifany')


subset(allPopsPrepd, is.na(CGYr1) )

# Step 5 prepare for INLA analysis #####

dim(allPopsPrepd)
allPopsPrepd<- merge(allPopsPrepd, plots); dim(allPopsPrepd)


prepInlaPop <- function(myData){
	
	
	myData$TaxonNum1<- as.numeric(as.factor(myData$Taxon))
	myData$TaxonNum2<- myData$TaxonNum1 + max(myData$TaxonNum1)
	
	#year covariates
	myData$cYear <- myData$Year - floor(median(myData$Year))
	myData$iYear <- myData$Year - min(myData$Year) + 1
	myData$rYear <- myData$iYear
	myData$rYear2 <- myData$iYear
	
	#random intercept indices (these are nested)
	myData$Datasource_ID_4INLA <- as.numeric(factor(myData$Datasource_ID))
	myData$Plot_ID_4INLA <- interaction(myData$Datasource_ID,myData$Plot_ID)
	myData$Plot_ID_4INLA <- as.numeric(factor(myData$Plot_ID_4INLA))   
	
	# This is now a crossed random effect: accounting for datasets that were collected at the same location
	#  myData$Location[is.na(myData$Location)] <- 1#dummy value
	myData$Location_4INLA <- interaction(myData$Datasource_ID,myData$Location) # this is not necessary anymore
	myData$Location_4INLA <- as.numeric(factor(myData$Location))
	
	
	#random slope indices
	myData$Plot_ID_4INLAs <- myData$Plot_ID_4INLA+max(myData$Plot_ID_4INLA)
	myData$Datasource_ID_4INLAs <- myData$Datasource_ID_4INLA+max(myData$Datasource_ID_4INLA)
	myData$Location_4INLAs <- myData$Location_4INLA+max(myData$Location_4INLA)
	
	#taxon-plot combinations
	myData$TaxonPlot_4INLA <- interaction(myData$Plot_ID, myData$Taxon)
	myData$TaxonPlot_4INLA <- as.numeric(factor(myData$TaxonPlot_4INLA))
	myData$TaxonLoc_4INLA <- interaction(myData$Location, myData$Taxon)
	myData$TaxonLoc_4INLA <- as.numeric(factor(myData$TaxonLoc_4INLA))
	myData$TaxonDS_4INLA <- interaction(myData$Plot_ID, myData$Datasource_ID)
	myData$TaxonDS_4INLA <- as.numeric(factor(myData$TaxonDS_4INLA))
	# random slopes
	myData$TaxonPlot_4INLAs <- myData$TaxonPlot_4INLA+max(myData$TaxonPlot_4INLA)
	myData$TaxonLoc_4INLAs <-  myData$TaxonLoc_4INLA +max(myData$TaxonLoc_4INLA)
	myData$TaxonDS_4INLAs   <- myData$TaxonDS_4INLA  +max(myData$TaxonDS_4INLA)	
	#myData$Unit<- droplevels(myData$Unit)
	
	
	
	
	return(myData)
}

allPopsPrepd2<- prepInlaPop(allPopsPrepd); dim(allPops)


# step 6 save file 
saveRDS(allPopsPrepd2, "allPopulations 2023.rds")






















