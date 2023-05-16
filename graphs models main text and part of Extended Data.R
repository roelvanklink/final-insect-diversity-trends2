rm(list=ls()) 

library(INLA)
library(brinla)
library(ggplot2)
library(ggnewscale)
library(data.table)
library(tidyverse)
library(reshape2)
#source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/plotting functions.R")



# set theme and color scheme for figures	
theme_clean<- theme_grey() + theme(panel.grid.major = element_blank(), 
																	 panel.grid.minor = element_blank(),
																	 panel.background = element_blank(), 
																	 axis.line = element_line(colour = "black") , 
																	 legend.key=element_blank())

col.scheme.realm<-c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "chocolate4")
col.scheme.realm2 <- c(  "Freshwater"  = "dodgerblue2", "Freshwater2"  = "dodgerblue2",  "Terrestrial" = "chocolate4", "Terrestrial2" = "chocolate4")
col.scheme.realm3<-c(  "Fw"  = "dodgerblue2", "Ter" = "chocolate4")
shps<- c("Freshwater" = 24, "Terrestrial" = 21 )#, "Both realms" = 22)
col.scheme.global<- c(  "Global"  = "grey10", "Observed" = "grey70")  #
col.scheme.black<- c(  "Global"  = "black", "Observed" = "black")  #
sz = 0.5

mypalett<- colorRampPalette  (c("#CC0000", "#FF6666", "cornsilk2", "dodgerblue2", "dodgerblue4"), space = "rgb")

# set tick marks for figures 	
brks<- c(-0.02, -0.01, 0, 0.01, 0.02)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")




# load data files
plots<-as.data.frame(fread( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/PlotData 5.0.csv")); dim(plots)
studies<-read.csv(file = "studies 5.2.csv", header = T); dim(studies)
allPops<- readRDS(file = "C:\\Dropbox\\Insect Biomass Trends/csvs/allpopulations.rds"); dim(allPops)# 
load("C:\\Dropbox\\Insect Biomass Trends/csvs/completeData2022pure.Rdata"); dim(completeData2022pure) 
completeData2022pure$pltYr<- paste0(completeData2022pure$Plot_ID, "_", completeData2022pure$Year)
completeData2022pure$Continent[completeData2022pure$Continent == "Central America" | completeData2022pure$Continent == "South America" ] <- "Latin America"



setwd("C:\\Dropbox\\Insect Biomass Trends/csvs") # work
figure_path<- "D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/Figures/" # to store figures





# exclude experimental plots and plots with bad taxonomy
exptPlots<- c(5, # alaska
							921, 922, 924,925, #smes
							643, 644, 646, 647, # hemlock removal
							137, 138, 139  #brazil fragmentation experiment
)
exptDatasources<- c(300,1364, 1357,1410) #Kellogg, Luquillo CTE, Cedar creek big bio, some German grassland


# exclude plots with a trend in taxonomic resolution: 
bad_tax<- 
	c(849L, 132L, 1442L, 1527L, 10001L, 10003L, 10005L, 10006L, 10007L, 10008L, 10009L, 10011L, 10012L, 10015L, 10016L, 10017L, 10018L, 
		10019L, 10021L, 10024L, 10025L, 10026L, 10028L, 10029L, 10032L, 10033L, 10034L, 10035L, 10036L, 10038L, 10039L, 10044L, 10048L, 
		10049L, 10051L, 10052L, 10056L, 10057L, 10058L, 10059L, 10063L, 10066L, 10067L, 10068L, 10070L, 10072L, 10075L, 10079L, 10080L, 
		10081L, 10082L, 10084L, 10085L, 10087L, 10092L, 10093L, 10097L, 10099L, 10106L, 10108L, 10113L, 10114L, 10115L, 10119L, 10124L, 
		10125L, 10127L, 10128L, 10130L, 10135L, 10137L, 10138L, 10145L, 10146L, 10147L, 10149L, 10158L, 10160L, 10163L, 10164L, 10165L, 
		10168L, 10169L, 10170L, 10171L, 10173L, 10175L, 10178L, 10180L, 10181L, 10182L, 10185L, 10187L, 10188L, 10189L, 10192L, 10193L, 
		10195L, 10196L, 10198L, 10349L, 10200L, 10201L, 10204L, 10206L, 10208L, 10212L, 10215L, 10216L, 10219L, 10221L, 10223L, 10224L, 
		10225L, 10226L, 10227L, 10229L, 10230L, 10233L, 10236L, 10249L, 10250L, 10252L, 10253L, 10254L, 10255L, 10256L, 10257L, 10258L, 
		10259L, 10277L, 10279L, 10280L, 10281L, 10282L, 10283L, 10284L, 10285L, 10286L, 10287L, 10289L, 10290L, 10291L, 10292L, 10294L, 
		10295L, 10297L, 10298L, 10299L, 10301L, 10302L, 10303L, 10304L, 10305L, 10306L, 10307L, 10308L, 10309L, 10311L, 10312L, 10313L, 
		10314L, 10315L, 10316L, 10317L, 10318L, 10320L, 10321L, 10322L, 10323L, 10325L, 10326L, 10327L, 10328L, 10329L, 10331L, 10332L, 
		10334L, 10335L, 10336L, 10337L, 10338L, 10339L, 10340L, 10342L, 10343L, 10344L, 10346L, 10348L, 10469L, 10350L, 10351L, 10352L, 
		10353L, 10354L, 10355L, 10356L, 10357L, 10361L, 10362L, 10364L, 10365L, 10366L, 10368L, 10369L, 10370L, 10371L, 10373L, 10374L, 
		10375L, 10376L, 10378L, 10379L, 10380L, 10381L, 10382L, 10383L, 10384L, 10385L, 10386L, 10387L, 10388L, 10389L, 10390L, 10391L, 
		10392L, 10393L, 10394L, 10395L, 10398L, 10399L, 10400L, 10401L, 10403L, 10404L, 10407L, 10408L, 10409L, 10410L, 10412L, 10413L, 
		10414L, 10415L, 10416L, 10417L, 10418L, 10419L, 10421L, 10422L, 10424L, 10425L, 10426L, 10427L, 10428L, 10429L, 10430L, 10431L, 
		10432L, 10433L, 10434L, 10435L, 10436L, 10437L, 10438L, 10439L, 10475L, 10440L, 10441L, 10442L, 10443L, 10444L, 10445L, 10446L, 
		10448L, 10449L, 10485L, 10452L, 10454L, 10455L, 10457L, 10458L, 10459L, 10460L, 10463L, 10464L, 10465L, 10467L, 10468L, 10470L, 
		10474L, 10499L, 10491L, 10490L, 10498L, 10489L, 10506L, 10503L, 10511L, 10504L, 10500L, 10501L)

completeData2022pure<- completeData2022pure[!completeData2022pure$Plot_ID %in% bad_tax, ]
completeData2022pure<- completeData2022pure[!completeData2022pure$Datasource_ID %in% exptDatasources, ]
completeData2022pure<- completeData2022pure[!completeData2022pure$Plot_ID %in% exptPlots, ]
dim(completeData2022pure)

allPops<- allPops[!allPops$Datasource_ID %in% exptDatasources, ]
allPops<- allPops[!allPops$Plot_ID %in% exptPlots, ]
allPops<- allPops[!allPops$Plot_ID %in% bad_tax, ]
dim(allPops)

# rename for ease of use
completeData2022<- completeData2022pure









# make Supplementary data 1####
dim(completeData2022pure)
unique(completeData2022pure$Unit)
completeData2022pureSelectedMetrics<- completeData2022pure[ completeData2022pure$Unit %in% c("abundance", "richness",  "rarefiedRichness",     "Shannon", "ENSPIE", "coverageRichness.8" ,  # Fig 1
																																							"logNr020", "logNr2040", "logNr4060",  "logNr6080", "logNr80100",  # Fig 2
																																							"logNrQ1"  , "logNrQ2",  "logNrQ3",   "logNrQ4"	),]  # Fig S6
dim(completeData2022pureSelectedMetrics)	
	
# rename 'unit' to metric 
names(completeData2022pureSelectedMetrics)[names(completeData2022pureSelectedMetrics) == "Unit"]<- "Metric"

# select only used columns
completeData2022pureSelectedMetrics<- 	completeData2022pureSelectedMetrics[,  c("Plot_ID", "Datasource_ID", "Datasource_name",
"Year","Period", "Metric", "Number", "Realm", "Continent", "Region","Country","Country_State","Location", "cYear", "iYear", 
"rYear", "rYear2", "Period_4INLA", "Plot_ID_4INLA", "Datasource_ID_4INLA", "Location_4INLA", "Plot_ID_4INLAs", "Datasource_ID_4INLAs", "Location_4INLAs"  )  ] # columns to remove
	
saveRDS( completeData2022pureSelectedMetrics, file = "C:\\Dropbox\\Insect Biomass Trends/csvs/Supplementary Data 1.rds")

	
# make Supplementary data 2 (populations)#####
allPopsSelectedCols<- 	allPops[,  c("Datasource_ID","Plot_ID", "Location","Realm", "Year",  "Number" ,    
																	"Taxon",  "Level" ,   "Rank" ,    
																	"Phylum", "Class",   "Subclass"   , "Order" , "Suborder" , "Family", "Subfamily", "Genus", "Species", "validTaxon" ,    
																	 "Note" ,              
																   "cYear" ,                  
													         "CGYr1.0", "CG1.2", "CG1.5",  "CGallYrs" , 
																	 "iYear" , "rYear", "rYear2" , 
																	 "Datasource_ID_4INLA"  ,"Plot_ID_4INLA", "Location_4INLA" ,"Plot_ID_4INLAs","Datasource_ID_4INLAs" , "Location_4INLAs" ,        
																	 "TaxonPlot_4INLA", "TaxonPlot_4INLAs" )]
												
saveRDS( allPopsSelectedCols, file = "C:\\Dropbox\\Insect Biomass Trends/csvs/Supplementary Data 2.rds")
	
	
	

	
	
# Basic dataset description #####	
#How many datasets from InsectChaNGE did we use here? 

dsNEW<- unique(subset(completeData2022pure, Realm == "Terrestrial")$Datasource_ID)	
dsOLD<- unique(read.csv("C:/Dropbox/Insect Biomass Trends/csvs/knb achive 7-4-20/DataSources.csv"	)$DataSource_ID)	
length(intersect (dsNEW,dsOLD))		# studies taken from Insectchange
length(setdiff(dsNEW,dsOLD)) # new studies
length(unique(subset(completeData2022pure, Realm == "Terrestrial")$Plot_ID)	) # total number of plots






# make Extended Data Table 1 Data availability: #####

metadata_metrics<- completeData2022 %>%
	group_by (Unit, Realm) %>%
	summarise(
		n_datasets = length(unique(Datasource_ID)),
		n_plots= length(unique(Plot_ID))
	)
metadata_metrics2 <- metadata_metrics
plotRealm<- dcast(metadata_metrics, Unit ~ Realm, value.var = "n_plots" )
names(plotRealm)<- c("Metric"  ,"F_plots",  "T_plots")
DsRealm<- dcast(metadata_metrics, Unit ~ Realm , value.var = "n_datasets")
names(DsRealm)<- c("Metric"  ,      "F_datasets" , "T_datasets")
metadata_metrics<- merge(DsRealm, plotRealm)
metadata_metrics$all_datasets<- rowSums(metadata_metrics[, c("F_datasets", "T_datasets")], na.rm = T)
metadata_metrics$all_plots<- rowSums(metadata_metrics[, c("F_plots", "T_plots")], na.rm = T)




# relect rows and order columns
tabS1<- metadata_metrics[metadata_metrics$Metric %in% c("abundance", "richness", "rarefiedRichness", "ENSPIE", "coverageRichness.7", "logNr4060")    , c("Metric", "T_datasets", "T_plots")   ]
tabS1[c(1,6,3,5,2, 4),]


# make Extended Data Table 5 (all studies)#####
metadata_per_dataset <-subset(completeData2022, Realm == "Terrestrial") %>% 
	group_by(Datasource_ID) %>%
	summarise(
		Datasource_name = unique(Datasource_name),
		Start = min(Year), 
		End = max(Year),
		Time_span_yrs = (max(Year) - min(Year))+1,
		Nr_yrs_data = length(unique(Year)),
		Nb_plots = length(unique(Plot_ID))
			
	)
subset(metadata_per_dataset, Time_span_yrs == max(metadata_per_dataset$Time_span_yrs)) # maximum time span from start to finish
subset(metadata_per_dataset, Nb_plots == max(metadata_per_dataset$Nb_plots)) # highest number of plots
subset(metadata_per_dataset, Nb_plots > 50) # which studies have more than 50 plots?



# make dataframe of which metrics are calculated for each study
metadata_per_dataset_per_metric<- completeData2022 %>% 
	group_by(Datasource_ID, Unit) %>%
	summarise(
		Datasource_name = unique(Datasource_name), 
		count = 1)
md<- reshape2::dcast(metadata_per_dataset_per_metric, Datasource_ID ~ Unit, fill = "0" )
md$ab<- ""; md$ab [md$abundance == 1]<- "A"
md$Rich<-  ""; md$Rich [md$richness == 1]<- "R"
md$comp<- "" ; md$comp [md$logNr020 == 1]<- "C"
md$rarrich<- "" ; md$comp [md$rarefiedRichness ==1 & md$coverageRichness.8 == 0]<- "Rr"
md$diversity<- "" ; md$comp [md$ENSPIE ==1 & md$logNr020 == 0]<- "D"
md$diversity<- "" ; md$comp [md$Shannon ==1 & md$logNr020 == 0]<- "D"

md$Metrics<- paste0(md$ab, md$Rich, md$comp)


# Add number of species species for each dataset
metadata_populations<- allPops %>%
	filter(Realm == 'Terrestrial') %>%
	group_by(Datasource_ID, Datasource_nameREDUNDANT) %>%
	summarise(
		nSpec = length(unique(Taxon)), 
		meanTaxLevel = mean(Rank) )

cbind(
	min(metadata_populations$nSpec ), 
	median(metadata_populations$nSpec ), 
	max(metadata_populations$nSpec ))

# merge in in populations and metrics
metadata_per_dataset<- merge( metadata_per_dataset, metadata_populations[, c(1,3)], all = T)
metadata_per_dataset$nSpec[is.na(metadata_per_dataset$nSpec) ] <- "-" # replace NA with '-'
metadata_per_dataset<- merge( metadata_per_dataset, md[, c(1, ncol(md))], all.x = T)

# add access licenses
metadata_per_dataset$License<- ''
metadata_per_dataset$License[metadata_per_dataset$Datasource_ID %in% c(380, 465, 502, 1310, 1312, 1324,1335, 1339,1365,1367, 1376,1377,1378,1379,1381,
																																			 1382,1384,1385,1387,1388,1391,1392,1393,1394,1395,1396,1397,1398,1400,
																																			 1401,1402,1403,1405,1406,1407, 1411,1412,1413,1414,1415,1417,
																																			 1418,1419,1421,1422,1423,1425,1426,1427,1428,1430,1431,1433,1434,1435,1437,
																																			 1439,1440,1441,1446,1448,1449,1453,1455,1457,1458,1459,1460,1461,1462,1464,
																																			 1465,1467,1468,1470,1471,1472,1473, 1476,1477, 1478,1487,1493,1494,1496,1497,
																																			 1498,1499,1502,1503,1504,1506,1510,1512,1513,1515,1516,
																																			 1517, 1519, 1521, 1524,1527, 1529,1530,1531,1533,1535,1543,1546, 1548,1549,1558,
																																			 1559,1560,1561)] <- 'a' #= '†' #PD: public domain (all data extracted from papers), 
metadata_per_dataset$License[metadata_per_dataset$Datasource_ID %in% c(1006,1263, 1266,1267, 1542  )] <- 'b' #= '‡' #OGL: Open Government License (UK), 
metadata_per_dataset$License[metadata_per_dataset$Datasource_ID %in% c(63,79,294,301,313,375, 478,1102,1261,1319,1328,1346,1347,1349,1361,1474,
																																			 1475, 1479, 1480,1481,1501, 1518, 1525, 1526,1532,1539,1547,1551,
																																			 1553,1554,1555,1556,1557)] <- 'c'#='§' #CC-BY
metadata_per_dataset$License[metadata_per_dataset$Datasource_ID %in% c(249,1340,1351,1353, 1408, 1445 ,1488,1505 ,1528,1541,1544, 1562 )] <- 'd' #= '¶' #CC0, 
metadata_per_dataset$License[metadata_per_dataset$Datasource_ID %in% c(1444, 1491, 1520, 1550  )] <- 'e' #=  '#' #CC-BY-NC
metadata_per_dataset$License[metadata_per_dataset$Datasource_ID %in% c(  )] <- 'f' #= '**'#CC-BY-ND, 
metadata_per_dataset$License[metadata_per_dataset$Datasource_ID %in% c(1429  )] <- 'g' #=  '††' #ODC: Open Data Commons
metadata_per_dataset$License[metadata_per_dataset$Datasource_ID %in% c(1404, 1416  )] <- 'h'  #='‡‡' #no shar: data openly accessible, but no redistribution of data or derived products is allowed, 

	
# fix location of sampling for larger countries
metadata_per_dataset<- merge(metadata_per_dataset, studies[, c("Datasource_ID", "Invertebrate_group", "Country_State", "Country")])
metadata_per_dataset$Place<- metadata_per_dataset$Country
bigCountries<- c('USA','Russia', "China", 'Brazil', 'Australia'   )
metadata_per_dataset$Place[metadata_per_dataset$Country %in% bigCountries]<- # replace USA, Russia, canada, brazil, china with states
	paste0(metadata_per_dataset$Country[metadata_per_dataset$Country %in% bigCountries], ": ", 
				metadata_per_dataset$Country_State[metadata_per_dataset$Country %in% bigCountries])


#reorder columns
tableS5<- metadata_per_dataset[, c("Datasource_ID", "Invertebrate_group", "Place", "Metrics", "Start","End","Time_span_yrs","Nr_yrs_data",       
																	 "Nb_plots", "nSpec", "License")   ]


write.csv(tableS5, file = "D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/Figures/Table S5.csv")



# make Extended data table 2 #####
t(rbind(
	apply(metadata_per_dataset[,c('Nb_plots','Time_span_yrs','Nr_yrs_data','Start','End')],2, min ), 
	apply(metadata_per_dataset[,c('Nb_plots','Time_span_yrs','Nr_yrs_data','Start','End')],2, median ), 
	apply(metadata_per_dataset[,c('Nb_plots','Time_span_yrs','Nr_yrs_data','Start','End')],2, max )) )




# taxonomic resolution #####
samples <-read.csv( file = "Sample_Info 5.2.csv"); dim(samples)
samples<- merge(samples, studies, by = "Datasource_ID"); dim(samples)
studiesIncl<- samples[samples$Datasource_ID %in% completeData2022$Datasource_ID, ]
studiesIncl<- unique(studiesIncl[, c("Datasource_ID", "Taxonomic_precision")]); dim(studiesIncl)
sum(studiesIncl$Taxonomic_precision == "species") / 171 # 55% species level
171 - sum(studiesIncl$Taxonomic_precision == "species")



# prep metadata fot use in graphs or for selection
metadata_per_metric<- completeData2022 %>% 
	group_by(Unit, Realm) %>%
	summarise(
		Nb_datasets = length(unique(Datasource_ID	)), 
		Nb_plots = length(unique(Plot_ID))
	)
print(metadata_per_metric, n = Inf)

dcast(metadata_per_metric, Unit ~ Realm, value.var = "Nb_datasets")
dcast(metadata_per_metric, Unit ~ Realm, value.var = "Nb_plots")


startEndYears<- completeData2022 %>% 
  group_by(Plot_ID) %>%
  summarise(
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T), 
    nYrs = length(unique(Year)),
    )
startEndYears$pltStartYr<- paste0(startEndYears$Plot_ID, "_", startEndYears$Start_year)

metadata_populations<- allPops %>%
	filter(Realm == 'Terrestrial') %>%
	group_by(Datasource_ID, Datasource_nameREDUNDANT) %>%
	summarise(
nSpec = length(unique(Taxon)), 
meanTaxLevel = mean(Rank) )


# check min and max number of species per dataset
cbind(
	min(metadata_populations$nSpec ), 
	median(metadata_populations$nSpec ), 
	max(metadata_populations$nSpec ))




# make object for each biodiversity metric (data the analyses are based on)
cDrichness <- subset(completeData2022, Unit == "richness"); 
cDrarRichness <- subset(completeData2022, Unit == "rarefiedRichness"); dim(cDrarRichness)
cDabund    <- subset(completeData2022, Unit == "abundance"); dim(cDabund)
cDenspie   <- subset(completeData2022, Unit == "ENSPIE")   ;dim(cDenspie)
	cDenspie$Number[is.infinite(cDenspie$Number)] <- 0
cDhorn     <- subset(completeData2022, Unit == "Horn"); dim(cDhorn)
	cDhorn$dif <-  cDhorn$Number - cDhorn$ExpectedBeta
	cDhorn$SES <- (cDhorn$Number - cDhorn$ExpectedBeta) / cDhorn$SDexpectedBeta
	cDhorn<- cDhorn[!cDhorn$pltYr %in% startEndYears$pltStartYr, ] ;dim(cDhorn)
cDbray     <- subset(completeData2022, Unit == "Bray"); dim(cDbray)
	cDbray$dif <-  cDbray$Number - cDbray$ExpectedBeta
	cDbray$SES <- (cDbray$Number - cDbray$ExpectedBeta) / cDbray$SDexpectedBeta
	cDbray<- cDbray[!cDbray$pltYr %in% startEndYears$pltStartYr, ]; dim(cDbray)
cDjaccard  <- subset(completeData2022, Unit == "Jaccard") ; dim(cDjaccard)
	cDjaccard$Number[is.nan(cDjaccard$Number)] <- NA
	cDjaccard$dif <-  cDjaccard$Number - cDjaccard$ExpectedBeta
	cDjaccard$SES <- (cDjaccard$Number - cDjaccard$ExpectedBeta) / cDjaccard$SDexpectedBeta
	cDjaccard<- cDjaccard[!cDjaccard$pltYr %in% startEndYears$pltStartYr, ] ; dim(cDjaccard)
cDlogNr80   <- subset(completeData2022, Unit ==  "logNr80"   )       ; dim(cDlogNr80)   
cDlogNr20   <- subset(completeData2022, Unit == "logNr20"    ); dim(cDlogNr20)  
cDlogNr10   <- subset(completeData2022, Unit == "logNr10"    ); dim(cDlogNr10)  
cDlogNr90   <- subset(completeData2022, Unit ==  "logNr90"   ); dim(cDlogNr90)  
cDdom       <- subset(completeData2022, Unit == "dominanceRel"); dim(cDdom)  
cDlogNr020   <- subset(completeData2022, Unit ==  "logNr020"   ); dim(cDlogNr020)  
cDlogNr2040   <- subset(completeData2022, Unit ==  "logNr2040"   ); dim(cDlogNr2040)  
cDlogNr4060   <- subset(completeData2022, Unit ==  "logNr4060"   ); dim(cDlogNr4060)  
cDlogNr6080   <- subset(completeData2022, Unit ==  "logNr6080"   ); dim(cDlogNr6080)  
cDlogNr80100   <- subset(completeData2022, Unit ==  "logNr80100"   ); dim(cDlogNr80100)  
cDlogQ1   <- subset(completeData2022, Unit ==  "logNrQ1"   ); dim(cDlogQ1)  
cDlogQ2   <- subset(completeData2022, Unit ==  "logNrQ2"   ); dim(cDlogQ2)  
cDlogQ3   <- subset(completeData2022, Unit ==  "logNrQ3"   ); dim(cDlogQ3)  
cDlogQ4   <- subset(completeData2022, Unit ==  "logNrQ4"   ); dim(cDlogQ4)  
cDshan  <- subset(completeData2022, Unit == "Shannon"); dim(cDshan)  
cDpielou <- subset(completeData2022, Unit == "Pielou"); dim(cDpielou)  
cDmcN <- subset(completeData2022, Unit == "dominanceMcNaught"); dim(cDmcN )  
cdCoverageR7	<-subset(completeData2022, Unit == "coverageRichness.7"); dim(cdCoverageR7)
cdCoverageR8	<-subset(completeData2022, Unit == "coverageRichness.8"); dim(cdCoverageR8)
cdCoveragePIE7	<-subset(completeData2022, Unit == "coverageENSpie.7"); dim(cdCoveragePIE7)
cdCoveragePIE8	<-subset(completeData2022, Unit == "coverageENSpie.8"); dim(cdCoveragePIE8)

cDHillNrs <- subset(completeData2022, Unit == "ENSPIE"| Unit == "Shannon" | Unit == "richness")
cDHillNrs <- reshape2::dcast(cDHillNrs[, c(1:9,11:14,37,40, 55, 57:74)], ...~Unit, value.var = "Number") # need to get the Hill numbers as columns
cDHillNrs$ShanEven <- exp(cDHillNrs$Shannon) / cDHillNrs$richness
cDHillNrs$SimpEven<- (cDHillNrs$ENSPIE) / cDHillNrs$richness
cDHillNrs$ShanEven[is.infinite(cDHillNrs$ShanEven)] <- NA
cDHillNrs$SimpEven[is.infinite(cDHillNrs$SimpEven)] <- NA

# remove plots with only NA values: 
onlyNA<- subset( reshape2::dcast(cDHillNrs, Plot_ID ~ "nonNA", value.var = "SimpEven", function(x){sum(!is.na(x))}), nonNA ==0)
cDHillNrs<- subset(cDHillNrs, !Plot_ID %in% onlyNA$Plot_ID)

 # select only datasets with full underlying species data
# for sensitivity analysis
topNotch<- unique(cDlogNr020$Datasource_ID)

cDrichnessReduced<- subset(cDrichness, Datasource_ID %in% topNotch); dim(cDrichnessReduced)
cDabundReduced	<- subset(cDabund, Datasource_ID %in% topNotch); dim(cDabundReduced)
cDenspieReduced	<- subset(cDenspie, Datasource_ID %in% topNotch); dim(cDenspieReduced)
cDrarRichReduced	<- subset(cDrarRichness , Datasource_ID %in% topNotch); dim(cDrarRichReduced)




# make Extended Data Table 3  continental breakdown ####
metadata_abundance<- cDabund %>%
	group_by (Datasource_ID, Datasource_name) %>%
	summarise(
		Continent = unique(Continent), 
		Region = unique(Region),
		Realm = unique(Realm), 
		n_plots= length(unique(Plot_ID))
	)
ab<- data.frame(metric = "Abundance",  table(metadata_abundance$Realm, metadata_abundance$Continent))


metadata_richness<- cDrichness %>%
	group_by (Datasource_ID) %>%
	summarise(
		Continent = unique(Continent), 
		Region = unique(Region),
		Realm = unique(Realm), 
		n_plots= length(unique(Plot_ID))
	)
rich<- data.frame(metric = "Richness",  table(metadata_richness$Realm, metadata_richness$Continent))
dcast(metadata_richness, Continent ~ Realm, value.var = "n_plots" , sum)

propEurNA<- table(subset(metadata_richness, Realm == "Terrestrial")$Continent)

metadata_PIE<- cDenspie %>%
	group_by (Datasource_ID) %>%
	summarise(
		Continent = unique(Continent), 
		Region = unique(Region),
		Realm = unique(Realm), 
		n_plots= length(unique(Plot_ID))
	)
comp<- data.frame(metric = "Composition",  table(metadata_PIE$Realm, metadata_PIE$Continent))

# extended Data Table 3: 
data_availability<- dcast(rbind(ab, rich, comp), factor(Var2)  ~  #, levels = c("North America" "Africa"        "Oceania"       "Europe"        "Latin America" "Asia" )
				factor(metric, levels = c("Abundance", "Richness", "Composition" )) + 
				factor(Var1, levels = c("Terrestrial", "Freshwater")   )); data_availability
colSums(data_availability[, -1], na.rm = T)








# Continental breakdown 
metadata_cont<- completeData2022 %>%
	group_by (Datasource_ID) %>%
	summarise(
		Realm = unique(Realm),
		Metric = unique(Unit),
		Continent = unique(Continent)
		)

# for abundance 
props<- prop.table(table(subset(metadata_cont, Realm == "Terrestrial" & Metric == "abundance")$Continent))
props
props[3] + props[5] # 69% of all abundance datasets are from europe and NA

props1<- prop.table(table(subset(metadata_cont, Realm == "Terrestrial" & Metric == "richness")$Continent))
props1
props1[2] + props1[4] # 73% of all richness datasets are from europe and NA


props2<- prop.table(table(subset(metadata_cont,  Realm == "Terrestrial" & Metric == "logNr4060")$Continent))
props2
props2[2] + props2[4] # 74% of all composition europe and NA

reshape2::dcast(subset(metadata_cont,  Realm == "Terrestrial" & Metric %in% c("abundance", "logNr4060", "richness")), 
								Metric ~ Continent)






# ANALYSIS Part 1: COMMUNITY METRICS#####
# make metadata for Fig 1 (number of studies and sites per metric)
metadata_metrics2$x <- NA
metadata_metrics2$y <- NA
metadata_metrics2$y[ metadata_metrics2$Realm == "Terrestrial"] <- 658.3702/2
metadata_metrics2$y[ metadata_metrics2$Realm == "Freshwater"] <- 200
metadata_metrics2$x <- 0.019
metadata_metrics2$Metric <- NA
metadata_metrics2$Metric[metadata_metrics2$Unit == "richness"]<- "Richness"
metadata_metrics2$Metric[metadata_metrics2$Unit == "abundance"]<- "Abundance"
metadata_metrics2$Metric[metadata_metrics2$Unit == "rarefiedRichness"]<- "Rarefied\nrichness"
metadata_metrics2$Metric[metadata_metrics2$Unit == "coverageRichness.8"]<- "Coverage\nrichness"
metadata_metrics2$Metric[metadata_metrics2$Unit == "ENSPIE"]<- "Diversity\n(Simpson)"
metadata_metrics2$Metric[metadata_metrics2$Unit == "Shannon"]<- "Diversity\n(Shannon)"

ev<- subset(metadata_metrics2, Unit == "ENSPIE")
ev$Metric<- "Evenness"
ev$Unit<- "Evenness"
metadata_metrics2<- rbind(metadata_metrics2, ev)

metadata_metrics2$text<- paste(  metadata_metrics2$n_datasets, "|", metadata_metrics2$n_plots)
metadata_metrics2<- subset(metadata_metrics2, !is.na(Metric))
metadata_metrics2$Metric<- factor(metadata_metrics2$Metric, levels = c ("Abundance", "Richness", "Rarefied\nrichness","Coverage\nrichness", "Diversity\n(Shannon)",
																																				"Diversity\n(Simpson)", "Evenness"))#, "Evenness (Pielou)", "Evenness (Shannon)"  ))
	
# load INLA model outputs
setwd("C:/Users/rk59zeqi/Documents/model outputs richness paper/")
# Richness 
inlaRichSum<- as.data.frame(readRDS("inlaRichnessTSUMMARY.rds"))
richRandom<- readRDS("inlaRichnessTrandomSlopes.rds")
richMarg<- readRDS("inlaRichnessTMarginal.rds")
richMarg$Metric<- "Richness"

inlaRarRichSum<- as.data.frame(readRDS("inlaRarRichTSUMMARY.rds"))
rarRichRandom<- readRDS("inlaRarRichTrandomSlopes.rds")
rarRichMarg<- readRDS("inlaRarRichTMarginal.rds")
rarRichMarg$Metric<- "Rarefied\nrichness"

inlaCovSum<- as.data.frame(readRDS("inlaCoverageR8TSUMMARY.rds"))
CovRandom<- readRDS("inlaCoverageR8TrandomSlopes.rds")
CovMarg<- readRDS("inlaCoverageR8TMarginal.rds")
CovMarg$Metric<- "Coverage\nrichness"

inlaAbSum<- as.data.frame(readRDS("inlaAbunTSUMMARY.rds"))
abRandom<- readRDS("inlaAbunTrandomSlopes.rds")
abMarg<- readRDS("inlaAbunTMarginal.rds" )
abMarg$Metric<- "Abundance"

inlapieSum<- as.data.frame(readRDS("inlaENSPIETSUMMARY.rds"))
pieRandom<- readRDS("inlaENSPIETrandomSlopes.rds")
pieMarg<- readRDS("inlaENSPIETMarginal.rds" )
pieMarg$Metric<- "Diversity\n(Simpson)"

# load marginal for reduced dataset for richness and abundance 
redRichSum<- as.data.frame(readRDS("inlaReducedRichnessTSUMMARY.rds"))
redRichMarg<- readRDS("inlaReducedRichnessTMarginal.rds")
redRichMarg$Metric<- "Richness"

redAbSum<- as.data.frame(readRDS("inlaReducedAbunTSUMMARY.rds"))
redAbMarg<- readRDS("inlaReducedAbunTMarginal.rds" )
redAbMarg$Metric<- "Abundance"

reduced<- rbind(redRichMarg, redAbMarg) 
reduced$Metric<- factor(reduced$Metric, levels = c ("Abundance", "Richness", "Rarefied richness", "Coverage richness", "ENS-PIE" ))#, "Evenness (Pielou)", "Evenness (Shannon)" ))
reduced$dat <- "55 datasets with full \ncommunity data only"

inlaShanSum<- as.data.frame(readRDS("inlaShanTSUMMARY.rds"))
shanMarg<- readRDS("InlaShanTMarginal.rds")
shanMarg$Metric<- "Diversity\n(Shannon)"

inlaShannonevenness<- as.data.frame(readRDS("inlaShannonevennessTSUMMARY.rds"))
ShannonevennessMarg<- readRDS("inlaShannonevennessTMarginal.rds" )
ShannonevennessMarg$Metric <- "Evenness"

inlaSimpsonevenness<- as.data.frame(readRDS("inlaSimpsonevennessTSUMMARY.rds"))
SimpsonevennessMarg<- readRDS("inlaSimpsonevennessTMarginal.rds" )
SimpsonevennessMarg$Metric <- "Evenness"




# make Fig 2 biodiversity metrics ######

univar<- rbind(abMarg, richMarg, rarRichMarg, CovMarg,  pieMarg)#, pielouMarg),  ShannonevennessMarg
univar<- rbind(abMarg, richMarg, rarRichMarg, CovMarg,  pieMarg, shanMarg,  SimpsonevennessMarg)#, pielouMarg)
univar$Metric<- factor(univar$Metric, levels = c ("Abundance", "Richness", "Rarefied\nrichness", "Coverage\nrichness",
																									 "Diversity\n(Shannon)", "Diversity\n(Simpson)", "Evenness"  ))#, "Evenness (Pielou)", ))



brks<- c(-0.02, -0.01, -0.005, 0, 0.005, 0.01, 0.015, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
ltyp<- c("55 datasets with full \ncommunity data only" = 'dotted')

ggplot(subset(univar, y>1)  , aes(x = x, y = y))+
	geom_vline(xintercept = -0.012 )+
	geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8, stat = "identity")+
	geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6, stat = "identity")+
	geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3, stat = "identity")+
	geom_area(  aes(x = x, y = y,   fill = Realm), alpha = 0.3, stat = "identity")+
	geom_line(data = subset(reduced, y>1), aes(x =x , y = y, linetype = dat ), color = 'black')+ # line for reduced dataset of only data with all raw data available
	geom_vline(xintercept = 0, linetype = 'dashed')+
	facet_grid(rows = vars(Metric), switch = "y")+
	ylab ("")+  xlab("Trend slope  \n % change per year")+
	scale_fill_manual(values = "grey50" )+ 
	scale_linetype_manual (values = ltyp)+
	scale_x_continuous(breaks = brks, labels = l, limits=c(-0.012, 0.022))+
	geom_text(aes(x = x, y = y, label = text), data = subset(metadata_metrics2, Realm == "Terrestrial" ), size = 4)+
	theme_classic()+
	theme(text = element_text(size =11),
		axis.text.y=element_blank(),
				axis.ticks.y=element_blank(), 
				legend.key=element_blank(), 
				legend.position="bottom", 
				strip.text.y.left = element_text(size=13, angle=0, hjust = 1),
				strip.background = element_rect(colour = "white"))

ggsave(filename = "Fig 2a univar.png" , path = figure_path, width = 11, height = 16,  units = "cm",dpi = 600, device = "png")
ggsave(filename = "Fig 2a univar.pdf" , path = figure_path, width = 11, height = 16,  units = "cm",dpi = 300, device = "pdf")






# table of mean estimates of this analysis
Realms<- c( "Terrestrial" )
mnChangeEsts<- rbind(
cbind(Metric = "Abundance",         Realms, inlaAbSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
cbind(Metric = "Richness",          Realms, inlaRichSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
cbind(Metric = "Rarefied richness", Realms, inlaRarRichSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
cbind(Metric = "ENS-Shannon",       Realms, inlaShanSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
cbind(Metric = "ENS-PIE",           Realms, inlapieSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
cbind(Metric = "Reduced Abundance", Realms, redAbSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
cbind(Metric = "Reduced Richness",  Realms, redRichSum[ 2, c(1,2,5,6,7, 11,12,13)] )
)

mnChangeEsts$lower2.5Perc10Yr <- (10^(mnChangeEsts$`0.025quant`*10 )-1)   *100
mnChangeEsts$meanPerc10Yr <- (10^(mnChangeEsts$mean*10 )-1)   *100
mnChangeEsts$upper97.5Perc10Yr <- (10^(mnChangeEsts$`0.975quant`*10 )-1)   *100

mnChangeEsts$lower2.5PercYr <- (10^(mnChangeEsts$`0.025quant` )-1)   *100
mnChangeEsts$meanPercYr <- (10^(mnChangeEsts$mean )-1)   *100
mnChangeEsts$upper97.5PercYr <- (10^(mnChangeEsts$`0.975quant` )-1)   *100
mnChangeEsts$Realm2<- mnChangeEsts$Realms
mnChangeEsts$Metric2<- mnChangeEsts$Metric
mnChangeEsts










# ANALYSIS Part 2: SAD changes #####
# number of dominant and rare species: (upper and lower 20% of abundance ) 
setwd("C:/Users/rk59zeqi/Documents/model outputs richness paper")

sads<- subset(completeData2022pure, Unit ==  "logNr020" |  Unit ==   "logNr2040"|  Unit == "logNr4060" |  Unit == "logNr6080"|  Unit ==  "logNr80100")
piv<- dcast(subset(sads, !is.na(Number)), Plot_ID+ Year ~ Unit, value.var = "Number", mean)
#pivot_wider(sads, id_cols = Plot_ID, from = Unit, values_from = Number, values_fn = mean)

quantile80100Sum<- as.data.frame(readRDS("quantile80100TSUMMARY.rds"))
q80100Marg<- readRDS("quantile80100TMarginal.rds" )
q80100Marg$Metric<- "80-100%"

quantile6080Sum<- as.data.frame(readRDS("quantile6080TSUMMARY.rds"))
q6080Marg<- readRDS("quantile6080TMarginal.rds" )
q6080Marg$Metric<- "60-80%"

quantile4060Sum<- as.data.frame(readRDS("quantile4060TSUMMARY.rds"))
q4060Marg<- readRDS("quantile4060TMarginal.rds" )
q4060Marg$Metric<- "40-60%"

quantile2040Sum<- as.data.frame(readRDS("quantile2040TSUMMARY.rds"))
q2040Marg<- readRDS("quantile2040TMarginal.rds" )
q2040Marg$Metric<- "20-40%"

quantile020Sum<- as.data.frame(readRDS("quantile020TSUMMARY.rds"))
q020Marg<- readRDS("quantile020TMarginal.rds" )
q020Marg$Metric<- "0-20%"


quantilesData<- rbind(q020Marg, q2040Marg,q4060Marg, q6080Marg, q80100Marg)
quantilesData$Metric<- ordered(quantilesData$Metric, 
															 levels = (c("0-20%", "20-40%",  "40-60%", "60-80%", "80-100%" )))


quantilesDataNeg<- quantilesData
quantilesDataNeg[, 4:7] <- -quantilesDataNeg[, 4:7]
quantilesDataNeg$Realm[quantilesDataNeg$Realm == "Terrestrial"]<- "Terrestrial2"
quantilesDataNeg$Realm[quantilesDataNeg$Realm == "Freshwater"]<- "Freshwater2"

quantilesData<- rbind(quantilesData, quantilesDataNeg)

# cut off tails for better plotting
quantilesData <- quantilesData[quantilesData$x <0.01 & quantilesData$x >-0.005 , ]


brks<- c(-0.02, -0.01, -0.005, -0.0025,  0, 0.0025, 0.005, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
col.scheme.realm2<- c(Terrestrial = "grey50",  Terrestrial2 = "grey50")


# make Fig 3 SAD changes ####
ggplot(subset(quantilesData, y>1 | y< -1 ), aes(x = x, y = y))+
	#	geom_line( )+
	geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8)+
	geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6)+
	geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3)+
	geom_area( aes(x = x, y = y, fill = Realm), alpha = 0.3)+
	geom_vline(xintercept = 0, linetype = 'dashed')+
	coord_flip()+
	facet_grid(cols = vars(Metric), switch = "x")+
	ylab ("SAD interval")+  xlab("Trend slope of number of species  \n % change per year")+
	scale_fill_manual(values = col.scheme.realm2)+
	scale_x_continuous(breaks = brks,labels = l)+#, limits=c(-0.005,  0.01))+
	#ggtitle("Number of species per SAD interval")+
	theme_classic()+
	theme(text = element_text(size =13),
				axis.text.x=element_blank(),
				axis.ticks.x=element_blank(), 
				legend.key=element_blank(), 
				legend.position="none", 
				strip.background = element_rect(colour = "white")
	)

ggsave(filename = "Fig 3 SAD changes.png" , path = figure_path, width = 12, height = 8,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig 3 SAD changes.pdf" , path = figure_path, width = 12, height = 8,  units = "cm",dpi = 300, device = "pdf")

# pane b: simple explanation of SAD: 
sim<- data.frame(
	Number = c(20, 15, 5 ,2, 1), 
	SADSection = c('0-20%', '20-40%' , '40-60%' , '60-80%', '80-100%'    )
)

ggplot(sim,	 aes(SADSection, Number))+
	geom_col(position = "identity", alpha = 0.5)+
	ylab('Number of\nspecies')+
	xlab('SAD interval')+
	theme_clean+
	theme(legend.position="none")
ggsave(filename = "Fig 3b example SAD.png" , path = figure_path, width = 12, height = 3,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig 3b example SAD.pdf" , path = figure_path, width = 12, height = 3,  units = "cm",dpi = 300, device = "pdf")



# calculate % change per year and per decade
Realms<- c( "Terrestrial" )
quantChangeEsts<- rbind(
	cbind(Metric = "0-20%",         Realms, quantile020Sum[ 2, c(1,2,5, 13)] ), 
	cbind(Metric = "20-40%",          Realms, quantile2040Sum[ 2, c(1,2,5, 13)] ), 
	cbind(Metric = "40-60%", Realms, quantile4060Sum[ 2, c(1,2,5, 13)] ), 
	cbind(Metric = "60-80%",       Realms, quantile6080Sum[ 2, c(1,2,5, 13)] ), 
	cbind(Metric = "80-1000%",           Realms, quantile80100Sum[ 2, c(1,2,5, 13)] ) 
)

quantChangeEsts$lower2.5Perc10Yr <- (10^(quantChangeEsts$`0.025quant`*10 )-1)   *100
quantChangeEsts$meanPerc10Yr <- (10^(quantChangeEsts$mean*10 )-1)   *100
quantChangeEsts$upper97.5Perc10Yr <- (10^(quantChangeEsts$`0.975quant`*10 )-1)   *100

quantChangeEsts$lower2.5PercYr <- (10^(quantChangeEsts$`0.025quant` )-1)   *100
quantChangeEsts$meanPercYr <- (10^(quantChangeEsts$mean )-1)   *100
quantChangeEsts$upper97.5PercYr <- (10^(quantChangeEsts$`0.975quant` )-1)   *100
quantChangeEsts












# ANALYSIS Part 3: Population changes by SAD interval #####
# make meta data for number of populations and species in each bracket 

exptPlots<- c(5, 	921, 922, 924,925,	643, 644, 646, 647,	137, 138, 139  )
exptDatasources<- c(300,1364, 1357,1410) #Kellogg, Luiquillo CTE, Cedar creek big bio, some german grassland
allPops<- allPops[!allPops$Datasource_ID %in% exptDatasources, ]
allPops<- allPops[!allPops$Plot_ID %in% exptPlots, ]

# exclude plots with a trend in taxonomic resolution:
bad_tax<- 
	c(849L, 132L, 1442L, 1527L, 10001L, 10003L, 10005L, 10006L, 10007L, 10008L, 10009L, 10011L, 10012L, 10015L, 10016L, 10017L, 10018L, 10019L, 10021L, 10024L, 10025L, 10026L, 10028L, 10029L, 10032L, 10033L, 10034L, 10035L, 10036L, 10038L, 10039L, 10044L, 10048L,	10049L, 10051L, 10052L, 10056L, 10057L, 10058L, 10059L, 10063L, 10066L, 10067L, 10068L, 10070L, 10072L, 10075L, 10079L, 10080L, 		10081L, 10082L, 10084L, 10085L, 10087L, 10092L, 10093L, 10097L, 10099L, 10106L, 10108L, 10113L, 10114L, 10115L, 10119L, 10124L, 	10125L, 10127L, 10128L, 10130L, 10135L, 10137L, 10138L, 10145L, 10146L, 10147L, 10149L, 10158L, 10160L, 10163L, 10164L, 10165L, 		10168L, 10169L, 10170L, 10171L, 10173L, 10175L, 10178L, 10180L, 10181L, 10182L, 10185L, 10187L, 10188L, 10189L, 10192L, 10193L, 		10195L, 10196L, 10198L, 10349L, 10200L, 10201L, 10204L, 10206L, 10208L, 10212L, 10215L, 10216L, 10219L, 10221L, 10223L, 10224L, 		10225L, 10226L, 10227L, 10229L, 10230L, 10233L, 10236L, 10249L, 10250L, 10252L, 10253L, 10254L, 10255L, 10256L, 10257L, 10258L, 		10259L, 10277L, 10279L, 10280L, 10281L, 10282L, 10283L, 10284L, 10285L, 10286L, 10287L, 10289L, 10290L, 10291L, 10292L, 10294L, 		10295L, 10297L, 10298L, 10299L, 10301L, 10302L, 10303L, 10304L, 10305L, 10306L, 10307L, 10308L, 10309L, 10311L, 10312L, 10313L, 
		10314L, 10315L, 10316L, 10317L, 10318L, 10320L, 10321L, 10322L, 10323L, 10325L, 10326L, 10327L, 10328L, 10329L, 10331L, 10332L, 		10334L, 10335L, 10336L, 10337L, 10338L, 10339L, 10340L, 10342L, 10343L, 10344L, 10346L, 10348L, 10469L, 10350L, 10351L, 10352L, 		10353L, 10354L, 10355L, 10356L, 10357L, 10361L, 10362L, 10364L, 10365L, 10366L, 10368L, 10369L, 10370L, 10371L, 10373L, 10374L, 		10375L, 10376L, 10378L, 10379L, 10380L, 10381L, 10382L, 10383L, 10384L, 10385L, 10386L, 10387L, 10388L, 10389L, 10390L, 10391L, 		10392L, 10393L, 10394L, 10395L, 10398L, 10399L, 10400L, 10401L, 10403L, 10404L, 10407L, 10408L, 10409L, 10410L, 10412L, 10413L, 		10414L, 10415L, 10416L, 10417L, 10418L, 10419L, 10421L, 10422L, 10424L, 10425L, 10426L, 10427L, 10428L, 10429L, 10430L, 10431L, 		10432L, 10433L, 10434L, 10435L, 10436L, 10437L, 10438L, 10439L, 10475L, 10440L, 10441L, 10442L, 10443L, 10444L, 10445L, 10446L, 	10448L, 10449L, 10485L, 10452L, 10454L, 10455L, 10457L, 10458L, 10459L, 10460L, 10463L, 10464L, 10465L, 10467L, 10468L, 10470L, 		10474L, 10499L, 10491L, 10490L, 10498L, 10489L, 10506L, 10503L, 10511L, 10504L, 10500L, 10501L)

allPops<- allPops[!allPops$Plot_ID %in% bad_tax, ]
dim(allPops)

terPops<- subset(allPops, Realm == "Terrestrial")
uniqPops<- unique(terPops[, c("CGYr1.0", "Taxon", "Plot_ID", "Datasource_ID")]); dim(uniqPops)# 40320
table(uniqPops$CGYr1.0)
length(unique(subset(terPops, CGYr1.0 == 0)$Taxon))

uniqSp<- unique(terPops[, c("CGYr1.0", "Taxon")]); dim(uniqSp)#
table(uniqSp$CGYr1)

uniqPlots<- unique(terPops[, c("CGYr1.0", "Plot_ID")]); dim(uniqPlots)# 
table(uniqPlots$CGYr1)
table(uniqPlots$Plot_ID , uniqPlots$CGYr1)

uniqStudies<- unique(terPops[, c("CGYr1.0", "Datasource_ID")]); dim(uniqStudies)
table(uniqStudies$CGYr1)

metadata_pops<- as.data.frame(table(uniqPops$CGYr1))
names(metadata_pops)[names(metadata_pops) == "Var1"] <- "SADinterval"
metadata_pops$x <- -0.035
metadata_pops$y <- 0
metadata_pops$CG<- as.character(metadata_pops$SADinterval)
metadata_pops$CG[metadata_pops$CG == 0] <- "Absent \nat start"
metadata_pops$CG[metadata_pops$CG == 5] <-"80-100%"
metadata_pops$CG[metadata_pops$CG == 2] <-"20-40%"
metadata_pops$CG[metadata_pops$CG == 3] <-"40-60%"
metadata_pops$CG[metadata_pops$CG == 4] <-"60-80%"
metadata_pops$CG[metadata_pops$CG == 1] <-"<20%"

metadata_pops$pops<- as.data.frame(table(uniqPops$CGYr1))$Freq
metadata_pops$pops[1] <- paste("Populations:\n", as.data.frame(table(uniqPops$CGYr1))$Freq[1])
metadata_pops$species <- as.data.frame(table(uniqSp$CGYr1))$Freq
metadata_pops$species[1] <- paste( "Species:\n", as.data.frame(table(uniqSp$CGYr1))$Freq[1])
metadata_pops$plots <- as.data.frame(table(uniqPlots$CGYr1))$Freq
metadata_pops$plots[1] <- paste("Plots:", as.data.frame(table(uniqPlots$CGYr1))$Freq[1])

#metadata_pops$pops<- paste(as.data.frame(table(uniqPops$CGYr1))$Freq, "\n populations")
metadata_pops$studies <- paste("Studies:", as.data.frame(table(uniqStudies$CGYr1))$Freq)





# load all population models
setwd("C:/Users/rk59zeqi/Documents/model outputs richness paper/Population models/") #Autoreg plot/
# models on autoregression at plot level (NOT at population level, since this required more than 1TB of RAM to run)


# 1) calculate correction factor
# load high quality data , no censoring
hq0 <- as.data.frame(readRDS("inlaPop0RtMSUMMARY.rds"))[ 2, ]
hq1 <- as.data.frame(readRDS("inlaPop1RtMSUMMARY.rds"))[ 2, ]
hq2 <- as.data.frame(readRDS("inlaPop2RtMSUMMARY.rds"))[ 2, ]
hq3 <- as.data.frame(readRDS("inlaPop3RtMSUMMARY.rds"))[ 2, ]
hq4 <- as.data.frame(readRDS("inlaPop4RtMSUMMARY.rds"))[ 2, ]
hq5 <- as.data.frame(readRDS("inlaPop5RtMSUMMARY.rds"))[ 2, ]
hqall<- as.data.frame(readRDS("inlaPopAllRtMSUMMARY.rds"))[ 2, ]


# load models on high quality data (min 15 years data) with 1 yr censored 
hq0cens1 <- as.data.frame(readRDS("inlaPop0RtM1SUMMARY.rds"))[ 2, ]
hq1cens1 <- as.data.frame(readRDS("inlaPop1RtM1SUMMARY.rds"))[ 2, ]
hq2cens1 <- as.data.frame(readRDS("inlaPop2RtM1SUMMARY.rds"))[ 2, ]
hq3cens1 <- as.data.frame(readRDS("inlaPop3RtM1SUMMARY.rds"))[ 2, ]
hq4cens1 <- as.data.frame(readRDS("inlaPop4RtM1SUMMARY.rds"))[ 2, ]
hq5cens1 <- as.data.frame(readRDS("inlaPop5RtM1SUMMARY.rds"))[ 2, ]
hqAllcens1 <- as.data.frame(readRDS("inlaPopAllRtM1SUMMARY.rds"))[ 2, ]

# load models on high quality data (min 10 years data) with 1 yr censored 
hq10yrs0cens1 <- as.data.frame(readRDS("inlaPop0RtM1_10yrSUMMARY.rds"))[ 2, ]
hq10yrs1cens1 <- as.data.frame(readRDS("inlaPop1RtM1_10yrSUMMARY.rds"))[ 2, ]
hq10yrs2cens1 <- as.data.frame(readRDS("inlaPop2RtM1_10yrSUMMARY.rds"))[ 2, ]
hq10yrs3cens1 <- as.data.frame(readRDS("inlaPop3RtM1_10yrSUMMARY.rds"))[ 2, ]
hq10yrs4cens1 <- as.data.frame(readRDS("inlaPop4RtM1_10yrSUMMARY.rds"))[ 2, ]
hq10yrs5cens1 <- as.data.frame(readRDS("inlaPop5RtM1_10yrSUMMARY.rds"))[ 2, ]



#load models on high quality data (min 15 years data) with 3 years censored 
hq0cens3 <- as.data.frame(readRDS("inlaPop0RtM3SUMMARY.rds"))[ 2, ]
hq1cens3 <- as.data.frame(readRDS("inlaPop1RtM3SUMMARY.rds"))[ 2, ]
hq2cens3 <- as.data.frame(readRDS("inlaPop2RtM3SUMMARY.rds"))[ 2, ]
hq3cens3 <- as.data.frame(readRDS("inlaPop3RtM3SUMMARY.rds"))[ 2, ]
hq4cens3 <- as.data.frame(readRDS("inlaPop4RtM3SUMMARY.rds"))[ 2, ]
hq5cens3 <- as.data.frame(readRDS("inlaPop5RtM3SUMMARY.rds"))[ 2, ]
hqAllcens3 <- as.data.frame(readRDS("inlaPopAllRtM3SUMMARY.rds"))[ 2, ]


# show difference in estimates
hqs<- rbind(cbind(hq0cens1, Realm = "Terrestrial", SADinterval = "Absent \nat start", Censoring = "1 year", 'Minimum n data points' = 15),
								cbind(hq1cens1, Realm = "Terrestrial", SADinterval = "<20%", Censoring = "1 year", 'Minimum n data points' = 15),
								cbind(hq2cens1, Realm =  "Terrestrial", SADinterval = "20-40%", Censoring = "1 year", 'Minimum n data points' = 15),
								cbind(hq3cens1, Realm =  "Terrestrial", SADinterval = "40-60%", Censoring = "1 year", 'Minimum n data points' = 15),
								cbind(hq4cens1, Realm =  "Terrestrial", SADinterval = "60-80%", Censoring = "1 year", 'Minimum n data points' = 15),
								cbind(hq5cens1, Realm =  "Terrestrial", SADinterval = "80-100%", Censoring = "1 year", 'Minimum n data points' = 15),
								cbind(hq0 , Realm = "Terrestrial", SADinterval = "Absent \nat start", Censoring = "No censoring", 'Minimum n data points' = 15),
								cbind(hq1 , Realm = "Terrestrial", SADinterval = "<20%", Censoring = "No censoring", 'Minimum n data points' = 15),
								cbind(hq2 , Realm =  "Terrestrial", SADinterval = "20-40%", Censoring = "No censoring", 'Minimum n data points' = 15),
								cbind(hq3 , Realm =  "Terrestrial", SADinterval = "40-60%", Censoring = "No censoring", 'Minimum n data points' = 15),
								cbind(hq4 , Realm =  "Terrestrial", SADinterval = "60-80%", Censoring = "No censoring", 'Minimum n data points' = 15),
								cbind(hq5 , Realm =  "Terrestrial", SADinterval = "80-100%", Censoring = "No censoring", 'Minimum n data points' = 15),
								cbind(hq0cens3, Realm = "Terrestrial", SADinterval = "Absent \nat start", Censoring = "3 years", 'Minimum n data points' = 15),
								cbind(hq1cens3, Realm =  "Terrestrial", SADinterval = "<20%", Censoring = "3 years", 'Minimum n data points' = 15),
								cbind(hq2cens3, Realm =  "Terrestrial", SADinterval = "20-40%", Censoring = "3 years", 'Minimum n data points' = 15),
								cbind(hq3cens3, Realm =  "Terrestrial", SADinterval = "40-60%", Censoring = "3 years", 'Minimum n data points' = 15),
								cbind(hq4cens3, Realm =  "Terrestrial", SADinterval = "60-80%", Censoring = "3 years", 'Minimum n data points' = 15),
								cbind(hq5cens3, Realm =  "Terrestrial", SADinterval = "80-100%", Censoring = "3 years", 'Minimum n data points' = 15)
)
hqs$Censoring<- ordered(hqs$Censoring, # Reorder for clarity
														 levels = (c("No censoring",  "1 year", "3 years" )))
hqs$SADinterval <- ordered(hqs$SADinterval, # Reorder for clarity
				levels = (c("Absent \nat start", "<20%", "20-40%", "40-60%", "60-80%", "80-100%" , "All")))


# make Extended Data Fig 6: Effect of left censoring #####
brks<- c(-0.04, -0.03,  -0.02,  -0.01,   0, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")

ggplot(hqs)+
	geom_hline(yintercept=0,linetype="dashed") +
	geom_errorbar(aes(x=as.factor(SADinterval),ymin=`0.025quant`, ymax=`0.975quant`, color = Censoring),
								linewidth = 1, width=0, alpha = 0.7, position=position_dodge(width= 0.5))+  
	geom_errorbar(aes(x=as.factor(SADinterval),ymin=`0.05quant`, ymax=`0.95quant`, color = Censoring),
								linewidth = 1.5, width=0, alpha = 0.7, position=position_dodge(width= 0.5))+  
	geom_errorbar(aes(x=as.factor(SADinterval),ymin=`0.1quant`, ymax=`0.9quant`, color = Censoring),
								linewidth = 2, width=0, alpha = 0.7, position=position_dodge(width= 0.5))+  
	geom_point(aes(x=as.factor(SADinterval),   y=mean, shape = Censoring),
						 size = 2, position=  position_dodge(width = 0.5), alpha=1 ,  fill = "black", color = "black")+
	scale_y_continuous(breaks = brks,labels = l)+#, limits=c(-0.005,  0.01))+
	xlab ("Start SAD interval") + ylab("Trend slope")+
	theme_clean +
	theme(strip.background =element_rect(fill="white"), 
				axis.line=element_line() ,
				#	axis.text.x  = element_text(angle=45, vjust=1, hjust = 1), 
				legend.position = "bottom")   

ggsave(filename = "Fig S6 Censoring sensitivity.png" , path = figure_path, width = 12, height = 12,  units = "cm",dpi = 600, device = "png")
ggsave(filename = "Fig S6 Censoring sensitivity.pdf" , path = figure_path, width = 12, height = 12,  units = "cm",dpi = 300, device = "pdf")

# calculate correction factors
corr1CG0 <- hq0cens1$mean - hq0$mean
corr1CG1 <- hq1cens1$mean - hq1$mean
corr1CG2 <- hq2cens1$mean - hq2$mean
corr1CG3 <- hq3cens1$mean - hq3$mean
corr1CG4 <- hq4cens1$mean - hq4$mean
corr1CG5 <- hq5cens1$mean - hq5$mean

corr3CG0 <- hq0cens3$mean - hq0$mean
corr3CG1 <- hq1cens3$mean - hq1$mean
corr3CG2 <- hq2cens3$mean - hq2$mean
corr3CG3 <- hq3cens3$mean - hq3$mean
corr3CG4 <- hq4cens3$mean - hq4$mean
corr3CG5 <- hq5cens3$mean - hq5$mean


# make Extended Data Table 4 #####
data.frame('Initial_abundance_interval' = c('Absent at start', '<20%', '20-40%', '40-60%', '60-80%', '80-100%' ), 
					 '1_year_censoring' = c(corr1CG0, corr1CG1, corr1CG2, corr1CG3, corr1CG4, corr1CG5),
					 '3_year_censoring' = c(corr3CG0, corr3CG1, corr3CG2, corr3CG3, corr3CG4, corr3CG5) )



# all data, all years ( to be corrected for final plots)
# commonness assigned in relation to highest value in Year 1
pop0sum <- as.data.frame(readRDS("inlaPop0T1SUMMARY.rds"))[ 2, ]
pop1sum <- as.data.frame(readRDS("inlaPop1T1SUMMARY.rds"))[ 2, ]
pop2sum <- as.data.frame(readRDS("inlaPop2T1SUMMARY.rds"))[ 2, ]
pop3sum <- as.data.frame(readRDS("inlaPop3T1SUMMARY.rds"))[ 2, ]
pop4sum <- as.data.frame(readRDS("inlaPop4T1SUMMARY.rds"))[ 2, ]
pop5sum <- as.data.frame(readRDS("inlaPop5T1SUMMARY.rds"))[ 2, ]
popAllsum <- as.data.frame(readRDS("inlaPopAllTSUMMARY.rds"))[ 2, ]

sumPops<- rbind(cbind(pop0sum + corr1CG0, Realm = "Terrestrial", SADinterval = "0", Correction = "1 year censored"),
								cbind(pop1sum + corr1CG1, Realm = "Terrestrial", SADinterval = "1", Correction = "1 year censored"),
								cbind(pop2sum + corr1CG2, Realm =  "Terrestrial", SADinterval = "2", Correction = "1 year censored"),
								cbind(pop3sum + corr1CG3, Realm =  "Terrestrial", SADinterval = "3", Correction = "1 year censored"),
								cbind(pop4sum + corr1CG4, Realm =  "Terrestrial", SADinterval = "4", Correction = "1 year censored"),
								cbind(pop5sum + corr1CG5, Realm =  "Terrestrial", SADinterval = "5", Correction = "1 year censored"),
								#cbind(popAllsum , Realm = "Terrestrial", SADinterval = "All", Correction = "Not corrected"),
								cbind(pop0sum , Realm = "Terrestrial", SADinterval = "0", Correction = "All years included"),
								cbind(pop1sum , Realm = "Terrestrial", SADinterval = "1", Correction = "All years included"),
								cbind(pop2sum , Realm =  "Terrestrial", SADinterval = "2", Correction = "All years included"),
								cbind(pop3sum , Realm =  "Terrestrial", SADinterval = "3", Correction = "All years included"),
								cbind(pop4sum , Realm =  "Terrestrial", SADinterval = "4", Correction = "All years included"),
								cbind(pop5sum , Realm =  "Terrestrial", SADinterval = "5", Correction = "All years included"),
								cbind(pop0sum + corr3CG0, Realm = "Terrestrial", SADinterval = "0", Correction = "3 years censored"),
								cbind(pop1sum + corr3CG1, Realm =  "Terrestrial", SADinterval = "1", Correction = "3 years censored"),
								cbind(pop2sum + corr3CG2, Realm =  "Terrestrial", SADinterval = "2", Correction = "3 years censored"),
								cbind(pop3sum + corr3CG3, Realm =  "Terrestrial", SADinterval = "3", Correction = "3 years censored"),
								cbind(pop4sum + corr3CG4, Realm =  "Terrestrial", SADinterval = "4", Correction = "3 years censored"),
								cbind(pop5sum + corr3CG5, Realm =  "Terrestrial", SADinterval = "5", Correction = "3 years censored")
								)
sumPops$Correction<- ordered(sumPops$Correction, # Reorder for clarity
															 levels = (c("All years included", "1 year censored", "3 years censored" )))

#percentual change per commoness group
mnPopChange<- sumPops[1:6, ]
rownames(mnPopChange)<- mnPopChange$SADinterval

percPopChange<- (10^(mnPopChange[, 1:15] )-1)   *100

percPopChange[, c("mean", '0.025quant', '0.05quant',  '0.1quant', '0.9quant',  '0.95quant', '0.975quant' )]


brks<- c(-0.04, -0.03,  -0.02,  -0.01,   0, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")




setwd("C:/Users/rk59zeqi/Documents/model outputs richness paper/Population models/")
# models on autoregression at plot level (NOT at population level, since this required more than 1TB of RAM to run)


# calculate correcte ddata
# load hq data , no censoring, add correction factor to all estimates
pop0sum <- as.data.frame(readRDS("inlaPop0T1SUMMARY.rds"))[ 2, ]
pop0Marg<- readRDS("inlaPop0T1Marginal.rds" )
pop0Marg$CG<- "0 \n(absent at start)"
pop0Marg$xc <- pop0Marg$x + corr1CG0
pop0rand<- readRDS("inlaPop0T1allRandomSlopes.rds" )

pop1sum <- as.data.frame(readRDS("inlaPop1T1SUMMARY.rds"))[ 2, ]
pop1Marg<- readRDS("inlaPop1T1Marginal.rds" )
pop1Marg$CG<- "1-20% \n(very rare)"
pop1Marg$xc <- pop1Marg$x + corr1CG1 # apply correction factor to whole posterior
pop1rand<- readRDS("inlaPop1T1allRandomSlopes.rds" )

pop2sum <- as.data.frame(readRDS("inlaPop2T1SUMMARY.rds"))[ 2, ]
pop2Marg<- readRDS("inlaPop2T1Marginal.rds" )
pop2Marg$CG<- "20-40%"
pop2Marg$xc <- pop2Marg$x + corr1CG2
pop2rand<- readRDS("inlaPop2T1allRandomSlopes.rds" )

pop3sum <- as.data.frame(readRDS("inlaPop3T1SUMMARY.rds"))[ 2, ]
pop3Marg<- readRDS("inlaPop3T1Marginal.rds" )
pop3Marg$CG<- "40-60%"
pop3Marg$xc <- pop3Marg$x + corr1CG3
pop3rand<- readRDS("inlaPop3T1allRandomSlopes.rds" )

pop4sum <- as.data.frame(readRDS("inlaPop4T1SUMMARY.rds"))[ 2, ]
pop4Marg<- readRDS("inlaPop4T1Marginal.rds" )
pop4Marg$CG<- "60-80%"
pop4Marg$xc <- pop4Marg$x + corr1CG4
pop4rand<- readRDS("inlaPop4T1allRandomSlopes.rds" )

pop5sum <- as.data.frame(readRDS("inlaPop5T1SUMMARY.rds"))[ 2, ]
pop5Marg<- readRDS("inlaPop5T1Marginal.rds" )
pop5Marg$CG<- "80-100% \n(very abundant)"
pop5Marg$xc <- pop5Marg$x + corr1CG5
pop5rand<- readRDS("inlaPop5T1allRandomSlopes.rds" )
#
popCGs<- rbind(pop0Marg, pop1Marg,pop2Marg, pop3Marg, pop4Marg, pop5Marg)


popCGsNeg<- popCGs
popCGsNeg[, 4:7] <- -popCGsNeg[, 4:7]
popCGsNeg$Realm[popCGsNeg$Realm == "Terrestrial"]<- "Terrestrial2"
popCGs<- rbind(popCGs, popCGsNeg)



# make object with mean estimates
sumPops<- rbind(cbind(pop0sum + corr1CG0, Realm = "Terrestrial", SADinterval = "Absent \nat start", Correction = "1 year censored"),
								cbind(pop1sum + corr1CG1, Realm = "Terrestrial", SADinterval = "<20%", Correction = "1 year censored"),
								cbind(pop2sum + corr1CG2, Realm =  "Terrestrial", SADinterval = "20-40%", Correction = "1 year censored"),
								cbind(pop3sum + corr1CG3, Realm =  "Terrestrial", SADinterval = "40-60%", Correction = "1 year censored"),
								cbind(pop4sum + corr1CG4, Realm =  "Terrestrial", SADinterval = "60-80%", Correction = "1 year censored"),
								cbind(pop5sum + corr1CG5, Realm =  "Terrestrial", SADinterval = "80-100%", Correction = "1 year censored"),
								cbind(popAllsum , Realm =  "Terrestrial", SADinterval = "All", Correction = "1 year censored")
								
)
sumPops$SADinterval<- ordered(sumPops$SADinterval, # Reorder for clarity
														 levels = (c("Absent \nat start", "<20%", "20-40%", "40-60%", "60-80%", "80-100%" , "All")))



# object for Random effect estimates per species 
randPops<- rbind(data.frame(SADinterval = "Absent \nat start", TaxonPlot_4INLAs = pop0rand$TaxonPlot_4INLAs, slope = rowSums (pop0rand[, c("Study_Slope_mean", "Loc_slp_mean", "Plot_slp_mean", "Spec_Slope_mean"  )])+ pop0sum$mean + corr1CG0, 
														CImin = rowSums (pop0rand[, c("Study_Slope_0.1quant", "Loc_slp_0.1quant", "Plot_slp_0.1quant", "Spec_Slope_0.1quant"  )])+ pop0sum$mean + corr1CG0, 
														CImax = rowSums (pop0rand[, c("Study_Slope_0.9quant", "Loc_slp_0.9quant", "Plot_slp_0.9quant", "Spec_Slope_0.9quant"  )])+ pop0sum$mean + corr1CG0),
								 data.frame(SADinterval = "<20%", TaxonPlot_4INLAs = pop1rand$TaxonPlot_4INLAs, slope = rowSums (pop1rand[, c("Study_Slope_mean", "Loc_slp_mean", "Plot_slp_mean", "Spec_Slope_mean"  )])+ pop1sum$mean + corr1CG1, 
								 					 CImin = rowSums (pop1rand[, c("Study_Slope_0.1quant", "Loc_slp_0.1quant", "Plot_slp_0.1quant", "Spec_Slope_0.1quant"  )])+ pop1sum$mean + corr1CG1, 
								 					 CImax = rowSums (pop1rand[, c("Study_Slope_0.9quant", "Loc_slp_0.9quant", "Plot_slp_0.9quant", "Spec_Slope_0.9quant"  )])+ pop1sum$mean + corr1CG1), 
								 data.frame(SADinterval = "20-40%", TaxonPlot_4INLAs = pop2rand$TaxonPlot_4INLAs, slope = rowSums (pop2rand[, c("Study_Slope_mean", "Loc_slp_mean", "Plot_slp_mean", "Spec_Slope_mean"  )])+ pop2sum$mean + corr1CG2, 
								 					 CImin = rowSums (pop2rand[, c("Study_Slope_0.1quant", "Loc_slp_0.1quant", "Plot_slp_0.1quant", "Spec_Slope_0.1quant"  )])+ pop2sum$mean + corr1CG2, 
								 					 CImax = rowSums (pop2rand[, c("Study_Slope_0.9quant", "Loc_slp_0.9quant", "Plot_slp_0.9quant", "Spec_Slope_0.9quant"  )])+ pop2sum$mean + corr1CG2), 
								 data.frame(SADinterval = "40-60%", TaxonPlot_4INLAs = pop3rand$TaxonPlot_4INLAs, slope = rowSums (pop3rand[, c("Study_Slope_mean", "Loc_slp_mean", "Plot_slp_mean", "Spec_Slope_mean"  )])+ pop3sum$mean + corr1CG3, 
								 					 CImin = rowSums (pop3rand[, c("Study_Slope_0.1quant", "Loc_slp_0.1quant", "Plot_slp_0.1quant", "Spec_Slope_0.1quant"  )])+ pop3sum$mean + corr1CG3, 
								 					 CImax = rowSums (pop3rand[, c("Study_Slope_0.9quant", "Loc_slp_0.9quant", "Plot_slp_0.9quant", "Spec_Slope_0.9quant"  )])+ pop3sum$mean + corr1CG3), 
								 data.frame(SADinterval = "60-80%", TaxonPlot_4INLAs = pop4rand$TaxonPlot_4INLAs, slope = rowSums (pop4rand[, c("Study_Slope_mean", "Loc_slp_mean", "Plot_slp_mean", "Spec_Slope_mean"  )])+ pop4sum$mean + corr1CG4, 
								 					 CImin = rowSums (pop4rand[, c("Study_Slope_0.1quant", "Loc_slp_0.1quant", "Plot_slp_0.1quant", "Spec_Slope_0.1quant"  )])+ pop4sum$mean + corr1CG4, 
								 					 CImax = rowSums (pop4rand[, c("Study_Slope_0.9quant", "Loc_slp_0.9quant", "Plot_slp_0.9quant", "Spec_Slope_0.9quant"  )])+ pop4sum$mean + corr1CG4), 
								 data.frame(SADinterval = "80-100%", TaxonPlot_4INLAs = pop5rand$TaxonPlot_4INLAs, slope = rowSums (pop5rand[, c("Study_Slope_mean", "Loc_slp_mean", "Plot_slp_mean", "Spec_Slope_mean"  )])+ pop5sum$mean + corr1CG5, 
								 					 CImin = rowSums (pop5rand[, c("Study_Slope_0.1quant", "Loc_slp_0.1quant", "Plot_slp_0.1quant", "Spec_Slope_0.1quant"  )])+ pop5sum$mean + corr1CG5, 
								 					 CImax = rowSums (pop5rand[, c("Study_Slope_0.9quant", "Loc_slp_0.9quant", "Plot_slp_0.9quant", "Spec_Slope_0.9quant"  )])+ pop5sum$mean + corr1CG5),
								 data.frame(SADinterval = "All", TaxonPlot_4INLAs = 0, slope = 0, 
								 					 CImin = 0, CImax = 0 )
								 					 
								 ); dim(randPops)
randPops$`Population trend`<- "No trend"
randPops$`Population trend`[randPops$CImin>0]<- ">90% certain positive" 
randPops$`Population trend`[randPops$CImax<0]<- ">90% certain negative" 

randPops$SADinterval<- ordered(randPops$SADinterval, # Reorder for clarity
														 levels = (c("Absent \nat start", "<20%", "20-40%", "40-60%", "60-80%", "80-100%" , "All")))

# there are some duplicates here
dups<- (randPops[duplicated(randPops$TaxonPlot_4INLAs), ])$TaxonPlot_4INLAs         
arrange(randPops[randPops$TaxonPlot_4INLAs %in% dups, ], TaxonPlot_4INLAs)


# make Fig 4 #####
library(ggdist)
library(wesanderson)

brks<- c(-0.15, -0.10, -0.05, -0.025,   0, 0.025, 0.05, 0.75)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")


ggplot(randPops )+
	geom_vline(xintercept=0,linetype="dashed") +
	geom_dots (aes(x= slope, y = SADinterval, fill = `Population trend`, color = `Population trend`), alpha = 0.5)+
	geom_point(data = sumPops, aes( x = mean, y = SADinterval), position = position_nudge(y = -0.05),
						 size = 1, alpha=1 , shape = 3,  fill = "black", color = "black")+
	geom_errorbar(data = sumPops, aes(y=SADinterval, xmin=`0.025quant`, xmax=`0.975quant`),
								linewidth = 0.5, width=0, position = position_nudge(y = -0.05))+  
	geom_errorbar(data = sumPops, aes(y=SADinterval, xmin=`0.1quant`, xmax=`0.9quant`),
								linewidth = 1, width=0, position = position_nudge(y = -0.05))+  
	coord_flip()+
	ylab ("Initial abundance group")+  xlab("Population trend slope \n % change per year")+
	scale_x_continuous(breaks = brks,labels = l)+#, limits=c(-0.005,  0.01))+
	 geom_text(data = metadata_pops, aes(x = -0.11, y = as.numeric(as.character(SADinterval))+1 ,
	 																		label = pops), size = 3, color = "black")+
	 geom_text(aes(x = -0.09, y = as.numeric(SADinterval) , 
	 							label = species), data = metadata_pops, size = 3, color = "black")+
	scale_fill_manual(values = wes_palette("GrandBudapest1"))+
	scale_color_manual(values = wes_palette("GrandBudapest1"))+
	theme_classic()+
	theme(legend.key=element_blank(), 
		legend.position="bottom", 
		strip.background = element_rect(colour = "white")
	)

ggsave(filename = "Fig 4 Population changes dots.png" , path = figure_path, width = 17, height = 12,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig 4 Population changes dots.pdf" , path = figure_path, width = 17, height = 12,  units = "cm",dpi = 300, device = "pdf")



# calculate change estimates 
Realms<- c( "Terrestrial" )
popChangeEsts<- rbind(cbind(pop0sum + corr1CG0, Realm = "Terrestrial", SADinterval = "0", Correction = "corrected 1"),
														 cbind(pop1sum + corr1CG1, Realm = "Terrestrial", SADinterval = "1", Correction = "corrected 1"),
														 cbind(pop2sum + corr1CG2, Realm =  "Terrestrial", SADinterval = "2", Correction = "corrected 1"),
														 cbind(pop3sum + corr1CG3, Realm =  "Terrestrial", SADinterval = "3", Correction = "corrected 1"),
														 cbind(pop4sum + corr1CG4, Realm =  "Terrestrial", SADinterval = "4", Correction = "corrected 1"),
														 cbind(pop5sum + corr1CG5, Realm =  "Terrestrial", SADinterval = "5", Correction = "corrected 1"))

popChangeEsts$lower2.5Perc10Yr <- (10^(popChangeEsts$`0.025quant`*10 )-1)   *100
popChangeEsts$meanPerc10Yr <- (10^(popChangeEsts$mean*10 )-1)   *100
popChangeEsts$upper97.5Perc10Yr <- (10^(popChangeEsts$`0.975quant`*10 )-1)   *100

popChangeEsts$lower2.5PercYr <- (10^(popChangeEsts$`0.025quant` )-1)   *100
popChangeEsts$meanPercYr <- (10^(popChangeEsts$mean )-1)   *100
popChangeEsts$upper97.5PercYr <- (10^(popChangeEsts$`0.975quant` )-1)   *100
popChangeEsts

#Which are the species with the most negative trends? 
allPops$Family[is.na(allPops$Family)]<- "" # remove NA's
allPops$validTaxon[is.na(allPops$validTaxon)]<- "" # remove NA's

unique_pops<- unique(allPops[, c("TaxonPlot_4INLAs","Plot_ID", "Datasource_ID",  "CGYr1.0",  "Taxon", "Class" , "Order",    "Level")]); dim(unique_pops) #"validTaxon",
sp_randpops<- merge(randPops,unique_pops ) ; dim(sp_randpops)
sp_randpops<- merge(sp_randpops, plots[c("Plot_ID", "Datasource_ID", "Datasource_nameREDUNDANT", "Location","Plot_name")]) ; dim(sp_randpops)
sp_randpops[duplicated(sp_randpops$TaxonPlot_4INLAs), ]
sp_randpops$percchange<-  (10^(sp_randpops$slope )-1)   *100

subset(sp_randpops, TaxonPlot_4INLAs == 163290  ) # some duplicates but only because family and valid taxon is sometimes NA... need to fix this for the final round 


sp_randpops<- sp_randpops[, c("Datasource_ID" ,  "Datasource_nameREDUNDANT", "Location" , "Plot_ID",  "Plot_name",   "TaxonPlot_4INLAs" ,"CGYr1.0"  ,   
															"Class",   "Order" , "Taxon" , "Level",   "slope", "CImin" , "CImax" , "percchange", "Population trend" 	)]


# make Supplementary Data S3 ####
saveRDS(sp_randpops, file = "C:/Dropbox/Insect Biomass Trends/csvs/Supplementary Data 3.rds")


# steepest declines: 
qs<- quantile(sp_randpops$slope, probs = seq(0, 1, 0.001)); length(qs)


# look at some of the more extreme values
q1 = 990

sp_randpops[sp_randpops$slope <qs[q1+1] & sp_randpops$slope >qs[q1] , 
						c("slope", "percchange", "Population trend", "CGYr1.0", "Taxon",  "Family","Order", "Datasource_nameREDUNDANT") ]; paste(q1, "-", q1+1)
q1<-q1+1


# worst declines (2% steepest)
worst<- sp_randpops[sp_randpops$slope <qs[20] , 
						c("slope", "percchange", "Population trend", "CGYr1.0", "Taxon",  "Family","Order", "Datasource_nameREDUNDANT") ]
arrange(worst, slope)
arrange(worst, desc(CGYr1))
unique(worst$Datasource_nameREDUNDANT)
arrange(subset(worst, Datasource_nameREDUNDANT == "UK hoverflies Owen" ), slope)

best<- sp_randpops[sp_randpops$slope >qs[981] , 
										c("slope", "percchange",  "Population trend", "CGYr1.0", "Taxon",  "Family","Order", "Datasource_nameREDUNDANT") ]
arrange(best, desc(slope))
arrange(best, (CGYr1))
unique(best$Datasource_nameREDUNDANT)
arrange(subset(best, Datasource_nameREDUNDANT ==  "Germany Schiemenz Schuch" ), desc(slope))



# 3 year cencoring 
sumPops3<- rbind(cbind(pop0sum + corr3CG0, Realm = "Terrestrial", SADinterval = "0", Correction = "3 year censored"),
								cbind(pop1sum + corr3CG1, Realm = "Terrestrial", SADinterval = "1", Correction = "3 year censored"),
								cbind(pop2sum + corr3CG2, Realm =  "Terrestrial", SADinterval = "2", Correction = "3 year censored"),
								cbind(pop3sum + corr3CG3, Realm =  "Terrestrial", SADinterval = "3", Correction = "3 year censored"),
								cbind(pop4sum + corr3CG4, Realm =  "Terrestrial", SADinterval = "4", Correction = "3 year censored"),
								cbind(pop5sum + corr3CG5, Realm =  "Terrestrial", SADinterval = "5", Correction = "3 year censored")
)

# object for Random effect estimates per species 
randPops3<- rbind(data.frame(SADinterval = "0", slope = rowSums (pop0rand[, c("Study_Slope_mean", "Loc_slp_mean", "Plot_slp_mean", "Spec_Slope_mean"  )])+ pop0sum$mean + corr3CG0, 
																		 CImin = rowSums (pop0rand[, c("Study_Slope_0.1quant", "Loc_slp_0.1quant", "Plot_slp_0.1quant", "Spec_Slope_0.1quant"  )])+ pop0sum$mean + corr3CG0, 
																		 CImax = rowSums (pop0rand[, c("Study_Slope_0.9quant", "Loc_slp_0.9quant", "Plot_slp_0.9quant", "Spec_Slope_0.9quant"  )])+ pop0sum$mean + corr3CG0),
													data.frame(SADinterval = "1", slope = rowSums (pop1rand[, c("Study_Slope_mean", "Loc_slp_mean", "Plot_slp_mean", "Spec_Slope_mean"  )])+ pop1sum$mean + corr3CG1, 
																		 CImin = rowSums (pop1rand[, c("Study_Slope_0.1quant", "Loc_slp_0.1quant", "Plot_slp_0.1quant", "Spec_Slope_0.1quant"  )])+ pop1sum$mean + corr3CG1, 
																		 CImax = rowSums (pop1rand[, c("Study_Slope_0.9quant", "Loc_slp_0.9quant", "Plot_slp_0.9quant", "Spec_Slope_0.9quant"  )])+ pop1sum$mean + corr3CG1), 
													data.frame(SADinterval = "2", slope = rowSums (pop2rand[, c("Study_Slope_mean", "Loc_slp_mean", "Plot_slp_mean", "Spec_Slope_mean"  )])+ pop2sum$mean + corr3CG2, 
																		 CImin = rowSums (pop2rand[, c("Study_Slope_0.1quant", "Loc_slp_0.1quant", "Plot_slp_0.1quant", "Spec_Slope_0.1quant"  )])+ pop2sum$mean + corr3CG2, 
																		 CImax = rowSums (pop2rand[, c("Study_Slope_0.9quant", "Loc_slp_0.9quant", "Plot_slp_0.9quant", "Spec_Slope_0.9quant"  )])+ pop2sum$mean + corr3CG2), 
													data.frame(SADinterval = "3", slope = rowSums (pop3rand[, c("Study_Slope_mean", "Loc_slp_mean", "Plot_slp_mean", "Spec_Slope_mean"  )])+ pop3sum$mean + corr3CG3, 
																		 CImin = rowSums (pop3rand[, c("Study_Slope_0.1quant", "Loc_slp_0.1quant", "Plot_slp_0.1quant", "Spec_Slope_0.1quant"  )])+ pop3sum$mean + corr3CG3, 
																		 CImax = rowSums (pop3rand[, c("Study_Slope_0.9quant", "Loc_slp_0.9quant", "Plot_slp_0.9quant", "Spec_Slope_0.9quant"  )])+ pop3sum$mean + corr3CG3), 
													data.frame(SADinterval = "4", slope = rowSums (pop4rand[, c("Study_Slope_mean", "Loc_slp_mean", "Plot_slp_mean", "Spec_Slope_mean"  )])+ pop4sum$mean + corr3CG4, 
																		 CImin = rowSums (pop4rand[, c("Study_Slope_0.1quant", "Loc_slp_0.1quant", "Plot_slp_0.1quant", "Spec_Slope_0.1quant"  )])+ pop4sum$mean + corr3CG4, 
																		 CImax = rowSums (pop4rand[, c("Study_Slope_0.9quant", "Loc_slp_0.9quant", "Plot_slp_0.9quant", "Spec_Slope_0.9quant"  )])+ pop4sum$mean + corr3CG4), 
													data.frame(SADinterval = "5", slope = rowSums (pop5rand[, c("Study_Slope_mean", "Loc_slp_mean", "Plot_slp_mean", "Spec_Slope_mean"  )])+ pop5sum$mean + corr3CG5, 
																		 CImin = rowSums (pop5rand[, c("Study_Slope_0.1quant", "Loc_slp_0.1quant", "Plot_slp_0.1quant", "Spec_Slope_0.1quant"  )])+ pop5sum$mean + corr3CG5, 
																		 CImax = rowSums (pop5rand[, c("Study_Slope_0.9quant", "Loc_slp_0.9quant", "Plot_slp_0.9quant", "Spec_Slope_0.9quant"  )])+ pop5sum$mean + corr3CG5) 
); dim(randPops3)
randPops3$`Population trend`<- "No trend"
randPops3$`Population trend`[randPops3$CImin>0]<- "90% certain positive" 
randPops3$`Population trend`[randPops3$CImax<0]<- "90% certain negative" 
 dim(randPops3)


# same graph, but with  correction based on 3 year censoring (not in paper)
 brks<- c(-0.15, -0.10, -0.05, -0.025,   0, 0.025, 0.05, 0.75)
 perc<-(10^(brks )  *100) - 100
 l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
 
  ggplot(randPops3 )+
 	geom_vline(xintercept=0,linetype="dashed") +
 	geom_dots (aes(x= slope, y = SADinterval, fill = `Population trend`, color = `Population trend`), alpha = 0.5)+
 	geom_point(data = sumPops3, aes( x = mean, y = SADinterval), position = position_nudge(y = -0.05),
 						 size = 2, alpha=1 ,  fill = "black", color = "black")+
 	geom_errorbar(data = sumPops3, aes(y=SADinterval, xmin=`0.025quant`, xmax=`0.975quant`),
 								linewidth = 0.5, width=0, position = position_nudge(y = -0.05))+  
 	geom_errorbar(data = sumPops3, aes(y=SADinterval, xmin=`0.1quant`, xmax=`0.9quant`),
 								linewidth = 1, width=0, position = position_nudge(y = -0.05))+  
 	coord_flip()+
 	ylab ("Abundance group at start")+  xlab("Trend slope \n % change per year")+
 	scale_x_continuous(breaks = brks,labels = l)+#, limits=c(-0.005,  0.01))+
 	geom_text(data = metadata_pops, aes(x = -0.12, y = as.numeric(as.character(SADinterval))+1 ,
 																			label = pops), size = 3, color = "black")+
 	geom_text(aes(x = -0.10, y = as.numeric(SADinterval) , 
 								label = species), data = metadata_pops, size = 3, color = "black")+
 	scale_fill_manual(values = wes_palette("GrandBudapest1"))+
 	scale_color_manual(values = wes_palette("GrandBudapest1"))+
 	
 	#	geom_text(aes(x = x-0.14, y = as.numeric(SADinterval) , label = plots), data = metadata_pops, size = 3, color = "black")+
 	ggtitle("corrected mean trend by 3 year censoring")+
 	theme_classic()+
 	theme(#axis.text.x=element_blank(),
 		#axis.ticks.x=element_blank(), 
 		legend.key=element_blank(), 
 		legend.position="bottom", 
 		strip.background = element_rect(colour = "white")
 	)
 




# ANALYSIS Part 4 relation between random slopes ######


#relations between abundance, PIE  and richness slopes #####
# load random effects of biddiversity models 
setwd("C:/Users/rk59zeqi/Documents/model outputs richness paper")
abRandom      <- readRDS("inlaAbunTrandomSlopes.rds")
richRandom    <- readRDS("inlaRichnessTrandomSlopes.rds")
pieRandom  <- readRDS("inlaENSPIETrandomSlopes.rds")


richRandom$richnessSlope<- richRandom$slope
richRandom$richnessSlopeSD<- richRandom$`DataID_Slope_ sd`
abRandom$abundanceSlope <- abRandom$slope
abRandom$abundanceSlopeSD <- abRandom$`DataID_Slope_ sd`
pieRandom$enspieSlope<- pieRandom$slope


randomSlopes<- merge(richRandom[, c("Realm", "Datasource_ID", "Datasource_name", "richnessSlope", "richnessSlopeSD") ],  
										 abRandom[, c("Realm", "Datasource_ID", "Datasource_name", "abundanceSlope", "abundanceSlopeSD")], all = T)
randomSlopes<- merge(randomSlopes,  pieRandom[, c("Realm", "Datasource_ID", "Datasource_name", "enspieSlope")], all = T)
head(randomSlopes)

# rename realms for fig 
randomSlopes$Realm2<- "Terrestrial"




# random slopes of population models and total abundance models #####
setwd("C:/Users/rk59zeqi/Documents/model outputs richness paper/")
pop0rand<- readRDS("Population models/inlaPop0TrandomSlopes.rds" )
pop1rand<- readRDS("Population models/inlaPop1TrandomSlopes.rds" )
pop2rand<- readRDS("Population models/inlaPop2TrandomSlopes.rds" )
pop3rand<- readRDS("Population models/inlaPop3TrandomSlopes.rds" )
pop4rand<- readRDS("Population models/inlaPop4TrandomSlopes.rds" )
pop5rand<- readRDS("Population models/inlaPop5TrandomSlopes.rds" )

pop5rand$pop5Slope<- pop5rand$slope
pop5rand$pop5SlopeSD<- pop5rand$`DataID_Slope_ sd`
pop4rand$pop4Slope<- pop4rand$slope
pop4rand$pop4SlopeSD<- pop4rand$`DataID_Slope_ sd`
pop3rand$pop3Slope<- pop3rand$slope
pop3rand$pop3SlopeSD<- pop3rand$`DataID_Slope_ sd`
pop2rand$pop2Slope<- pop2rand$slope
pop2rand$pop2SlopeSD<- pop2rand$`DataID_Slope_ sd`
pop1rand$pop1Slope<- pop1rand$slope
pop1rand$pop1SlopeSD<- pop1rand$`DataID_Slope_ sd`
pop0rand$pop0Slope<- pop0rand$slope
pop0rand$pop0SlopeSD<- pop0rand$`DataID_Slope_ sd`


randomSlopes4<- merge(pop5rand[, c("Realm", "Datasource_ID", "Datasource_name",  "pop5Slope", "pop5SlopeSD") ],  
											pop4rand[, c( "Realm", "Datasource_ID", "Datasource_name",  "pop4Slope", "pop4SlopeSD")])
randomSlopes4<- merge(randomSlopes4,  pop3rand[, c( "Realm", "Datasource_ID", "Datasource_name", "pop3Slope", "pop3SlopeSD")], all = T)
randomSlopes4<- merge(randomSlopes4,  pop2rand[, c( "Realm", "Datasource_ID", "Datasource_name", "pop2Slope", "pop2SlopeSD")], all = T)
randomSlopes4<- merge(randomSlopes4,  pop1rand[, c( "Realm", "Datasource_ID", "Datasource_name",  "pop1Slope", "pop1SlopeSD")], all = T)
randomSlopes4<- merge(randomSlopes4,  pop0rand[, c( "Realm", "Datasource_ID", "Datasource_name",  "pop0Slope", "pop0SlopeSD")], all = T)

# rename realms for fig 
randomSlopes4$Realm2<-"Terrestrial"
# assign labels: 

labs<- c('pop0Slope' = 'Absent at start', 
				 'pop1Slope' = '<20%', 
				 'pop2Slope' = '20-40%',
				 'pop3Slope' = '40-60%', 
				 'pop4Slope' = '60-80%',  
				 'pop5Slope' = '80-100%')





randomSlopes5<- merge(randomSlopes, randomSlopes4, all = T); dim(randomSlopes5)


# better plot:
rs5<- reshape2::melt(randomSlopes5, id.vars = c('Realm',  'Datasource_ID', 'Datasource_name','richnessSlope', 'abundanceSlope', 'enspieSlope'),
										 measure.vars = c('pop0Slope', 'pop1Slope', 'pop2Slope','pop3Slope', 'pop4Slope',  'pop5Slope'   ) , variable.name = 'Metric' , value.name = 'Random slope')

regressions<- NULL
for(i in 1:(length(unique(rs5$Metric)))){
	Met<- unique(rs5$Metric)[i]
	mod<- summary(lm(`Random slope`~abundanceSlope , data = subset(rs5, Metric == Met  )))
	reg<- cbind(Metric = Met, 
							intrcpt = mod$coefficients[1,1 ],
							as.data.frame(mod$coefficients)[2, ])
	reg$sign<- ""
	reg$sign[reg$`Pr(>|t|)`<0.05]<- '*' ; reg$sign[reg$`Pr(>|t|)`<0.01]<- '**'; reg$sign[reg$`Pr(>|t|)`<0.001]<- '***' 
	regressions<- rbind(regressions, reg)
}
regressions
# slopes get steeper towards higher abundance classes, but interestingly also for absent species 
# i.e places with more positive abundance slopes have less negative bracket 5 populations, and more positive colonizing species 

# Make Extened Data Fig 8a relation between population slopes and total abundances slope #####
ggplot(rs5, aes(x = abundanceSlope, y = `Random slope`))+
	geom_point()+
	facet_wrap(vars(Metric), nrow = 1, labeller = labeller(Metric = labs))+
	geom_hline(yintercept=0, linetype = "dashed")+
	geom_vline(xintercept=0, linetype = "dashed")+
	geom_abline(intercept = 0, slope = 1, linetype = 'dotted')+
	geom_abline(data = regressions,  aes(intercept = intrcpt  , slope =  Estimate), color = 'blue', size = 1)+
	
	geom_text(aes(x = -0.03, y = -0.1, label = paste('\U03B2 =', round(Estimate, 3), sign)), data = regressions, size = 3, color = "black")+
	xlab ('Abundance random slopes')+
	ylab ('Populations random slopes')+
	#	stat_smooth(method = 'lm', se = F)+
	theme_clean

ggsave(filename = "Fig S8a random effects populations and tot abundance.png" , path = figure_path, width = 24, height = 7,  units = "cm",dpi = 600, device = "png")
ggsave(filename = "Fig S8a random effects populations and tot abundance.pdf" , path = figure_path, width = 24, height = 7,  units = "cm",dpi = 300, device = "pdf")








# make Fig S1 bivariate map #####
richRandom$richlwr <- richRandom$`DataID_Slope_ 0.1quant` + richRandom$fixedSlp
richRandom$richupr <-  richRandom$`DataID_Slope_ 0.9quant` + richRandom$fixedSlp 
richRandom$richdir<- NA 
richRandom$richdir[richRandom$richupr <0 ]<- -1 
richRandom$richdir[richRandom$richlwr >0]<- 1 
richRandom$richdir[richRandom$richlwr<0 & richRandom$richupr>0]<-0 


abRandom$ablwr = abRandom$`DataID_Slope_ 0.1quant` + abRandom$fixedSlp
abRandom$abupr = abRandom$`DataID_Slope_ 0.9quant` + abRandom$fixedSlp 
abRandom$abdir<- NA 
abRandom$abdir[abRandom$abupr <0 ]<- -1 
abRandom$abdir[abRandom$ablwr >0]<- 1 
abRandom$abdir[abRandom$ablwr<0 & abRandom$abupr>0]<-0 





randomSlopes2<- merge(richRandom[, c("Realm", "Datasource_ID", "Datasource_name", "richnessSlope", "richdir") ],  
											abRandom[, c("Realm", "Datasource_ID", "Datasource_name", "abundanceSlope", "abdir")], all = T)


randomSlopes2$colo <- NA
randomSlopes2$colo[randomSlopes2$richdir == 0 & randomSlopes2$abdir == 0 ]  <- "none"
randomSlopes2$colo[randomSlopes2$richdir == 1 & randomSlopes2$abdir == 0 ]  <- "richpos0"
randomSlopes2$colo[randomSlopes2$richdir == -1 & randomSlopes2$abdir == 0 ] <- "richneg0"
randomSlopes2$colo[randomSlopes2$richdir == 0 & randomSlopes2$abdir == 1 ]  <- "abpos0"
randomSlopes2$colo[randomSlopes2$richdir == 1 & randomSlopes2$abdir == 1 ]  <- "bothpos"
randomSlopes2$colo[randomSlopes2$richdir == -1 & randomSlopes2$abdir == 1 ] <- "richnegabpos"
randomSlopes2$colo[randomSlopes2$richdir == 0 & randomSlopes2$abdir == -1 ]  <- "abneg0"
randomSlopes2$colo[randomSlopes2$richdir == 1 & randomSlopes2$abdir == -1 ]  <- "richposabneg"
randomSlopes2$colo[randomSlopes2$richdir == -1 & randomSlopes2$abdir == -1 ] <- "bothneg"

# get coordinates
metadata_per_dataset2 <-completeData2022 %>% 
	group_by(Datasource_ID) %>%
	summarise(
		Datasource_name = unique(Datasource_name),
		Country = unique(Country),
		Country_State  = unique(Country_State),
		Start = min(Year), 
		Mean_longitude = mean(unique(Longitude), na.rm = T),
		Mean_latitude = mean(unique(Latitude)),
		End = max(Year),
		Time_span_yrs = (max(Year) - min(Year))+1,
		Nr_yrs_data = length(unique(Year)),
		Nb_plots = length(unique(Plot_ID))
	)

randomSlopes2<- merge(randomSlopes2, metadata_per_dataset2)

unique(randomSlopes2$colo)
col.scheme.bivar<- c(	"none"       = '#a5add3',
											"richpos0"    = '#8c62aa',   
											"richneg0"    = '#ace4e4',
											"abpos0"      = '#5698b9',
											"bothpos"     = '#3b4994',
											"richnegabpos" = '#5ac8c8', 
											"abneg0"      = '#dfb0d6',
											"richposabneg"= '#be64ac',
											"bothneg"     = '#e8e8e8')

# test colorscheme and if colorings make sense
ggplot(subset(randomSlopes2, !is.na(colo)), aes(x=abundanceSlope, y=richnessSlope, color =colo)) +
	geom_point()+
	geom_hline(yintercept=0,linetype="dashed")+
	geom_vline(xintercept=0,linetype="dashed")+
	scale_color_manual(values = col.scheme.bivar) +
	scale_fill_manual(values = col.scheme.realm) +
	scale_x_continuous(breaks = brks,labels = l, limits=c(-0.05,  0.03))+
	scale_y_continuous(breaks = brks,labels = l, limits=c(-0.05,  0.03))+ 	
	theme_clean + 
	theme(  strip.background = element_blank(), 
					plot.title = element_text(hjust = 0.5))#,
# looks believable

library(rgdal)
library(sp)
library(broom)



#  ++++ RUN TWICE ####
pts.wgs <- randomSlopes2
pts.wgs$slope.lim<- pts.wgs$colo

pts.wgs <- SpatialPointsDataFrame(coords = data.frame(lon = pts.wgs$Mean_longitude,
																											lat = pts.wgs$Mean_latitude),
																	proj4string = CRS(WGS84),
																	data = pts.wgs)


setwd("C:\\Dropbox\\Insect Biomass Trends/csvs") # work
source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/map_preparation.R")
####   ++++ RUN TWICE UNTIL HERE ++++ ####


# scale slopes 
library(biscale)
legend <- bi_legend(pal = "DkBlue",
										dim = 3,
										xlab = "Abundance trend",
										ylab = "Richness trend",
										size = 8)


#png("map fw.png", width=4400, height=1800, res = 360)
#fw.wgs
#dev.off()


# terrestrial
terr.wgs <-
	p.wgsLIGHT+
	geom_path(data=NE_graticules_rob, aes(long, lat, group=group),  color="grey50", size = 0.2) +
	geom_point(data = subset(pts.rob, Realm =="Terrestrial" & !is.na(slope.lim))@data, size = 2, #pch = 21,color = "grey30" ,
						 aes(x = x,   y = y,  color = slope.lim, group = NULL) , 
						 position=position_jitter(h=1, w=1)) +
	scale_color_manual(values = col.scheme.bivar) +
	theme(legend.position = "none")+
	#	ggtitle("a. Terrestrial fauna")  +
	theme(panel.background = element_rect(fill = "grey40", colour = "black"))

#png("map terr.png", width=4400, height=2000, res = 360)
#terr.wgs
#dev.off()

library(cowplot)
finalMap <- ggdraw() +
	draw_plot(terr.wgs, width = 1,height = 1) +
	#draw_plot(fw.wgs, 0, 0, 1, 0.5) +
	draw_plot(legend, 0.025, .2, 0.25, 0.25)

finalMap
ggsave(filename = "Fig S1 map.png" , path = figure_path, width = 2500, height = 1500,  units = "px",dpi = 300, device = "png")
ggsave(filename = "Fig S1 map.pdf" , path = figure_path, width = 2500, height = 1500,  units = "px",dpi = 300, device = "pdf")





































#  Fig S7 fitted vs predicted values    #####
cDRichness$preds <- inlaRich$summary.fitted.values$mean  #need to have control.predictor = list(link = 1) in model
ggplot(cDRichness,aes(x=preds,y=log10(Number+1)))+
  geom_point (aes(color = Realm), size =sz)+
  geom_abline(slope=1,intercept=0)+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  xlab ("Predicted values") + ylab ("Observed values")+
  facet_wrap(.~Realm, scales = "free")+
 theme_clean + 
  theme( strip.background = element_blank())

metadata_realm<-  cDRichness %>% 
  group_by(Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID))) 

metadata_realm$Unit<- "Abundance &\n biomass"

Rslope<- inlaRealmSum[3:4,]
varsR<- data.frame(do.call(rbind, strsplit(rownames(Rslope), split = ":")))
Rslope<-cbind(Rslope, varsR)
Rslope$X3<- Rslope$X2
Rslope$Realm<-gsub("Realm", "", Rslope$X1)
Rslope$Unit<- "Abundance &\n biomass"
Rslope$AB <-Rslope$Unit
Rslope<- merge(Rslope, metadata_realm)
Rslope$P<- ps

# percentage change per year and per decade
data.frame(
  var =   c("FW 1 yr" ,"Terr 1 yr", "FW 10 yr", "Terr 10 yr"), 
  CI2.5 =  c((10^(inlaRichSum  [3:4,4]  )-1 ) *100, (10^(inlaRichSum  [3:4,4] *10)-1 )  *100),#0.025 CI
  mean =   c((10^(inlaRichSum  [3:4,1]  )-1)  *100, (10^(inlaRichSum  [3:4,1] *10)-1 )  *100), # proportional changes per year
  CI97.5 = c((10^(inlaRichSum  [3:4,12] )-1 ) *100, (10^(inlaRichSum  [3:4,12] *10)-1)  *100)# 0.975
)

# positive effect for fw, no trend for richness


table(inlaRich$cpo$failure)
cpo = inlaRich$cpo$cpo
ind <- 1:length(cpo)
df = data.frame(ind,cpo,
                realm=cDrichness$Realm,
                unit=cDrichness$Unit,
                dataset=cDrichness$Datasource_ID)

qplot(ind,cpo,data=subset(df,cpo<0.05))+
  geom_point(aes(colour=factor(dataset)))+
  facet_wrap(realm~unit) # some datasets have weird values

pit <- inlaRealm$cpo$pit
n = length(sort(pit))
samples <- (1:n)/(n+1)
plot(samples, sort(pit), xlab="uniform quantiles", ylab="Sorted PIT values")
abline(0,1)




