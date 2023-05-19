library(ggplot2)
library(ggnewscale)
library(tidyverse)
library(data.table)
library(reshape2)
col.scheme.realm<-c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "chocolate4")
col.scheme.realm2 <- c(  "Freshwater"  = "dodgerblue2", "Freshwater2"  = "dodgerblue2",  "Terrestrial" = "chocolate4", "Terrestrial2" = "chocolate4")
lntps<-  c("Terrestrial" = 2,  "Freshwater" = 3)

source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/calculate metrics.R")
setwd("C:/Users/rk59zeqi/Documents/model outputs richness paper") # work

load("C:\\Dropbox\\Insect Biomass Trends/csvs/completeData2022pure.Rdata"); dim(completeData2022pure) # 

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

dim(completeData2022pure)


plots<-as.data.frame(fread( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/PlotData 5.0.csv")); dim(plots)
UKfwPlots<- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/UKfwSites.csv")
plots<- rbind(plots[, names(UKfwPlots)], UKfwPlots); dim(plots)

figure_path<- "D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/Figures/"
completeData2022<- completeData2022pure


# make explanatory figure for SAD intervals#####
# fig  Methods / under Fig 4 and S4 
Biotime <- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/BioTIMEstd2022.csv"); unique(Biotime$Datasource_name)

uk<- subset(Biotime, Datasource_name == "BT380 UK chalk grassland butterflies"  )
dk<- subset(Biotime, Datasource_name == "BT249 DK lighttrap"  )
cc<- subset(Biotime, Datasource_name == "BT313 Cedar cr grasshoppers"  )

hist(log10(dk$Number+1))
quartiles<- quantile(log10(dk$Number+1))

hist(log10(cc$Number+1))
hist(cc$Number)
quantile(log10(cc$Number+1))
table((cc$Number+1))

max(log10(cc$Number+1))



data<- cc
data<- dk

yrMax<- dk$Year[which(dk$Number == max(dk$Number))]
yrMin<- 1999

dkMax<- subset(dk, Year == yrMax)
dkMin<- subset(dk, Year == yrMin)
quartiles<- quantile(log10(dkMax$Number+1))

dkBoth<- rbind(dkMin, dkMax )
dkBoth$Year[dkBoth$Year == 2006] <- 'Year 1'
dkBoth$Year[dkBoth$Year == 1999] <- 'Year 10'

# base figure of SAD
ggplot(dkBoth,  aes(log10(Number), group = (Year), fill = (Year)),alpha = 0.5)+
	geom_histogram(position = "identity", alpha = 0.5, bins = 9)+
	ylab('Number of species')+
	geom_vline(xintercept = 0, color = 'red')+
	geom_vline(xintercept = quartiles[2], color = 'red')+
	geom_vline(xintercept = quartiles[3], color = 'red')+
	geom_vline(xintercept = quartiles[4], color = 'red')+
	geom_vline(xintercept = quartiles[5], color = 'red')+
geom_vline(xintercept = max(log10(dkMax$Number)), col="blue")+
	geom_vline(xintercept=max(log10(data$Number)*0.8), col="blue")+
geom_vline(xintercept=max(log10(data$Number)*0.6), col="blue")+
geom_vline(xintercept=max(log10(data$Number)*0.4), col="blue")+
geom_vline(xintercept=max(log10(data$Number)*0.2), col="blue")	+
		theme_clean

ggsave(filename = "Fig S1 SAD sections expl pt1.png" , path = figure_path, width = 12, height = 10,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig S1 SAD sections expl pt1.pdf" , path = figure_path, width = 12, height = 10,  units = "cm",dpi = 300, device = "pdf")





pvt<- reshape2::dcast(subset(dk, Year %in% c(1999, 2006)),  Year ~ Taxon , value.var = "Number", sum)

dim(pvt)
rowSums(pvt>0)

alpha<- 	calculate_alpha_metrics(pvt) 


sel<- subset(alpha, Unit_in_data %in% c('logNr020','logNr2040','logNr4060','logNr6080','logNr80100' ))
sel$SADSection<- rep(c("0-20%", "20-40%",  "40-60%", "60-80%", "80-100%" ), each = 2)
sel$Year[sel$Year == 2006] <- 'Year 1'
sel$Year[sel$Year == 1999] <- 'Year 10'
sel<- arrange(sel, Year)

# bar chart 
ggplot(sel,	 aes(SADSection, Number, fill = (Year)))+
	geom_col(position = "identity", alpha = 0.5)+
	ylab('Number of species')+
	xlab('SAD interval (equal spacing)')+
	ggtitle('Equally spaced SAD sections')+
	theme_clean+
	theme(legend.position="none")

ggsave(filename = "Fig S1 SAD sections expl pt2.png" , path = figure_path, width = 11, height = 10,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig S1 SAD sections expl pt2.pdf" , path = figure_path, width = 11, height = 10,  units = "cm",dpi = 300, device = "pdf")

#  Quartiles 
Qs<- quantile(log10(as.matrix(pvt[pvt!= 0])), c(0.25, 0.5, 0.75)) # zeroes were removed

sel<- subset(alpha, Unit_in_data %in% c('logNrQ1','logNrQ2','logNrQ3','logNrQ4' ))
sel$SADQuartile<- rep(c("1", "2-3",  "4-10", ">10"), each = 2)
sel$SADQuartile<- ordered(sel$SADQuartile, levels=c("1", "2-3",  "4-10", ">10"))
sel$Year[sel$Year == 2006] <- 'Year 1'
sel$Year[sel$Year == 1999] <- 'Year 10'

vec<- c(-0.1, Qs, log10(max(dk$Number)))
sel$midpoints<- rep(vec[-length(vec)] + diff(vec) / 2, each = 2)
sel<- arrange(sel, Year)
wdt<-   rep(diff(c(0.02, Qs, log10(max(dk$Number))))-0.05 ,   2)#/log10(max(dk$Number))
# bar chatr instead of histogram 
ggplot(sel,	 aes(midpoints, Number, fill = (Year)))+
	geom_col(position = "identity", alpha = 0.5, width = wdt)+
	ylab('Number of species')+
	xlab('log10(Abundance)')+ 
	ggtitle('SAD Quartiles with equal species numbers')+
	theme_clean+
	theme(legend.position="none")

ggsave(filename = "Fig S1 SAD sections expl pt3.png" , path = figure_path, width = 11, height = 10,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig S1 SAD sections expl pt3.pdf" , path = figure_path, width = 11, height = 10,  units = "cm",dpi = 300, device = "pdf")






# old visualization
hist(log10(dkMax$Number+1), xlab = "log10 (Abundance)", main = "")
hist(log10(dkMin$Number+1), xlab = "log10 (Abundance)", main = "", col = 'darkgrey', add = T, 
		 breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5))
# position of quartiles 
abline(v=quartiles[2], col="red")
abline(v=quartiles[3], col="red")
abline(v=quartiles[4], col="red")
abline(v=quartiles[5], col="red")
abline(v=quartiles[1], col="red")
# position of equal prackets 


brks<- c(max(log10(data$Number+1)+0.0001),
				 max(log10(data$Number+1)*0.8),
				 max(log10(data$Number+1)*0.6),
				 max(log10(data$Number+1)*0.4),
				 max(log10(data$Number+1)*0.2), 
				 0)




# sensitivity analyses #####




# Prep data to test influence of short and long timeseries #######

# cut everything down to last 10 years, exclude everything that can't be cut 

# what's the cutoff year for each plot? 
metadata_per_plotZ<- completeData2022pure %>% 
	group_by(Plot_ID) %>%
	summarise(
		Plot_ID = unique(Plot_ID),
		Datasource_ID = unique(Datasource_ID),
		Datasource_name = unique(Datasource_name), 
		Realm = unique(Realm),
		Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T),
		cutoffYear = max(Year, na.rm = T)-10
	)

completeData2022pure<-   merge(completeData2022pure, metadata_per_plotZ )

completeData2022pureShort <-subset(completeData2022pure, Year> cutoffYear); dim(completeData2022pureShort)
# 
metadata_per_plotZ1<- completeData2022pureShort %>% 
	group_by(Plot_ID) %>%
	summarise(
		Plot_ID = unique(Plot_ID),
		Datasource_ID = unique(Datasource_ID),
		Datasource_name = unique(Datasource_name), 
		Realm = unique(Realm),
		Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T),
		cutoffYear = max(Year, na.rm = T)-10
	) # correct 

metadata_per_plotZ1<- subset(completeData2022pureShort, !is.na(Number)) %>% 
	group_by(Plot_ID) %>%
	summarise(
		Plot_ID = unique(Plot_ID),
		Datasource_ID = unique(Datasource_ID),
		Datasource_name = unique(Datasource_name), 
		Realm = unique(Realm),
		Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
		Number_of_years_data = length(unique(Year)), 
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T)
	) # correct 


# make the needed datasets
metadata_per_plotZ2<- subset(metadata_per_plotZ1, Duration >=10)
dim(metadata_per_plotZ2) #1630 plots
length(unique(metadata_per_plotZ2$Datasource_ID)) # 127 datasets

# now exclude all datasets that are not long enough
completeData2022Short <-subset(completeData2022pure, Plot_ID %in% metadata_per_plotZ2$Plot_ID)
saveRDS(completeData2022Short, file = "C:\\Dropbox\\Insect Biomass Trends/csvs/completeData2022short.rds")



# Long: keep only 20+ years data #
metadata_per_plotZ3<- subset(metadata_per_plotZ, Duration > 20); dim(metadata_per_plotZ3)#549
length(unique(metadata_per_plotZ3$Datasource_ID)) # 77 datasets

#hypothetically 30 yr cutoff
metadata_per_plotZ4<- subset(metadata_per_plotZ, Duration > 30); dim(metadata_per_plotZ4)#148
length(unique(metadata_per_plotZ4$Datasource_ID)) # 34 datasets



completeData2022long <-subset(completeData2022pure, Plot_ID %in% metadata_per_plotZ3$Plot_ID); dim(completeData2022long)
saveRDS(completeData2022long, file = "C:\\Dropbox\\Insect Biomass Trends/csvs/completeData2022long.rds")


# Plot Fig S2 short & long####
# alldata
inlaRichSum<- as.data.frame(readRDS("inlaRichnessTSUMMARY.rds"))
richMarg<- readRDS("inlaRichnessTMarginal.rds")
richMarg$Metric<- "Richness"
richMarg$Data<- "All" 

inlaAbSum<- as.data.frame(readRDS("inlaAbunTSUMMARY.rds"))
abMarg<- readRDS("inlaAbunTMarginal.rds" )
abMarg$Metric <- "Abundance"
abMarg$Data<- "All"

inlapieSum<- as.data.frame(readRDS("inlaENSPIETSUMMARY.rds"))
pieMarg<- readRDS("inlaENSPIETMarginal.rds" )
pieMarg$Metric<- "Simpson diversity (ENS)"
pieMarg$Data<- "All"

inlaRichLongSum<- as.data.frame(readRDS("sensitivity analysis/inlaRichLongTSUMMARY.rds"))
inlaRichLongMarg<- readRDS("sensitivity analysis/inlaRichLongTMarginal.rds" )
inlaRichLongMarg$Metric<- "Richness"
inlaRichLongMarg$Data<- "20 years"

inlaAbLongSum<- as.data.frame(readRDS("sensitivity analysis/inlaAbundLongTSUMMARY.rds"))
inlaAbLongMarg<- readRDS("sensitivity analysis/inlaAbundLongTMarginal.rds" )
inlaAbLongMarg$Metric<- "Abundance"
inlaAbLongMarg$Data<- "20 years"

inlaPieLongSum<- as.data.frame(readRDS("sensitivity analysis/inlaENSPIELongTSUMMARY.rds"))
inlaPieLongMarg<- readRDS("sensitivity analysis/inlaENSPIELongTMarginal.rds" )
inlaPieLongMarg$Metric<- "Simpson diversity (ENS)"
inlaPieLongMarg$Data<- "20 years"

inlaRichShortSum<- as.data.frame(readRDS("sensitivity analysis/inlaRichShortTSUMMARY.rds"))
inlaRichShortMarg<- readRDS("sensitivity analysis/inlaRichShortTMarginal.rds" )
inlaRichShortMarg$Metric<- "Richness"
inlaRichShortMarg$Data<- "10 years"

inlaAbShortSum<- as.data.frame(readRDS("sensitivity analysis/inlaAbundShortTSUMMARY.rds"))
inlaAbShortMarg<- readRDS("sensitivity analysis/inlaAbundShortTMarginal.rds" )
inlaAbShortMarg$Metric<- "Abundance"
inlaAbShortMarg$Data<- "10 years"

inlaPieShortSum<- as.data.frame(readRDS("sensitivity analysis/inlaENSPIEShortTSUMMARY.rds"))
inlaPieShortMarg<- readRDS("sensitivity analysis/inlaENSPIEShortTMarginal.rds" )
inlaPieShortMarg$Metric<- "Simpson diversity (ENS)"
inlaPieShortMarg$Data<- "10 years"


univar<- rbind(abMarg, richMarg, pieMarg,  inlaAbLongMarg, inlaRichLongMarg, inlaPieLongMarg, 
							 inlaAbShortMarg, inlaRichShortMarg, inlaPieShortMarg)
univar$Metric<- factor(univar$Metric, levels = c ("Abundance", "Richness", "Simpson diversity (ENS)" ))
univar$Data<- factor(univar$Data, levels = c ("All", "20 years", "10 years" ))

univar<- subset(univar, y> 1)

brks<- c(-0.02, -0.01, -0.005, 0, 0.005, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")


data_labs<- c( "All",  ">19 years data", "Last 10 years data" )
names(data_labs)<- c("All", "20 years", "10 years")

mean <- univar %>%
	group_by(Realm, Metric ,Data) %>%
	filter(y == max(y, na.rm=TRUE))

ggplot(univar, aes(x = x, y = y))+
	#	geom_line( )+
	geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8)+
	geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6)+
	geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3)+
	geom_area( aes(x = x, y = y, fill = Realm), alpha = 0.3)+
	geom_vline(data= subset(mean, Realm == 'Terrestrial'), aes(xintercept = x), linetype= 'dotted')+
	geom_vline(xintercept = 0, linetype = 'dashed')+
	facet_grid(cols = vars(Metric), rows = vars(Data), scales = "free_x", 
						 labeller = labeller(Data = data_labs))+ #, switch = "y"
	ylab ("Density")+  xlab("Trend slope  \n % change per year")+
	scale_linetype_manual(values = lntps)+
	scale_fill_manual(values = "grey50")+
	scale_x_continuous(breaks = brks,labels = l)+#, limits=c(-0.02,  0.012))+
	
	theme_classic()+
	theme(axis.text.y=element_blank(),
				axis.ticks.y=element_blank(), 
				legend.key=element_blank(), 
				legend.position="none"
	)

ggsave(filename = "Fig S2 timeseries length.png" , path = figure_path, width = 18, height = 18,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig S2 timeseries length.pdf" , path = figure_path, width = 18, height = 18,  units = "cm",dpi = 300, device = "pdf")








#  Prep data to test effect of large datasets (large number of plots):  #####


metadata_per_dataset<-  completeData2022pure %>% 
	     group_by(Datasource_ID) %>%
	     summarise(
		         Datasource_ID = unique(Datasource_ID),
		         Datasource_name = unique(Datasource_name), 
		         Number_of_metrics = length(unique(Unit)),
		         Realm = unique(Realm),
		         Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
		         Number_of_years_data = length(unique(Year)), 
		         Start_year = min(Year, na.rm = T),
		         End_year = max(Year, na.rm = T),
		         Number_of_plots =  length(unique(Plot_ID)), # should be 1
		         Number_of_samples = length(unique(paste(Year, Period))),
		         Continent = unique(Continent), 
		         Country_State = unique(Country_State),
		         Country = unique(Country),
		         Region = unique(Region),
		         Realm = unique(Realm)
		 	     )
 ggplot(metadata_per_dataset, aes(x = Datasource_name , y = Number_of_plots))+
	          geom_point()+ coord_flip()+ facet_wrap(.~Realm)
 hist(metadata_per_dataset$Number_of_plots)
 median(metadata_per_dataset$Number_of_plots)
 3
 mean(metadata_per_dataset$Number_of_plots)
 11.7

 ggplot(subset(metadata_per_dataset, Number_of_plots >5),
 			 aes(x = Datasource_name , y = Number_of_plots))+
 	geom_point()+ coord_flip()+ facet_wrap(.~Realm)+
 scale_y_log10()+
 		geom_hline(yintercept = 20)+# 25 datasets
 geom_hline(yintercept = 50)+
 geom_hline(yintercept = 10)
 
 
 nrow(subset(metadata_per_dataset, Number_of_plots >10)) # 36
 nrow(subset(metadata_per_dataset, Number_of_plots >50)) # 11
 
 
 
# loop for selecting 10, 20 or 50 plots from each dataset that has
 
 for ( q in c(1:3)){
		  maxNrPlots<- 	c(10,20,50)[q]
		badDatasets<-  (subset(metadata_per_dataset, Number_of_plots > maxNrPlots)) # 36
		 
		 
		# split dataset in data to be used as is, and data that needs to be made smaller 
		completeData2022Good<- subset(completeData2022, !Datasource_ID %in% badDatasets$Datasource_ID) 
		completeData2022bad<- subset(completeData2022, Datasource_ID %in% badDatasets$Datasource_ID) 
		
		badDatasets<- badDatasets$Datasource_ID 
		badDatasets<- badDatasets[badDatasets %in% completeData2022$Datasource_ID]
		 
		
		
		
		
		
		# for each dataset with over maxNrPlots: 
		selectedPlots<- NULL
		
		for(i in 1:length(badDatasets)){
		 # 1) get number of locations 
			
		smpPltsPerDataset<- NULL
			
		dataset<- badDatasets[i]
		#dataset <-1267 # sweden as testcase # done 
		
		
		
		dat1<- subset(completeData2022bad, Datasource_ID == dataset)
		#get number of locations: 
		length(unique(dat1$Plot_ID))
		nLocs<- length(unique(dat1$Location))
 
			# if al plots are in 1 location (= close together) take maxNrPlots longest plots 
			if (nLocs  == 1 ){
			
					metadat<-  subset(dat1, !is.na(Number)) %>% 
					group_by(Plot_ID) %>%
					summarise(
						Plot_ID = unique(Plot_ID),
						Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
						Number_of_years_data = length(unique(Year)), 
						Number_of_samples = length(unique(paste(Year, Period))),
						total_N = sum(Number[Unit == "abundance"], na.rm = T),
						richness = max(Number[Unit == "richness"], na.rm = T)
							)
								metadat<- arrange(metadat, -Number_of_years_data, total_N)	
									
								smpPlot<- metadat$Plot_ID[1:maxNrPlots]	
								
								smpPltsPerDataset<- c(smpPltsPerDataset, smpPlot)
								
								selectedPlots<- c(selectedPlots,  smpPlot ) # stitch together all selected plots 
							
	cat(paste( "Dataset",  badDatasets[i], unique(dat1$Datasource_name), ":",  nLocs, "Locs:",length(smpPltsPerDataset), "plots" , "\r\n"))
									}
							
# 

				if (nLocs != 1 & nLocs< maxNrPlots ){
		  	# take the n (maxNrPlots / nLocations) plots with longest timeseries and the highest total N from each loc (probably correlated)
					metadat<-  subset(dat1, !is.na(Number)) %>% 
							group_by(Plot_ID) %>%
							summarise(
								Plot_ID = unique(Plot_ID),
								Location = unique(Location),
								Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
								Number_of_years_data = length(unique(Year)), 
								Number_of_samples = length(unique(paste(Year, Period))),
								total_N = sum(Number[Unit == "abundance"], na.rm = T),
								richness = max(Number[Unit == "richness"], na.rm = T)
							)
					metadat<- arrange(metadat, Location, -Number_of_years_data, -total_N)					

					idealNrPlotsPerLoc<- maxNrPlots / nLocs
					proportional<- table(metadat$Location)/ nrow(metadat)
					proportional <- as.data.frame(	proportional*maxNrPlots)	# proportial dividion of plots oiver locations 
				# but what if there are not enough plots in each Location? 
				
				# take proportionally to how they're divided over the plots, but enforce represenation of all locations
			locs<- unique(metadat$Location)	
			
			for(k in 1:nLocs){
				
				meta1<- subset(metadat, Location == locs[k])
				# for locations with fewer (less than half) plots, take nr of plots rounded up
				if(proportional$Freq[proportional$Var1 == locs[k] ] <=median(proportional$Freq))
				nPlots<- ceiling(proportional[proportional$Var1 == locs[k], ]$Freq)
				
				# for locations with many plots (upper half) 
				if(proportional$Freq[proportional$Var1 == locs[k] ] > median(proportional$Freq))
					nPlots<- floor(proportional[proportional$Var1 == locs[k], ]$Freq)
				
				
				smpPlot<- meta1$Plot_ID[1:nPlots]	
				
				smpPltsPerDataset<- c(smpPltsPerDataset, smpPlot)
				
				
				selectedPlots<- c(selectedPlots,  smpPlot ) # stitch together all selected plots 
				
				
			}
					cat(paste( "Dataset",  badDatasets[i], unique(dat1$Datasource_name), ":",  nLocs, "Locs:",length(smpPltsPerDataset), "plots" , "\r\n"))
					}
					
	#
	
	
 
				 if (nLocs > maxNrPlots){
		  	
				 	
				 	# We want to keep some geographical spread, so we look for the major axis of variation
				 	#remove Location column 
				 	dat1<- dat1[, -(which(names(dat1) == 'Location'))]
				 	
				 	
						dat1<- merge(dat1, plots)
						dat1$Latitude[dat1$Latitude == 0]<- NA # rmove any 0's for missing data
						dat1$Longitude[dat1$Longitude == 0]<- NA
						
							#take major axis of variation in space (lat or long or both and divide data into equally sized groups
							long<- max(dat1$Longitude, na.rm = T) - min (dat1$Longitude, na.rm = T)			
							lat <- max(dat1$Latitude, na.rm = T) - min (dat1$Latitude, na.rm = T)			
							
							# if major axis of variation is latitude
							if(lat>long){
							lat<- unique(dat1$Latitude)
							lat<-data.frame(Latitude =  lat, 
												 Group = as.numeric(cut_number(lat, maxNrPlots))) # divide plots in equal numbers over geographical gradient
							table(lat$Group)
							dat1<- merge(dat1, lat); dim(dat1)
							
							}else{
								long<- unique(dat1$Longitude)
								long<-data.frame(Longitude =  long, 
																Group = as.numeric(cut_number(long, maxNrPlots))) # divide plots in equal numbers over geographical gradient
								table(long$Group)
								dat1<- merge(dat1, long); dim(dat1)}
							
							
							
							dat1%>% group_by(Group) %>%
								summarise(
									Plot_IDs = length(unique(Plot_ID)))
							
							# for each group get an overview of the quality of the plots 
												selectedPlots<- NULL
														for (j in 1: maxNrPlots){
											dat2<- subset(dat1, Group == j); dim(dat2)
											
											metadat<-  subset(dat2, !is.na(Number)) %>% 
												group_by(Plot_ID) %>%
												summarise(
													Plot_ID = unique(Plot_ID),
													Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
													Number_of_years_data = length(unique(Year)), 
													Start_year = min(Year, na.rm = T),
													End_year = max(Year, na.rm = T),
													Number_of_years_data = length(unique(Year)), 
													Number_of_samples = length(unique(paste(Year, Period))),
													total_N = sum(Number[Unit == "abundance"], na.rm = T),
													richness = max(Number[Unit == "richness"], na.rm = T)
													)
											
											maxYr<- max(metadat$Number_of_years_data)
			
			# if there's only 1 sample with highest number of years, take it
			if(sum(metadat$Number_of_years_data == maxYr) == 1){
			
			smpPlot<- metadat$Plot_ID[metadat$Number_of_years_data == maxYr]}
			
			#if there's multiple , take the one with highest N
			if(sum(metadat$Number_of_years_data == maxYr) > 1){
				
			smpPlot<- metadat[metadat$Number_of_years_data == maxYr, ]
			smpPlot<- smpPlot$Plot_ID[smpPlot$total_N  == max(smpPlot$total_N)]
			}
			
			smpPltsPerDataset<- c(smpPltsPerDataset, smpPlot)
											
			selectedPlots<- c(selectedPlots,  smpPlot )
			
														
				}												
											cat(paste( "Dataset",  badDatasets[i], unique(dat1$Datasource_name),  length(smpPltsPerDataset), "plots" , "\r\n"))
											}

												}

	# close loop ====================================================================											

			length(selectedPlots) # should be nr Datasets * maxNrPlots
			length(unique(selectedPlots))# should be same 
			length(badDatasets)*maxNrPlots

			# select these from DF 
 
			completeData2022badselected<- subset(completeData2022bad, Plot_ID %in% selectedPlots) 
			
			#stitch back together and assign name
				cDfixed<- rbind(completeData2022Good, completeData2022badselected) 
				path<- "C:\\Dropbox\\Insect Biomass Trends/csvs/"
				dfname<- paste0("completeData2022max", maxNrPlots, "plots"); dfname
				
				#assign(dfname, cDfixed)
				
				saveRDS(cDfixed, file = paste0(path, dfname, ".rds"))
				
}				
				dim(cDfixed) #works


# Plot Fig S3####
#	get standard model outputs: Abundance, richness, pie. 

	inlaRichSum<- as.data.frame(readRDS("inlaRichnessTSUMMARY.rds"))
	richMarg<- readRDS("inlaRichnessTMarginal.rds")
	richMarg$Metric<- "Richness"
	richMarg$Data<- "All data" 
	
	inlaAbSum<- as.data.frame(readRDS("inlaAbunTSUMMARY.rds"))
	abMarg<- readRDS("inlaAbunTMarginal.rds" )
	abMarg$Metric <- "Abundance"
	abMarg$Data<- "All data"
	
	inlaPieSum<- as.data.frame(readRDS("inlaENSPIETSUMMARY.rds"))
	pieMarg<- readRDS("inlaENSPIETMarginal.rds" )
	pieMarg$Metric <- "Simpson diversity (ENS)"
	pieMarg$Data<- "All data"
	
	
	# get model outputs of sensitivity checks:
	# model summary:
	inlaRich50Sum<- as.data.frame(readRDS("sensitivity analysis/inlaRichness50plotsTSUMMARY.rds"))
	# marginals:
	inlaRich50Marg<- readRDS("sensitivity analysis/inlaRichness50plotsTMarginal.rds" )
	# add info for faceting: 
	inlaRich50Marg$Metric<- "Richness"
	inlaRich50Marg$Data<- "Max 50 plots per dataset"
	
	inlaAb50Sum<- as.data.frame(readRDS("sensitivity analysis/inlaAbundance50plotsTSUMMARY.rds"))
	inlaAb50Marg<- readRDS("sensitivity analysis/inlaAbundance50plotsTMarginal.rds" )
	inlaAb50Marg$Metric<- "Abundance"
	inlaAb50Marg$Data<- "Max 50 plots per dataset"
	
	inlaPIE50Sum<- as.data.frame(readRDS("sensitivity analysis/inlaENSPIE50plotsTSUMMARY.rds"))
	inlaPIE50Marg<- readRDS("sensitivity analysis/inlaENSPIE50plotsTMarginal.rds" )
	inlaPIE50Marg$Metric<- "Simpson diversity (ENS)"
	inlaPIE50Marg$Data<- "Max 50 plots per dataset"
	
	inlaRich20Sum<- as.data.frame(readRDS("sensitivity analysis/inlaRichness20plotsTSUMMARY.rds"))
	inlaRich20Marg<- readRDS("sensitivity analysis/inlaRichness20plotsTMarginal.rds" )
	inlaRich20Marg$Metric<- "Richness"
	inlaRich20Marg$Data<- "Max 20 plots per dataset"
	
	inlaAb20Sum<- as.data.frame(readRDS("sensitivity analysis/inlaAbundance20plotsTSUMMARY.rds"))
	inlaAb20Marg<- readRDS("sensitivity analysis/inlaAbundance20plotsTMarginal.rds" )
	inlaAb20Marg$Metric<- "Abundance"
	inlaAb20Marg$Data<- "Max 20 plots per dataset"
	
	inlaPIE20Sum<- as.data.frame(readRDS("sensitivity analysis/inlaENSPIE20plotsTSUMMARY.rds"))
	inlaPIE20Marg<- readRDS("sensitivity analysis/inlaENSPIE20plotsTMarginal.rds" )
	inlaPIE20Marg$Metric<- "Simpson diversity (ENS)"
	inlaPIE20Marg$Data<- "Max 20 plots per dataset"
	
	inlaRich10Sum<- as.data.frame(readRDS("sensitivity analysis/inlaRichness10plotsTSUMMARY.rds"))
	inlaRich10Marg<- readRDS("sensitivity analysis/inlaRichness10plotsTMarginal.rds" )
	inlaRich10Marg$Metric<- "Richness"
	inlaRich10Marg$Data<- "Max 10 plots per dataset"
	
	inlaAb10Sum<- as.data.frame(readRDS("sensitivity analysis/inlaAbundance10plotsTSUMMARY.rds"))
	inlaAb10Marg<- readRDS("sensitivity analysis/inlaAbundance10plotsTMarginal.rds" )
	inlaAb10Marg$Metric<- "Abundance"
	inlaAb10Marg$Data<- "Max 10 plots per dataset"
	
	inlaPIE10Sum<- as.data.frame(readRDS("sensitivity analysis/inlaENSPIE10plotsTSUMMARY.rds"))
	inlaPIE10Marg<- readRDS("sensitivity analysis/inlaENSPIE10plotsTMarginal.rds" )
	inlaPIE10Marg$Metric<- "Simpson diversity (ENS)"
	inlaPIE10Marg$Data<- "Max 10 plots per dataset"
	
# bind together: 
	univarPlts<- rbind(abMarg, richMarg, pieMarg, inlaAb10Marg, inlaRich10Marg, inlaPIE10Marg, inlaAb20Marg, 
										 inlaRich20Marg, inlaPIE20Marg , inlaAb50Marg, inlaRich50Marg, inlaPIE50Marg)
	univarPlts$Metric<- factor(univarPlts$Metric, levels = c ("Abundance", "Richness", "Simpson diversity (ENS)" ))
	univarPlts$Data<- factor(univarPlts$Data, levels = c ("All data", "Max 50 plots per dataset" , "Max 20 plots per dataset"  ,  "Max 10 plots per dataset" ))
	
	mean <- univarPlts %>%
		group_by(Realm, Metric ,Data) %>%
		filter(y == max(y, na.rm=TRUE))
	
	
	univarPlts<- subset(univarPlts, y> 1) # this is only to remove white space from the plots
	
		
	brks<- c(-0.02, -0.01, -0.005, 0, 0.005, 0.01, 0.02, 0.03, 0.04)
	perc<-(10^(brks )  *100) - 100
	l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
	
	ggplot(univarPlts, aes(x = x, y = y))+
		#	geom_line( )+
		geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8)+
		geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6)+
		geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3)+
		geom_area( aes(x = x, y = y, fill = Realm), alpha = 0.3)+
		geom_vline(data= subset(mean, Realm == 'Terrestrial'), aes(xintercept = x), linetype='dotted')+
		geom_vline(xintercept = 0, linetype = 'dashed')+
		facet_grid(cols = vars(Metric), rows = vars(Data), scales = 'free_x')+ #, switch = "y"
		ylab ("Density")+  xlab("Trend slope  \n % change per year")+
		scale_fill_manual(values = "grey50")+
		scale_x_continuous(breaks = brks,labels = l)+#, limits=c(-0.012,  0.014))+
		scale_linetype_manual(values = lntps)+
		theme_classic()+
		theme(axis.text.y=element_blank(),
					axis.ticks.y=element_blank(), 
					legend.key=element_blank(), 
					legend.position="none"
		)
	
	
	ggsave(filename = "Fig S3 number of plots.png" , path = figure_path, width = 18, height = 20,  units = "cm",dpi = 300, device = "png")
	ggsave(filename = "Fig S3 number of plots.pdf" , path = figure_path, width = 18, height = 20,  units = "cm",dpi = 300, device = "pdf")
	
	
	
				
# Plot Fig S5 quartiles #####
# Quartiles based on distribution of abundance values (0.25, median, 0.75)
sads<- subset(completeData2022pure, Unit ==  "logNrQ1" |  Unit ==   "logNrQ2"|  Unit == "logNrQ3" |  Unit == "logNrQ4")
piv<- dcast(subset(sads, !is.na(Number)), Plot_ID+ Year ~ Unit, value.var = "Number", mean)
pivot_wider(sads, id_cols = Plot_ID, from = Unit, values_from = Number, values_fn = mean)

quart1<- as.data.frame(readRDS("quartile1TSUMMARY.rds"))
q1Marg<- readRDS("quartile1TMarginal.rds" )
q1Marg$Metric<- "Quartile 1"

quart2<- as.data.frame(readRDS("quartile2TSUMMARY.rds"))
q2Marg<- readRDS("quartile2TMarginal.rds" )
q2Marg$Metric<- "Quartile 2"

quart3<- as.data.frame(readRDS("quartile3TSUMMARY.rds"))
q3Marg<- readRDS("quartile3TMarginal.rds" )
q3Marg$Metric<- "Quartile 3"

quart4<- as.data.frame(readRDS("quartile4TSUMMARY.rds"))
q4Marg<- readRDS("quartile4TMarginal.rds" )
q4Marg$Metric<- "Quartile 4"



quartilesData<- rbind(q1Marg, q2Marg,q3Marg, q4Marg)
quartilesData$Metric<- as.factor(quartilesData$Metric)

quartilesDataNeg<- quartilesData
#quantilesDataNeg$x<- quantilesDataNeg$x+0.000000001
quartilesDataNeg[, 4:7] <- -quartilesDataNeg[, 4:7]
quartilesDataNeg$Realm[quartilesDataNeg$Realm == "Terrestrial"]<- "Terrestrial2"
quartilesDataNeg$Realm[quartilesDataNeg$Realm == "Freshwater"]<- "Freshwater2"

quartilesData<- rbind(quartilesData, quartilesDataNeg)

# cut off tails 
quartilesData <- quartilesData[quartilesData$x <0.01 & quartilesData$x >-0.005 , ]


brks<- c(-0.02, -0.01, -0.005, -0.0025, 0, 0.0025, 0.005, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
col.scheme.realm2<- c(Terrestrial = "grey50",  Terrestrial2 = "grey50")

ggplot(subset(quartilesData, y>1 | y< -1 ), aes(x = x, y = y))+
	#	geom_line( )+
	geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8)+
	geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6)+
	geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3)+
	geom_area( aes(x = x, y = y, fill = Realm), alpha = 0.3)+
	geom_vline(xintercept = 0)+
	coord_flip()+
	facet_grid(cols = vars(Metric), switch = "x")+
	ylab ("SAD quartile")+  xlab("Trend slope  \n % change per year")+
	scale_fill_manual(values = col.scheme.realm2)+
	scale_x_continuous(breaks = brks,labels = l)+#, limits=c(-0.005,  0.01))+
	ggtitle("Number of species per SAD quartile")+
	theme_classic()+
	theme(axis.text.x=element_blank(),
				axis.ticks.x=element_blank(), 
				legend.key=element_blank(), 
				legend.position="none", 
				strip.background = element_rect(colour = "white")
	)

ggsave(filename = "Fig S5 quartiles.png" , path = figure_path, width = 12, height = 12,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig S5 quartiles.pdf" , path = figure_path, width = 12, height = 12,  units = "cm",dpi = 300, device = "pdf")


				
				
						
				
				
				
				
				
				
				
 # Prep data for effect of bias towards Europe and North America #####
			
			metadata_per_datasetX<- subset(completeData2022pure, Unit == "richness") %>% 
				group_by(Datasource_ID) %>%
				summarise(
					Datasource_ID = unique(Datasource_ID),
					Datasource_name = unique(Datasource_name), 
					Realm = unique(Realm),
					Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
					Number_of_years_data = length(unique(Year)), 
					Start_year = min(Year, na.rm = T),
					End_year = max(Year, na.rm = T),
					Number_of_plots =  length(unique(Plot_ID)), # should be 1
					Number_of_samples = length(unique(paste(Year, Period))),
					Continent = unique(Continent), 
					Country_State = unique(Country_State),
					Country = unique(Country),
					Region = unique(Region)
				)
			

			
datasets<- as.data.frame(table(metadata_per_datasetX$Continent, metadata_per_datasetX$Realm )	 )
 
median(subset(datasets, Var2 == "Freshwater")$Freq )
 




#   
completeData2022$Continent2<- completeData2022$Continent
completeData2022$Continent2[completeData2022$Continent2 == "Oceania"] <- "Rest"
completeData2022$Continent2[completeData2022$Continent2 == "Asia"] <- "Rest"
completeData2022$Continent2[completeData2022$Continent2 == "Latin America"] <- "Rest"
completeData2022$Continent2[completeData2022$Continent2 == "Africa"] <- "Rest"
# sorry people
metadata_per_cont<- completeData2022 %>% 
	group_by(Continent2, Realm, Unit ) %>%
	summarise(
		Datasets = length(unique(Datasource_ID)),
		Number_of_plots =  length(unique(Plot_ID))	)

metadata_per_cont<- subset(metadata_per_cont, Unit %in% c("richness", "abundance", "ENSPIE"))
metadata_per_cont$Metric <- NA
metadata_per_cont$Metric[metadata_per_cont$Unit == "richness"]<- "Richness"
metadata_per_cont$Metric[metadata_per_cont$Unit == "abundance"]<- "Abundance"
metadata_per_cont$Metric[metadata_per_cont$Unit == "ENSPIE"]<- "Simpson diversity (ENS)"
names(metadata_per_cont)[names(metadata_per_cont) == "Continent2"]<- "Continent"
metadata_per_cont$text<- paste(  metadata_per_cont$Datasets, "|", metadata_per_cont$Number_of_plots)
metadata_per_cont$Metric<- factor(metadata_per_cont$Metric, levels = c ("Abundance", "Richness", "Simpson diversity (ENS)" ))
metadata_per_cont$x<- NA
metadata_per_cont$x[metadata_per_cont$Metric == "Simpson diversity (ENS)" & metadata_per_cont$Realm == "Terrestrial"] <- -0.0015
metadata_per_cont$x[metadata_per_cont$Metric == "Simpson diversity (ENS)" & metadata_per_cont$Realm == "Freshwater"] <- 0.008
metadata_per_cont$x[metadata_per_cont$Metric == "Richness" & metadata_per_cont$Realm == "Terrestrial"] <- -0.005
metadata_per_cont$x[metadata_per_cont$Metric == "Richness" & metadata_per_cont$Realm == "Freshwater"] <- 0.005
metadata_per_cont$x[metadata_per_cont$Metric == "Abundance" & metadata_per_cont$Realm == "Terrestrial"] <- -0.01
metadata_per_cont$x[metadata_per_cont$Metric == "Abundance" & metadata_per_cont$Realm == "Freshwater"] <- 0.01
metadata_per_cont$y<- NA
metadata_per_cont$y[metadata_per_cont$Continent == "Europe"] <- 400 
metadata_per_cont$y[metadata_per_cont$Continent == "North America"] <- 300 
metadata_per_cont$y[metadata_per_cont$Continent == "Rest"] <- 200 

metadata_per_cont$Continent<- factor(metadata_per_cont$Continent, levels = c ("Rest", "Europe", "North America"   ))

# not used: 
contPIESum<- as.data.frame(readRDS("sensitivity analysis/inlaENSPIEFixContTSUMMARY.rds"))
contAbSum<- as.data.frame(readRDS("sensitivity analysis/inlaAbundFixContTSUMMARY.rds"))
contRichSum<- as.data.frame(readRDS("sensitivity analysis/inlaRichFixContTSUMMARY.rds"))

# used
cont2PIESum<- as.data.frame(readRDS("sensitivity analysis/inlaENSPIEFixCont2TSUMMARY.rds"))
cont2AbSum<- as.data.frame(readRDS("sensitivity analysis/inlaAbundFixCont2TSUMMARY.rds"))
cont2RichSum<- as.data.frame(readRDS("sensitivity analysis/inlaRichFixCont2TSUMMARY.rds"))
cont2PIEmarg<- (readRDS("sensitivity analysis/inlaENSPIEFixCont2TMarginal.rds"))
cont2Abmarg<-   (readRDS("sensitivity analysis/inlaAbundFixCont2TMarginal.rds"))
cont2Richmarg<-   (readRDS("sensitivity analysis/inlaRichFixCont2TMarginal.rds"))



# Plot Fig S9 continents #####

metrics <- c("Abundance", "Richness", "ENSPIE" )	
marginals<- list(cont2Abmarg, cont2Richmarg, cont2PIEmarg)
summaries<-  list(cont2AbSum, cont2RichSum, cont2PIESum)


#select correct list objects for slopes
elements_to_take<- c(4:6)


ContMargs<- NULL
for ( j in 1:3){

# take each list 
obj<- marginals [[j]]
sum<- summaries [[j]]	
metric<- metrics[j]

obj<- obj [elements_to_take]
sum<- sum[elements_to_take, ]



vars<- names(obj)


for(i in 1: length(vars)){
# for each object  loop through variables


var<-vars[i]	
#Realm<- strsplit(var, ":")[[1]][1]
Continent<- strsplit(var, ":")[[1]][1]
#Realm<-gsub("Realm", "", Realm)
Continent<-gsub("Continent2", "", Continent)


estName<- sum[rownames(sum) == var, ]

marg<- obj[[var]]

marg <-  data.frame(Metric = metric, 
								 Realm = "Terrestrial", 
								 Continent = Continent, 
								 x = marg$x, 
								 y = marg$y)
marg$y80<- marg$y
marg$y90<- marg$y
marg$y95<- marg$y

	marg$y80  [marg$x< estName$`0.1quant`] <- NA # allocate NA to evrything below the 80% quantile 
	marg$y80  [marg$x> estName$`0.9quant` ]<- NA
	marg$y90  [marg$x< estName$`0.05quant`]<- NA # allocate 0 to evrything below the 90% quantile 
	marg$y90  [marg$x> estName$`0.95quant` ]<- NA
	marg$y95  [marg$x< estName$`0.025quant`]<- NA # allocate 0 to evrything below the 95% quantile 
	marg$y95  [marg$x> estName$`0.975quant`]<- NA


	ContMargs<- rbind(ContMargs, marg)

}
	}
dim(ContMargs)	
table(ContMargs$Continent, ContMargs$Metric)
	
ContMargs$y[ContMargs$y < 1 ]<- NA	
	
ContMargs$Metric[ContMargs$Metric == "ENSPIE"]<- "Simpson diversity (ENS)"
#ContMargs$Metric<- factor(ContMargs$Metric, levels = c ("Abundance", "Richness", "Simpson diversity (ENS)" ))
ContMargs$Continent<- factor(ContMargs$Continent, levels = c ("Rest", "Europe", "North America"   ))
	

	
	brks<- c(-0.02, -0.01, -0.005, 0, 0.005, 0.01, 0.02, 0.03, 0.04)
	perc<-(10^(brks )  *100) - 100
	l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
	
	ggplot(ContMargs, aes(x = x, y = y))+
		geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8)+
		geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6)+
		geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3)+
		geom_area( aes(x = x, y = y, fill = Realm), alpha = 0.3)+
		#geom_vline(data= mean, aes(xintercept = x, linetype=Realm))+
		geom_vline(xintercept = 0, linetype = 'dashed')+
		facet_grid(cols = vars(Metric), rows = vars(Continent), switch = "y", scales = 'free')+ #
		ylab ("Density")+  xlab("Trend slope")+
		scale_linetype_manual(values = lntps)+
		scale_fill_manual(values = 'grey50')+
		scale_x_continuous(breaks = brks)+ #,  limits=c(-0.02,  0.022)
		
		geom_text(aes(x = x, y = y, label = text), data = subset(metadata_per_cont, Realm == "Terrestrial" ), size = 3)+
	#	geom_text(aes(x = x, y = y, label = text), data = subset(metadata_per_cont, Realm == "Freshwater" ), size = 3)+
		
		theme_classic()+
		theme(axis.text.y=element_blank(),
					axis.ticks.y=element_blank(), 
					legend.key=element_blank(), 
					legend.position="none", 
					strip.text.y.left = element_text(size=10, angle=90, hjust = 1),
					strip.background = element_rect(colour = "white")
		)
 
	ggsave(filename = "Fig S8 Geography.png" , path = figure_path, width = 18, height = 12,  units = "cm",dpi = 300, device = "png")
	ggsave(filename = "Fig S8 Geography.pdf" , path = figure_path, width = 18, height = 12,  units = "cm",dpi = 300, device = "pdf")
	
	
	

	
	
	
	











#  NOT USED: Taxonomic resolution #####



raw3<- readRDS(file = "rawInsectsForRichnessAggregatedPerYear.RDS")
raw3ab<- subset(raw3, Unit_in_data == "abundance")

plots<-read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/PlotData 5.0.csv"); dim(plots)
UKfwPlots<- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/UKfwSites.csv")
plots<- rbind(plots[, names(UKfwPlots)], UKfwPlots)


length(unique(raw3ab$Plot_ID))		

# plots with > 2 years: 	


plts<- unique(raw3ab$Plot_ID)

ps<- NULL
for(i in 1:length(plts)){
	
	dat<- 	subset(raw3ab, Plot_ID == plts[i])
	dim(dat)
	length(unique(dat$Year))
	sum<- summary(lm(Rank ~ Year, data = dat))
	
	p<- data.frame( Plot_ID = plts[i],
									Datasource_ID = unique(dat$Datasource_ID), 
									Slope_est = sum$coefficients[2,1],
									p = sum$coefficients[2,4])
	
	ps<- rbind(ps, p)
	
}

hist(ps$p)
hist(ps$Slope_est)
sum(ps$p<0.05 & ps$Slope_est>0, na.rm = T) # 301, that's more than the expected 63
sum(ps$p<(0.05/1269)& ps$Slope_est>0, na.rm = T) #174 seem to have a real slope 


plotsWithSlope<- subset(ps, p<(0.05/1269) & Slope_est>0)

merge(plotsWithSlope, studies[, c("Datasource_ID", "Datasource_name")])


for(i in 1:20){
	
	plt<- plotsWithSlope$Plot_ID[i]
	dat<- 	subset(raw3ab, Plot_ID == plt)
	
	sum<- summary(lm(Rank ~ Year, data = dat))
	p = sum$coefficients[2,4]
	
	print(
		ggplot(dat, aes(x = Year, y = Rank))+
			geom_point()+
			stat_smooth(method = 'lm')+ 
			ggtitle(paste(plt, "p =", p))
	)
}



sum(ps$p<(0.05)& ps$Slope_est>0, na.rm = T) #174 seem to have a real slope 


plotsWithWeakerSlope<- subset(ps, p<(0.05) & Slope_est>0)

merge(plotsWithWeakerSlope, studies[, c("Datasource_ID", "Datasource_name")])

table(merge(plotsWithWeakerSlope, studies[, c("Datasource_ID", "Datasource_name")])$Datasource_name)



bad_tax<- plotsWithWeakerSlope$Plot_ID

very_bad_tax<- plotsWithSlope$Plot_ID


