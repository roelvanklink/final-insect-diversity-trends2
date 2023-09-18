suppressPackageStartupMessages( library (INLA))
INLA:::inla.dynload.workaround()



library(tidyverse)
library(reshape2)
parameters<- read.csv("sensitivityanalyses.csv", stringsAsFactors = F)


args <- commandArgs(trailingOnly = T)
output_dir <- args[1]
taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))
threads <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
load("completeData2023pure.RData") ; dim(completeData2023pure)
completeData2023max10plots<- readRDS(file = "completeData2023max10plots.rds")  ; dim(completeData2023max10plots)
completeData2023max20plots<- readRDS(file = "completeData2023max20plots.rds")  ; dim(completeData2023max20plots)
completeData2023max50plots<- readRDS(file = "completeData2023max50plots.rds")  ; dim(completeData2023max50plots)
completeData2023short<- readRDS(file = "completeData2023short.rds");dim(completeData2023short)
completeData2023long<- readRDS(file = "completeData2023long.rds") ; dim(completeData2023long)





exptPlots<- c(5, # alaska
							921, 922, 924,925, #smes
							643, 644, 646, 647, # hemlock removal
							137, 138, 139  #brazil fragmentation experiment
)
exptDatasources<- c(300,1364, 1357,1410, 1387, #Kellogg, Luiquillo CTE, Cedar creek big bio, some german grassland
										1353 , 1402)  # also remove russian springtails (data may be unreliable accoridng to author) and 1 Portal ant dataset
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

completeData2023pure<- completeData2023pure[!completeData2023pure$Datasource_ID %in% exptDatasources, ]
completeData2023pure<- completeData2023pure[!completeData2023pure$Plot_ID %in% exptPlots, ]; dim(completeData2023pure)
completeData2023pure<- completeData2023pure[!completeData2023pure$Plot_ID %in% bad_tax, ]; dim(completeData2023pure)

completeData2023max50plots<- completeData2023max50plots[!completeData2023max50plots$Plot_ID %in% exptPlots, ]; dim(completeData2023max50plots)
completeData2023max50plots<- completeData2023max50plots[!completeData2023max50plots$Datasource_ID %in% exptDatasources, ]; dim(completeData2023max50plots)
completeData2023max50plots<- completeData2023max50plots[!completeData2023max50plots$Datasource_ID %in% bad_tax, ]; dim(completeData2023max50plots)

max20plots<- completeData2023max20plots[!completeData2023max20plots$Plot_ID %in% exptPlots, ]; dim(completeData2023max20plots)
completeData2023max20plots<- completeData2023max20plots[!completeData2023max20plots$Datasource_ID %in% exptDatasources, ]; dim(completeData2023max20plots)
completeData2023max20plots<- completeData2023max20plots[!completeData2023max20plots$Datasource_ID %in% bad_tax, ]; dim(completeData2023max20plots)

completeData2023max10plots<- completeData2023max10plots[!completeData2023max10plots$Plot_ID %in% exptPlots, ]; dim(completeData2023max10plots)
completeData2023max10plots<- completeData2023max10plots[!completeData2023max10plots$Datasource_ID %in% exptDatasources, ]; dim(completeData2023max10plots)
completeData2023max10plots<- completeData2023max10plots[!completeData2023max10plots$Datasource_ID %in% bad_tax, ]; dim(completeData2023max10plots)

completeData2023short<- completeData2023short[!completeData2023short$Plot_ID %in% exptPlots, ]; dim(completeData2023short)
completeData2023short<- completeData2023short[!completeData2023short$Datasource_ID %in% exptDatasources, ]; dim(completeData2023short)
completeData2023short<- completeData2023short[!completeData2023short$Datasource_ID %in% bad_tax, ]; dim(completeData2023short)

completeData2023long<- completeData2023long[!completeData2023long$Plot_ID %in% exptPlots, ]; dim(completeData2023long)
completeData2023long<- completeData2023long[!completeData2023long$Datasource_ID %in% exptDatasources, ]; dim(completeData2023long)
completeData2023long<- completeData2023long[!completeData2023long$Datasource_ID %in% bad_tax, ]; dim(completeData2023long)

# for beta metrics: 
completeData2023pure$pltYr<- paste0(completeData2023pure$Plot_ID, "_", completeData2023pure$Year)

startEndYears<- completeData2023pure%>% 
	group_by(Plot_ID) %>%
	summarise(
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T), 
		nYrs = length(unique(Year)),
	)
startEndYears$pltStartYr<- paste0(startEndYears$Plot_ID, "_", startEndYears$Start_year)
startEndYears$pltEndYr<- paste0(startEndYears$Plot_ID, "_", startEndYears$End_year)




# rename 
completeData2023<- completeData2023pure

# create needed objects by selecting from dataframes
completeData2023max50plotsRichness <- subset(completeData2023max50plots, Unit == "richness"); dim(completeData2023max50plotsRichness)
completeData2023max20plotsRichness <- subset(completeData2023max20plots, Unit == "richness"); dim(completeData2023max20plotsRichness)
completeData2023max10plotsRichness <- subset(completeData2023max10plots, Unit == "richness"); dim(completeData2023max10plotsRichness)
completeData2023max50plotsAbundance <- subset(completeData2023max50plots, Unit == "abundance"); dim(completeData2023max50plotsAbundance)
completeData2023max20plotsAbundance <- subset(completeData2023max20plots, Unit == "abundance"); dim(completeData2023max20plotsAbundance)
completeData2023max10plotsAbundance <- subset(completeData2023max10plots, Unit == "abundance"); dim(completeData2023max10plotsAbundance)
completeData2023max50plotsENSPIE <-    subset(completeData2023max50plots, Unit == "ENSPIE"); dim(completeData2023max50plotsENSPIE)
completeData2023max50plotsENSPIE$Number[completeData2023max50plotsENSPIE$Number == Inf] <- NA
completeData2023max20plotsENSPIE <-    subset(completeData2023max20plots, Unit == "ENSPIE"); dim(completeData2023max20plotsENSPIE)
completeData2023max20plotsENSPIE$Number[completeData2023max20plotsENSPIE$Number == Inf] <- NA
completeData2023max10plotsENSPIE <-    subset(completeData2023max10plots, Unit == "ENSPIE"); dim(completeData2023max10plotsENSPIE)
completeData2023max10plotsENSPIE <-  completeData2023max10plotsENSPIE[!is.infinite(completeData2023max10plotsENSPIE$Number),] ; dim(completeData2023max10plotsENSPIE)
completeData2023max10plotsENSPIE$Number[completeData2023max10plotsENSPIE$Number == Inf] <- NA
completeData2023shortRichness <-  	 subset(completeData2023short, Unit == "richness"); dim(completeData2023shortRichness)
completeData2023shortAbundance <- 	 subset(completeData2023short, Unit == "abundance"); dim(completeData2023shortAbundance)
completeData2023shortENSPIE <- 		subset(completeData2023short, Unit == "ENSPIE"); dim(completeData2023shortENSPIE)
completeData2023shortENSPIE$Number[completeData2023shortENSPIE$Number == Inf] <- NA; dim(completeData2023shortENSPIE)
completeData2023longRichness <- 	subset(completeData2023long, Unit == "richness"); dim(completeData2023longRichness)
completeData2023longAbundance <-  subset(completeData2023long, Unit == "abundance"); dim(completeData2023longAbundance)
completeData2023longENSPIE <- 		subset(completeData2023long, Unit == "ENSPIE"); dim(completeData2023longENSPIE)
completeData2023longENSPIE$Number[completeData2023longENSPIE$Number == Inf] <- NA


cDhorn     <- subset( completeData2023, Unit == "HornLast"); dim(cDhorn)
cDhorn$dif <-  cDhorn$Number - cDhorn$ExpectedBeta
cDhorn$SES <- (cDhorn$Number - cDhorn$ExpectedBeta) / cDhorn$SDexpectedBeta
cDhorn<- cDhorn[!cDhorn$pltYr %in% startEndYears$pltEndYr, ] ;dim(cDhorn) # remove last year (0 dissimilarity) 
cDbray     <- subset(completeData2023, Unit == "BrayLast"); dim(cDbray)
cDbray$dif <-  cDbray$Number - cDbray$ExpectedBeta
cDbray$SES <- (cDbray$Number - cDbray$ExpectedBeta) / cDbray$SDexpectedBeta
cDbray<- cDbray[!cDbray$pltYr %in% startEndYears$pltEndYr, ]; dim(cDbray)
cDjaccard  <- subset(completeData2023, Unit == "JaccardLast") ; dim(cDjaccard)
cDjaccard$Number[is.nan(cDjaccard$Number)] <- NA
cDjaccard$dif <-  cDjaccard$Number - cDjaccard$ExpectedBeta
cDjaccard$SES <- (cDjaccard$Number - cDjaccard$ExpectedBeta) / cDjaccard$SDexpectedBeta
cDjaccard<- cDjaccard[!cDjaccard$pltYr %in% startEndYears$pltEndYr, ] ; dim(cDjaccard)




all.df<-list(completeData2023max50plotsRichness = completeData2023max50plotsRichness,
						 completeData2023max20plotsRichness = completeData2023max20plotsRichness, 
						 completeData2023max10plotsRichness = completeData2023max10plotsRichness,
						 completeData2023max50plotsAbundance =completeData2023max50plotsAbundance,
						 completeData2023max20plotsAbundance =completeData2023max20plotsAbundance,
						 completeData2023max10plotsAbundance = completeData2023max10plotsAbundance,
						 completeData2023max50plotsENSPIE =completeData2023max50plotsENSPIE,
						 completeData2023max20plotsENSPIE =completeData2023max20plotsENSPIE,
						 completeData2023max10plotsENSPIE =completeData2023max10plotsENSPIE,
						 completeData2023shortRichness =completeData2023shortRichness,
						 completeData2023shortAbundance =completeData2023shortAbundance,
						 completeData2023shortENSPIE =completeData2023shortENSPIE,
						 completeData2023longRichness =completeData2023longRichness,
						 completeData2023longAbundance =completeData2023longAbundance,
						 completeData2023longENSPIE =completeData2023longENSPIE,
						 cDjaccard =cDjaccard,
						 cDhorn =cDhorn,
						 cDbray = cDbray
)




taskID

print("model name:")
metric<- parameters$model_name[taskID]
metric

startTime<- Sys.time()

dat<- all.df[[as.character(parameters$input_file[taskID])]] 
rlm<- parameters$subset[taskID]
dat<- subset(dat, Realm == rlm)
dim(dat)


# set priors
vect<- dat[[parameters$yForPrior[taskID]]]

if(parameters$family[taskID] =="gaussian"){
	sd.res<-	3 * sd(log10(vect+1), na.rm = T)
}
if(parameters$family[taskID] =="beta"){
	sd.res<-	3 *sd(boot::logit(vect)[boot::logit(vect) != Inf & boot::logit(vect) != -Inf ], na.rm = T)
}


prior.prec <- list(prec = list(prior = "pc.prec", param = c(sd.res, 0.01))) #1% prob bigger than 1
fam <-   parameters$family[taskID]

# replace 1 with .999999, otherwise beta regression will crash 
if(fam == "beta"){ 
	dat[, parameters$y[taskID]][dat[, parameters$y[taskID]] ==1 & !is.na(dat[, parameters$y[taskID]])]<- 1-0.000001
	dat[, parameters$y[taskID]][dat[, parameters$y[taskID]] ==0 & !is.na(dat[, parameters$y[taskID]])]<- 0+0.000001
} 





formul<-as.formula(paste(parameters$y[taskID], " ~ ", parameters$model_formula[taskID]   ,
												 "+f(Period_4INLA,model='iid', hyper = prior.prec)+
                         f(Plot_ID_4INLA,model='iid', hyper = prior.prec )+
                         f(Location_4INLA,model='iid', hyper = prior.prec)+
                         f(Datasource_ID_4INLA,model='iid', hyper = prior.prec)+
                         f(Plot_ID_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(Location_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(Datasource_ID_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(iYear, model='ou',  replicate=as.numeric(Plot_ID_4INLA), 
			hyper = list(theta1 = list(prior='pc.prec')))" 
))


print(formul)

model <- inla( formul,
							 control.compute = list(config = FALSE, 
							 											 dic=TRUE,
							 											 waic=TRUE, 
							 											 cpo = TRUE, 
							 											 openmp.strategy="huge"), 
							 control.inla = list(int.strategy="eb", 
							 										tolerance =  1e-08), 
							 control.predictor = list(link = 1) , verbose = T, 
							 quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999)  ,    
							 num.threads = threads, blas.num.threads = threads,
							 data=dat)

fixed <- model$summary.fixed

parameters$model_name[taskID]
print("done:")
Sys.time() - startTime
#assign(as.character(parameters$model_name[i]), model)

model_file <- file.path(output_dir, paste0(as.character(parameters$model_name[taskID]),"TEST.rds"))
fixed_file <- file.path(output_dir, paste0(as.character(parameters$model_name[taskID]),"SUMMARY.rds"))

saveRDS (model, file =  model_file)
saveRDS (fixed, file = fixed_file)




# extract posterior marginals and sample posteriors
# m <-  data.frame(Metric = metric, 
# 		 Realm = "Freshwater",
# 		 inla.smarginal(model$marginals.fixed$`RealmFreshwater:cYear`, factor = 50))
n <-  data.frame(Metric = metric, 
								 Realm = "Terrestrial",
								 inla.smarginal(model$marginals.fixed$`cYear`, factor = 50))
marg<- n#rbind(m,n)

marg$y80<- marg$y
#marg$y80  [ marg$Realm == "Freshwater"  & marg$x< fixed["RealmFreshwater:cYear", "0.1quant"] ]<- NA # allocate 0 to evrything below the 80% quantile 
#marg$y80  [ marg$Realm == "Freshwater"  & marg$x> fixed["RealmFreshwater:cYear", "0.9quant"] ]<- NA
marg$y80  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.1quant"] ]<- NA # allocate 0 to evrything below the 80% quantile 
marg$y80  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.9quant"] ]<- NA
marg$y90<- marg$y
#marg$y90  [ marg$Realm == "Freshwater"  & marg$x< fixed["RealmFreshwater:cYear", "0.05quant"] ]<- NA # allocate 0 to evrything below the 90% quantile 
#marg$y90  [ marg$Realm == "Freshwater"  & marg$x> fixed["RealmFreshwater:cYear", "0.95quant"] ]<- NA
marg$y90  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.05quant"] ]<- NA # allocate 0 to evrything below the 90% quantile 
marg$y90  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.95quant"] ]<- NA
marg$y95<- marg$y
#marg$y95  [ marg$Realm == "Freshwater"  & marg$x< fixed["RealmFreshwater:cYear", "0.025quant"] ]<- NA # allocate 0 to evrything below the 95% quantile 
#marg$y95  [ marg$Realm == "Freshwater"  & marg$x> fixed["RealmFreshwater:cYear", "0.975quant"] ]<- NA
marg$y95  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.025quant"] ]<- NA # allocate 0 to evrything below the 95% quantile 
marg$y95  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.975quant"] ]<- NA


marg_file <- file.path(output_dir, paste0(metric,"Marginal.rds"))

saveRDS (marg, file =  marg_file)




# pull out random effect estimates 
RandEfDataset <- 	unique(dat[,c("Datasource_ID", "Datasource_name", "Datasource_ID_4INLA", "Datasource_ID_4INLAs", "Realm")])
#pull out random intercepts and slopes:

#data source ID
intercepts     <- model$summary.random$Datasource_ID_4INLA
slopes         <- model$summary.random$Datasource_ID_4INLAs
slopes_Location<-model$summary.random$Location_4INLAs
slopes_plot    <-model$summary.random$Plot_ID_4INLAs
names(intercepts)[2:ncol(intercepts)]      <- paste("DataID_Intercept_", names(intercepts)[2:ncol(intercepts)]) # names for dataset intercepts
names(slopes)[2:ncol(intercepts)]          <- paste("DataID_Slope_", names(slopes)[2:ncol(intercepts)])             # names for dataset slopes
names(slopes_Location)[2:ncol(intercepts)] <- paste("Loc_slp_", names(slopes_Location)[2:ncol(intercepts)]) # names for Location slopes
names(slopes_plot)[2:ncol(intercepts)]     <- paste("Plot_slp_", names(slopes_plot)[2:ncol(intercepts)])        # names for plot slopes

# datasource level slopes for Fig 1
RandEfDataset <- merge(RandEfDataset, intercepts, by.x="Datasource_ID_4INLA", by.y="ID")
RandEfDataset <- merge(RandEfDataset, slopes, by.x="Datasource_ID_4INLAs", by.y="ID")

# add up fixed slope and random slopes

fx<-data.frame(Realm =  "Terrestrial", #
							 fixedSlp = model$summary.fixed$mean[2], 
							 fixedIntercept = (model$summary.fixed$mean[1]  ) )
RandEfDataset<- merge(RandEfDataset, fx, by = "Realm" )
RandEfDataset$slope <- RandEfDataset$'DataID_Slope_ mean'+ RandEfDataset$fixedSlp # sum of fixed and random slopes  

rand_file <- file.path(output_dir, paste0(metric,"randomSlopes.rds"))


saveRDS (RandEfDataset, file =  rand_file)

















