suppressPackageStartupMessages( library (INLA))
INLA:::inla.dynload.workaround()



library(tidyverse)
library(reshape2)
library(boot)
library(data.table)

# load task file with models to run
parameters<- read.csv("metrics.csv", stringsAsFactors = F)


args <- commandArgs(trailingOnly = T)
output_dir <- args[1]
taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))
threads <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
load("completeData2023pure.RData") 

# exclude freshwater
completeData2023pure<- subset(completeData2023pure, Realm != 'Freshwater')

exptPlots<- c(5, # alaska
							921, 922, 924,925, #smes
							643, 644, 646, 647, # hemlock removal
							137, 138, 139  #brazil fragmentation experiment
)
exptDatasources<- c(300,1364, 1357,1387, 1410) #Kellogg, Luquillo CTE, Cedar creek big bio, some German grassland, austr sipders

completeData2023pure<- completeData2023pure[!completeData2023pure$Datasource_ID %in% exptDatasources, ]
completeData2023pure<- completeData2023pure[!completeData2023pure$Plot_ID %in% exptPlots, ]

# exclude plots with a trend in taxonomic resolution:
bad_tax<- 
	c(849L, 132L, 1442L, 1527L)

completeData2023pure<- completeData2023pure[!completeData2023pure$Plot_ID %in% bad_tax, ]
dim(completeData2023pure)

completeData2023pure<- subset(completeData2023pure, Datasource_ID != 1353 & Datasource_ID != 1402 ) 
# remove 1 Portal ant dataset and russia springtails

length(unique(completeData2023pure$Datasource_ID)) # 106

unique(completeData2023pure[, c("Datasource_ID", "Datasource_name")]) # datasource_IDs
length(unique(completeData2023pure$Plot_ID))#923

completeData2023pure$pltYr<- paste0(completeData2023pure$Plot_ID, "_", completeData2023pure$Year)

startEndYears<- completeData2023pure%>% 
	group_by(Plot_ID) %>%
	summarise(
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T), 
		nYrs = length(unique(Year)),
	)
startEndYears$pltStartYr<- paste0(startEndYears$Plot_ID, "_", startEndYears$Start_year)




# rename for simplicity
completeData2023<- completeData2023pure

# make objects needed for different models
cDrichness <- subset(completeData2023, Unit == "richness"); dim(cDrichness)
cDrarRichness <- subset(completeData2023, Unit == "rarefiedRichness"); dim(cDrarRichness)
cDabund    <- subset(completeData2023, Unit == "abundance"); dim(cDabund)
cDenspie   <- subset(completeData2023, Unit == "ENSPIE")   ;dim(cDenspie)
cDenspie$Number[is.infinite(cDenspie$Number)] <- 0
cDhorn     <- subset(completeData2023, Unit == "Horn"); dim(cDhorn)
cDhorn$dif <-  cDhorn$Number - cDhorn$ExpectedBeta
cDhorn$SES <- (cDhorn$Number - cDhorn$ExpectedBeta) / cDhorn$SDexpectedBeta
cDhorn<- cDhorn[!cDhorn$pltYr %in% startEndYears$pltStartYr, ] ;dim(cDhorn)
cDbray     <- subset(completeData2023, Unit == "Bray"); dim(cDbray)
cDbray$dif <-  cDbray$Number - cDbray$ExpectedBeta
cDbray$SES <- (cDbray$Number - cDbray$ExpectedBeta) / cDbray$SDexpectedBeta
cDbray<- cDbray[!cDbray$pltYr %in% startEndYears$pltStartYr, ]; dim(cDbray)
cDjaccard  <- subset(completeData2023, Unit == "Jaccard") ; dim(cDjaccard)
cDjaccard$Number[is.nan(cDjaccard$Number)] <- NA
cDjaccard$dif <-  cDjaccard$Number - cDjaccard$ExpectedBeta
cDjaccard$SES <- (cDjaccard$Number - cDjaccard$ExpectedBeta) / cDjaccard$SDexpectedBeta
cDjaccard<- cDjaccard[!cDjaccard$pltYr %in% startEndYears$pltStartYr, ] ; dim(cDjaccard)
cDlogNr10   <- subset(completeData2023, Unit == "logNr10"    ); dim(cDlogNr10)  
cDlogNr90   <- subset(completeData2023, Unit ==  "logNr90"   ); dim(cDlogNr90)  
cDlogNr020   <- subset(completeData2023, Unit ==  "logNr020"   ); dim(cDlogNr020)  
cDlogNr2040   <- subset(completeData2023, Unit ==  "logNr2040"   ); dim(cDlogNr2040)  
cDlogNr4060   <- subset(completeData2023, Unit ==  "logNr4060"   ); dim(cDlogNr4060)  
cDlogNr6080   <- subset(completeData2023, Unit ==  "logNr6080"   ); dim(cDlogNr6080)  
cDlogNr80100   <- subset(completeData2023, Unit ==  "logNr80100"   ); dim(cDlogNr80100)  
cDlogQ1   <- subset(completeData2023, Unit ==  "logNrQ1"   ); dim(cDlogQ1)  
cDlogQ2   <- subset(completeData2023, Unit ==  "logNrQ2"   ); dim(cDlogQ2)  
cDlogQ3   <- subset(completeData2023, Unit ==  "logNrQ3"   ); dim(cDlogQ3)  
cDlogQ4   <- subset(completeData2023, Unit ==  "logNrQ4"   ); dim(cDlogQ4)  
cDdom       <- subset(completeData2023, Unit == "dominanceRel"); dim(cDdom)  
cDshan  <- subset(completeData2023, Unit == "Shannon"); dim(cDshan)  
cDpielou <- subset(completeData2023, Unit == "Pielou"); dim(cDpielou)  
cDevar   <- subset(completeData2023, Unit == "Evar"); dim(cDevar)
cDmcN <- subset(completeData2023, Unit == "dominanceMcNaught"); dim(cDmcN )  
cdCoverageR7	<-subset(completeData2023, Unit == "coverageRichness.7"); dim(cdCoverageR7)
cdCoverageR8	<-subset(completeData2023, Unit == "coverageRichness.8"); dim(cdCoverageR8)
cdCoveragePIE7	<-subset(completeData2023, Unit == "coverageENSpie.7"); dim(cdCoveragePIE7)
cdCoveragePIE8	<-subset(completeData2023, Unit == "coverageENSpie.8"); dim(cdCoveragePIE8)

cDHillNrs <- subset(completeData2023, Unit == "ENSPIE"| Unit == "Shannon" | Unit == "richness")
cDHillNrs <- reshape2::dcast(cDHillNrs[, c(1:9,11:14,37,40, 55, 57:74)], ...~Unit, value.var = "Number") # excluded the Inla indices for interactions between 
cDHillNrs$ShanEven <- exp(cDHillNrs$Shannon) / cDHillNrs$richness
cDHillNrs$ShanEven <- round(cDHillNrs$ShanEven , 6) # round to 6 decimals. for some reason some 1's are recognized as >1
cDHillNrs$ShanEven[cDHillNrs$ShanEven >1] <- NA
cDHillNrs$SimpEven <- (cDHillNrs$ENSPIE) / cDHillNrs$richness
cDHillNrs$ShanEven[is.infinite(cDHillNrs$ShanEven)] <- NA
cDHillNrs$SimpEven[is.infinite(cDHillNrs$SimpEven)] <- NA
# remove plots with only NA values: 
onlyNA<- subset( reshape2::dcast(cDHillNrs, Plot_ID ~ "nonNA", value.var = "SimpEven", function(x){sum(!is.na(x))}), nonNA ==0)
cDHillNrs<- subset(cDHillNrs, !Plot_ID %in% onlyNA$Plot_ID)

topNotch<- unique(cDlogNr020$Datasource_ID) # select only datasets with full underlying species data
length(topNotch)
length(unique(unique(cDlogNr020$Plot_ID)))

cDrichnessReduced<- subset(cDrichness, Datasource_ID %in% topNotch); dim(cDrichnessReduced)
cDabundReduced	<- subset(cDabund, Datasource_ID %in% topNotch); dim(cDabundReduced)
cDenspieReduced	<- subset(cDenspie, Datasource_ID %in% topNotch); dim(cDenspieReduced)
cDrarRichReduced	<- subset(cDrarRichness , Datasource_ID %in% topNotch); dim(cDrarRichReduced)



# put everything in a list to select from
all.df<-list(cDrichness = cDrichness,
						 cDrarRichness = cDrarRichness,
						 cDabund = cDabund,
						 cDenspie =   cDenspie,
						 cDhorn =   cDhorn,
						 cDbray =   cDbray,
						 cDjaccard =   cDjaccard,
						 cDshan=	cDshan, 
						 cDpielou=		cDpielou, 
						 cDevar  =   cDevar,
						 cDlogNr10=	cDlogNr10,
						 cDlogNr90=	cDlogNr90,
						 cDlogNr020=	cDlogNr020,
						 cDlogNr2040=	cDlogNr2040,
						 cDlogNr4060=	cDlogNr4060 ,
						 cDlogNr6080=	cDlogNr6080  ,
						 cDlogNr80100=	cDlogNr80100  ,
						 cDlogQ1	=	cDlogQ1   ,
						 cDlogQ2   =	cDlogQ2   ,
						 cDlogQ3   =	cDlogQ3   ,
						 cDlogQ4   =	cDlogQ4   ,
						 cDdom	=	cDdom, 
						 cDmcN	=	cDmcN, 
						 cDHillNrs = cDHillNrs,
						 cdCoverageR7 = cdCoverageR7,
						 cdCoverageR8 = cdCoverageR8,
						 cdCoveragePIE7 = cdCoveragePIE7,
						 cdCoveragePIE8 = cdCoveragePIE8,
						 cDrichnessReduced = cDrichnessReduced, 
						 cDabundReduced = cDabundReduced, 
						 cDenspieReduced = cDenspieReduced, 
						 cDrarRichReduced= cDrarRichReduced
)






taskID

print("model name:")
parameters$model_name[taskID]

# select metric from the task file
metric<- parameters$model_name[taskID]
metric


# save start time to check total run time later
startTime<- Sys.time()

# grab needed dataframe from list 
dat<- all.df[[as.character(parameters$input_file[taskID])]] ; dim(dat)

# select realm (not necessary, because we only test terrestrial)
rlm<- parameters$subset[taskID]
dat<- subset(dat, Realm == rlm)
dim(dat)
#str(dat)

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

# replace 1 with .999999, otherwise beta regression will crash (only evenness models)
if(fam == "beta"){ 
	dat[, parameters$y[taskID]][dat[, parameters$y[taskID]] ==1 & !is.na(dat[, parameters$y[taskID]])]<- 1-0.000001
	dat[, parameters$y[taskID]][dat[, parameters$y[taskID]] ==0 & !is.na(dat[, parameters$y[taskID]])]<- 0+0.000001
} 


# set model formula using the task file
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

#Run INLA model, needs 1TB memory and 20 minutes - few hours run time:
model <- inla( formul,
							 family = fam,  
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

fixed = model$summary.fixed

parameters$model_name[taskID]
print("done after:")
Sys.time() - startTime
#assign(as.character(parameters$model_name[i]), model)

# save whole model and summary of fixed effects
model_file <- file.path(output_dir, paste0(as.character(parameters$model_name[taskID]),"TEST.rds"))
fixed_file <- file.path(output_dir, paste0(as.character(parameters$model_name[taskID]),"SUMMARY.rds"))
saveRDS (model, file =  model_file)
saveRDS (fixed, file = fixed_file)



# extract posterior marginals and sample posteriors
n <-  data.frame(Metric = metric, 
								 Realm = "Terrestrial",
								 inla.smarginal(model$marginals.fixed$`cYear`, factor = 50))
marg<- n

marg$y80<- marg$y
marg$y80  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.1quant"] ]<- NA # allocate 0 to evrything below the 80% quantile 
marg$y80  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.9quant"] ]<- NA
marg$y90<- marg$y
marg$y90  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.05quant"] ]<- NA # allocate 0 to evrything below the 90% quantile 
marg$y90  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.95quant"] ]<- NA
marg$y95<- marg$y
marg$y95  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.025quant"] ]<- NA # allocate 0 to evrything below the 95% quantile 
marg$y95  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.975quant"] ]<- NA

# save marginal
marg_file <- file.path(output_dir, paste0(metric,"Marginal.rds"))
saveRDS (marg, file =  marg_file)


#pull out random intercepts and slopes:
RandEfDataset <- 	unique(dat[,c("Datasource_ID", "Datasource_name", "Datasource_ID_4INLA", "Datasource_ID_4INLAs", "Realm")])

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

# save random effects
saveRDS (RandEfDataset, file =  rand_file)



