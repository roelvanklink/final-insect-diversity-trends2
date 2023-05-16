# this is a series of functions for calculating all used biodiversity metrics, and a few more. 

# 'addZeroes' adds the missing 0 counts for all years in which a species was not observed in a plot. 

# 'calculate_alpha_metrics' calculates  the alpha scale biodiversity metrics for each year
# it returns the data in the long format

# 'calculate_beta' calculates beta diversity between years in a plot. these are not used in the paper 

# 'calculate_density' calculates the kernel density estimate of each plot. This is not used in the paper

library(reshape2)
library(vegan)
library(mobr)
library(iNEXT)
library(betapart)



addZeroes<- function (myDf) {
	
	completeData<- NULL
	
	
	for(i in 1:length(unique(myDf$Plot_ID))){
		
		plt<- sort(unique(myDf$Plot_ID))[i]
		myData<- myDf[myDf$Plot_ID == plt , ]
		
		# throw error if has multiple periods
		if(length(unique(myData$Period))>1){
			print ("WARNING: more than one period in data")
		}
		
		
		# add up males and females and different dates
		taxonCounts<- aggregate(Number~Taxon + Year , myData, sum)
		
		
		constantData <- unique(myData[,c("Plot_ID","Datasource_ID", "Taxon", "Unit_in_data", "Realm")])#these are defo unique
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




# Evar function by Arnott Aquatic ecology lab
# http://arnottlab.blogspot.ch/2011/11/evar-function.html
Evar<-function(x)   {
	v<-as.numeric(ncol(x)-1)
	for(k in 1:nrow(x)) {
		y<-x[k,2:ncol(x)]
		a<-as.numeric(ncol(x)-1); b<-a; d<-a
		for(i in 1:(ncol(x)-1)) {
			a[i]<-if(y[i]!=0) log(y[i]) else 0 }
		S<-sum(y>0)
		for(j in 1:(ncol(x)-1))    {
			b[j]<-a[[j]]/S}
		c<-sum(b)
		for(i in 1:(ncol(x)-1)) {
			d[i]<-if(y[i]!=0) ((a[[i]]-c)^2/S) else 0 }
		f<-sum(d)
		v[k]<-(1-2/pi*atan(f))   }
	v }










calculate_alpha_metrics<-  function (x) {
	randomMatrix<- x
	
	metrics<- data.frame(
		Year= randomMatrix[, 1],
		abundance= rowSums (as.data.frame(randomMatrix[, -(1)])),
		richness=     vegan::specnumber(as.data.frame(randomMatrix[, -(1)]), MARGIN = 1)
	)
	
	
	
	
	if (sum( vegan::specnumber(as.data.frame(randomMatrix[, -(1)]), MARGIN = 1))!= 0){# if there are any species in the time series
		randomMatrix2<- as.data.frame(randomMatrix[, -(1)])
		rownames(randomMatrix2)<- randomMatrix[, (1)]
		Qs<- quantile(log10(as.matrix(randomMatrix2[randomMatrix2!= 0])), c(0.25, 0.5, 0.75)) # zeroes were removed
		
		
		metrics1<- data.frame(
			Pielou = vegan::diversity(randomMatrix2)/log(vegan::specnumber(randomMatrix2, MARGIN = 1)),
			dominanceAbs =       apply(randomMatrix2, 1, max) ,  # abundance of most abundant species
			dominanceRel =       apply(randomMatrix2, 1, function(x){max(x)/sum(x) }), # relative abundance of most abundant species = Berger-Parker dominance 
			dominanceMcNaught  = apply(randomMatrix2, 1,  function(x){sum(rev(sort(x))[1:2]) / sum(x) }),  # relative abundance of 2 most abudnant species (McNaughton dominance)
			skewness     =       apply(randomMatrix2, 1, e1071::skewness),
			PctRare1    =       apply(randomMatrix2, 1, function(x){sum(x[x != 0] < (sum(x) * 0.01))}),  # number of species with abunandances smaller than 1 perc of n
			PctRare5    =       apply(randomMatrix2, 1, function(x){sum(x[x != 0] < (sum(x) * 0.05)) }),  # number of species with abunandances smaller than 5 perc of n
			PctRare10   =       apply(randomMatrix2, 1, function(x){sum(x[x != 0] < (sum(x) * 0.1)) }),  # number of species with abunandances smaller than 5 perc of n
			PctareNS    =       apply(randomMatrix2, 1, function(x){sum(x[x != 0] < (sum(x) / length(x[x != 0] ))) }), # number of species with abunandances smaller than n/S
			logNr1      =       apply(randomMatrix2, 1, function(x){ sum(log10(x[x != 0]) <= log10(max(randomMatrix2)) * 0.01) }), # number of species with abundances less than 1 perc of max observed abundance
			lognr5      =       apply(randomMatrix2, 1, function(x){ sum(log10(x[x != 0]) <= log10(max(randomMatrix2)) * 0.05) }), # number of species with abundances less than 5 perc of max observed abundance
			logNr10     =       apply(randomMatrix2, 1, function(x){ sum(log10(x[x != 0]) <= log10(max(randomMatrix2)) * 0.1 ) }), # number of species with abundances less than 10 perc of max observed abundance
			logNr020    =       apply(randomMatrix2, 1, function(x){ sum(log10(x[x != 0]) <= log10(max(randomMatrix2)) * 0.2 ) }), # number of species with abundances less than 20 perc of max observed abundance
			logNr2040   =       apply(randomMatrix2, 1, function(x){ sum(log10(x[x != 0]) > log10(max(randomMatrix2)) * 0.2 & log10(x[x != 0]) <= log10(max(randomMatrix2)) * 0.4  ) }),  
			logNr4060   =       apply(randomMatrix2, 1, function(x){ sum(log10(x[x != 0]) > log10(max(randomMatrix2)) * 0.4 & log10(x[x != 0]) <= log10(max(randomMatrix2)) * 0.6  ) }),
			logNr6080   =       apply(randomMatrix2, 1, function(x){ sum(log10(x[x != 0]) > log10(max(randomMatrix2)) * 0.6 & log10(x[x != 0]) <= log10(max(randomMatrix2)) * 0.8  ) }),
			logNr80100  =       apply(randomMatrix2, 1, function(x){ sum(log10(x[x != 0]) > log10(max(randomMatrix2)) * 0.8 ) }), # number of species with abundances more than 80 perc of max observed abundance
			logNr90     =       apply(randomMatrix2, 1, function(x){ sum(log10(x[x != 0]) >= log10(max(randomMatrix2)) * 0.9 ) }), # number of species with abundances more than 90 perc of max observed abundance
			logNr95     =       apply(randomMatrix2, 1, function(x){ sum(log10(x[x != 0]) >= log10(max(randomMatrix2)) * 0.95) }), # number of species with abundances more than 95 perc of max observed abundance
			logNr99     =       apply(randomMatrix2, 1, function(x){ sum(log10(x[x != 0]) >= log10(max(randomMatrix2)) * 0.99) }),
			logNrQ1     =       apply(randomMatrix2, 1, function(x){ sum(log10(x[x != 0]) <= Qs[1] ) }),
			logNrQ2	    =       apply(randomMatrix2, 1, function(x){ sum(log10(x[x != 0]) > Qs[1]  & log10(x[x != 0]) <= Qs[2]  ) }),
			logNrQ3     =       apply(randomMatrix2, 1, function(x){ sum(log10(x[x != 0]) > Qs[2] & log10(x[x != 0]) <= Qs[3]  ) }),
			logNrQ4     =       apply(randomMatrix2, 1, function(x){ sum(log10(x[x != 0]) > Qs[3] ) })
			
		)# number of species with abundances more than 99 perc of max observed abundance
		metrics<- cbind(metrics, metrics1)
	}
	
	if(ncol(randomMatrix)>2){ # calculate Hillnumber only if more than 1 species is present in time serie
		metrics2<- data.frame(
			Evar = Evar(randomMatrix2) ,
			Shannon = vegan::diversity(randomMatrix2, MARGIN = 1,  index='shannon'),
			Simpson = vegan::diversity(randomMatrix2, MARGIN = 1,  index='simpson'),  # complement of simpson
			ENSPIE = vegan::diversity(randomMatrix2, MARGIN = 1,  index='invsimpson'),
			Hill1 = vegan::renyi(as.data.frame(randomMatrix[, -(1)]), scales = 1, hill = T))
		metrics<- cbind(metrics, metrics2)
	}
	
	if (all(round( randomMatrix2) == randomMatrix2) & ncol(as.data.frame(randomMatrix2))>2){ # calculate only on integers
		minN<- min(rowSums(randomMatrix2))
		if(minN <10){minN<- 10}
		
		# this breaks on lines with only 0's. we calculate on other lines and add 0s manually in order to not overestimate 
		rm<- randomMatrix2[rowSums(randomMatrix2)>0, ]# remove lines with 0 species
		l<-as.list(as.data.frame(t(rm)))
		zeroLines<- expand.grid(Assemblage = rownames(randomMatrix2)[rowSums(randomMatrix2)==0 ] , # add zero estimations for empty lines manually
			m        = 0,
			Method = 0,
			Order.q   = c(0:2)     ,
			SC        = 0,
			qD      = 0,
			qD.LCL    = 0,
			qD.UCL= 0
		)
		coverage.7<- estimateD(l, datatype = "abundance", base="coverage", level=0.7, conf=0.95)
		coverage.7<- rbind(coverage.7, zeroLines)
		coverage.7<- coverage.7[order(coverage.7$Assemblage), ]
		coverage.8<- estimateD(l, datatype = "abundance", base="coverage", level=0.8, conf=0.95)
		coverage.8<- rbind(coverage.8, zeroLines)
		coverage.8<- coverage.8[order(coverage.8$Assemblage), ] # sort correctly
		
		
		metrics2<- data.frame(
			rarefiedRichness = vegan::rarefy(randomMatrix2, minN, se = F, MARGIN = 1),
			Singletons   =       apply(randomMatrix2, 1, function(x){sum( x  == 1 )}) ,  # number of species in lowest abundance class (singletons) 
			PIE = mobr::calc_PIE(randomMatrix2) ,
			coverageRichness.7 = subset(coverage.7	, Order.q == 0 )$qD  , 
			coverageRichness.8 = subset(coverage.8	, Order.q == 0 )$qD   ,
			coverageENSshan.7 = subset(coverage.7	, Order.q == 1 )$qD  , 
			coverageENSshan.8 = subset(coverage.8	, Order.q == 1 )$qD   ,
			coverageENSpie.7 = subset(coverage.7	, Order.q == 2 )$qD  , 
			coverageENSpie.8 = subset(coverage.8	, Order.q == 2 )$qD   
			

			)
		metrics<- cbind(metrics, metrics2)
	}
	
	# for datasets with only 1 species, replace shannon, PIE, pielou: 
	if(ncol(randomMatrix)==2){
		metrics$Shannon <- 0
		metrics$ENSPIE <- 0
		metrics$Pielou <- NaN
		metrics$Hill1 <-  NA 
		metrics$rarefiedRichness <- 1
		metrics$PIE  <- 0
	}
	
	
	metricsLong<- reshape2::melt(metrics, id.vars = c( "Year"), 
															 value.name = "Number",
															 variable.name = "Unit_in_data")
	metricsLong <- subset(metricsLong, Unit_in_data != "run")
	
	return(metricsLong)
}







calculate_beta<- function (x) {
	randomMatrix<- x
	randomMatrix_binary <- ifelse(randomMatrix > 0, 1, 0)
	
	
	# calculate multivariate metrics. I keep these as objects in case we later decide to pul out all pairwise distances:
	betaBray<- as.matrix(vegan::vegdist(randomMatrix[, -(1)]))
	betaJac<- as.matrix(vegan::vegdist(randomMatrix_binary[, -(1)], method = "jaccard"))
	betaHorn <- as.matrix(vegan::vegdist(randomMatrix[, -(1)], method = "horn"))
	betaCao <- as.matrix(vegan::vegdist(randomMatrix[, -(1)], method='cao'))
	betaGains <- as.matrix(vegan::designdist(randomMatrix[, -(1)], method = "B-J", terms = "binary",  abcd = FALSE, alphagamma = FALSE, "gains"))
	betaLosses <- as.matrix(vegan::designdist(randomMatrix[, -(1)], method = "A-J", terms = "binary",  abcd = FALSE, alphagamma = FALSE, "losses"))
	# two steps for Jaccard components (so as calculation is done only once)
	J_components <- betapart::beta.pair(randomMatrix_binary[, -(1)], index.family='jaccard')	# distance
	Jbeta <- as.matrix(J_components$beta.jac)
	Jtu <- as.matrix(J_components$beta.jtu)
	Jne <- as.matrix(J_components$beta.jne)
	#n <- length(unique(rare_samp$YEAR))
	
	
	#Put all of this into a Dataframe with 1 value per year:
	metrics<- data.frame(
		Year= randomMatrix[, 1],
		Bray= betaBray [,1],  # beta diversity of each year compared to the first year
		BrayLast= betaBray [,ncol(betaBray)],  # beta diversity of each year compared to the last year
		Jaccard = betaJac [,1],
		JaccardLast = betaJac [,ncol(betaJac)],
		Horn = betaHorn [, 1] ,
		HornLast= betaHorn [, ncol(betaHorn)] ,
		Cao = betaCao[,1],
		gains = betaGains[,1],
		losses = betaLosses[,1],
		Jturnover = Jtu[,1],
		Jnested = Jne[,1]
	)

	
	
	metricsLong<- reshape2::melt(metrics, id.vars = c( "Year"), 
															 value.name = "Number",
															 variable.name = "Unit_in_data")
	metricsLong <- subset(metricsLong, Unit_in_data != "run")
	
	
	
	
	return(metricsLong)
	
}



calculate_density<- function (x, BW = 0.3) {
	randomMatrix<- x
	
	S<- specnumber(randomMatrix[, -(1)])
	mx<- max(randomMatrix[, -(1)])
	mn<- min(as.matrix(randomMatrix[, -(1)])[is.finite(log(as.matrix(randomMatrix[, -(1)])))]) # take min of all non-zero values
	dif <- log10(mx) - log10(mn)
	Year<- randomMatrix[,1 ]
	
	randomMatrix1<- randomMatrix[S>1 , ]  # keep only lines with more than 1 species  
	Year<- Year[S>1  ]  # keep only lines with more than 1 species  
	
	
	
	
	dens<-   apply(randomMatrix1[, -(1)], 1,  function(x){ density(log10(x[x != 0]) , bw = BW,  from = log10(mn), to =  log10(mx), n = 100)$y })
	dens<- as.data.frame(t(dens))
	colnames(dens)<- paste0("p", seq(1:100)) # percentiles
	densAbs<-dens*S
	
	
	densFitsAbs<- cbind( Year, densityMode = "absolute", correction = F,  densAbs)
	densFitsRel<- cbind( Year, densityMode = "relative", correction = F,  dens)
	
	densities<- rbind(densFitsRel, densFitsAbs)
	
	# if the smallest non-0 number is 1, these must be true counts, and the corrected value can be calculated
	nrs<- as.matrix(randomMatrix1[, -1])
	if (min(nrs[nrs != 0]) == 1  )  {
		
		
		d2<- 	apply(randomMatrix1[, -(1)], 1,  function(x){ density(log10(x[x != 0]), bw = BW,  from = -log10(mx), to = log10(mx) , n = 200)$y	 })
		d2<- as.data.frame(t(d2))       
		negd2 <- rev(d2)	 # reverse all estimates along whole line
		
		dsum2<- d2 + negd2  # sum these up to gett sum of all 
		dCor<- dsum2[101:200] # cut off negative values on x
		colnames(dCor)<- paste0("p", seq(1:100)) # percentiles
		dCorAbs<-dCor*S
		
		densFitsCorRel<- cbind( Year, densityMode = "relative", correction = T,  dCor)
		densFitsCorAbs<- cbind( Year, densityMode = "absolute", correction = T,  dCorAbs)
		
		densities<- rbind(densities, densFitsCorRel, densFitsCorAbs)  }
	
	return(densities)
	
}


KDE<- function(randomMatrix, BW = 0.3, mx = 10, mn = 1){
#	mx<- max(randomMatrix)
#	mn<- min(as.matrix(randomMatrix)[is.finite(log(as.matrix(randomMatrix)))])
	
	d<- 	 density(log10(randomMatrix[randomMatrix != 0]), bw = BW,  from = -log10(mx), to = log10(mx) , n = 200)	
	d2<- 	 d$y	
   
	negd2 <- rev(d2)	 # reverse all estimates along whole line
	
	dsum2<- d2 + negd2  # sum these up to gett sum of all 
	dCor<- dsum2[101:200] # cut off negative values on x
	d$y<- as.matrix(dCor)
	d$x<- d$x[101:200]
	
	
	return(d)
	
	
}

