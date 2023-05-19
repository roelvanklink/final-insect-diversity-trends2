rm(list=ls()) 
# run everything! 

setwd("c:\\Dropbox\\Insect Biomass Trends\\greenland arthropods\\View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_emergence230120191043238647.xlsx\\data/")
library(tidyverse)
library(reshape2)
source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/calculate metrics.R")
source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/effort_rarefaction.R")
source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/calculate expected beta diversity.R")


dat<-     read.csv("C:\\Dropbox\\Insect Biomass Trends\\greenland arthropods\\View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_emergence170320211313433873.csv", sep = "\t")
dim(dat) # dims differ 

sum(duplicated(dat))
head(dat[duplicated(dat), ])
tail(dat[duplicated(dat), ])




#Check dates
unique(dat[,1])  #443 dates
#remove one wrong date with 0 observations
dat<- subset(dat, Date != "9999-08-15" )

dat$date<- as.Date(dat[,1])
dat$Year<- format (dat$date, "%Y")
dat$Period<- format (dat$date, "%m")




# Check some years that were reported to be problematic: 


# data from 2011 were corrupted in a previous version check if fixed now: 
dat2011<- subset(dat, Year == 2011)
dat2011
tail(dat2011, 30)
sample_n(subset(dat, Year == 2011), 25)
unique(dat2011$date) # looks unproblematic to me 


# the samples from 2010 were lost in transport, but have recently (march 2021) turned up in Cambridge (pers comm Niels Martin Schmidt)
dat2010<- subset(dat, Year == 2010)
unique(dat2010$date)
dat<- subset(dat, Year != 2010)

# art1 = windowtrap 
# rest are yellow pitfalls in different vegetation zones
# coordinates: 74o28'N, 20o34'W

dat[dat == -9999]<- NA
dat[dat == -999]<- NA







#check Species

dat$uniq.sp<- apply(dat[,13:17],MARGIN = 1,  function(x) x[max(which(!is.na(x)))])
as.data.frame(unique(dat$uniq.sp) )  # 91, but messy 
as.data.frame(unique(dat[, c(13:17, 32)]) )

subset(dat, is.na(uniq.sp)) # none


dat$uniq.sp[dat$uniq.sp == "unidentified"]<- "Araneae" # fuckup in names


# first calc number ind per trap per day
dat$mn.A<- dat$A / dat$Days.A
dat$mn.B<- dat$B / dat$Days.B
dat$mn.C<- dat$C / dat$Days.C
dat$mn.D<- dat$D / dat$Days.D
dat$mn.E<- dat$E / dat$Days.E
dat$mn.F<- dat$F / dat$Days.F
dat$mn.G<- dat$G / dat$Days.G
dat$mn.H<- dat$H / dat$Days.H
dat$totalCatch<- rowSums(dat[, c("A", "B",  "C",  "D",  "E",  "F" , "G" , "H")], na.rm = T)


# then calc mean value of all traps per plot: 
dat$all.mn<- apply(dat[,  c("mn.A","mn.B","mn.C", "mn.D", "mn.E", "mn.F","mn.G","mn.H")], MARGIN = 1, mean, na.rm = T) # seems fine 
dim( subset(dat, all.mn != "NaN"))
dat<- subset(dat, all.mn != "NaN") # remove not-sampled dates

# there are Inf values here! 
subset(dat, all.mn == Inf)


# assign plot names
dat$Plot_ID<-NA
dat$Plot_ID[dat$Plot.ID == "Art1"]<-1053
dat$Plot_ID[dat$Plot.ID == "Art2"]<-1054
dat$Plot_ID[dat$Plot.ID == "Art3"]<-1055
dat$Plot_ID[dat$Plot.ID == "Art4"]<-1056
dat$Plot_ID[dat$Plot.ID == "Art5"]<-1057
dat$Plot_ID[dat$Plot.ID == "Art7"]<-1058
dat$Plot_ID[dat$Plot.ID == "Art6"]<-1880


gl<- dat  # for richness rarefaction, see below



# Processing for Science paper 2020 #####
# Best to take mean of all active traps at each date. 
# this should then also somehow be standardized for trapping days 

# selection of good data

dat<- subset(dat, Plot.ID != "Art6")  # remove Art6 (only sampled 96-98)
window<-subset(dat, Plot.ID == "Art1") # separate window trap from rest 
dat<- subset(dat, Plot.ID != "Art1") # exclude window trap


sum(duplicated(dat)) #21
sum(duplicated(window)) # 115




dim(dat) # 30.000 lost 



# calc mean dates
# not possible, becaue Days is actually more a reflection of trapping effort (also correcting for number of pooled traps)
# just take date trap was emptied



# make file ready for use 

Datasource_name <- "Greenland arthropods"
Unit<- "abundance"
Transformed_number<- NA;    Sex <- NA
Error <- NA;               
Sample_ID<-292

Greenland<-data.frame(Datasource_name, 
                             Plot_ID = dat$Plot_ID, 
                             Plot_name = dat$Plot.ID, 
                             Sample_ID, 
                             Year = dat$Year,
                             Period = dat$Period,
                             Date = dat$date,
                             Taxon = dat$uniq.sp, 
                             Sex, 
                             Unit, 
                             Original_number  = dat$all.mn, 
                             Transformed_number, 
                             Number = dat$all.mn, 
                             Error
)

dim(subset(Greenland, Number == 0)) # 50.000 zeros
dim(Greenland) # 85537 on 19-12-19

# duplicates?
sum(duplicated(Greenland)) # 69
subset(Greenland[duplicated(Greenland),], Number !=0) # 4 

dim(Greenland)
Greenland<- subset(Greenland, !duplicated(Greenland) | Number >0)


# windowtrap (sum of all partial traps , in stead of mean )

window$Plot_ID<-NA
window$Plot_ID[window$Plot.ID == "Art1"]<-1053

window$w.sum<- apply(window[,  c("mn.A","mn.B","mn.C", "mn.D")], MARGIN = 1, sum, na.rm = T)

Greenland.window<-data.frame(Datasource_name, 
                      Plot_ID = window$Plot_ID, 
                      Plot_name = window$Plot.ID, 
                      Sample_ID = 291, 
                      Year = window$Year,
                      Period = window$Period,
                      Date = window$date,
                      Taxon = window$uniq.sp, 
                      Sex, 
                      Unit, 
                      Original_number  = window$w.sum, 
                      Transformed_number, 
                      Number = window$w.sum, 
                      Error
)

dim(Greenland.window) # 12683 on 19-12-19
dim(subset(Greenland.window, Number == 0)) # 7900 zeros

# duplicates
sum(duplicated(Greenland.window)) # yes! 119 
subset(Greenland.window[duplicated(Greenland.window),], Number !=0)
# all zero's -> remove

Greenland.window<- Greenland.window[!duplicated(Greenland.window),]





Greenland.all<- rbind(Greenland.window, Greenland)


# remove 2010 because moste samples were lost 

Greenland.all<- subset(Greenland.all, Year != 2010) 


subset(Greenland.all, Number == Inf)
dim(subset(Greenland.all, Number == 0)) # 50.000 zeros
dim(Greenland.all)

Greenland.all$Taxon<-gsub(" ", "_", Greenland.all$Taxon)


dcast(Greenland, Year ~ Plot_ID, value.var = "Number", sum, na.rm = T)

sum(duplicated(Greenland.all)) # 188

#write.csv(Greenland.all, "C:\\Dropbox\\Insect Biomass Trends\\csvs/Greenland2020.csv")








# Rarefaction for richness calculation 2021 ####

# check what's wrong with the trapdays in  windowtraps , especially in 2017
# these were reported incorrectly and should have all been the maximum of that sampling date (pers comm Niels Martin Schmidt March 2021)
gl$Days.A[gl$Plot.ID == "Art1" & gl$date == "2015-07-20" ]<- 28
gl$Days.A[gl$Plot.ID == "Art1" & gl$date == "2015-07-27" ]<- 28
gl$Days.A[gl$Plot.ID == "Art1" & gl$date == "2015-08-03" ]<- 28
gl$Days.A[gl$Plot.ID == "Art1" & gl$date == "2015-08-14" ]<- 44
gl$Days.A[gl$Plot.ID == "Art1" & gl$date == "2015-08-17" ]<- 12
gl$Days.A[gl$Plot.ID == "Art1" & gl$date == "2015-08-24" ]<- 28
gl$Days.A[gl$Plot.ID == "Art1" & gl$date == "2015-08-31" ]<- 28




# Following Hoye 2021 PNAS:
#remove may, september and october 
gl<- subset(gl, Period != "05" & Period != "09" & Period != "10") # lost 500 
dim(gl)
# calculate total trapdates per date (number of traps x number of days)
gl$trapDays<- rowSums(gl[, 5:12], na.rm = T )


uniqueSampleEvents<- unique(gl[, c("Plot.ID", "Date", "Year", "Period", "Days.A" ,"Days.B" ,"Days.C", "Days.D", "Days.E", "Days.F", "Days.G", "Days.H" , "trapDays")])
uniqueSampleEvents
# in some years there were 2 traps per station, and in some years only one, that's why each week has between 7 and 14 trapdays. At the start of the season sometimes less.
# stranger is that some weeks in 2015 have multiple entries
sort(subset(gl, Plot.ID == "Art1" & date ==  "2015-07-20" )$uniq.sp) # no duplicates. I guess this is a mistake in enrty of the trapdays. But it only occurs for the present species! 

# trapdays per week
dcast(uniqueSampleEvents, date~ Plot.ID +Period , value.var = "trapDays", sum)

# trapdays per month
dcast(uniqueSampleEvents, Year~ Plot.ID +Period , value.var = "trapDays", sum)  # variable. Some months will need to be excluded in some plots



# fix up taxonomy following Hoye 2021
#"Chironomidae and Ceratopogonidae (hereafter
# called “Chironomidae”), Anthomyiidae andMuscidae (hereafter called “Muscidae”),
# and Mycetophilidae and Sciaridae (hereafter called “Sciaridae”)."
# ignore the rest because it is FLAGGED anyway. We'll check if this makes a difference for the results later  
gl$uniq.sp<- gsub(" ", "_", gl$uniq.sp)
as.data.frame(sort(unique(gl$uniq.sp)))

gl$uniq.sp[gl$uniq.sp == "Ceratopogonidae"] <- "Chironomidae"
gl$uniq.sp[gl$uniq.sp == "Anthomyiidae"] <- "Muscidae"
gl$uniq.sp[gl$uniq.sp == "Mycetophilidae"] <- "Sciaridae"
gl$Family[gl$Family == "Ceratopogonidae"] <- "Chironomidae"
gl$Family[gl$Family == "Anthomyiidae"] <- "Muscidae"
gl$Family[gl$Family == "Mycetophilidae"] <- "Sciaridae"


# sum up all individuals caught in all traps 
gl$totalCatch<-  rowSums(gl[, c("A", "B",  "C",  "D",  "E",  "F" , "G" , "H")], na.rm = T)


taxa<-read.csv( file = "C:\\Dropbox\\Insect Biomass Trends\\csvs/taxa 5.0.csv"); dim(taxa)
dim(gl)
unique(gl$Phylum)
unique(gl$Order)
gl <- subset(gl, Phylum == "Arthropoda"); dim(gl)
gl <- subset(gl, Order != "Ostracoda" & Order !=  "Notostraca"  & Order !=  "Copepoda") ; dim(gl)   



# dim on 3.8.21: 74826    44


# rarefaction model 
#set the minimum number of trap days 

# We use rarefaction based on the number of individuals observed. We standardize the effort to the minimum number of trap days observed per month 
# and rarefy the individuals down to the expected number of induviduals under that sampling effort per month
# we sum over 3 months


gl$Year<- as.numeric(gl$Year)
# rarefaction model: #####
allMeta<- NULL  
all.metrics<- NULL
all.densities<- NULL
all.spMeanslong<- NULL

  min_samples_per_period <- 14 # 
  beta_randomizations<- 100
  sites<- sort(unique(gl$Plot_ID))
  
  
for (h in 1: length(sites)){
    
    randomSamples<- NULL
    loc<- sites[h]
    print(loc)
    dat<- subset(gl, Plot_ID == loc)
    dim(dat)
    
    uniqueSampleEvents<- unique(dat[, c("Plot_ID", "date", "Year", "Period", "Days.A" ,"Days.B" ,"Days.C", "Days.D", "Days.E", "Days.F", "Days.G", "Days.H" , "trapDays")])
    t<-   dcast(uniqueSampleEvents, Year~ Period , value.var = "trapDays", sum)
    
    
    monthMins<-   apply(t[, -(1)], 2, min)
    yearMins<-   apply(t[, -(1)], 1, min)
    yrs<- (unique(dat$Year))
    cbind(t, SUM
          = rowSums(t[, -(1)]), yearMins)
    
    
    # remove bad months
    badMonths<- names(monthMins)[which(monthMins<min_samples_per_period )]
    dat<- dat[!dat$Period %in% badMonths , ]; dim(dat)
    
    # check for years with 1 month with less than 14 trapdays
    vector_bad_years<- (t$year)[which(yearMins<min_samples_per_period )]
    vector_bad_years
    dat<- dat[!dat$Year %in% vector_bad_years , ]; dim(dat)
    
  #  if(as.numeric(max(dat$Year)) -  as.numeric(min(dat$Year)) +1<  9){next} # skip sites with too short sampling periods (after year selection)
 
    
    uniqueSampleEvents<- unique(dat[, c("Plot_ID", "date", "Year", "Period", "Days.A" ,"Days.B" ,"Days.C", "Days.D", "Days.E", "Days.F", "Days.G", "Days.H" , "trapDays")])
    t<-   dcast(uniqueSampleEvents, Year~ Period , value.var = "trapDays", sum)
   if(ncol(t) == 2){
      monthMins = min(t[,2])
      names(monthMins)<- colnames(t)[2]
        }
    if (ncol(t) > 2){  monthMins<-   apply(t[, -(1)], 2, min)
    }
    meta<- data.frame( 
       Site = sites[h],
      MinSamplesPerMonth = min_samples_per_period, 
     yearsKept =  length(yrs)  -length(vector_bad_years),
      yearsLost = length(vector_bad_years),
      samplesPerYear =  sum(monthMins )  #, 
      #  fractionLost = round(sum(t(t[, 2:ncol(t)])- mins) /sum(t[, 2:ncol(t)]), 3)
    )
    allMeta<- rbind(allMeta, meta )
    arrange (allMeta, Site )
    
    
    
    # make empty df with all species and only 0's
    empty<- reshape2::dcast(dat, Year~uniq.sp, value.var = "totalCatch", sum) 
    empty[,]<- 0 # 
    
    arr <- array(0, dim=c(length(unique(dat$Year)),length(unique(dat$uniq.sp))+1, 100)) # new
    
	pb <- txtProgressBar(min = 0, max = 100, style = 3) # progressbar
    
for (k in 1: 100){
	#print(timestart<- Sys.time()

  #print(k)
    # get list of samples per year and period 
    randomSamples<- NULL
   
    # insert year loop
    for (i in (1: length(unique(dat$Year)))){
    yr<- sort(unique(dat$Year))[i] #; print(yr)
    datYr<- subset(dat, Year == yr)
    
     random_matrix<- NULL
    # insert month loop
    mnts<- unique(datYr$Period)
   for (j in 1:length(unique(datYr$Period))){
     mnt<- mnts[j]
    datMonth<- subset(datYr, Period == mnt)
    
    	
    sad<- dcast(datMonth, Period~uniq.sp , value.var = "totalCatch", sum)# must be replaced by validTaxon 
   # sad<- round(sad, 0) # need to round the numbers to prevent drawing 25 ind from 24.9 ind. note that this potentially loses species when aggregation was done across months. this seems a negligible problem, and the lost species should show up in the next month. 
    
    
    
daysSampled<-  t[t$Year == yr, mnts[j]]
indSampled<- sum(sad[,-(1)])
effort<-  round(indSampled * (monthMins[mnt ]/daysSampled), 0)


# subsample matrices
if (ncol(sad) == 2 ){
random_matrix1<-  data.frame(V1 = effort    ,  # draw the right number of inds from this one species ( == sampling effort in indiiduals ) 
                             Taxon = colnames(sad)[2]) } else# if only 1 species present -> sample from thsi species and add to the matrix

{ random_matrix1<-  data.frame(
              V1 = t(sample_sad_N(sad[,-(1)], effort)), 
              Taxon = colnames(sad[,-(1)]))}

random_matrix <- rbind(random_matrix, random_matrix1)    
}
    
    aggMatrix<- aggregate(list (Number =  random_matrix$V1), by = list(Taxon = random_matrix$Taxon), sum) # aggregate the three months into one year matrix 
    randomSamples<- rbind(randomSamples, (cbind(Plot_ID = loc, Year = yr, aggMatrix)  ))   # add this to the other years
    }
    

  # append this to other years 


    pvt<- dcast(randomSamples,  Year ~ Taxon , value.var = "Number", sum)
     
    
    alpha<- cbind(Plot_ID = sites[h],
               run = k, 
               calculate_alpha_metrics(pvt) )
    # beta<- cbind(Plot_ID = sites[h],
    #                      run = k,
    #                      merge(calculate_beta(pvt),
    #                      calculate_expected_beta(pvt, beta_randomizations), all.x = T ))
    metrics<- alpha #merge(alpha, beta, all.x = T, all.y = T )

    # if(all( specnumber(as.data.frame(pvt[, -(1)]))<2 )) next # if all years have only one sp, skip
    # densities<- cbind(Plot_ID = sites[h],
    #             			calculate_density(pvt, BW = 0.3) )
    # 
    # all.densities<- rbind(all.densities, densities)
     all.metrics<- rbind(all.metrics, metrics)
    

    
    missing<- setdiff(names(empty), names(pvt) ) # SPECIES not in randomized dataset
    missing<- subset(empty, select = missing) # select these from empty df
    pvt<- cbind(pvt, missing) # bind to pvt
    pvt<- pvt[, names(empty)] # alphabetical order
    arr[,,k]<- as.matrix(pvt) # add new random df to array # new
    dimnames(arr)<- list(pvt$Year, colnames(pvt), 1:100) # give names
    
    
      setTxtProgressBar(pb, k)
  }
#print(difftime(Sys.time() , timestart)))
	# get means per species per year over all runs
	spMeans <- as.data.frame(apply(arr, 1:2, mean)) # new
	# remove species with only 0's: 
	spMeans1<- spMeans[, apply(spMeans, 2, sum) != 0]; dim(spMeans1) # # new 
	
	# then melt into long df
	spMeanslong <- melt(spMeans1, id.vars = c("Year"), variable.name = "Taxon",  value.name = "Number")# new
	spMeanslong$Plot_ID <- sites[h]# new
	all.spMeanslong <- rbind(all.spMeanslong, spMeanslong)# new
	
	
	
	
	# remove species that were not caught in this plot
	arr<- arr[,apply(spMeans, 2, sum) != 0 ,]; dim(arr) # new
	# save list of random matrices
	saveRDS(arr, file = paste0("C:\\Dropbox\\Insect Biomass Trends\\greenland arthropods/ randomizations plt ", loc, ".rds") )# new

		close(pb)
}
  Sys.time()
dim(allMeta)


table(all.metrics$Unit_in_data)

all.metricsOld<- readRDS( file =     "C:/Dropbox/Insect Biomass Trends/csvs/GreenlandAllValues 20210804.rds")
missingMetrics<- setdiff(unique(all.metrics$Unit_in_data), unique(all.metricsOld$Unit_in_data))
all.metricsNew<- subset(all.metrics, Unit_in_data %in% missingMetrics)
table(all.metricsNew$Plot_ID, all.metricsNew$Unit_in_data)

library(data.table)
plots<-fread( file = "c:\\Dropbox\\Insect Biomass Trends\\csvs/PlotData 4.1.csv"); dim(plots)

all.metricsNew <- merge(all.metricsNew, plots[, c("Plot_ID", "Plot_name")])
all.metricsOld <- merge(all.metricsOld, plots[, c("Plot_ID", "Plot_name")])

all.metrics1<- merge(all.metricsNew, all.metricsOld,  all = T); dim(all.metrics1)
nrow(all.metrics1) == nrow(all.metricsNew) + nrow( all.metricsOld) # should be TRUE

tab<- table(all.metrics1$Unit_in_data, all.metrics1$Year,all.metrics1$Plot_ID)
tab[,,7] #starnge  values
 

all.metrics<- subset(all.metrics1, Year != 9999)


dim(all.metrics)
saveRDS(all.spMeanslong, file = "C:/Dropbox/Insect Biomass Trends/csvs/Greenland spMeans.rds") 
saveRDS(all.metrics, file = "C:/Dropbox/Insect Biomass Trends/csvs/GreenlandAllValues 20221122.rds")
saveRDS(all.densities, file = "C:/Dropbox/Insect Biomass Trends/csvs/GreenlandAllDensities 20210804.rds")

meanDens<-     aggregate(.~    Plot_name+   Year + densityMode + correction, data = all.densities,  FUN = "mean")
write.csv(meanDens, file = "C:\\Dropbox\\Insect Biomass Trends\\csvs/greenland meanDensities 20210731.csv")



means<- aggregate(list(Number = all.metrics$Number), by = list(Plot_ID = all.metrics$Plot_ID, Year = all.metrics$Year, Unit_in_data = all.metrics$Unit_in_data), FUN = "mean")
meanExp<- aggregate(list(Expected = all.metrics$Expected), by = list(Plot_name = all.metrics$Plot_name, Year = all.metrics$Year, Unit_in_data = all.metrics$Unit_in_data), FUN = "mean")
SDs<- aggregate(list(Error = all.metrics$Number), by = list(Plot_name = all.metrics$Plot_name, Year = all.metrics$Year, Unit_in_data = all.metrics$Unit_in_data), FUN = "sd")
meanExpSD<- aggregate(list(SDexp = all.metrics$SDexp), by = list(Plot_name = all.metrics$Plot_name, Year = all.metrics$Year, Unit_in_data = all.metrics$Unit_in_data), FUN = "mean")

means<- merge(means, SDs) ; dim(means)
means<- merge(means, meanExp, all.x = T); dim(means)
means<- merge(means, meanExpSD, all.x = T); dim(means)
arrange(means, Plot_name, Unit_in_data,  Year)

unique(means$Plot_ID)
means<-merge(means, plots)
dim(means)
unique(means$Plot_name)
unique(means$Plot_ID)


greenlandTest<- data.frame(
  Datasource_name = "greenland", 
  Plot_ID = means$Plot_ID, 
  Plot_name = means$Plot_name, 
  Sample_ID = 435, 
  Year = means$Year,
  Period = "",
  Date = "",
  Taxon = "all arthropods", 
  Sex = "", 
  Unit = means$Unit, 
  Original_number  = means$Number, 
  Transformed_number = "", 
  Number = means$Number, 
  Error = SDs$Error,
  ExpectedBeta = means$Expected, 
  SDexpectedBeta = means$SDexp

)
greenlandTest$Sample_ID[greenlandTest$Plot_ID > 1053]<- 436 # replace sample_id for pitfalls

write.csv(greenlandTest, file = "C:\\Dropbox\\Insect Biomass Trends/csvs/greenlandRarefied 20210804.csv", row.names = F)



greenlandTestOld <- read.csv( file = "C:/Dropbox/Insect Biomass Trends/csvs/greenlandRarefied 20210804.csv")        


plot(subset(greenlandTestOld, Unit == "richness")$Number, subset(means, Unit_in_data == "richness")$Number)

