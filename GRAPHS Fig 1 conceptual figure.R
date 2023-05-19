# this script creates the conceptual figures in Box 1
# 
# conceptual figues 

library(ggplot2)
library(vegan)
library(tidyverse)
library(INLA)

source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/calculate metrics.R")

figure_path<- "D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/Figures/"
setwd(figure_path)

colscheme<- c("2. Abundant species decline more"="#00AFBB", "1. Proportional declines" = "#FC4E07", "3. Rare species decline more" = "#E7B800") 
# names for scenarios:
props<- "1. Proportional declines"
doms <- "2. Abundant species decline more"
rares<- "3. Rare species decline more"

theme_clean<- theme_grey() + theme(panel.grid.major = element_blank(), 
																	 panel.grid.minor = element_blank(),
																	 panel.background = element_blank(), 
																	 axis.line = element_line(colour = "black") , 
																	 legend.key=element_blank())



# make ideal SAD  
T1raw<- c(
	rep(0, 12),  # singletons in bin 1
	rep(0.25, 10), # doubletons in bin 1
	rep(0.5, 8),   # tripletons in bin 2
	rep(0.75, 6),   #6 in bin 3
	rep(1, 4),      #8 in bin 4
	rep(1.25, 2),   #18 in bin 5
	1.5) # in bin 5

T1<-round(10^T1raw)           #32
log10(T1)/max(log10(T1))
# absolute cutoff points: 
max(log10(T1))/5 # = 0.30103





length(T1)
# make this a bit more realistic
T1[41] <- 23 # replace an 18 with 23
T1[T1 == 10] <- c(9, 10, 10, 12)
T1[T1 == 6] <- c(6,6,6, 6,8, 7)
T1[T1 == 3] <- c(3,3,3,3,3,3,4,4)

hist(T1)
hist(log10(T1))




#Population changes under 3 scenarios: #####
# 1) all species decline proportionally to eachother
# 2) abundant species decline more than rare species 
# 3) rare species decline more than abundant species
# the decline rates have been chosen purely on their ability 
# to explain the changes in a graphical way. 
# we have aimed to keep the total community decline approximately equal in all 3 scenarios

#Scenario 1. 
T1*0.969 # ~ 3% decline per year

C1 = -0.0135 # log10 of 0.9693933 # del?  
C<- -0.0054 # slope in log space for the relation of slopes to starting abundance (panel d) del?

#Scenario 2
10^(0-0.018*log10(T1)) # steepest decline ~ -6%


# scenario 3
10^-0.027 +0.035*log10(T1) # steepest decline (for rare species) -6%



# proportional declines for all 
yr<- 0:10
T1df<- data.frame(start = T1, 
									Species = paste0('sp', 1:length(T1)))

df1prop<- expand.grid(Year = 0:20, 
											Species = T1df$Species)
df1prop<- merge(df1prop, T1df)
df1prop$Number <- (df1prop$start* 0.969^df1prop$Year)
df1prop$NumberRounded <- round(df1prop$Number)
df1prop$NumberwError<- df1prop$Number +  rnorm(df1prop$Number, mean=0, sd=0.5) # add a bit of error so that some species go extinct faster. is more realistic for richness etc.
df1prop$NumberwErRounded <- round(df1prop$NumberwError) # round number with error to integers
df1prop$NumberwErRounded[df1prop$NumberwErRounded<0] <-0 # insert 0 for any species that might have a negative abundance now 


ggplot(subset(df1prop, Number >=0.5), aes(x = Year, y= Number, color = Species))+
	geom_line()+
	scale_y_continuous(trans='log10')+
	ggtitle("Same declines for all species") # looks fine

ggplot(subset(df1prop), aes(x = Year, y= NumberRounded, color = Species))+
	geom_line()+
	scale_y_continuous(trans='log10')+
	ggtitle("Same declines for all species, rounded numbers") # as expected


# Scenario 2: rare species decline less or not at all ####
df1disp<- expand.grid(Year = 0:20, 
												 Species = T1df$Species						 )
df1disp<- merge(T1df,df1disp )

df1disp$Number <- df1disp$start* (10^(0-0.018*log10(df1disp$start)))^df1disp$Year
df1disp$NumberRounded <- round(df1disp$Number)
df1disp$NumberwError<- df1disp$Number +  rnorm(df1disp$Number, mean=0, sd=0.5)
df1disp$NumberwErRounded <- round(df1disp$NumberwError)
df1disp$NumberwErRounded[df1disp$NumberwErRounded<0] <-0

ggplot(df1disp, aes(x = Year, y= Number, color = Species))+
	geom_line()+
	scale_y_continuous(trans='log10')+
	ggtitle("Steeper declines for common species")


# Scenario 3: rare species decline more than common ones 
df1disp2<- expand.grid(Year = 0:20, 
											Species = T1df$Species						 )
df1disp2<- merge(T1df,df1disp2 )

df1disp2$Number <- df1disp2$start*(10^-0.027 +0.035*log10(df1disp2$start))^df1disp2$Year

df1disp2$NumberRounded <- round(df1disp2$Number)
df1disp2$NumberwError<- df1disp2$Number +  rnorm(df1disp2$Number, mean=0, sd=0.5)
df1disp2$NumberwErRounded <- round(df1disp2$NumberwError)
df1disp2$NumberwErRounded[df1disp2$NumberwErRounded<0] <-0

ggplot(subset(df1disp2, NumberRounded>=1) , aes(x = Year, y= Number, color = Species))+
	geom_line()+
	scale_y_continuous(trans='log10')+
#	ylim(0.5, 35)+
	ggtitle("Steeper declines for rare species")





# Plot A: 2 scenario's"#####
df1<- rbind(cbind(df1prop, Scenario = props), 
						cbind(df1disp, Scenario = doms), 
						cbind(df1disp2, Scenario = rares)) 
df1$Scenario<- ordered(df1$Scenario, # Reorder for clarity
											 levels = (c("Proportional declines", "Abundant species decline more", "Rare species decline more" )))



ggplot(df1, aes(x = Year, y= Number, group =Species,  color = Scenario))+
	geom_line()+
	scale_y_log10(limits = c(0.5, 35))+
	scale_color_manual(values = colscheme)+
	
	facet_wrap(.~Scenario, ncol = 3, scales = "free_y")+
	ggtitle("a) Population abundances")+
	ylab("Number of individuals")+
	theme_clean +
	theme(strip.background =element_rect(fill="white"), 
				axis.line=element_line() ,
				#axis.text.x  = element_text(angle=45, vjust=1, hjust = 1), 
				legend.position = "none") 

ggsave(filename = "Fig C1a populations.png" , path = figure_path, width = 16, height = 7,  units = "cm",dpi = 600, device = "png")
ggsave(filename = "Fig C1a populations.pdf" , path = figure_path, width = 16, height =7,  units = "cm",dpi = 300, device = "pdf")



# Plot B histograms ####

# histograms of number of species per abundance bracket only year 0 and year 20 
allHists<-  rbind(cbind(subset(df1prop, Year  %in% c(0,20) & NumberwErRounded>0), Scenario = props),
									cbind(subset(df1disp, Year  %in% c(0,20) & NumberwErRounded>0), Scenario = doms),
									cbind(subset(df1disp2, Year  %in% c(0,20) & NumberwErRounded>0), Scenario = rares))
allHists$Time<- "Year 1"
allHists$Time[allHists$Year>10 ] <- "Year 20"
allHists$Scenario<- ordered(allHists$Scenario, # Reorder for clarity
											 levels = (c("1. Proportional declines", "2. Abundant species decline more", "3. Rare species decline more" )))
allHists$Time<- ordered(allHists$Time, # Reorder for clarity
														levels = (c("Year 1", "Year 20" )))


ggplot(data=allHists,  aes(x=NumberRounded, fill = Scenario, alpha = Time, group = Year)) + 
	geom_histogram(bins = 8, position = "dodge") +
	scale_alpha_discrete(range = c(1, 0.4)) +
	scale_fill_manual(values = colscheme)+
	xlab("Abundance")+ ylab ("Number of species")+
	facet_wrap(.~Scenario, ncol = 3, scales = 'free_x')+
	ggtitle ("b) SAD")+
 	theme_clean +
	theme(strip.background =element_rect(fill="white"), 
					legend.position = "right")

ggsave(filename = "Fig C1b SAD.png" , path = figure_path, width = 16, height = 7,  units = "cm",dpi = 600, device = "png")
ggsave(filename = "Fig C1b SAD.pdf" , path = figure_path, width = 16, height =7,  units = "cm",dpi = 300, device = "pdf")


# log scale histograms
ggplot(data=allHists,  aes(x=(NumberRounded), fill = Scenario, alpha = Time, group = Year)) + 
	geom_histogram(bins = 5, position = position_dodge(width = 0.25)) +
	scale_x_log10()+
	scale_alpha_discrete(range = c(0.9, 0.5)) +
	scale_fill_manual(values = colscheme)+
	xlab("Abundance")+ ylab ("Number of species")+
	facet_wrap(.~Scenario, ncol = 3)+
	#guides(fill = F)+
	ggtitle ("b) SAD")+
	theme_clean +
	theme(strip.background =element_rect(fill="white"), 
				#	axis.line.x (),
				legend.position = "right")

ggsave(filename = "Fig C1b log SAD.png" , path = figure_path, width = 22, height = 7,  units = "cm",dpi = 600, device = "png")
ggsave(filename = "Fig C1b log SAD.pdf" , path = figure_path, width = 22, height =7,  units = "cm",dpi = 300, device = "pdf")




			 


# accumulation curves (not in paper)
library(iNEXT)
strt<- subset(df1prop, Year  ==0)
strt$NumberwErRounded <- strt$Number
allAccum<-  rbind(cbind(strt , Scenario = "Start"),
									cbind(subset(df1prop, Year  ==20 ), Scenario = props),
									cbind(subset(df1disp, Year  ==20 ), Scenario = doms),
									cbind(subset(df1disp2, Year ==20 ), Scenario = rares))
accumPiv<- as.data.frame(pivot_wider(allAccum, id_cols = Species, names_from = Scenario,   values_from = NumberwErRounded   ))
rownames(accumPiv)<- accumPiv[,1]
accumPiv<- accumPiv[,-(1)]

nex<- iNEXT(accumPiv)
colscheme2<- c( '#c0c0c0',"#FC4E07" ,  "#00AFBB" ,  "#E7B800")

nex$iNextEst$size_based 

# exclude extrapolation for plotting
	nex$iNextEst$size_based <- subset(nex$iNextEst$size_based , Method  != "Extrapolation"       )
	nex$iNextEst$size_based$Assemblage<- ordered(as.factor(nex$iNextEst$size_based$Assemblage), levels = c("Start", props, doms, rares))
	#plot(nex , col = colscheme2, se= F, ylab('Species richness'), ggtitle("Year 20"))
	colscheme2<- c(Start= '#c0c0c0', colscheme)
	
		ggiNEXT(nex,  se= F)+
			scale_color_manual(values = colscheme2)+
		ggtitle('Year 0 vs year 20')+
			ylab('Species richness')+
	theme_clean

		



# make wide format data for further processing: 
df1propwide<- as.data.frame(pivot_wider(df1prop, id_cols = 'Year', names_from = 'Species', values_from = 'NumberwErRounded'))
df1dispwide<- as.data.frame(pivot_wider(df1disp, id_cols = 'Year', names_from = 'Species', values_from = 'NumberwErRounded'))
df1disp2wide<- as.data.frame(pivot_wider(df1disp2, id_cols = 'Year', names_from = 'Species', values_from = 'NumberwErRounded'))



# calculate biodiversity metrics and SAD density ######
alphaProp<- calculate_alpha_metrics(df1propwide)
densProp<- calculate_density(df1propwide, BW = 0.15)
alphaPropWError<- calculate_alpha_metrics(as.data.frame(pivot_wider(df1prop, id_cols = 'Year', names_from = 'Species', values_from = 'NumberwErRounded')))
alphaDisp<- calculate_alpha_metrics(df1dispwide)
densDisp<- calculate_density(df1dispwide, BW = 0.15)
alphaDispWError<- calculate_alpha_metrics(as.data.frame(pivot_wider(df1disp, id_cols = 'Year', names_from = 'Species', values_from = 'NumberwErRounded')))
alphaDisp2<- calculate_alpha_metrics(df1disp2wide)
densDisp2<- calculate_density(df1disp2wide, BW = 0.15)
alphaDisp2WError<- calculate_alpha_metrics(as.data.frame(pivot_wider(df1disp2, id_cols = 'Year', names_from = 'Species', values_from = 'NumberwErRounded')))




#for easier extraction of densities # not in paper, only in response to reviewers
densPropWide<- data.frame(d = 1: 100, 
											t(subset(densProp, Year %in% c(0,20) & densityMode == "relative" & correction == TRUE  )[4:103]))
densPropWideAbs<- data.frame(d = 1: 100, 
													t(subset(densProp, Year %in% c(0,20) & densityMode == "absolute" & correction == TRUE  )[4:103]))

densDispWide<- data.frame(d = 1: 100, 
													t(subset(densDisp, Year %in% c(0,20) & densityMode == "relative" & correction == TRUE  )[4:103]))


# compare absolute and relative histograms 
densities<- data.frame(Scenario = rep(c("Proportional declines", "Dominants decline more", "Rare decline more"), each = 4200),
											 rbind(melt(subset(densProp,  correction == TRUE ), id.vars = c('densityMode', 'Year','correction')),
											 			melt(subset(densDisp,  correction == TRUE ), id.vars = c('densityMode', 'Year', 'correction')),
											 melt(subset(densDisp2,  correction == TRUE ), id.vars = c('densityMode', 'Year', 'correction')))
)
densities<- densities[order(densities$variable), ]
densities$d <- rep(seq(0,log10(max(df1prop$NumberRounded)), length.out = 100), each = 126) 

ggplot(subset(densities, densityMode == 'relative') , aes(y = value, x= d, color = as.ordered(Year)))+
	geom_line()+
	facet_wrap(.~Scenario,  ncol = 3 )+ #, scales = 'free'
	theme_clean+
	xlab("species abundance")+
	ylab("density")
	

















# Plot D: change in number of species per SAD section #####
# we select the data for the SAD interval, and run a model on each 
sad1<- subset(alphaProp,   Unit_in_data == "logNr020") # select metric to model
# run model
modSAD1<- inla( log10(Number)~ Year, 
								data = sad1 )

sad2<- subset(alphaProp,   Unit_in_data == "logNr2040" )#& Year %in% c(0,20) )
#plot(sad2$Number, main = "Equal declines, 20-40%")
modSAD2<- inla( log10(Number)~ Year, 
								data = sad2 )

sad3<- subset(alphaProp,   Unit_in_data == "logNr4060" )#& Year %in% c(0,20) )
#plot(sad3$Number, main = "Equal declines, 40-60%")
modSAD3<- inla( log10(Number)~ Year, 
								data = sad3 )

sad4<- subset(alphaProp,   Unit_in_data == "logNr6080" )#& Year %in% c(0,20) )
#plot(sad4$Number, main = "Equal declines, 60-80%")
modSAD4<- inla( log10(Number)~ Year, 
								data = sad4 )

sad5<- subset(alphaProp,   Unit_in_data == "logNr80100" )#& Year %in% c(0,20) )
#plot(sad5$Number, main = "Equal declines, 80-100%")
modSAD5<- inla( log10(Number)~ Year, 
								data = sad5 )


 # Scenario 2
 sad1d<- subset(alphaDisp,   Unit_in_data == "logNr020")# & Year %in% c(0,20) )
 #plot(sad1d$Number, main = "Disproportional declines, 0-20%")
 modSAD1d<- inla( log10(Number)~ Year, 
 								data = sad1d )
 
 sad2d<- subset(alphaDisp,   Unit_in_data == "logNr2040" )#& Year %in% c(0,20) )
 #plot(sad2d$Number, main = "Disproportional declines, 20-40%")
 modSAD2d<- inla( log10(Number)~ Year, 
 								data = sad2d )
 
 sad3d<- subset(alphaDisp,   Unit_in_data == "logNr4060" )#& Year %in% c(0,20) )
 #plot(sad3d$Number, main = "Disproportional declines, 40-60%")
 modSAD3d<- inla( log10(Number)~ Year, 
 								data = sad3d )
 
 sad4d<- subset(alphaDisp,   Unit_in_data == "logNr6080" )#& Year %in% c(0,20) )
 #plot(sad4d$Number, main = "Disproportional declines, 60-80%")
 modSAD4d<- inla( log10(Number)~ Year, 
 								data = sad4d )
 
 sad5d<- subset(alphaDisp,   Unit_in_data == "logNr80100" )#& Year %in% c(0,20) )
 #plot(sad5d$Number, main = "Disproportional declines, 80-100%")
 modSAD5d<- inla( log10(Number)~ Year, 
 								data = sad5d )
 
 
 
# scenario 3 
 sad1d2<- subset(alphaDisp2,   Unit_in_data == "logNr020")# & Year %in% c(0,20) )
 #plot(sad1d2$Number)
 modSAD1d2<- inla( log10(Number)~ Year, 
 								 data = sad1d2 )
 
 sad2d2<- subset(alphaDisp2,   Unit_in_data == "logNr2040" )#& Year %in% c(0,20) )
 modSAD2d2<- inla( log10(Number)~ Year, 
 								 data = sad2d2 )
 
 sad3d2<- subset(alphaDisp2,   Unit_in_data == "logNr4060" )#& Year %in% c(0,20) )
 modSAD3d2<- inla( log10(Number)~ Year, 
 								 data = sad3d2 )
 
 sad4d2<- subset(alphaDisp2,   Unit_in_data == "logNr6080" )#& Year %in% c(0,20) )
 modSAD4d2<- inla( log10(Number)~ Year, 
 								 data = sad4d2 )
 
 sad5d2<- subset(alphaDisp2,   Unit_in_data == "logNr80100" )#& Year %in% c(0,20) )
 plot(sad5d2$Number)
 modSAD5d2<- inla( log10(Number)~ Year, 
 								 data = sad5d2 )
 
# put all model outputs in 1 dataframe for plotting 
SADmods<- rbind(cbind(modSAD1$summary.fixed[2,], SADinterval = "0-20%", SADint = 1,  Scenario = props),
 								 cbind(modSAD2$summary.fixed[2,], SADinterval = "20-40%", SADint = 2, Scenario = props),
 								 cbind(modSAD3$summary.fixed[2,], SADinterval = "40-60%", SADint = 3, Scenario = props),
 								 cbind(modSAD4$summary.fixed[2,], SADinterval = "60-80%", SADint = 4, Scenario = props),
 								 cbind(modSAD5$summary.fixed[2,], SADinterval = "80-100%", SADint = 5,Scenario = props),
 								 cbind(modSAD1d$summary.fixed[2,], SADinterval = "0-20%",  SADint = 1, Scenario = doms),
 								 cbind(modSAD2d$summary.fixed[2,], SADinterval = "20-40%", SADint = 2, Scenario = doms),
 								 cbind(modSAD3d$summary.fixed[2,], SADinterval = "40-60%", SADint = 3, Scenario = doms),
 								 cbind(modSAD4d$summary.fixed[2,], SADinterval = "60-80%", SADint = 4, Scenario = doms),
 								 cbind(modSAD5d$summary.fixed[2,], SADinterval = "80-100%", SADint = 5,Scenario = doms),
 								 cbind(modSAD1d2$summary.fixed[2,], SADinterval = "0-20%",  SADint = 1, Scenario = rares),
 								  cbind(modSAD2d2$summary.fixed[2,], SADinterval = "20-40%", SADint = 2, Scenario = rares),
 								  cbind(modSAD3d2$summary.fixed[2,], SADinterval = "40-60%", SADint = 3, Scenario = rares),
 								  cbind(modSAD4d2$summary.fixed[2,], SADinterval = "60-80%", SADint = 4, Scenario = rares),
 								  cbind(modSAD5d2$summary.fixed[2,], SADinterval = "80-100%", SADint = 5,Scenario = rares))
 



SADmods$Scenario<- ordered(SADmods$Scenario, # Reorder for clarity
											 levels = (c("1. Proportional declines", "2. Abundant species decline more", "3. Rare species decline more" )))



ggplot(SADmods)+
	geom_hline(yintercept=0,linetype="dashed") +
	geom_errorbar(aes(x=(SADint),ymin=`0.025quant`, ymax=`0.975quant`, color = Scenario),
								linewidth = 2, width=0,  position=position_dodge(width= 0.5))+  
	# geom_line(aes(x=as.numeric(SADint), y = mean, color = Scenario),
	# 					linewidth = 1, width=0, position=position_dodge(width= 0.5))+
	geom_point(aes(x=(SADint),   y=mean, shape = Scenario),
						 size = 2, position=  position_dodge(width = 0.5),   fill = "black", color = "black")+
	ggtitle("d) Change in species number \nper SAD interval")+
	#	scale_y_continuous(breaks = brks,labels = l)+#, limits=c(-0.005,  0.01))+
	scale_color_manual(values = colscheme)+
	xlab ("Start SAD interval (on log10 scale)") + ylab("Trend slope of # species per interval")+
	theme_clean +
	theme(strip.background =element_rect(fill="white"), 
				axis.line=element_line() ,
				axis.text.x  = element_text(angle=45, vjust=1, hjust = 1), 
				legend.position = "bottom")   




ggsave(filename = "Fig C1e SAD change.png" , path = figure_path, width = 10, height = 11,  units = "cm",dpi = 600, device = "png")
ggsave(filename = "Fig C1e SAD change.pdf" , path = figure_path, width = 10, height = 11,  units = "cm",dpi = 300, device = "pdf")



brks<- c(-0.06, -0.05, -0.04, -0.03, -0.02, -0.01,   0,   0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")

# Fig 3 SAD changes 
SADmodMargs<- as.data.frame(rbind(cbind(modSAD1$marginals.fixed$Year, SADinterval = "0-20%", SADint = 1,  Scenario = props),
										cbind(modSAD2$marginals.fixed$Year, SADinterval = "20-40%", SADint = 2, Scenario = props),
										cbind(modSAD3$marginals.fixed$Year, SADinterval = "40-60%", SADint = 3, Scenario = props),
										cbind(modSAD4$marginals.fixed$Year, SADinterval = "60-80%", SADint = 4, Scenario = props),
										cbind(modSAD5$marginals.fixed$Year, SADinterval = "80-100%", SADint = 5,Scenario = props),
										cbind(modSAD1d$marginals.fixed$Year, SADinterval = "0-20%",  SADint = 1, Scenario = doms),
										cbind(modSAD2d$marginals.fixed$Year, SADinterval = "20-40%", SADint = 2, Scenario = doms),
										cbind(modSAD3d$marginals.fixed$Year, SADinterval = "40-60%", SADint = 3, Scenario = doms),
										cbind(modSAD4d$marginals.fixed$Year, SADinterval = "60-80%", SADint = 4, Scenario = doms),
										cbind(modSAD5d$marginals.fixed$Year, SADinterval = "80-100%", SADint = 5,Scenario = doms),
										cbind(modSAD1d2$marginals.fixed$Year, SADinterval = "0-20%",  SADint = 1, Scenario = rares),
										cbind(modSAD2d2$marginals.fixed$Year, SADinterval = "20-40%", SADint = 2, Scenario = rares),
										cbind(modSAD3d2$marginals.fixed$Year, SADinterval = "40-60%", SADint = 3, Scenario = rares),
										cbind(modSAD4d2$marginals.fixed$Year, SADinterval = "60-80%", SADint = 4, Scenario = rares),
										cbind(modSAD5d2$marginals.fixed$Year, SADinterval = "80-100%", SADint = 5,Scenario = rares)))

SADmodMargs$x<- as.numeric(SADmodMargs$x)
SADmodMargs$y<- as.numeric(SADmodMargs$y)

SADmodMargsNeg<- SADmodMargs
#quantilesDataNeg$x<- quantilesDataNeg$x+0.000000001
SADmodMargsNeg$y <- -SADmodMargsNeg$y
SADmodMargsNeg$Scenario[SADmodMargsNeg$Scenario == props]<- "1. Proportional declines2"
SADmodMargsNeg$Scenario[SADmodMargsNeg$Scenario == doms]<- "2. Abundant species decline more2"
SADmodMargsNeg$Scenario[SADmodMargsNeg$Scenario == rares]<- "3. Rare species decline more2"

SADmodMargs<- rbind(SADmodMargs, SADmodMargsNeg)


colschemeDouble<- colscheme
names(colschemeDouble)<- paste0(names(colschemeDouble), 2)
colschemeDouble<- c(colscheme, colschemeDouble)

ggplot(SADmodMargs, aes(x = x, y = y, group = Scenario))+
	#	geom_line( )+
	# geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8)+
	# geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6)+
	# geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3)+
	geom_area( aes(x = x, y = y, color = Scenario, fill = Scenario), position = "identity", alpha = 0.8)+
	geom_vline(xintercept = 0, linetype = 'dashed')+
	coord_flip()+
	facet_grid(cols = vars(SADinterval), switch = "x")+
	ylab ("SAD interval")+  xlab("Trend slope")+
	scale_color_manual(values = colschemeDouble)+
	scale_fill_manual(values = colschemeDouble)+
	ggtitle("d) Change in species number \nper SAD interval")+
	theme_classic()+
	theme(axis.text.x=element_blank(),
				axis.ticks.x=element_blank(), 
				legend.key=element_blank(), 
				legend.position="bottom", 
				strip.background = element_rect(colour = "white")
	)

ggsave(filename = "Fig C1e bubble SAD change.png" , path = figure_path, width = 10, height = 9,  units = "cm",dpi = 600, device = "png")
ggsave(filename = "Fig C1e bubble SAD change.pdf" , path = figure_path, width = 10, height = 12,  units = "cm",dpi = 300, device = "pdf")





# Plot E: mean population changes per commonness class #####
 
 #allocate initial abundance groups based on 1st year: 
 head(df1prop)
 yr1<- subset(df1prop, Year == 0)
 maxSAD<- log10(max(df1prop$Number))
 yr1$SADpropYr1 <- log10(yr1$Number)/maxSAD 
 
 yr1$CGYr1 <- NA
 
 yr1$CGYr1 [yr1$SADpropYr1  >= 0   & yr1$SADpropYr1 <=  0.2] <- 1
 yr1$CGYr1 [yr1$SADpropYr1  >  0.2 & yr1$SADpropYr1 <=  0.4] <- 2
 yr1$CGYr1 [yr1$SADpropYr1  >  0.4 & yr1$SADpropYr1 <=  0.6] <- 3
 yr1$CGYr1 [yr1$SADpropYr1  >  0.6 & yr1$SADpropYr1 <=  0.8] <- 4
 yr1$CGYr1 [yr1$SADpropYr1  >  0.8 & yr1$SADpropYr1 <=  1] <- 5
 yr1$CGYr1 [yr1$SADpropYr1 ==  -Inf] <- 0
 yr1$CGYr1 [yr1$SADpropYr1 ==  Inf] <- 0 # for dividing by negative number
 
 table(yr1$CGYr1) # looks believable 
 df1prop<- merge(df1prop, yr1[, c("Species", "SADpropYr1", "CGYr1")], by = "Species")
 
 # run models on each group in each scenario, and get the quantiles of the posterior 
 pop1<- subset(df1prop,   CGYr1 == 1 )
 modpop1<- inla( log10(Number)~ Year, 
 								data = pop1 , 
 								quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999) )
 
 pop2<- subset(df1prop,   CGYr1 ==2 )
 modpop2<- inla( log10(Number)~ Year, 
 								data = pop2 , 
 								quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999))
 
 pop3<- subset(df1prop,   CGYr1 ==3 )
 modpop3<- inla( log10(Number)~ Year, 
 								data = pop3 , 
 								quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999))
 pop4<- subset(df1prop,   CGYr1 ==4 )
 modpop4<- inla( log10(Number)~ Year, 
 								data = pop4 , 
 								quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999))
 pop5<- subset(df1prop,   CGYr1 ==5 )
 modpop5<- inla( log10(Number)~ Year, 
 								data = pop5 , 
 								quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999))
 
 df1disp<- merge(df1disp, yr1[, c("Species", "SADpropYr1", "CGYr1")], by = "Species")
 
 pop1d<- subset(df1disp,   CGYr1 == 1 )
 modpop1d<- inla( log10(Number)~ Year, 
 								data = pop1d , 
 								quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999))
 
 pop2d<- subset(df1disp,   CGYr1 ==2 )
 modpop2d<- inla( log10(Number)~ Year, 
 								data = pop2d , 
 								quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999))
 
 pop3d<- subset(df1disp,   CGYr1 ==3 )
 modpop3d<- inla( log10(Number)~ Year, 
 								data = pop3d , 
 								quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999))
 pop4d<- subset(df1disp,   CGYr1 ==4 )
 modpop4d<- inla( log10(Number)~ Year, 
 								data = pop4d , 
 								quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999))
 pop5d<- subset(df1disp,   CGYr1 ==5 )
 modpop5d<- inla( log10(Number)~ Year, 
 								data = pop5d , 
 								quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999))
 
 df1disp2<- merge(df1disp2, yr1[, c("Species", "SADpropYr1", "CGYr1")], by = "Species")
 
 pop1d2<- subset(df1disp2,   CGYr1 == 1 )
 modpop1d2<- inla( log10(Number)~ Year, 
 								 data = pop1d2 , 
 									quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999))
 
 pop2d2<- subset(df1disp2,   CGYr1 ==2 )
 modpop2d2<- inla( log10(Number)~ Year, 
 								 data = pop2d2 , 
 									quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999))
 
 pop3d2<- subset(df1disp2,   CGYr1 ==3 )
 modpop3d2<- inla( log10(Number)~ Year, 
 								 data = pop3d2 , 
 									quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999))
 pop4d2<- subset(df1disp2,   CGYr1 ==4 )
 modpop4d2<- inla( log10(Number)~ Year, 
 								 data = pop4d2 , 
 									quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999))
 pop5d2<- subset(df1disp2,   CGYr1 ==5 )
 modpop5d2<- inla( log10(Number)~ Year, 
 								 data = pop5d2 , 
 									quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999))
 
 
 poptrends<- rbind(cbind(modpop1$summary.fixed[2,], SADinterval = "<20%",   Scenario = props),
 								 cbind(modpop2$summary.fixed[2,], SADinterval = "20-40%",   Scenario = props),
 								 cbind(modpop3$summary.fixed[2,], SADinterval = "40-60%",   Scenario = props),
 								 cbind(modpop4$summary.fixed[2,], SADinterval = "60-80%",   Scenario = props),
 								 cbind(modpop5$summary.fixed[2,], SADinterval = "80-100%",   Scenario = props),
								cbind(modpop1d$summary.fixed[2,], SADinterval = "<20%",  Scenario = doms),
 								cbind(modpop2d$summary.fixed[2,], SADinterval = "20-40%",   Scenario = doms),
 								cbind(modpop3d$summary.fixed[2,], SADinterval = "40-60%",   Scenario = doms),
 								cbind(modpop4d$summary.fixed[2,], SADinterval = "60-80%",   Scenario = doms),
 								cbind(modpop5d$summary.fixed[2,], SADinterval = "80-100%",   Scenario = doms),
								cbind(modpop1d2$summary.fixed[2,], SADinterval = "<20%",  Scenario = rares),
 								cbind(modpop2d2$summary.fixed[2,], SADinterval = "20-40%",   Scenario = rares),
 								cbind(modpop3d2$summary.fixed[2,], SADinterval = "40-60%",   Scenario = rares),
 								cbind(modpop4d2$summary.fixed[2,], SADinterval = "60-80%",   Scenario = rares),
 								cbind(modpop5d2$summary.fixed[2,], SADinterval = "80-100%",   Scenario = rares))
 

poptrends$Scenario<- ordered(poptrends$Scenario, # Reorder for clarity
													 levels = (c(props, doms, rares )))

ggplot(poptrends)+
	geom_hline(yintercept=0,linetype="dashed") +
	geom_line(aes(x=(SADinterval), y = mean, color = Scenario, group = Scenario),
						linewidth = 1,  position=position_dodge(width= 0.5))+
	geom_errorbar(aes(x=(SADinterval),ymin=`0.025quant`, ymax=`0.975quant`, group = Scenario),
								linewidth = 0.5, width = 0, position=position_dodge(width= 0.5))+  
	 geom_errorbar(aes(x=(SADinterval),ymin=`0.05quant`, ymax=`0.95quant`, group = Scenario),
	 							linewidth = 0.8, width = 0,  position=position_dodge(width= 0.5))+  
	 geom_errorbar(aes(x=(SADinterval),ymin=`0.1quant`, ymax=`0.9quant`, group = Scenario),
	 							linewidth = 1.2, width = 0, position=position_dodge(width= 0.5))+  
	
	geom_point(aes(x=(SADinterval),   y=mean, shape = Scenario),
						 size = 2, position=  position_dodge(width = 0.5), alpha=1 ,  fill = "black", color = "black")+
	
	scale_color_manual(values = colscheme)+
	ggtitle("e) Mean population changes \n per SAD interval")+
	#	scale_y_continuous(breaks = brks,labels = l)+#, limits=c(-0.005,  0.01))+
	ylim(-0.03, 0.005)+
	xlab ("Initial abundance interval") + ylab("Population trend per interval")+
	theme_clean +
	theme(strip.background =element_rect(fill="white"), 
				axis.line=element_line() ,
				#axis.text.x  = element_text(angle=45, vjust=1, hjust = 1), 
				legend.position = "bottom")   

ggsave(filename = "Fig C1d v2 population trends.png" , path = figure_path, width = 10, height = 11,  units = "cm",dpi = 600, device = "png")
ggsave(filename = "Fig C1d v2 population trends.pdf" , path = figure_path, width = 10, height = 11,  units = "cm",dpi = 300, device = "pdf")





# Plot C: Biodiversity metrics #####
# select metric to use
ABprop<- subset(alphaProp, Unit_in_data == "abundance") 
ABdisp<- subset(alphaDisp, Unit_in_data == "abundance") 
ABdisp2<- subset(alphaDisp2, Unit_in_data == "abundance") 

# put in data frame
AB<- rbind(cbind(ABprop,  Scenario = props),  
						cbind(ABdisp,  Scenario = doms),
					 cbind(ABdisp2,  Scenario = rares))

ggplot(AB, aes(x = Year, y = Number, color = Scenario))+
	geom_point()+
	scale_color_manual(values = colscheme)+
	stat_smooth(method = 'lm')+
	ylab("Number of individuals")+
	ggtitle("a) total abundance")+
	theme_clean+
	theme(legend.position = "bottom")  
# looks ok. slopes are not exactly the same, but good enough for illustrative purposes

# run models on abundance
modabprop<- inla( log10(Number)~ Year, 
									 data = ABprop )
modabdisp<- inla( log10(Number)~ Year, 
									 data = ABdisp )
modabdisp2<- inla(log10(Number)~ Year, 
									 data = ABdisp2)

abmarg<- rbind(data.frame(Scenario = props, modabprop$marginals.fixed$Year), 
								data.frame(Scenario = doms, modabdisp$marginals.fixed$Year),
								data.frame(Scenario = rares, modabdisp2$marginals.fixed$Year))		 

ggplot(abmarg  , aes(x = x, y = y))+
	geom_area(  aes(x = x, y = y,   fill = Scenario), alpha = 0.8, stat = "identity")+
	geom_vline(xintercept = 0, linetype = 'dashed')+
	scale_fill_manual(values = colscheme )+ #col.scheme.realm
	theme_classic()+
	theme(axis.title.x=element_blank(),
				axis.ticks=element_blank(),
				axis.title.y=element_blank(),
				axis.text.x=element_blank(),
				axis.text.y=element_blank(),
				legend.position = "none")
ggsave(filename = "Fig C1c abundance.pdf" , path = figure_path, width = 3, height = 3,  units = "cm",dpi = 300, device = "pdf")








# species richness
Sprop<- subset(alphaPropWError, Unit_in_data == "richness") 
Sdisp<- subset(alphaDispWError, Unit_in_data == "richness") 
Sdisp2<- subset(alphaDisp2WError, Unit_in_data == "richness") 

modSdisp<- inla( log10(Number)~ Year, 
									 data = Sdisp )
modSprop<- inla( log10(Number)~ Year, 
								 data = Sprop )
modSdisp2<- inla( log10(Number)~ Year, 
								 data = Sdisp2 )

S<- rbind(cbind(Sprop,  Scenario = props),  
					 cbind(Sdisp,  Scenario = doms), 
					cbind(Sdisp2,  Scenario = rares))

ggplot(S, aes(x = Year, y = Number, color = Scenario))+
	geom_point()+
	stat_smooth(method = 'lm')+
	ylab("Number of species")+
	ggtitle("b) Species richness")+
	scale_color_manual(values = colscheme)+
	theme_clean+
	theme(legend.position = "bottom") 
# as expected

Smarg<- rbind(data.frame(Scenario = props, modSprop$marginals.fixed$Year), 
							data.frame(Scenario = doms, modSdisp$marginals.fixed$Year),
							data.frame(Scenario = rares, modSdisp2$marginals.fixed$Year))		 

ggplot(Smarg  , aes(x = x, y = y))+
	geom_area(  aes(x = x, y = y,   fill = Scenario), alpha = 0.8, stat = "identity")+
	geom_vline(xintercept = 0, linetype = 'dashed')+
	scale_fill_manual(values = colscheme )+ #col.scheme.realm
	theme_classic()+
	theme(axis.title.x=element_blank(),
				axis.ticks=element_blank(),
				axis.title.y=element_blank(),
				axis.text.x=element_blank(),
				axis.text.y=element_blank(),
				legend.position = "none")
ggsave(filename = "Fig C1c richness.pdf" , path = figure_path, width = 3, height = 3,  units = "cm",dpi = 300, device = "pdf")






# 1/simpson
PIEprop<- subset(alphaProp, Unit_in_data == "ENSPIE") 
modpieprop<- inla( log10(Number)~ Year, 
								 data = PIEprop )
modpieprop$summary.fixed

PIEdisp<- subset(alphaDisp, Unit_in_data == "ENSPIE") 
modpiedisp<- inla( log10(Number)~ Year, 
									 data = PIEdisp )
modpiedisp$summary.fixed

PIEdisp2<- subset(alphaDisp2, Unit_in_data == "ENSPIE") 
modpiedisp2<- inla( log10(Number)~ Year, 
									 data = PIEdisp2 )


PIE<- rbind(cbind(PIEprop,  Scenario = props),  
					  cbind(PIEdisp,  Scenario = doms), 
						cbind(PIEdisp2,  Scenario = rares))

ggplot(PIE, aes(x = Year, y = Number, color = Scenario))+
	geom_point()+
	stat_smooth(method = 'lm')+
	ylab("Effective number of species\n of PIE")+
	ggtitle("c) Diversity")+
	scale_color_manual(values = colscheme)+
	theme_clean+
	theme(legend.position = "bottom") 

PIEmarg<- rbind(data.frame(Scenario = props, modpieprop$marginals.fixed$Year), 
							data.frame(Scenario = doms, modpiedisp$marginals.fixed$Year),
							data.frame(Scenario = rares, modpiedisp2$marginals.fixed$Year))		 

ggplot(PIEmarg  , aes(x = x, y = y))+
	geom_area(  aes(x = x, y = y,   fill = Scenario), alpha = 0.8, stat = "identity")+
	geom_vline(xintercept = 0, linetype = 'dashed')+
	scale_fill_manual(values = colscheme )+ #col.scheme.realm
			theme_classic()+
	theme(axis.title.x=element_blank(),
				axis.ticks=element_blank(),
				axis.title.y=element_blank(),
				axis.text.x=element_blank(),
				axis.text.y=element_blank(),
				legend.position = "none")
ggsave(filename = "Fig C1c pie.pdf" , path = figure_path, width = 3, height = 3,  units = "cm",dpi = 300, device = "pdf")


# Rarefied richness (not in paper)
RRprop<- subset(alphaPropWError, Unit_in_data == "rarefiedRichness") 
modRRprop<- inla( log10(Number)~ Year, 
									 data = RRprop )
modRRprop$summary.fixed


RRdisp<- subset(alphaDispWError, Unit_in_data == "rarefiedRichness") 
modRRdisp<- inla( log10(Number)~ Year, 
									 data = RRdisp )
modRRdisp$summary.fixed

RRdisp2<- subset(alphaDisp2WError, Unit_in_data == "rarefiedRichness") 


RR<- rbind(cbind(RRprop,  Scenario = props),  
						cbind(RRdisp,  Scenario = doms), 
					 cbind(RRdisp2,  Scenario = rares))

ggplot(RR, aes(x = Year, y = Number, color = Scenario))+
	geom_point()+
	stat_smooth(method = 'lm')+
	ylab("Rarefied number of species")+
	ggtitle("d) Rarefied richness")+
	scale_color_manual(values = colscheme)+
	theme_clean+
	theme(legend.position = "bottom") 

# Berger parker dominance (not in paper)
DOMprop<- subset(alphaProp, Unit_in_data == "dominanceRel") 
modDOMprop<- inla( log10(Number)~ Year, 
									data = DOMprop )
modRRprop$summary.fixed

DOMdisp<- subset(alphaDisp, Unit_in_data == "dominanceRel") 
plot(DOMdisp$Number~ DOMdisp$Year)
modDOMdisp<- inla( log10(Number)~ Year, 
									data = DOMdisp )
modDOMdisp$summary.fixed

DOMrare<- subset(alphaDisp2, Unit_in_data == "dominanceRel") 
plot(DOMrare$Number~ DOMrare$Year)
modDOMrare<- inla( log10(Number)~ Year, 
									 data = DOMrare )


DOM<- rbind(cbind(DOMprop,  Scenario = props),  
					 cbind(DOMdisp,  Scenario = doms), 
						cbind(DOMrare, Scenario = rares))

ggplot(DOM, aes(x = Year, y = Number, color = Scenario))+
	geom_point()+
	stat_smooth(method = 'lm')+
	ylab("Proportional abundance of most abundant species")+
	ggtitle("Relative dominance")+
	scale_color_manual(values = colscheme)+
	theme_clean+
	theme(legend.position = "bottom") 





# various evenness metrics

Pprop<- subset(alphaPropWError, Unit_in_data == "Pielou") 
Pdisp<- subset(alphaDispWError, Unit_in_data == "Pielou") 
Pdisp2<- subset(alphaDisp2WError, Unit_in_data == "Pielou") 

Evarprop<- subset(alphaPropWError, Unit_in_data == "Evar") 
Evardisp<- subset(alphaDispWError, Unit_in_data == "Evar") 
Evardisp2<- subset(alphaDisp2WError, Unit_in_data == "Evar") 

# simpson evenness here means ENS simpson / S (= Hill2/ Hill0)
SimEvenprop <- data.frame(Year = 0:20, Unit_in_data = "Simpson evenness", 
													 Number =  (subset(alphaPropWError, Unit_in_data == "ENSPIE")$Number) / Sprop$Number)
SimEvendisp <- data.frame(Year = 0:20, Unit_in_data = "Simpson evenness", 
													 Number =  (subset(alphaDispWError, Unit_in_data == "ENSPIE")$Number) / Sdisp$Number)
SimEvendisp2 <- data.frame(Year = 0:20, Unit_in_data = "Simpson evenness", 
													 Number =  (subset(alphaDisp2WError, Unit_in_data == "ENSPIE")$Number) / Sdisp2$Number)

evenness<- rbind(cbind(Pprop,  Scenario = props, Metric = "Pielou"),  
								 cbind(Pdisp,  Scenario = doms, Metric = "Pielou"), 
								 cbind(Pdisp2,  Scenario = rares, Metric = "Pielou"), 
								 cbind(Evarprop,  Scenario = props, Metric = "Evar"), 
								 cbind(Evardisp,  Scenario = doms, Metric = "Evar"), 
								 cbind(Evardisp2,  Scenario = rares, Metric = "Evar"), 
								 cbind(SimEvenprop,  Scenario = props, Metric = "Simpson evenness"), 
								 cbind(SimEvendisp,  Scenario = doms, Metric = "Simpson evenness"),
								 cbind(SimEvendisp2,  Scenario = rares, Metric = "Simpson evenness"))

# compare evennessmetrics 
ggplot(evenness, aes(x = Year, y = Number, color = Scenario))+
	geom_point()+
	scale_color_manual(values = colscheme)+
	stat_smooth(method = 'lm')+
	ylab("Evenness")+
	ggtitle("f) Evenness")+
	facet_wrap(.~Metric, ncol = 3, scales = "free_y")+
	theme_clean+
	theme(strip.background =element_rect(fill="white"), 
				axis.line=element_line() ,
				legend.position = "right")   
# all show the same pattern. we use Simpson evenness

modeveprop<- inla( (Number)~ Year, 
									 data = SimEvenprop , 
									 family  = 'beta')
modevedisp<- inla( (Number)~ Year, 
									 data = SimEvendisp , 
									 family  = 'beta')
modevedisp2<- inla(Number~ Year, 
									 data = SimEvendisp2, 
									 family  = 'beta')

evemarg<- rbind(data.frame(Scenario = props, modeveprop$marginals.fixed$Year), 
								data.frame(Scenario = doms, modevedisp$marginals.fixed$Year),
								data.frame(Scenario = rares, modevedisp2$marginals.fixed$Year))		 

ggplot(evemarg  , aes(x = x, y = y))+
	geom_area(  aes(x = x, y = y,   fill = Scenario), alpha = 0.8, stat = "identity")+
	geom_vline(xintercept = 0, linetype = 'dashed')+
	scale_fill_manual(values = colscheme )+ #col.scheme.realm
	theme_classic()+
	theme(axis.title.x=element_blank(),
				axis.ticks=element_blank(),
				axis.title.y=element_blank(),
				axis.text.x=element_blank(),
				axis.text.y=element_blank(),
				legend.position = "none")
ggsave(filename = "Fig C1c evenness.pdf" , path = figure_path, width = 3, height = 3,  units = "cm",dpi = 300, device = "pdf")



# Plot C final biodiversity metrics figure ####

allMetrics<- rbind(
	cbind(ABprop,  Scenario = props),  
	cbind(ABdisp,  Scenario = doms),
	cbind(ABdisp2,  Scenario = rares),
cbind(Sprop,  Scenario = props),  
cbind(Sdisp,  Scenario = doms), 
cbind(Sdisp2,  Scenario = rares),
	cbind(PIEprop,  Scenario = props),  
	cbind(PIEdisp,  Scenario = doms), 
	cbind(PIEdisp2,  Scenario = rares),
cbind(SimEvenprop,  Scenario = props), 
cbind(SimEvendisp,  Scenario = doms),
cbind(SimEvendisp2,  Scenario = rares))

labs<- c(abundance = "Number of individuals" , richness = "Number of species" ,  ENSPIE = "Diversity \n(ENS-PIE)", 'Shannon evenness' = "Evenness \n(ENS Shannon / richness)")


ggplot(allMetrics, aes(x = Year, y = Number, color = Scenario))+
	geom_point()+
	scale_color_manual(values = colscheme)+
	stat_smooth(method = 'lm')+
	ylab("")+
	ggtitle("c) biodiversity metrics")+
	facet_wrap(.~Unit_in_data, ncol = 4, scales = "free", labeller = labeller(Unit_in_data = labs))+
	theme_clean+
	theme(strip.background =element_rect(fill="white"), 
				axis.line=element_line() ,
				legend.position = "bottom")   

ggsave(filename = "Fig C1c biodiversity metrics.png" , path = figure_path, width = 18, height = 7,  units = "cm",dpi = 600, device = "png")
ggsave(filename = "Fig C1c biodiversity metrics.pdf" , path = figure_path, width = 18, height =7,  units = "cm",dpi = 300, device = "pdf")











