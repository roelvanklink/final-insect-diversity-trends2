# Script by Thore Engel 

#Effort rarefaction
#  1.	What was the real effort and what is the target_effort?
#  2.	What is the observed sample size N (i.e. number of individuals)?
#  3.	Calculate standardized sample size (i.e. individuals per unit effort) as N_stand=N/effort
#  4.	Calculate the expected sample size for the target effort as N_target= N_stand*target_effort
#  5.	Resample without replacement N_target individuals from the observed species abundance distribution
#  6.	Calculate the desired biodiversity metric (e.g. species richness S) from the resampled species abundance distribution
#  7.	Repeat steps 5 and 6 many times and average resulting metrics across iterations.
  

sample_sad_N<-function(x,N, replace=F){
	sample_fun<-function(x,N, replace){
		index=1:length(x)
		y=rep(index,x)
		samp<-data.frame(Species=sample(y, size = N, replace = replace)) %>%
			table() %>%
			as.data.frame()
		# if no individuals were sampled
		if(N ==0){
			samp<- data.frame(Species =index, Freq = 0)
		}
		
		samp$Species<- as.numeric(as.character(samp$Species))
		# add species that were not sampled
		missing=data.frame(Species=setdiff(index, samp$Species))
		# samp=samp %>% full_join(missing, by = "Species") %>% arrange(Species) %>% pull(var = n)
		
		samp <- merge(missing, samp,    all = T) %>% # merge in all species with 0 counts
			arrange(Species)%>% pull(var = Freq) # sort by sp name (should not be necessary), then pull out frequencies
		
		samp[is.na(samp)]<-0
		return(samp)
	}
	
	# if(any(rowSums(x)==
	if(is.data.frame(x)){x<- as.matrix(x)} # if input is a sxs matrix 
	
	if(is.vector(x)){  # if input is only 1 SAD
		names= list(1,   names(x))
		x<-matrix(x, byrow = T, nrow = 1)
	} else{
		names<- dimnames(x)
	}
	# if(any(rowSums(x)==0)) stop("Remove sites without individuals!")
	out<-apply(x,1,sample_fun, replace = replace, N= N)
	out<-t(out)
	#  if(dim(out)[1]==1){
	#    out= out[1,,drop=T]
	#    names(out)= names[[2]]
	#  }else{
	dimnames(out)<-names
	#  }
	return(out)
}



#This is the function that implements the algorithm:
rarefy_effort <- function(m,
                          effort,
                          target_effort,
                          draws = 500) {
  if(is.data.frame(m)){
    name<-colnames(m)
    m = as.numeric(m[1,, drop=T]) 
    names(m) = name
    
  }
  
  
  if (anyNA(m)) {
    warning("Community matrix m contains missing values. Replacing NA's by 0's.")
    m[is.na(m)] <- 0
  }
  N <- sum(m)
  N_stand <- N / effort
  target_N <- N_stand * target_effort
  
  d <- sapply(1:draws, function(x) {
    a <-sample_sad_N(x = m,N = target_N,replace = F)
    return(a)
  })
  d=as.data.frame(t(d))
  return(d)
  
  
}
