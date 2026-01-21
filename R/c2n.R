# Converting character into numeric

c2n <- function(x){   
     
	 if (base::missing(x)) {
	 return(cat("  df$brandV2<-c2n(df$brand)"))}
	 
	 groups = unique(x)   
     groups= sort(groups)
     tmp<-as.numeric(factor(x, levels=groups))
	 
	 return(tmp)
}
