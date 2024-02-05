cor33 <- function(x, y, z=NULL, selecting_z=0){
 
 if (base::missing(x)) {
	    return(cat("  cor33(df$conv, df$satprice, df$gender, 1) "))  }
 
 temp<-cbind(x,y,z)
 temp<-as.data.frame(temp)
 cat(' Number of original data:', nrow(temp), '\n')
 
 if(selecting_z==0) { tempx<-temp[complete.cases(temp), ] }
  else{ tempx<-temp[temp$z == selecting_z, ]
        tempx<-tempx[complete.cases(tempx), ]}
 
 tempx<-tempx[tempx$x != Inf, ]
 tempx<-tempx[tempx$x != -Inf, ]
 tempx<-tempx[tempx$y != Inf, ]
 tempx<-tempx[tempx$y != -Inf, ]
 tempx<-tempx[tempx$y != '', ]
 cat(' Number of data after refining:', nrow(tempx), '\n')
 answer <- cor(tempx$x, tempx$y)
 return(answer) }
