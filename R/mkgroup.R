# How To Use:
# quantile로 지정: cn<-quantile(df$edu, c(.25, .5, .75))
# cn을 직접 지정하기: cn<-c(1000, 4000, 7000) ','\n')
# df$edu2<-mkgroup(df$edu, 8th column, cn)

mkgroup<-function(name_dataset, select_columns, CuttingNumber, sign=1) {
   
   if (base::missing(name_dataset)) {
	    cat(" df<-mkgroup(df, 8, cn)   ", '\n')
	    cat("             data, 8th variable, cn=Cutting Number, Drop NUMBER --->  <  ", '\n')
	    cat(" * ---------------------------------------------------------------------- *", '\n')
		cat(" df<-mkgroup(df, 8, cn, 2)  ", '\n')
		cat("             data, 8th variable, cn=Cutting Number, sign=2 --->  <=  ", '\n')
		return(cat("      ") ) }
 
  c2n <- function(x_x01){ 
     groups = unique(x_x01)   
     groups= sort(groups)
     tmp<-as.numeric(factor(x_x01, levels=groups))
	 return(tmp) }

  n0<-nrow(name_dataset[select_columns])

  target_variable<-rep(NA, n0)

  for(i in 1:n0){
  target_variable[i]<-name_dataset[i,select_columns]
  if(is.na(name_dataset[i,select_columns])) {cat(' ** Remove NAs ----','\n')}
  }


  if( class(target_variable) == 'character') {target_variable<-c2n(target_variable)}
  
  CuttingNumber <- as.vector(CuttingNumber)
  
  nx_0<-length(target_variable)
  mx_0<-length(CuttingNumber)
    
  groupIndexx_0 <- matrix( rep(NA, nx_0), ncol=1)


## <
if(sign ==1 ) {
  for(ix_0 in 1:nx_0) {

    for(jx_0 in 1:mx_0 ) {
      countx<-0
      if( target_variable[ix_0] >= CuttingNumber[mx_0]) {
		  groupIndexx_0[ix_0] <- (mx_0+1) 
		  countx<-countx+1
		  break}
      if( target_variable[ix_0] < CuttingNumber[jx_0]) {
          groupIndexx_0[ix_0] <- jx_0
		  countx<-countx+1
		  break}
	if (countx==1) {break}
	}
	}
}


   
## <=
if(sign != 1) {
  for(ix_0 in 1:nx_0) {

    for(jx_0 in 1:mx_0 ) {
      countx<-0
      if( target_variable[ix_0] > CuttingNumber[mx_0]) {
		  groupIndexx_0[ix_0] <- (mx_0+1) 
		  countx<-countx+1
		  break}
      if( target_variable[ix_0] <= CuttingNumber[jx_0]) {
          groupIndexx_0[ix_0] <- jx_0
		  countx<-countx+1
		  break}
	if (countx==1) {break}
	}
	}
  }


  
  tmp_select_columns<-as.data.frame(groupIndexx_0)
  name_dataset2<-cbind(name_dataset, tmp_select_columns)
  ncolX0<-ncol(name_dataset2)
  colnames(name_dataset2)[ncolX0] <- paste0( "g_", colnames(name_dataset[select_columns]) , sep='')
  
  name_dataset2<-as.data.frame(name_dataset2)

return(name_dataset2)
}

