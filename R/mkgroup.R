# How To Use:
# quantile로 지정: cn<-quantile(df$edu, c(.25, .5, .75))
# cn을 직접 지정하기: cn<-c(1000, 4000, 7000) ','\n')
# df$edu2<-mkgroup(df$edu, cn)

mkgroup<-function(name_dataset, select_columns, CuttingNumber) {
   
   if (base::missing(name_dataset)) {
	 cat("    df<-mkgroup(df, 8, cn)  *NOTE: df=데이터셋, 8=그룹핑해야 하는 변수는 8번째 변수, cn=Cutting Number, 변수<cn 선택 ", '\n')
	    cat("                            *NOTE: cn<-c( 4, 6 ) or cn<-quantile(df$edu, c(.25, .5, .75)) ", '\n')
		cat("    df[8]의 값이 1,2,3, ..., 7,8이고, cn<-c( 4, 6 )이면 1~3은 1로, 4,5는 2로, 6이상은 3으로 ", '\n')
		return(cat("    df<-df%>%relocate(edu, .before=edu2)") ) }
 
  c2n <- function(x_x01){   
     groups = unique(x_x01)   
     groups= sort(groups)
     tmp<-as.numeric(factor(x_x01, levels=groups))
	 return(tmp) }

  n0<-nrow(name_dataset[select_columns])

  target_variable<-rep(NA, n0)

  for(i in 1:n0){
  target_variable[i]<-name_dataset[i,select_columns]
  }


  if( class(target_variable) == 'character') {target_variable<-c2n(target_variable)}
  
  CuttingNumber <- as.vector(CuttingNumber)
  
  nx_0<-length(target_variable)
  mx_0<-length(CuttingNumber)
  
  groupIndexx_0 <- matrix( rep(NA, nx_0), ncol=1)
   
  
  for(ix_0 in 1:nx_0) {
    for(jx_0 in 1:mx_0) {
      if( is.na(target_variable[ix_0]) ) { groupIndexx_0[ix_0] <- -9999}
      else if( target_variable[ix_0] < CuttingNumber[jx_0] ) {
        groupIndexx_0[ix_0] <- jx_0
        break}
    }
    if(is.na(groupIndexx_0[ix_0])) {groupIndexx_0[ix_0]<-(mx_0+1)}
    if(groupIndexx_0[ix_0]==-9999) {groupIndexx_0[ix_0]<-NA}
  }

  tmp_select_columns<-as.data.frame(groupIndexx_0)
  name_dataset2<-cbind(name_dataset, tmp_select_columns)
  ncolX0<-ncol(name_dataset2)
  colnames(name_dataset2)[ncolX0] <- paste0( "g_", colnames(name_dataset[select_columns]) , sep='')
  
  name_dataset2<-as.data.frame(name_dataset2)

return(name_dataset2)
}


