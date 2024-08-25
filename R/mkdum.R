mkdum <- function(name_dataset, select_columns) {

if (base::missing(name_dataset)) {
    cat("  Adata<-as.data.frame(Adata)", '\n')
	cat("  명령문1: Adata<-mkdum(Adata, '변수명') ", '\n')
	return( cat("  또는, 명령문2: Adata<-mkdum(Adata, 2)  *NOTE: making dummies for brand(2nd column of dataset) ", '\n') )
}

##-----------------------------------
# 변수명을 컬럼 번호로 변경시키는 함수
##------------------------------------
find_col2<-function(DataSet, index_id ){
  tmp_colnames<-colnames(DataSet)
  n<-length(tmp_colnames)  # DataSet의 총변수 갯수
  
  for(i in 1:n){
    if(index_id==tmp_colnames[i]) {return(as.numeric(i))}
  }
}

##----------------------------------------
# find_col2()를 사용해 컬럼번호 찾기
if(is.numeric(select_columns)==F) {select_columns<-find_col2(name_dataset, select_columns) }
##----------------------------------------

tmp<-(name_dataset[select_columns])
tmp[is.na(tmp)]<-"NA"
colnames(tmp)<-c("dum_")
tmp$dum_ <- as.factor( tmp$dum_ )
 
## if the codes of variable is only two types,
## directly make dummy variables by using following method.
if( length(unique(tmp$dum_))==2 ) {
    cat("  SUGGESTED COMMANDS: df$gender2[df$gender==1]<-1; df$gender2[df$gender==2]<-0 ", '\n')
    return(name_dataset)
    break}
  else {tmp_select_columns <- model.matrix(~ dum_ -1, tmp)}
 
  ## Changing variable names
  colnames(tmp_select_columns) <- paste0( colnames(name_dataset[select_columns]) , colnames(tmp_select_columns) , sep='')
 
  name_dataset2<-cbind(name_dataset, tmp_select_columns)
  name_dataset2<-as.data.frame(name_dataset2)
  return(name_dataset2)
}

