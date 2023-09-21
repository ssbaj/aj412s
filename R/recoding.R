recoding<-function(name_dataset,  select_columns ) {

if (base::missing(name_dataset)) {
        cat("  df<-as.data.frame(df)", '\n')
        return( cat("  df<-recoding(df, 2)  *NOTE: Recoding values in the 2nd column of dataset ", '\n') )
    }

name_dataset<-as.data.frame(name_dataset)
tmpx<-name_dataset[ , select_columns]
k<-ncol(name_dataset)
n<-nrow(name_dataset)
origindata=c()
for(i in 1:n){
  origindata=c(origindata, tmpx[i])
}

tmp_unique<-sort(unique(origindata))
tmp_unique_count<-length(tmp_unique)
origindata2<-rep(NA, n)
cat( 'The variable is defined as :', class(origindata)  , '\n')
cat("values in the '", colnames(name_dataset)[select_columns], "' is ",'\n')
cat('----------------------', '\n')
print(tmp_unique)
cat('----------------------', '\n')
cat(' ', '\n')

if( class(origindata)=="character") {
  for(j_count in 1:tmp_unique_count){
    cat(tmp_unique[j_count])
    input_number<-readline("Change it to :")
    origindata2[origindata==tmp_unique[j_count]]<-input_number
  }  }


if( class(origindata)=="numeric" | class(origindata)=="integer" ) {
  for(j_count in 1:tmp_unique_count){
    cat(tmp_unique[j_count])
    input_number<-readline("Change it to :")
    origindata2[origindata==tmp_unique[j_count]]<-input_number
  }  }

  name_dataset2<-cbind(name_dataset, origindata2)
  colnames(name_dataset2)[k+1] <- paste0(colnames(name_dataset[select_columns]), "2" , sep='')
  return(name_dataset2)
}

