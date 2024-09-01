# How To Use: df$index<-mkindex(df) 

mkindex<-function(tmp_data){

if (base::missing(tmp_data)) {
    cat("  df<-as.data.frame(df) ", '\n')
    return(cat("  df<-mkindex(df)") ) }


df_variabl_names<-colnames(tmp_data)
n_variables<-length(df_variabl_names)
temp_count<-0

for(i in 1:n_variables){
  if(df_variabl_names[i] == c('index')) {temp_count<-1}
}

n<-nrow(tmp_data)
index=c()

for(i in 1:n){
  index=c(index, i)
}


if(temp_count==0) { tmp_data<-cbind(tmp_data, index) }

if(temp_count==1) { 
  index00<-index
  tmp_data<-cbind(tmp_data, index00) 
  }

suppressPackageStartupMessages(library("dplyr"))
tmp_data <- tmp_data %>% relocate(index)
colnames(tmp_data)[1]<-"INDEX"

return(tmp_data)

}
