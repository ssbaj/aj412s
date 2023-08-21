# How To Use: df$index<-mkindex(df) 

mkindex<-function(tmp_data){

if (base::missing(tmp_data)) {
    cat("  df<-as.data.frame(df) ", '\n')
    return(cat("  df<-mkindex(df)") ) }

n<-nrow(tmp_data)
index00=c()

for(i in 1:n){
index00=c(index00, i)
}

tmp_data<-cbind(tmp_data, index00)

return(tmp_data)

}

