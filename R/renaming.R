renaming<-function(name_dataset ) {

if (base::missing(name_dataset)) {
        return( cat("  Adata<-renaming(Adata)", '\n') )
    }

name_dataset<-as.data.frame(name_dataset)
k<-ncol(name_dataset)
origin_col=colnames(name_dataset)
origin_col2=colnames(name_dataset)
cat('\033[1;34m--------------------------------------------', '\n')
cat("\033[1;34m Current variable names: ", origin_col, '\n')
cat('\033[1;34m--------------------------------------------', '\n')

for(i in 1:k){
    cat(origin_col[i])
    input_colname<-readline("Change it to : ")
    origin_col2[i]<-input_colname
}


  colnames(name_dataset)<-origin_col2
  cat('\033[1;34m--------------------------------------------', '\n')
  cat("\033[1;34m New variable names: ", colnames(name_dataset), '\n')
  cat(" ",  '\n')
  return(name_dataset)
}