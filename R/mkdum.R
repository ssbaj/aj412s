mkdum <- function(name_dataset, select_columns) {

if (base::missing(name_dataset)) {
cat(" \033[1;36m# Examples ---------- \033[0m", '\n' )
cat("\033[1;36m Adata<-as.data.frame(Adata) \033[0m", '\n')
return( cat("\033[1;36m COMMAND: Adata<-mkdum22(Adata, variable) \033[0m", '\n') )
}


##------------------------------------
find_col2<-function(DataSet, index_id ){
tmp_colnames<-colnames(DataSet)
n<-length(tmp_colnames)
for(i in 1:n){
  if(index_id==tmp_colnames[i]) {return(as.numeric(i))}
}
}

var_name <- deparse(substitute(select_columns))

select_columns<-find_col2(name_dataset, var_name)
##----------------------------------------

tmp<-(name_dataset[select_columns])
tmp[is.na(tmp)]<-"NA"
colnames(tmp)<-c("dum_")
tmp$dum_ <- as.factor( tmp$dum_ )

## if the codes of variable is only two types,
## directly make dummy variables by using following method.
if( length(unique(tmp$dum_))==2 ) {
cat(" SUGGESTED COMMANDS: df$gender2[df$gender==1]<-1; df$gender2[df$gender==2]<-0 ", '\n')
return(name_dataset)
break}
else {tmp_select_columns <- model.matrix(~ dum_ -1, tmp)}

## Changing variable names
colnames(tmp_select_columns) <- paste0( colnames(name_dataset[select_columns]) , colnames(tmp_select_columns) , sep='')

name_dataset2<-cbind(name_dataset, tmp_select_columns)
name_dataset2<-as.data.frame(name_dataset2)
return(name_dataset2)
}

