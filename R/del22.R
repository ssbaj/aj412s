del22 <- function(dataset_name, ...) {
 
if (base::missing(dataset_name)) {
cat(" \033[1;36m# Examples ---------- \033[0m", '\n' )
return( cat(" \033[1;36mdf <- del22(Adata, 지우려는 변수1, 지우려는 변수2) \033[0m", '\n' ) ) }

r=c()

find_col2<-function(DataSet, index_id ){
tmp_colnames<-colnames(DataSet)
n<-length(tmp_colnames)
for(i in 1:n){
  if(index_id==tmp_colnames[i]) {return(as.numeric(i))}
}
}


dataset_name <- deparse(substitute(dataset_name))
dataset <- get(dataset_name)

var_names <- as.character(substitute(list(...)))[-1] # 첫 번째 항목은 "list"

n<-length(var_names)

for(i in 1:n){
r<- rbind(r,  find_col2(dataset, var_names[i]) )
}

clean_data <- dataset[, -r]

return(clean_data)
}

