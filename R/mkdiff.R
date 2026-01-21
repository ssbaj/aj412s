mkdiff <- function(dataset_name , ...) {

# 함수명은 5글자 이상을 사용할 것을 추천함

# --------------------------------
  if (base::missing(dataset_name)) {
	  cat("Data set is shoul be data.frame type. After making it as data.frame format, run the command.", '\n')
	  return( cat("  Adata<-mkdiff(Adata, variable name)  *NOTE: making differenced variable, the 2nd column of dataset ", '\n') )
	}

# find_col2함수 ----------------
find_col2<-function(dataset_name, index_id ){
  tmp_colnames<-names(dataset_name)
  n<-length(tmp_colnames) # DataSet의 총변수 갯수
    for(i in 1:n){
        if(index_id==tmp_colnames[i]) {return(as.numeric(i))}
    }
  }

##--------------------------------
# 변수명을 읽는 함수
var_names <- as.character(substitute(list(...)))[-1]
no_var_names<-length(var_names)

r=c()

for(i in 1:no_var_names){
  t_counter<-find_col2(dataset_name, var_names[i])
  r=c(r, t_counter)
}

r_var_index_number <- r

##---------------------------------

tmp<-(dataset_name[r_var_index_number])
tmp<-as.data.frame(tmp)
tmp2<- tmp - dplyr::lag(tmp)
    
  ## Changing variable names
  colnames(tmp2) <- paste0( "d_" , colnames(tmp) , sep='')
  
  name_dataset2<-cbind(dataset_name, tmp2)
  name_dataset2<-as.data.frame(name_dataset2)
  return(name_dataset2)
}
