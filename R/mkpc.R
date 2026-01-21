mkpc <- function(dataset_name, ...) {
 #-------------------------
 if (base::missing(dataset_name)) {
  cat(" To make % value, multiply the result by 100. ", '\n')
  return(cat(" df<-mkpc(df, var1, var2, ... ) ")) }

 if (!require(dplyr)) {
  cat('Automatically Installing dplyr package because','\n')
  cat('dplyr is necessary for this function','\n')
  cat('If an error occurs, connect to the network','\n')
  install.packages("dplyr")

 }

 dataset_name<-as.data.frame(dataset_name)
 suppressPackageStartupMessages(library("dplyr"))

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

 r_var_index_number <- r ## 함수 명령문에서 ... 변수들variables에 해당되는 변수들 인덱스

 ##---------------------------------
 # -- differencing and making percent change variables --
 dataset_name<-as.data.frame(dataset_name)
 tmp_dataset <- dataset_name[ ,c(r_var_index_number)]
 diff_dataset <- tmp_dataset - dplyr::lag(tmp_dataset)
 pc_dataset <- (tmp_dataset - dplyr::lag(tmp_dataset) ) /dplyr::lag(tmp_dataset)
 all_dataset<-data.frame(dataset_name, diff_dataset, pc_dataset)
 dataset_counter<-length(names(dataset_name))
 diff_counter<-length(names(diff_dataset))
 pc_counter<-length(names(pc_dataset))
 all_counter <- dataset_counter + diff_counter

 ## Changing variable names -----------
 ## for(i in r_var_index_number){
 ##  dataset_counter <- dataset_counter + 1
 ##  colnames(all_dataset)[dataset_counter] <- paste0( "diff_" , colnames(dataset_name)[i] , sep='')
 ## }

 ##  for(i in r_var_index_number){
 ##  all_counter <- all_counter + 2
 ##  colnames(all_dataset)[all_counter] <- paste( "pc_" , colnames(dataset_name)[i] , sep='')
 ## }

#-------------------------------------
   return(all_dataset)

  }    