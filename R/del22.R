del22 <- function(dataset_name, ...) {

if (base::missing(dataset_name)) {
cat(" \033[1;31m# Original 명령문 ---------- \033[0m", '\n' )
cat(" \033[1;32m  new_df <- df[ , -c(1, 3:5, 6)] \033[0m", '\n' )
cat(" \033[1;32m   \033[0m", '\n' )
cat(" \033[1;32m  To delete variables(var1 and var2) from data set, use following command  \033[0m", '\n' )
cat(" \033[1;31m# del22 명령문 ---------- \033[0m", '\n' )
cat(" \033[1;32m  new_df <- del22(df, var1, var2)  \033[0m", '\n' )
return( cat("    ", '\n'))  }

dataset_name<-as.data.frame(dataset_name)
  
  if (!require(dplyr)) {
    cat('Automatically Installing dplyr package because','\n')
    cat('dplyr is necessary for this function','\n')
    cat('If an error occurs, connect to the network','\n')
    install.packages("dplyr")
  }
  
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
  
  r_var_index_number <- r  ## 함수 명령문에서 ... 변수들variables에 해당되는 변수들 인덱스
  
  ##---------------------------------
  
clean_data <- dataset_name[ ,-c(r_var_index_number)]

return(clean_data)
}

