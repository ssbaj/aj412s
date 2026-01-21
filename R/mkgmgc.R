mkgmgc <- function(data, ...) {

data<-as.data.frame(data)

if (base::missing(data)) {
cat(" \033[1;36m# Examples ---------- \033[0m", '\n' )
cat(" \033[1;36m# Making grand mean, group centered value \033[0m", '\n' )
return( cat("\033[1;36m COMMAND: newdata<-mkgmgc(data, id, target_var) \033[0m", '\n') )
}

  if (!require(dplyr)) {
    cat('Automatically Installing dplyr package because','\n')
    cat('dplyr is necessary for this function','\n')
    cat('If an error occurs, connect to the network','\n')
    install.packages("dplyr")
  }
  
  suppressPackageStartupMessages(library("dplyr"))
  
## -------------------------------------

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
  t_counter<-find_col2(data, var_names[i])
  r=c(r, t_counter)
}

r_var_index_number <- r


##---------------------------------
  id_var <- data[, r_var_index_number[1] ]
  target_var <- data[, r_var_index_number[2] ]
  tmp_data<-data.frame(id_var, target_var)
  
  tmp_data <- tmp_data %>%
   group_by(id_var) %>%
    mutate(
      gm = mean(target_var, na.rm = TRUE), # 그룹 평균
      gc = target_var - gm # 그룹 중심화된 값
    ) %>%
    ungroup()
  
  colnames(tmp_data)[3] <- paste0( colnames(tmp_data)[3] , '_', colnames(data)[r_var_index_number[2]] , sep='')
  colnames(tmp_data)[4] <- paste0( colnames(tmp_data)[4] , '_', colnames(data)[r_var_index_number[2]] , sep='')

  # 그룹별 평균 계산
  data<-data.frame(data, tmp_data[,3], tmp_data[,4])
  return(data)
}

