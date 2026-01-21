geodistance22<-function(CENTER, TARGET){
  if (base::missing(CENTER)) {
    cat(" \033[1;34m# library(aj412s2); library(geosphere); library(dplyr) ", '\n')
    cat(" \033[1;34m# 위도(lat)와 경도(long)로 CENTER와 TARGET의 거리 측정 --- ", '\n')
    cat(" \033[1;34m# CENTER 데이터셋과 TARGET 데이터셋에 각각 lat와 long 변수가 있어야 함 \033[0m ", '\n')
    return(cat(" \033[1;34m# geodistance22(CENTER, TARGET) \033[0m ") ) }
  
  
if (!requireNamespace("geosphere", quietly = TRUE)) {
  install.packages("geosphere")
  }

library(aj412s2); library(geosphere); library(dplyr)

project_i <- nrow(CENTER)
project_j <- nrow(TARGET)
total_no_data <- project_i * project_j
project_df <- data.frame( rep(NA, total_no_data) )
colnames(project_df)<-'center'
project_df$target<-NA
project_df$my_dist<-NA
my_counter<-0

for(i in 1:nrow(CENTER)){
  for(j in 1:nrow(TARGET)){
    my_counter<-my_counter+1
    centerlonglat<-c(CENTER$long[i], CENTER$lat[i])
    targetlonglat<-c(TARGET$long[j], TARGET$lat[j])
    distance <- distVincentySphere(centerlonglat, targetlonglat)
    project_df$center[my_counter] <- i
    project_df$target[my_counter] <- j
    project_df$my_dist[my_counter] <- distance
  }
  }

return(project_df)

}
