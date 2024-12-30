\name{e_data}
\alias{e_data}
\title{e_data}
\usage{
e_data()
}
\description{
e_data
}
\examples{

## 벡터 -------  
  x<-c(1:10, 14, 21:24) 
  x<-rep(8, 5) 
  x<-rep(1:3, 2) 
    
## 행렬 -------  
  x<-matrix(1:9, nrow=3, byrow=T)  
  x[1,3] 
    
## 데이터프레임 -------  
  df<-data.frame(   
     id=c(1,2,3,4),   
     gender=c('남','여','여','남'),   
     income=c(2000, 2300, 4500, 4000)  )   
  df[ ,-c(1,3)]   
  df[df$gender=='남', ]   
  df[df$gender=='남', c(2, 3)]   
  df[df$id>=3, c(2, 3)]   
    
## 리스트 -------  
  x<-rep(1:3, 3)   
  df<-data.frame(   
     id=c(1,2,3,4),   
     gender=c('남','남','여','여'),   
     income=c(2000, 2300, 4500, 4000)  )   
  datalist<-list(x, df)   
  datalist[[1]]   
  datalist[[2]]$income   

## 팩터 -------  
  area<-c('경기', '인천', '서울')
  x<-as.factor(area)

}