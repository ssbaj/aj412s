\name{geocode_parcel}
\alias{geocode_parcel}
\title{Converting PARCEL address to Geocode}
\usage{
geocode_parcel()
}
\description{
Converting PARCEL address to Geocode
}
\examples{

 library(httr); library(jsonlite); library(stringr)  
 my_vworld <- 'Your Vworld ApiKey'
 # my_vworld <- 'SHIMBIRO455-A612BFF9-049A-ELCA888-3B10-BED9-B79A19STU8884PID1038C6'
 # 도로명 주소를 addr_parcel에 입력 ----   
 address_parcel <- '경기도 수원시 영통구 원천동 산19'   
 geocode_parcel(my_vworld, address_parcel)  

 # 주소록 파일을 사용해 geocode를 가져오는 방법 ----   
 df<-opencsv(주소록파일.csv)

 df$lat<-NA 
 df$long<-NA

 n<-nrow(df)
 for(i in 1:n){
  address_parcel<-df$주소[i]
  df$lat[i]<-geocode_parcel(my_vworld, address_parcel)[1]
  df$long[i]<-geocode_parcel(my_vworld, address_parcel)[2]
 }


}
