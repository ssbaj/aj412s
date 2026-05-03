\name{geocode_road}
\alias{geocode_road}
\title{Converting Road address to Geocode}
\usage{
geocode_road()
}
\description{
Converting Road address to Geocode
}
\examples{

library(httr); library(jsonlite); library(stringr)  
  my_vworld <- 'Your Vworld Apikey'  
# my_vworld <- 'SHIMBIRO455-5C24FB8F-7481-33FB-B45B-CF7047E28830'  

#도로명 주소를 addr_road에 입력 ----   
  address_road <- '경기도 수원시 월드컵로 206'   
  geocode_road(my_apikey, address_road)  

# 주소록 파일을 사용해 geocode를 가져오는 방법 ----   
 df<-opencsv(주소록파일.csv)

 df$lat<-NA 
 df$long<-NA

 n<-nrow(df)

 for(i in 1:n){
  address_road<-df$주소[i]
  df$lat[i]<-geocode_road(my_apikey, address_road)[1]
  df$long[i]<-geocode_road(my_apikey, address_road)[2]
  }

}
