\name{geocode_kakao}
\alias{geocode_kakao}
\title{Converting PARCEL/Road address to Geocode}
\usage{
geocode_kakao()
}
\description{
Converting PARCEL/Road address to Geocode
}
\examples{

 library(httr); library(jsonlite); library(stringr)  
 my_kakao_rest <- 'Your Kakao Rest key'
 # my_kakao_rest <- '7af181-shimbi9645-9d01257-elan234-ecd75-stu843-e16354-pdi323-5f0675b9'
 # 지버/도로명 주소를 addr_parcel에 입력 ----   
 address <- '경기도 수원시 영통구 원천동 산19'   
 geocode_kakao(my_kakao_rest, address)  

 # 주소록 파일을 사용해 geocode를 가져오는 방법 ----   
 df<-opencsv(주소록파일.csv)

 df$lat<-NA 
 df$lng<-NA

 n<-nrow(df)
 for(i in 1:n){
  address_parcel<-df$주소[i]
  df$lat[i]<-geocode_kakao(my_kakao_rest, address_parcel)[1]
  df$lng[i]<-geocode_kakao(my_kakao_rest, address_parcel)[2]
 }


}
