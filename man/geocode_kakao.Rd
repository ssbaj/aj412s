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

  library(httr); library(jsonlite); library(stringr); library(aj412s); library(dplyr)  
 # 국토정보플랫폼: https://map.ngii.go.kr/  
 # 환경공간정보서비스: https://egis.me.go.kr/  
 # 실거래가공개시스템: https://rt.molit.go.kr/  
 # KB통계: https://kbland.kr/webview.html#/main/statistics?channel=kbland&tab=0  
 # TS자료: 월 -> 분기: r<-aggregate(ir, nfrequency=4)/3   
 # TS자료: 연 -> 분기: library(tempdisagg)  
 #                     td1<-td(cs~1, to='quarterly', converstion='last', method='denton-cholette')  
 # -----------------------------------------------------  
 # library(httr); library(jsonlite); library(stringr)  
 # my_kakao_rest <- 'shimbiro342-5439f3d7eef2c504d3f7dad5c5d7a610'  
 # 지번 주소 또는, 도로명 주소를 addr에 대입   
 # addr <- '경기도 수원시 영통구 원천동 원천동 산5-1'   
 # geocode_kakao(my_kakao_rest, addr)   
 # -----------------------------------------------------  
 # setwd('C:/Users/사용자이름/Documents')   
 # -----------------------------------------------------  
                 
  my_kakao_rest <- 'SHIMBIRO98-5439f3d7eef2c504d3f7dad5c5d7a610'    
      
  df=files22()
      
  df=data.frame(df)    
  # gsub를 이용한 패턴 변환    
  # ([0-9]+): 숫자를 그룹으로 묶음    
  # *0*: 숫자 앞의 0을 무시하기 위한 처리    
  df$jibun <- gsub('0?([0-9]+)월 0?([0-9]+)일', '\1-\2', df$jibun)    
      
  newdf<-df %>% filter(grepl('경기도 성남시 중원구 상대원동', bjd_nm))    
  
  newdf$addr<-paste(df$bjd_nm, df$jibun)    
      
  n<-nrow(newdf)    
  newdf$lat_y <- NA    
  newdf$long_x <- NA    
      
  for (i in 1:n) {    
  addr <- newdf$addr[i]         
      # 예외 처리 포함         
      longlat <- tryCatch({         
        geocode_kakao(my_kakao_rest, addr)         
      }, error = function(e) {         
        return(NULL)         
      })         
          
      # 결과가 없을 경우 건너뛰기         
      if (is.null(longlat) || length(longlat) < 2) {    
        newdf$lat_y[i] <- NA         
        newdf$long_x[i] <- NA         
        next         
      }         
          
      # 정상적으로 값이 있을 때만 저장         
      newdf$lat_y[i] <- longlat[1]         
      newdf$long_x[i] <- longlat[2]    
      cat(longlat, i, ':번째', '
')    
    }         
        
    mkcsv(newdf, newdf.csv)    
 # -----------------------------------------------------  

}
