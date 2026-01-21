geocode_road<-function(vworld_apikey, addr){

  if (base::missing(vworld_apikey)) {
    cat(" \033[1;34m# library(httr); library(jsonlite); library(stringr) ", '\n')
    cat(" \033[1;34m# 국토정보플랫폼: https://map.ngii.go.kr/ ", '\n')
    cat(" \033[1;34m# 환경공간정보서비스: https://egis.me.go.kr/ ", '\n')
    cat(" \033[1;34m# 실거래가공개시스템: https://rt.molit.go.kr/ ", '\n')
    cat(" \033[1;34m# 브이월드: https://www.vworld.kr/ ", '\n')
    cat(" \033[1;34m# KB통계: https://kbland.kr/webview.html#/main/statistics?channel=kbland&tab=0 ", '\n')
    cat(" \033[1;34m# TS자료: 월 -> 분기: r<-aggregate(ir, nfrequency=4)/3  ", '\n')
    cat(" \033[1;34m# TS자료: 연 -> 분기: library(tempdisagg) ", '\n')
    cat(" \033[1;34m#                     td1<-td(cs~1, to='quarterly', converstion='last', method='denton-cholette') ", '\n')
    cat(" \033[1;34m# ----------------------------------------------------- ", '\n')
    cat(" \033[1;34m# my_vworld <- 'SHIMBIRO455-A612BFF9-049A-ELCA888-3B10-BED9-B79A191038C6-yes' ", '\n')
    cat(" \033[1;34m# 도로명 주소를 addr_road에 입력 ---- \033[0m ", '\n')
    cat(" \033[1;34m# addr_road <- '경기도 수원시 월드컵로 206' \033[0m ", '\n')
    cat(" \033[1;34m# geocode_road(my_vworld, addr_road) \033[0m ", '\n')
    cat(" \033[1;34m# ----------------------------------------------------- ", '\n')
    cat(" \033[1;34m              \033[0m ", '\n')
    cat(" \033[1;32m# ----------------------------------------------------- ", '\n')
    cat(" \033[1;34m# setwd('C:/Users/사용자이름/Documents')  ", '\n')
    cat(" \033[1;34m# ----------------------------------------------------- ", '\n')
    cat(" \033[1;32m# 주소 만들기 = 시군구 + 번지  ", '\n')
    cat(" \033[1;34m library(aj412s2); library(dplyr) \033[0m ", '\n')
    cat(" \033[1;34m  \033[0m ", '\n')
    cat(" \033[1;34m df<-openxlsx(경기도_실거래가_원본.xlsx) \033[0m ", '\n')
    cat(" \033[1;34m # df 데이터셋의 변수: 시군구, 번지, 거래금액만원, 평당금액 \033[0m ", '\n')
    cat(" \033[1;34m #    도로명, 전용면적m2, 단지명 \033[0m ", '\n')
    cat(" \033[1;34m  \033[0m ",   '\n')
    cat(" \033[1;34m colnames(df)[컬럼번호]<-'새로운_변수명_공백_금지' \033[0m ", '\n')
    cat(" \033[1;34m df$addr <- paste0(df$시군구, ' ', df$도로명) \033[0m ", '\n')
    cat(" \033[1;34m df <- df%>%arrange(addr) \033[0m ", '\n')
    cat(" \033[1;34m colnames(df)   \033[0m ", '\n')
    cat(" \033[1;34m    \033[0m ", '\n')
    cat(" \033[1;34m new_df <- df %>%   \033[0m ", '\n')
    cat(" \033[1;34m   group_by(addr) %>%  \033[0m ", '\n')
    cat(" \033[1;34m   summarise(    \033[0m ", '\n')
    cat(" \033[1;34m     mean_거래금액 = mean(거래금액만원, na.rm = TRUE),   \033[0m ", '\n')
    cat(" \033[1;34m     mean_평당금액 = mean(평당금액, na.rm = TRUE),   \033[0m ", '\n')
    cat(" \033[1;34m     번지 = first(번지),   \033[0m ", '\n')
    cat(" \033[1;34m     전용면적m2 = first(전용면적m2),   \033[0m ", '\n')
    cat(" \033[1;34m     단지명 = first(단지명),   \033[0m ", '\n')
    cat(" \033[1;34m     매매건수 = n(),   \033[0m ", '\n')
    cat(" \033[1;34m     .groups = 'drop'   \033[0m ", '\n')
    cat(" \033[1;34m   )   \033[0m ", '\n')
    cat(" \033[1;34m    \033[0m ", '\n')
    cat(" \033[1;32m# ----------------------------------------------------- ", '\n')
    cat(" \033[1;32m# Vworld를 이용해 geocode 찾기 ------------------------ ", '\n')
    cat(" \033[1;34m library(httr); library(jsonlite); library(stringr)   \033[0m ", '\n')
    cat(" \033[1;34m my_vworld <- 'SHIMBIRO455-A612BFF9-049A-ELCA888-3B10-BED9-B79A191038C6-yes'   \033[0m ", '\n')
    cat(" \033[1;34m    \033[0m ", '\n')
    cat(" \033[1;34m n<-nrow(new_df)   \033[0m ", '\n')
    cat(" \033[1;34m new_df$lat_y <- NA   \033[0m ", '\n')
    cat(" \033[1;34m new_df$long_x <- NA   \033[0m ", '\n')
    cat(" \033[1;34m    \033[0m ", '\n')
    cat(" \033[1;34m for (i in 1:n) {   \033[0m ", '\n')
    cat(" \033[1;34m     addr <- new_df$addr[i]   \033[0m ", '\n')
    cat(" \033[1;34m    \033[0m ", '\n')
    cat(" \033[1;34m   # 예외 처리 포함   \033[0m ", '\n')
    cat(" \033[1;34m     longlat <- tryCatch({   \033[0m ", '\n')
    cat(" \033[1;34m       geocode_road(my_vworld, addr)   \033[0m ", '\n')
    cat(" \033[1;34m     }, error = function(e) {   \033[0m ", '\n')
    cat(" \033[1;34m       return(NULL)   \033[0m ", '\n')
    cat(" \033[1;34m     })   \033[0m ", '\n')
    cat(" \033[1;34m    \033[0m ", '\n')
    cat(" \033[1;34m   # 결과가 없을 경우 건너뛰기   \033[0m ", '\n')
    cat(" \033[1;34m     if (is.null(longlat) || length(longlat) < 2) {   \033[0m ", '\n')
    cat(" \033[1;34m       new_df$lat_y[i] <- NA   \033[0m ", '\n')
    cat(" \033[1;34m       new_df$long_x[i] <- NA   \033[0m ", '\n')
    cat(" \033[1;34m       next   \033[0m ", '\n')
    cat(" \033[1;34m     }   \033[0m ", '\n')
    cat(" \033[1;34m    \033[0m ", '\n')
    cat(" \033[1;34m     # 정상적으로 값이 있을 때만 저장   \033[0m ", '\n')
    cat(" \033[1;34m     new_df$lat_y[i] <- longlat[1]   \033[0m ", '\n')
    cat(" \033[1;34m     new_df$long_x[i] <- longlat[2]   \033[0m ", '\n')
    cat(" \033[1;34m }   \033[0m ", '\n')
    cat(" \033[1;34m    \033[0m ", '\n')
    cat(" \033[1;34m # 정리된 데이터를 xlsx파일로 저장   \033[0m ", '\n')
    cat(" \033[1;34m mkxlsx(new_df, 'new_df.xlsx')   \033[0m ", '\n')
    cat(" \033[1;32m# ----------------------------------------------------- ", '\n')
    cat(" \033[1;34m    \033[0m ", '\n')
    return(cat(" \033[1;32m   \033[0m ") ) }


## 패키지 설치 ----------------------------------------------
pkgs <- c("jsonlite", "httr", "stringr", "devtools", "dplyr", "readxl")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

# aj412s2 설치 여부 확인
if (!requireNamespace("aj412s2", quietly = TRUE)) {
  devtools::install_github("ssbaj/aj412s2")  # 실제 GitHub 경로로 수정 필요
}

## 패키지 설치 끝 --------------------------------------------

# URL 인코딩 함수 (Python의 quote_plus 역할)
# URL 인코딩 시 공백은 '+'로, 기타 특수문자는 %xx 형태로 인코딩
url_encode_plus <- function(string) {
    encoded_string <- URLencode(string, reserved = TRUE)
    encoded_string <- str_replace_all(encoded_string, "%20", "+") # 공백을 '+'로 변경
    return(encoded_string)
  }
  
 
url <- paste0(
  "http://api.vworld.kr/req/address?service=address&request=getCoord&type=ROAD&refine=false&key=", vworld_apikey, "&address=", url_encode_plus(addr)
)

# API 요청 및 응답 처리
response <- GET(url)

# HTTP 상태 코드 확인

if (status_code(response) == 200) {
  # 응답 본문 추출 및 UTF-8 디코딩 (httr::content 함수가 자동으로 처리)
  response_body <- content(response, "text", encoding = "UTF-8")
  # JSON 데이터 파싱
  jsonData <- fromJSON(response_body)
  
  # 위도(lat)와 경도(lng) 추출
  # VWorld API 응답 구조에 따라 경로 지정
  lat <- as.numeric(jsonData$response$result$point$y)
  lng <- as.numeric(jsonData$response$result$point$x)
  latlng22<-c(lat, lng)
  return(latlng22)
} else {
  cat(i,' data-Error: ', 'error code:', status_code(response), '\n')
}

}

