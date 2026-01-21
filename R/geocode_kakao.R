geocode_kakao <- function(REST_API_KEY, address) {

if (base::missing(REST_API_KEY)) {
    cat(" \033[1;34m# library(httr); library(jsonlite); library(stringr) ", '\n')
    cat(" \033[1;34m# 국토정보플랫폼: https://map.ngii.go.kr/ ", '\n')
    cat(" \033[1;34m# 환경공간정보서비스: https://egis.me.go.kr/ ", '\n')
    cat(" \033[1;34m# 실거래가공개시스템: https://rt.molit.go.kr/ ", '\n')
    cat(" \033[1;34m# KB통계: https://kbland.kr/webview.html#/main/statistics?channel=kbland&tab=0 ", '\n')
    cat(" \033[1;34m# TS자료: 월 -> 분기: r<-aggregate(ir, nfrequency=4)/3  ", '\n')
    cat(" \033[1;34m# TS자료: 연 -> 분기: library(tempdisagg) ", '\n')
    cat(" \033[1;34m#                     td1<-td(cs~1, to='quarterly', converstion='last', method='denton-cholette') ", '\n')
    cat(" \033[1;34m# ----------------------------------------------------- ", '\n')
    cat(" \033[1;34m# library(httr); library(jsonlite); library(stringr) ", '\n')
    cat(" \033[1;34m# my_kakao_rest <- '7af181-9d01257-shimbi9645-elan234-ecd75-stu843-e16354-pdi323-5f0675b9-no' ", '\n')
    cat(" \033[1;34m# 지번 주소 또는, 도로명 주소를 addr에 대입 \033[0m ", '\n')
    cat(" \033[1;34m# addr <- '경기도 수원시 영통구 원천동 원천동 산5-1' \033[0m ", '\n')
    cat(" \033[1;34m# geocode_kakao(my_kakao_rest, addr) \033[0m ", '\n')
    cat(" \033[1;34m# ----------------------------------------------------- ", '\n')
    cat(" \033[1;34m# setwd('C:/Users/사용자이름/Documents')  ", '\n')
    cat(" \033[1;34m# ----------------------------------------------------- ", '\n')
    cat(" \033[1;32m              \033[0m ", '\n')
    cat(" \033[1;32m library(httr); library(jsonlite); library(stringr); library(aj412s2)  \033[0m ", '\n')
    cat(" \033[1;34m my_kakao_rest <- '7af181-9d01257-shimbi9645-elan234-ecd75-stu843-e16354-pdi323-5f0675b9-no' ", '\n')
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
    cat(" \033[1;34m       geocode_kakao(my_kakao_rest, addr)   \033[0m ", '\n')
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

  # URL 인코딩 함수 (Python의 quote_plus 역할)
  # URL 인코딩 시 공백은 '+'로, 기타 특수문자는 %xx 형태로 인코딩
  url_encode_plus <- function(string) {
    encoded_string <- URLencode(string, reserved = TRUE)
    encoded_string <- str_replace_all(encoded_string, "%20", "+") # 공백을 '+'로 변경
    return(encoded_string)
  }


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


response<-GET( url <- "https://dapi.kakao.com/v2/local/search/address.json", 
                 query = list(query=address[1]),
                 add_headers(Authorization = paste0("KakaoAK ", REST_API_KEY)))
  varsx<-(content(response, as="text", encoding = "UTF-8"))
  vars <- fromJSON(varsx)
  tmp<-data.frame(vars)
  lat_x<-tmp$documents.x
  long_y<-tmp$documents.y
  tmp2<-c(lat_x, long_y)
  return(tmp2)

}
