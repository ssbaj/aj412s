geocode_kakao <- function(REST_API_KEY, address) {

if (base::missing(REST_API_KEY)) {
cat(" \033[1;32m library(httr); library(jsonlite); library(stringr); library(aj412s); library(dplyr) ", '\n')
cat(" \033[1;34m# 국토정보플랫폼: https://map.ngii.go.kr/ ", '\n')
cat(" \033[1;34m# 환경공간정보서비스: https://egis.me.go.kr/ ", '\n')
cat(" \033[1;34m# 실거래가공개시스템: https://rt.molit.go.kr/ ", '\n')
cat(" \033[1;34m# KB통계: https://kbland.kr/webview.html#/main/statistics?channel=kbland&tab=0 ", '\n')
cat(" \033[1;34m# TS자료: 월 -> 분기: r<-aggregate(ir, nfrequency=4)/3  ", '\n')
cat(" \033[1;34m# TS자료: 연 -> 분기: library(tempdisagg) ", '\n')
cat(" \033[1;34m#                     td1<-td(cs~1, to='quarterly', converstion='last', method='denton-cholette') ", '\n')
cat(" \033[1;34m# ----------------------------------------------------- ", '\n')
cat(" \033[1;34m# library(httr); library(jsonlite); library(stringr) ", '\n')
cat(" \033[1;34m# my_kakao_rest <- 'SHIMBIRO98-5439f3d7eef2c504d3f7dad5c5d7a610' ", '\n')
cat(" \033[1;34m# 콤파스 https://compas.lh.or.kr/subj/competition/data?subjNo=SBJ_2501_001 ", '\n')
cat(" \033[1;34m# 지번 주소 또는, 도로명 주소를 addr에 대입 \033[0m ", '\n')
cat(" \033[1;34m# addr <- '경기도 수원시 영통구 원천동 원천동 산5-1' \033[0m ", '\n')
cat(" \033[1;34m# geocode_kakao(my_kakao_rest, addr) \033[0m ", '\n')
cat(" \033[1;34m# ----------------------------------------------------- ", '\n')
cat(" \033[1;34m# setwd('C:/Users/사용자이름/Documents')  ", '\n')
cat(" \033[1;34m# ----------------------------------------------------- ", '\n')
cat(" \033[1;32m              \033[0m ", '\n')
cat(" \033[1;32m my_kakao_rest <- 'SHIMBIRO98-5439f3d7eef2c504d3f7dad5c5d7a610'  \033[0m ", '\n')
cat(" \033[1;32m   \033[0m ", '\n')
cat(" \033[1;32m df=files22()  \033[0m ", '\n')
cat(" \033[1;32m   \033[0m ", '\n')
cat(" \033[1;32m df=data.frame(df)  \033[0m ", '\n')
cat(" \033[1;32m # gsub를 이용한 패턴 변환  \033[0m ", '\n')
cat(" \033[1;32m # ([0-9]+): 숫자를 그룹으로 묶음  \033[0m ", '\n')
cat(" \033[1;32m # *0*: 숫자 앞의 0을 무시하기 위한 처리  \033[0m ", '\n')
cat(" \033[1;32m df$jibun <- gsub('0?([0-9]+)월 0?([0-9]+)일', '\\1-\\2', df$jibun)  \033[0m ", '\n')
cat(" \033[1;32m   \033[0m ", '\n')
cat(" \033[1;32m newdf<-df %>% filter(grepl('경기도 성남시 중원구 상대원동', bjd_nm))  \033[0m ", '\n')
cat(" \033[1;32m   \033[0m ", '\n')
cat(" \033[1;32m newdf$addr<-paste(df$bjd_nm, df$jibun)  \033[0m ", '\n')
cat(" \033[1;32m   \033[0m ", '\n')
cat(" \033[1;32m n<-nrow(newdf)  \033[0m ", '\n')
cat(" \033[1;32m newdf$lat_y <- NA  \033[0m ", '\n')
cat(" \033[1;32m newdf$long_x <- NA  \033[0m ", '\n')
cat(" \033[1;32m   \033[0m ", '\n')
cat(" \033[1;32m for (i in 1:n) {  \033[0m ", '\n')
cat(" \033[1;32m addr <- newdf$addr[i]       \033[0m ", '\n')
cat(" \033[1;32m     # 예외 처리 포함       \033[0m ", '\n')
cat(" \033[1;32m     longlat <- tryCatch({       \033[0m ", '\n')
cat(" \033[1;32m       geocode_kakao(my_kakao_rest, addr)       \033[0m ", '\n')
cat(" \033[1;32m     }, error = function(e) {       \033[0m ", '\n')
cat(" \033[1;32m       return(NULL)       \033[0m ", '\n')
cat(" \033[1;32m     })       \033[0m ", '\n')
cat(" \033[1;32m       \033[0m ", '\n')
cat(" \033[1;32m     # 결과가 없을 경우 건너뛰기       \033[0m ", '\n')
cat(" \033[1;32m     if (is.null(longlat) || length(longlat) < 2) {  \033[0m ", '\n')
cat(" \033[1;32m       newdf$lat_y[i] <- NA       \033[0m ", '\n')
cat(" \033[1;32m       newdf$long_x[i] <- NA       \033[0m ", '\n')
cat(" \033[1;32m       next       \033[0m ", '\n')
cat(" \033[1;32m     }       \033[0m ", '\n')
cat(" \033[1;32m       \033[0m ", '\n')
cat(" \033[1;32m     # 정상적으로 값이 있을 때만 저장       \033[0m ", '\n')
cat(" \033[1;32m     newdf$lat_y[i] <- longlat[1]       \033[0m ", '\n')
cat(" \033[1;32m     newdf$long_x[i] <- longlat[2]  \033[0m ", '\n')
cat(" \033[1;32m     cat(longlat, i, ':번째', '\n')  \033[0m ", '\n')
cat(" \033[1;32m   }       \033[0m ", '\n')
cat(" \033[1;32m     \033[0m ", '\n')
cat(" \033[1;32m   mkcsv(newdf, newdf.csv)  \033[0m ", '\n')
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


# aj412s 설치 여부 확인
if (!requireNamespace("aj412s", quietly = TRUE)) {
  devtools::install_github("ssbaj/aj412s")  # 실제 GitHub 경로로 수정 필요
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
