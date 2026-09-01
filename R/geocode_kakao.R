geocode_kakao <- function(REST_API_KEY, df) {

if (base::missing(REST_API_KEY)) {
cat(" \033[1;32m library(httr); library(jsonlite); library(stringr); library(aj412s); library(dplyr) ", '\n')
cat(" \033[1;34m# getwd()  ", '\n')
cat(" \033[1;34m# setwd('C:/Users/사용자이름/Documents')  ", '\n')
cat(" \033[1;34m# ----------------------------------------------------- ", '\n')
cat(" \033[1;34m# 국토정보플랫폼: https://map.ngii.go.kr/ ", '\n')
cat(" \033[1;34m# 환경공간정보서비스: https://egis.me.go.kr/ ", '\n')
cat(" \033[1;34m# 실거래가공개시스템: https://rt.molit.go.kr/ ", '\n')
cat(" \033[1;34m# KB통계: https://kbland.kr/webview.html#/main/statistics?channel=kbland&tab=0 ", '\n')
cat(" \033[1;34m# ----------------------------------------------------- ", '\n')
cat(" \033[1;34m# TS자료: 월 -> 분기: r<-aggregate(ir, nfrequency=4)/3  ", '\n')
cat(" \033[1;34m# TS자료: 연 -> 분기: library(tempdisagg) ", '\n')
cat(" \033[1;34m#                     td1<-td(cs~1, to='quarterly', converstion='last', method='denton-cholette') ", '\n')
cat(" \033[1;34m# ----------------------------------------------------- ", '\n')
cat(" \033[1;34m# my_kakao_rest <- 'YOUR_KAKAO_REST_KEY' ", '\n')
cat(" \033[1;34m# df 데이터프레임에 지번/도로명 주소를 담은 addr 변수가 있어야 합니다 \033[0m ", '\n')
cat(" \033[1;34m# df$addr <- paste(df$bjd_nm, df$jibun) \033[0m ", '\n')
cat(" \033[1;34m# df <- geocode_kakao(my_kakao_rest, df) \033[0m ", '\n')
cat(" \033[1;34m# ----------------------------------------------------- ", '\n')
return(cat(" \033[1;32m   \033[0m ") ) }

## 필요 패키지 설치 확인 --------------------------------------
pkgs <- c("jsonlite", "httr", "stringr", "devtools", "dplyr", "readxl")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}
## -------------------------------------------------------------

if (!"addr" %in% names(df)) {
  message("There is no address variable")
  return(invisible(NULL))
}

n <- nrow(df)
df$lat <- NA
df$lon <- NA

for (i in seq_len(n)) {

  address <- df$addr[i]

  # 주소 하나씩 get_geo()로 조회 (get_geo.R 참조)
  latlon <- get_geo(REST_API_KEY, address)

  if (is.null(latlon)) {
    cat(sprintf("[%d/%d] '%s' 좌표를 찾지 못했습니다.\n", i, n, address))
    next
  }

  df$lat[i] <- latlon["lat"]
  df$lon[i] <- latlon["lon"]
  cat(sprintf("[%d/%d] %s -> lat: %s, lon: %s\n", i, n, address, latlon["lat"], latlon["lon"]))
}

return(df)

}

