get_geo <- function(api_key, address) {
  # 'Shimbiro-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
  # 'ELCA-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'

  if (missing(api_key) || missing(address) ||
      is.null(api_key) || is.null(address) ||
      !nzchar(api_key) || !nzchar(address)) {
    cat("[오류] 주소와 API 키를 모두 입력해주세요.\n")
    return(NULL)
  }

  url <- "https://dapi.kakao.com/v2/local/search/address.json"

  tryCatch({
    response <- httr::GET(
      url,
      httr::add_headers(Authorization = paste0("KakaoAK ", api_key)),
      query = list(query = address)
    )
    httr::stop_for_status(response) # HTTP 오류 발생 시 예외 처리

    data <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))

    documents <- data$documents
    if (is.null(documents) || nrow(documents) == 0) {
      cat(sprintf("[결과 없음] '%s'에 대한 좌표를 찾을 수 없습니다.\n", address))
      return(NULL)
    }

    # 카카오 API 응답 기준: x = 경도(Longitude), y = 위도(Latitude)
    lon <- as.numeric(documents$x[1])
    lat <- as.numeric(documents$y[1])

    c(lat = lat, lon = lon)

  }, error = function(e) {
    cat("[API 통신 오류]", conditionMessage(e), "\n")
    return(NULL)
  })
}
