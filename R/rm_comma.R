rm_comma <- function(data, var) {

if (base::missing(data)) {
	cat("  # 숫자 변수에 있는 콤마 제거 후 변수를 numeric으로 지정 ----- ", '\n')
	cat("    df <- rm_comma(BasicData, asset) ", '\n')
	return( cat("   ", '\n') )
}


  # 변수명을 문자로 캡처
  var_name <- deparse(substitute(var))

  # 변수 존재 여부 확인
  if (!var_name %in% names(data)) {
    stop(paste0("데이터셋에 '", var_name, "' 변수가 존재하지 않습니다."))
  }

  # 콤마 제거 후 numeric 변환
  data[[var_name]] <- as.numeric(
    gsub(",", "", data[[var_name]])
  )

  # 변환 결과 확인
  if (any(is.na(data[[var_name]]))) {
    warning(paste0(
      "'", var_name,
      "' 변수 변환 중 NA가 발생했습니다. 숫자가 아닌 값이 포함되어 있을 수 있습니다."
    ))
  }

  return(data)
}
