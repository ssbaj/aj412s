addcomma <- function(input_string) {

no_spaces <- gsub(" ", ",", input_string)
no_spaces <- gsub("\t", ",", no_spaces)
no_spaces <- gsub(" ,", ",", no_spaces)
no_spaces <- gsub(", ", ",", no_spaces)
no_spaces <- gsub(",+", ",", no_spaces)
no_spaces <- gsub(" ", ",", no_spaces)
# 문자열 양끝에 불필요한 콤마 제거 (선택 사항)
cleaned_string <- gsub("^,|,$", "", no_spaces)
return(cleaned_string)
}
