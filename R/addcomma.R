addcomma <- function(input_string) {

if (base::missing(input_string)) {
     cat("\033[1;32m  # 명령문: x<-'1 2 3 4 5' \033[0m ", '\n')
     cat("\033[1;32m  # 명령문: x<-addcomma(x) \033[0m ", '\n')
     cat("\033[1;32m  # 명령문: x<-addquote(x) \033[0m ", '\n')
     return(cat("      ",'\n'))  }

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
