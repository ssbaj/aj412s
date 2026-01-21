addquote <- function(input_string) {

if (base::missing(input_string)) {
     cat("\033[1;32m  # 명령문: x<-'1 2 3 4 5' \033[0m ", '\n')
     cat("\033[1;32m  # 명령문: x<-addcomma(x) \033[0m ", '\n')
     cat("\033[1;32m  # 명령문: x<-addquote(x) \033[0m ", '\n')
     return(cat("      ",'\n'))  }

words <- strsplit(input_string, ",")[[1]]
words <- trimws(words) 

quoted_words <- paste0("'", words, "'")
result <- paste(quoted_words, collapse = ", ")
return(result)
}
