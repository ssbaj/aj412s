addquote <- function(input_string) {

if (base::missing(input_string)) {
cat(" \033[1;36m# Examples ---------- \033[0m", '\n' )
cat(" \033[1;36m variables <- 'v1, v2, v3' \033[0m", '\n' )
return( cat(" \033[1;36m addquote(variables) \033[0m", '\n' )) }

words <- strsplit(input_string, ",")[[1]]
words <- trimws(words) 

quoted_words <- paste0("'", words, "'")
result <- paste(quoted_words, collapse = ", ")
return(result)
}
