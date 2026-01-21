find_col <- function (dataset_name, ...)
{

 if (base::missing(dataset_name)) {
       return(cat("  find_col(데이터셋, 변수명1, 변수명2) "))
    }

r = c()

find_col2 <- function(DataSet, index_id) {
        tmp_colnames <- colnames(DataSet)
        n <- length(tmp_colnames)
        for (i in 1:n) {
            if (index_id == tmp_colnames[i]) {
  return(as.numeric(i))
          }
    }
 }

dataset_name <- deparse(substitute(dataset_name))
dataset <- get(dataset_name)
var_names <- as.character(substitute(list(...)))[-1]

n <- length(var_names)

for (i in 1:n) {
        r <- rbind(r, find_col2(dataset, var_names[i]))
 }
   
var_names<-as.data.frame((var_names))
col_names<-as.data.frame(r)
df<-data.frame(var_names, col_names)
colnames(df) <- c('Name', 'ColNo')

cat("   ", "\n")
cat("\033[1;32m  ------------------------------ \033[0m", "\n")
print(df)
cat("\033[1;32m  ------------------------------ \033[0m", "\n")
cat("   ", "\n")
}

