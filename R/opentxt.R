opentxt <- function(name_dataset, header=TRUE) {

if (base::missing(name_dataset)) {
	cat("# Loading txt file ( .txt ) ----", '\n')
	cat("  Adata<-opentxt( HSB1data.txt ) ", '\n')
	return(cat("  옵션: Adata<-opentxt( HSB1data.txt, header=T 또는 F )", '\n') )
	}

name_dataset <- deparse(substitute(name_dataset))

if(name_dataset == "file.choose()") {name_dataset <- file.choose() }

name_dataset[name_dataset==""]<-NA

tmp.df<-read.table(name_dataset, fileEncoding='CP949', encoding='UTF-8', header=header)

return(tmp.df) }
