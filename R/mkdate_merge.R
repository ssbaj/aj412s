mkdate_merge<-function(df, ydata, mdata ){

if (base::missing(df)) {
	    cat("  df<-as.data.frame(df) ", '\n')
		cat("  Input: Year=2023 and Month=4 // Output: DATE_merge=2023-04-01    ", '\n')
		return(cat("  df<-mkdate_merge(df, 2=column number of year, 3=column number of month) ") )  }

df<-as.data.frame(df)
df[ ,ydata]<-as.character(df[ ,ydata])
df[ ,mdata]<-as.character(df[ ,mdata])

n<-nrow(df)

for (i in 1:n){
if(nchar(df[,mdata][i])==1) {
df[,mdata][i]<-paste0('0', df[,mdata][i], sep='')
} }

df$DATE<-rep(NA, n)

for (i in 1:n){
df$DATE[i]<-paste0( df[,ydata][i], '-',df[,mdata][i], '-01', sep='')
}

df$DATE<-as.Date(df$DATE)

suppressPackageStartupMessages(library("dplyr"))
df <- df%>%relocate(DATE)

return(df)

}
