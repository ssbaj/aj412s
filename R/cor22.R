cor22<-function(k0_dataset, digits=2, method='pearson'){

options(warn = -1)

if (base::missing(k0_dataset)) {
     cat("  Two variable correlation --> jmv::corrMatrix(df, vars=vars(donation, age))", '\n' )
	 cat("  Use dplyr to make data.set and run cor22: dplyr를 사용하면 변수명이 cor22로 전달됨  ", '\n' )
	 return(cat("  cor22((df%>%select(var1, var2, ... ))), method='pearson' or 'kendall' or 'spearman') "))}

if (!require(dplyr)) {
  install.packages("dplyr")
}

MYc2n <- function(x){
     groups = unique(x)
     groups= sort(groups)
     tmp<-as.numeric(factor(x, levels=groups))
     return(tmp) }

k0_dataset<-as.data.frame(k0_dataset)
k8_dataset<-as.data.frame(k0_dataset)
k8_dataset[k8_dataset==""]<-NA
col_numbers<-ncol(k8_dataset)

for(i in 1:col_numbers){

if(class(k8_dataset[,i])=="character") {
k8_dataset[,i]<-MYc2n(k8_dataset[,i])
}

k8_dataset[,i][k8_dataset[,i] == Inf]<-NA
k8_dataset[,i][k8_dataset[,i] == -Inf]<-NA
k8_dataset[,i][k8_dataset[,i] == '']<-NA
}

k8_dataset<-k8_dataset[complete.cases(k8_dataset), ]

MYcorrelation1<-round( cor(k8_dataset, method=method) , digits)
MYcorrelation2<-MYcorrelation1
MYcorrelation2[upper.tri(MYcorrelation1)] <- ''
MYcorrelation3<-as.data.frame(MYcorrelation2)

cat("\033[1;34m ------------------------  ", '\n')
cat('  Number of original data : ', nrow(k0_dataset), '\n')
cat('  Number of data for calculation : ', nrow(k8_dataset), '\n')
cat("\033[1;34m ------------------------  ", '\n')

counting_cor<-0

for(i in 1:col_numbers){
if(class(k0_dataset[,i])=="character") {
cat('  ',  i,'-th variable is character',sep='','\n')
counting_cor<-counting_cor+1
}
}

if(counting_cor>0) {
cat("\033[1;34m ------------------------  ", '\n')
cat("\033[1;34m  Character data is converted into numeric data. \033[0m", '\n')
cat("\033[1;34m  DATA such as Inf, -Inf, and blank is replaced by NA. \033[0m", '\n')
cat("\033[1;34m  NA data is removed by using the 'complete.cases' command. \033[0m", '\n')
cor22<-function(k0_dataset, method='pearson'){

if (base::missing(k0_dataset)) {
	 return(cat("  cor2(df, method='pearson' or 'kendall' or 'spearman') "))}

MYc2n <- function(x){
     groups = unique(x)
     groups= sort(groups)
     tmp<-as.numeric(factor(x, levels=groups))
     return(tmp) }

k8_dataset<-k0_dataset
k8_dataset[k8_dataset==""]<-NA
col_numbers<-ncol(k8_dataset)

for(i in 1:col_numbers){

if(class(k8_dataset[,i])=="character") {
k8_dataset[,i]<-MYc2n(k8_dataset[,i])
}

k8_dataset[,i][k8_dataset[,i] == Inf]<-NA
k8_dataset[,i][k8_dataset[,i] == -Inf]<-NA
k8_dataset[,i][k8_dataset[,i] == '']<-NA
}

k8_dataset<-k8_dataset[complete.cases(k8_dataset), ]

MYcorrelation1<-round( cor(k8_dataset, method=method) , digits)
MYcorrelation2<-MYcorrelation1
MYcorrelation2[upper.tri(MYcorrelation1)] <- ''
MYcorrelation3<-as.data.frame(MYcorrelation2)

cat("\033[1;34m ------------------------  ", '\n')
cat('  Number of original data : ', nrow(k0_dataset), '\n')
cat('  Number of data for calculation : ', nrow(k8_dataset), '\n')
cat("\033[1;34m ------------------------  ", '\n')

counting_cor<-0

for(i in 1:col_numbers){
if(class(k0_dataset[,i])=="character") {
cat('  ',  i,'-variable is character',sep='','\n')
counting_cor<-counting_cor+1
}
}

if(counting_cor>0) {
cat("\033[1;34m ------------------------  ", '\n')
cat("\033[1;34m  Character data is converted into numeric data. \033[0m", '\n')
cat("\033[1;34m  DATA such as Inf, -Inf, and blank is replaced by NA. \033[0m", '\n')
cat("\033[1;34m  NA data is removed by using the 'complete.cases' command. \033[0m", '\n')
cat("\033[1;34m ------------------------  ", '\n')
cat("         ", '\n')
}

return(MYcorrelation3)
} 
}


return(MYcorrelation3)
} 