cor2<-function(k0_dataset){

if (base::missing(k0_dataset)) {
	 return(cat("  cor2(df) "))}

MYc2n <- function(x){
     groups = unique(x)
     groups= sort(groups)
     tmp<-as.numeric(factor(x, levels=groups))
     return(tmp) }

k8_dataset<-k0_dataset
col_numbers<-ncol(k8_dataset)

for(i in 1:col_numbers){
if(class(k8_dataset[,i])=="character") {k8_dataset[,i]<-MYc2n(k8_dataset[,i])}
k8_dataset[,i][k8_dataset[,i] == Inf]<-NA
k8_dataset[,i][k8_dataset[,i] == -Inf]<-NA
k8_dataset[,i][k8_dataset[,i] == '']<-NA
}

k8_dataset<-k8_dataset[complete.cases(k8_dataset), ]
cat('*** Number of Data : ', nrow(k8_dataset), '\n')

MYcorrelation1<-round( cor(k8_dataset, method='pearson') , 4)
MYcorrelation2<-MYcorrelation1
MYcorrelation2[upper.tri(MYcorrelation1)] <- ''
MYcorrelation3<-as.data.frame(MYcorrelation2)
return(MYcorrelation3)
} 