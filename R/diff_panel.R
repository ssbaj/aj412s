diff_panel<-function(AdataSet, T){

## AdataSet = name of dataset, 2019~2022 : T=4
## dplyr package::lag()

if (base::missing(AdataSet)) {
    return(	
	cat("  tmp<-diff_dataset(panel_dataset, 4)  *NOTE: 4=year of panel data", "\n")
	)}


AdataSet<-as.data.frame(AdataSet)
ncolumns<-ncol(AdataSet)

if (ncolumns == 1) {
        col_marking <- 1 }
    else{col_marking <- 999}

if(ncolumns==1) {
n<-nrow(AdataSet)
tmp_AdataSet<-rep(1,n)
AdataSet<-cbind(AdataSet, tmp_AdataSet)}

LagAdataSet<-dplyr::lag(AdataSet)

df_pdata<-AdataSet-LagAdataSet

ncolumns<-ncol(df_pdata)

n<-nrow(df_pdata)

iteration <- (n/T)

for(j in 1:ncolumns){

for(i in 0:(iteration-1) ){

df_pdata[i*T+1, j]<-NA }
}

df_pdata<-as.data.frame(df_pdata)

for(i in 1:ncolumns){
ans <- "d_"
tmp_name<-colnames(df_pdata)[i]
colnames(df_pdata)[i]<-paste0(ans,tmp_name, sep='' )
}

if(col_marking==1){df_pdata<-df_pdata[,-c(2)]}

return(df_pdata)
}