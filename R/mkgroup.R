mkgroup<-function(name_dataset, select_columns, CuttingNumber, sign=1) {
 
if (base::missing(name_dataset)) {
     cat("\033[1;31m # ---------------------------------------------------------------------- ", '\n')
     cat("\033[1;34m cn<-quantile(df$income, c(.25, .5, .75)) \033[0m", '\n')
cat("\033[1;34m 또는, cn<-c(23.175, 45.700, 61.775 ) \033[0m", '\n')
     cat("\033[1;31m # ---------------------------------------------------------------------- ", '\n')
     cat("\033[1;34m df<-mkgroup(df, income, cn) \033[0m", '\n')
     cat("\033[1;34m df, income, cn=Cutting Number, if there is no number then '<' \033[0m", '\n')
     cat("\033[1;31m # ---------------------------------------------------------------------- ", '\n')
     cat("\033[1;34m df<-mkgroup(df, income, cn, 2) \033[0m", '\n')
     cat("\033[1;34m df, income, cn=Cutting Number, number is 2 then '<=' \033[0m", '\n')
return(cat(" ") ) }

c2n <- function(x_x01){
groups = unique(x_x01)
groups= sort(groups)
tmp<-as.numeric(factor(x_x01, levels=groups))
     return(tmp) }

var_name <- deparse(substitute(select_columns))

##-----------------------------------
##------------------------------------
find_col2<-function(DataSet, index_id ){
tmp_colnames<-colnames(DataSet)
n<-length(tmp_colnames) # DataSet의 총변수 갯수

for(i in 1:n){
if(index_id==tmp_colnames[i]) {return(as.numeric(i))}
}
}

##----------------------------------------
select_columns<-find_col2(name_dataset, var_name)
##----------------------------------------

n0<-nrow(name_dataset[select_columns])

target_variable<-rep(NA, n0)

for(i in 1:n0){
target_variable[i]<-name_dataset[i,select_columns]
if(is.na(name_dataset[i,select_columns])) {cat(' ** Remove NAs ----','\n')}
}


if( class(target_variable) == 'character') {target_variable<-c2n(target_variable)}

CuttingNumber <- as.vector(CuttingNumber)

nx_0<-length(target_variable)
mx_0<-length(CuttingNumber)

groupIndexx_0 <- matrix( rep(NA, nx_0), ncol=1)


## <
if(sign ==1 ) {
for(ix_0 in 1:nx_0) {

for(jx_0 in 1:mx_0 ) {
countx<-0
if( target_variable[ix_0] >= CuttingNumber[mx_0]) {
         groupIndexx_0[ix_0] <- (mx_0+1)
         countx<-countx+1
         break}
if( target_variable[ix_0] < CuttingNumber[jx_0]) {
groupIndexx_0[ix_0] <- jx_0
         countx<-countx+1
         break}
    if (countx==1) {break}
    }
    }
}



## <=
if(sign != 1) {
for(ix_0 in 1:nx_0) {

for(jx_0 in 1:mx_0 ) {
countx<-0
if( target_variable[ix_0] > CuttingNumber[mx_0]) {
         groupIndexx_0[ix_0] <- (mx_0+1)
         countx<-countx+1
         break}
if( target_variable[ix_0] <= CuttingNumber[jx_0]) {
groupIndexx_0[ix_0] <- jx_0
         countx<-countx+1
         break}
    if (countx==1) {break}
    }
    }
}



tmp_select_columns<-as.data.frame(groupIndexx_0)
name_dataset2<-cbind(name_dataset, tmp_select_columns)
ncolX0<-ncol(name_dataset2)
colnames(name_dataset2)[ncolX0] <- paste0( "g_", colnames(name_dataset[select_columns]) , sep='')

name_dataset2<-as.data.frame(name_dataset2)

return(name_dataset2)
}
