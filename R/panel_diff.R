panel_diff<-function(AdataSet, T){
  
 ## AdataSet = name of dataset, 2018~2020 : T=3
  
if (base::missing(AdataSet)) {
    return(	
      cat("  diffDataSet<-diff_dataset(panel_dataset, 3)  *NOTE:3=Repeating되는 기간", "\n")
    )}
  
df<-as.data.frame(AdataSet)
n<-nrow(df)
T<-T
iteration <- (n/T)

Data_set<-df[1:T, ]
lag_Data_set<-dplyr::lag(Data_set)
Data_setALL<-Data_set-lag_Data_set

ncount<-(iteration-1)
for(i in 1:ncount){
  row1<-i*T + 1
  row2<-(T*(i+1))
  Data_set<-df[row1:row2, ]
  lag_Data_set<-dplyr::lag(Data_set)
  Data_set2<-Data_set-lag_Data_set
  Data_setALL<-rbind(Data_setALL,Data_set2)
}

return(Data_setALL)

}
