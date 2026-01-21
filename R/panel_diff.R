panel_diff<-function(AdataSet, T){
   

if (base::missing(AdataSet)) {
	    cat("  사용되는 Data set엔 numerical 변수만 있어야 합니다. 문자변수, 날짜변수는 제거 후 panel_diff를 사용하세요 ", '\n')
		cat("  diffDataSet<-panel_diff(Panel_Dataset, 1)  *NOTE: 1=시계열데이터  ", '\n')
		return( cat("  diffDataSet<-panel_diff(Panel_Dataset, 3)  *NOTE: 3=Repeating되는 기간 ", '\n') )  }

df<-as.data.frame(AdataSet)

if(T==1){
Data_set_SE<-df
lag_Data_set_SE<-dplyr::lag(Data_set_SE)
Data_setALL_SE<-Data_set_SE - lag_Data_set_SE
return(Data_setALL_SE)
exit
}

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
