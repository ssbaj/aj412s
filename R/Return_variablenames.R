Return_variablenames<-function(LogitResult) {
   tmpword<-LogitResult$terms
   count_tmpword<-nchar(tmpword)
   count_tmpword<-count_tmpword[3]
   
   tmpword<-as.character(tmpword)
   tmpword<-tmpword[3]
   tmpdataframe<-rep(NA, 100)  # 변수의 최대 갯수를 100개로 지정
   tmpdataframe<-as.data.frame(tmpdataframe)
   colnames(tmpdataframe)<-'vnames'
   
   tmp_count_check<-0
   tmp_count_origin<-1
   tmp_count<-0
   
   for(i in 1:count_tmpword){
   tmp_w<-substring(tmpword, i, i)
   
   if(tmp_w != "+") {
   tmp_count<-tmp_count+1 
    }
   
   if(tmp_w == "+") { 
   temp_word<-substring(tmpword, tmp_count_origin, (i-1) )
   temp_word<-gsub(" ", "", temp_word)
   tmp_count_check <- tmp_count_check+1
   tmpdataframe[tmp_count_check,1]<-temp_word
   tmp_count_origin<-i+1
   }
   
   temp_word<-substring(tmpword, tmp_count_origin, count_tmpword)
   tmpdataframe[(tmp_count_check+1),1]<-temp_word
   
   }

  tmpdataframe<-tmpdataframe[complete.cases(tmpdataframe), ]
  return(tmpdataframe)
}

