## 활용법: logit_prob(로짓결과)

logit_prob <- function( LogitResult, my_optionx=0 , my_input=c(1) ){

if (base::missing(LogitResult)) {
	cat("  더미는 1증가, 연속변수 1증가: logit_prob( LogitResult)   ", '\n')
	cat("  더미는 1증가, 연속변수 30%증가: logit_prob( LogitResult, 30)   ", '\n')
	cat("  ------------------------------------------------------------   ", '\n')
	cat("  독립변수에 임의의 값 지정: my_input<-c(1,0,2)  *NOTE: 1=constant, 0='value of X1', 2='value of X2' ", '\n')
	cat("  logit_prob( LogitResult, 0, my_input)   ", '\n')
	return(cat("  ") ) }
	

## Return Variable Names ----------------------------

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



## Return Variable Names ----------------------------

r=c()
max.ylev<-2
nvariables<-LogitResult$rank
OriginData<-NA

for(i in 1:nvariables){
if( ( nrow(unique(LogitResult$model[i])) > max.ylev )  ) { r=c(r, median(LogitResult$model[, i])) }
else {r<-c(r, 0)} }
r[1]<-1


if( length(my_input)==1 ) { OriginData <- r } else{ OriginData<-my_input } 


if(length(my_input)==1) {  cat('*** 확률계산을 위한 OriginData 입력값: ', OriginData, '\n')  }
if(length(my_input)==1) { cat('    OriginData의 첫 번째값 1은 상수항을 의미함', '\n') }
cat('*** 확률계산1) OriginData를 이용한 확률계산: ', 'logit_prob( Logit결과 ) ', '\n')
cat('*** 확률계산2) OriginData 중 연속형변수값을 30% 증가시켰을 때의 확률계산: ', 'logit_prob( Logit결과, 30 ) ', '\n')
cat('*** 확률계산3) 독립변수값 지정: ', 'logit_prob(Logit결과, 0, c(1, 독립변수1값, 독립변수2값 ...) ) ', '\n')
cat('*** ------------------------------------------------------------------------------    ', '\n')
if(length(my_input)==1) {  cat('   ','독립변수', '  Origin확률(%) ', ' 증가확률(%)=최종확률-Origin확률 ', ' 최종확률(%) ', '\n')  }

variable_names<-Return_variablenames(LogitResult)

for(i in 2:length(OriginData)){
   input<-OriginData

if( ( my_optionx>0 ) & ( nrow(unique(LogitResult$model[i])) > max.ylev ) )  {
	input[i] <- (median(LogitResult$model[,i]))*(1 + my_optionx/100)  }
else {input[i]<-input[i]+1 }


   확률계산자료<-as.data.frame(LogitResult$coef)
   확률계산자료<-cbind(확률계산자료, input)
   colnames(확률계산자료)[1]<-c('Result')
   확률계산자료$temp<-확률계산자료$Result * 확률계산자료$input
   tmp<-sum( 확률계산자료[ ,3] )
   tmp분자<- exp(tmp)
   tmp분모<- 1+exp(tmp)
   확률<-round( tmp분자/tmp분모*100, 3)

   확률계산자료se<-as.data.frame(LogitResult$coef)
   확률계산자료se<-cbind(확률계산자료se, OriginData)
   colnames(확률계산자료se)[1]<-c('Result')
   확률계산자료se$temp <- 확률계산자료se$Result * 확률계산자료se$OriginData
   tmp<-sum( 확률계산자료se[ ,3] )
   tmp분자<- exp(tmp)
   tmp분모<- 1+exp(tmp)
   확률se<-round( tmp분자/tmp분모*100, 3)

   if(length(my_input)>1) {
   m_count<-length(my_input)
  variable_names2<-c('Intercept', variable_names)
   for( m in 1:m_count) {
   cat(  variable_names2[m] , "=" , my_input[m]  , "; ")
    }
   cat('    ' , '\n')
   cat('*** 확률(%):', 확률se, '\n')
   cat('*** ------------------------------------------------------------------------------ ', '\n')
   break
   }


   ChPROB<-round((확률-확률se) , 3)
   cat('    ', variable_names[i-1] )
   cat('      ', 확률se)
   cat('             ', ChPROB)
   cat('                           ', 확률, '\n')

  }   }

