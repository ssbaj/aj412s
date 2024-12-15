## 활용법: probit_prob(로짓결과)

probit_prob <- function( ProbitResult, my_input=NULL ){

  if (is.null(my_input)) {
    cat("*** probit결과로 확률계산을 위해 다음의 default가 적용됩니다. ", '\n')
	cat("*** 기본 확률 계산을 위해 더미변수는 0으로, 연속형변수는 중위수를 할당해 기본 확률을 계산합니다. ", '\n')
	cat("*** 증가 확률 계산 시 모든 변수들은 기본 확률 계산에 사용된 값이 변수에 할당되며,  ", '\n')
	cat("*** 다만 왼쪽에 기록된 변수명의 변수값만 다음과 같이 변합니다", '\n')
	cat("*** 더미변수는 0->1로, 연속형 변수는 default로 할당된 중위수가 20% 증가했을 때의 확률입니다", '\n')
	my_input <- 20
  }


if (base::missing(ProbitResult)) {
    cat("  Default ------------------------------------------------------------   ", '\n')
	cat("*** probit결과로 확률계산을 위해 다음의 default가 적용됩니다. ", '\n')
	cat("*** 기본 확률 계산을 위해 더미변수는 0으로, 연속형변수는 중위수를 할당해 기본 확률을 계산합니다. ", '\n')
	cat("*** 증가 확률 계산 시 모든 변수들은 기본 확률 계산에 사용된 값이 변수에 할당되며,  ", '\n')
	cat("*** 다만 왼쪽에 기록되는 변수명의 변수값만 다음과 같이 변합니다", '\n')
	cat("*** 더미변수는 0->1로, 연속형 변수는 default로 할당된 중위수가 20% 증가했을 때의 확률입니다", '\n')
	cat("  Define1 ------------------------------------------------------------   ", '\n')
	cat("  Default: 더미는 0->1, 연속변수는 중위수->20%증가: probit_prob(ProbitResult)   ", '\n')
	cat("  Define1: 더미는 0->1, 연속변수의 default가 중위수이고 중윗값이 30%증가했을 확률을 구하려면  ", '\n')
	cat("           다음의 명령문을 사용: probit_prob(ProbitResult, 30)   ", '\n')
	cat("  Define2 ------------------------------------------------------------   ", '\n')
	cat("  Default: 더미는 0->1, 연속변수는 중위수->20%증가: probit_prob(ProbitResult)   ", '\n')
	cat("  Define2: 독립변수에 임의의 값 지정: ", '\n')
	cat("           my_input<-c(1,0,2)  *NOTE: 1=상수항, 0='독립변수1의 값', 2='독립변수2의 값' ", '\n')
	cat("  probit_prob( ProbitResult, my_input)   ", '\n')
	return(cat("  ") ) }


## Start of Return Variable Names ----------------------------
Return_variablenames<-function(ProbitResult) {
   tmpword<-ProbitResult$terms
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
## End of Return Variable Names ----------------------------


r=c()
max.ylev<-2
nvariables<-ProbitResult$rank
tmp_r=c()

## 상수항 및 중위수값을 r에 대입. 첫 번째 항에 1을 대입
for(i in 1:nvariables){
if( ( nrow(unique(ProbitResult$model[i])) > max.ylev )  ) { r=c(r, median(ProbitResult$model[, i])) }
else {r<-c(r, 0)} }
r[1]<-1

if( length(my_input)==1 ) { OriginData <- r } else{ OriginData<-my_input } 
cat('*** 기본 확률: 더미변수=0, 연속형변수=중위수 대입해 확률계산 ','\n') 
cat('*** 확률계산1) 기본확률 계산 중 연속형변수값을 20% 증가시켰을 때의 확률계산: ', 'probit_prob( Probit결과, 20 ) ', '\n')
cat('*** 확률계산2) 변수값 지정한 후 확률계산 => ', 'probit_prob(Probit결과, c(1, 독립변수1의 값, 독립변수2의 값 ...) ) ', '\n')
cat('               확률계산을 위한 첫 번째 입력값 1은 상수항을 의미함', '\n') 
cat('*** Return <-probit_prob(Probit결과, c(1, 독립변수1의 값, 독립변수2의 값...)) 최종확률이 return값으로 전달됨 ', '\n')
cat('*** ------------------------------------------------------------------------------    ', '\n')
if(length(my_input)==1) {  cat('   ','독립변수', '  기본확률(%) ', ' 증가확률(%)=최종확률-기본확률 ', ' 최종확률(%) ', '\n')
		                   cat('*** ------------------------------------------------------------------------------    ', '\n')
						}

variable_names<-Return_variablenames(ProbitResult)

for(i in 2:length(OriginData)){
   input<-OriginData

if( length(my_input)==1 ) { OriginData <- r } else{ OriginData<-my_input } 
if( is.null(my_input) ) { OriginData <- r }


## 퍼센트 증가 케이스
## 각 열에서 unique한 숫자들이 몇 개 있는지 확인하는 절차
## 각 열에서 unique한 숫자가 2개이면 더미변수이므로 퍼센트 증가 대신, 0->1로 변경함
if( ( length(my_input)==1 ) & ( nrow(unique(ProbitResult$model[i])) > max.ylev ) )  {
	input[i] <- (median(ProbitResult$model[,i]))*(1 + my_input/100)  
	tmp_r<-c(tmp_r, input[i])}
    else {input[i]<-input[i]+1
		  tmp_r<-c(tmp_r, input[i])
		  }


   ### 입력된 원자료에 1을 더한 자료 --------
   확률계산자료<-as.data.frame(ProbitResult$coef)
   확률계산자료<-cbind(확률계산자료, input)
   colnames(확률계산자료)[1]<-c('Result')
   확률계산자료$temp<-확률계산자료$Result * 확률계산자료$input
   tmp<-sum( 확률계산자료[ ,3] )
   확률<-round( pnorm(tmp)*100, 4)

   
   ### 입력된 원자료 --------
   확률계산자료se<-as.data.frame(ProbitResult$coef)
   확률계산자료se<-cbind(확률계산자료se, OriginData)
   colnames(확률계산자료se)[1]<-c('Result')
   확률계산자료se$temp <- 확률계산자료se$Result*확률계산자료se$OriginData
   tmp<-sum( 확률계산자료se[ ,3] )
   확률se<-round( pnorm(tmp)*100, 4)



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


   ChPROB<-round((확률-확률se) , 4)
   
   cat('    ', variable_names[i-1] )
   cat('      ', 확률se)
   cat('             ', ChPROB)
   cat('                           ', 확률, '\n')

  }   
  
if(length(my_input)==1) {cat('     ------------------------------------------------------------------------------ ', '\n') }
if(length(my_input)==1) {cat('     기본확률 계산 INPUT자료', r, '\n') }
if(length(my_input)==1) {cat('     증가확률 계산 INPUT자료', '#', tmp_r, '\n') }
if(length(my_input)>1) {return(확률se) }
  
  }


