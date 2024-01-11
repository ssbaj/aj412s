\name{e_ggplot2}
\alias{e_ggplot2}
\title{e_ggplot2}
\usage{
e_ggplot2()
}
\description{
e_ggplot2 Program
}
\examples{


# --------- 막대그래프 그리기 -------------------
library(dplyr); library(aj412s); library(readxl); library(ggplot2)

# Adata<-read_excel('Data41_Time_Series_data3.xlsx', skip=1)
#예제 데이터는 패키지에 KoreaMacro란 이름으로 내장되어 있음

Adata <- KoreaMacro
tmp<-mkdate(Adata$yearmonth)
Adata<-cbind(Adata, tmp)

경상수지<-Adata%>%group_by(year)%>%summarize(경상수지평균=mean(surplus))

# 오름차순: ggplot(data=경상수지, aes(x=reorder(year, -경상수지평균), y=경상수지평균)) ... )
# 내림차순: ggplot(data=경상수지, aes(x=reorder(year, 경상수지평균), y=경상수지평균)) ... )

ggplot(data=경상수지, aes(x=year, y=경상수지평균)) + geom_bar(stat='identity', fill='steelblue', col='black') + geom_text( data=경상수지, aes(y=경상수지평균, label = round(경상수지평균,0), vjust=-1, size=4) ) + theme(axis.text.x=element_text(color='black', size=13, angle=-40, vjust=-.5)) + theme(axis.text.y=element_text(color='black', size=13)) + theme(title=element_text(size=30, face='bold')) + theme(axis.title.x = element_text(size=25,face='bold')) + theme(axis.title.y = element_text(size=25,face='bold')) + labs(x='연도', y='경상수지평균', title='연도별 경상수지평균') + theme(legend.position = 'none') + scale_y_continuous(labels =scales:: comma)


# --------- 직선 그리기 -------------------

geom_line(mapping=aes(x=컬럼이름X,
                        y=컬럼이름Y,
                        group=컬럼이름K,
                        linetype=컬럼이름K,
                        color=컬럼이름K),
						linewidth=숫자A, ...)


코스피<-Adata%>%group_by(year)%>%summarize(KOSPI평균=mean(kospi))


ggplot(data=코스피, aes(x=year, y=KOSPI평균)) + geom_line(group=1, col='red', linewidth=1) + geom_point(col='red', size=3.0) + geom_text( data=코스피, aes(y=KOSPI평균, label = round(KOSPI평균,0), vjust=-1, size=4) ) + theme(axis.text.x=element_text(color='black', size=13, angle=-40, vjust=-.5)) + scale_y_continuous(labels =scales:: comma) + theme(axis.text.y=element_text(color='black', size=13)) + theme(title=element_text(size=30, face='bold')) + theme(axis.title.y = element_text(size=25,face='bold')) + theme(axis.title.x = element_text(size=25,face='bold')) + labs(x='연도', y='연간 코스피평균', title='연도별 코스피평균') + theme(legend.position = 'none') 



# ---- 산포도 그리기 --------------------

mpg=연비(hwy), cyl=엔진의 기통수, disp=배기량, hp=마력, drat=뒤차축비, wt=무게,
qsec=1/4mile 도달시간, vs=(0:V-엔진/1:Straight-엔진), am=변속기어(0:오토, 1=변속기어)
gear=전진기어 갯수, carb=캬뷰레터(기화기) 갯수,

ggplot(데이터A,
		aes=(x=컬럼이름X,
             y=컬럼이름Y))+
		geom_point()+ geom_smooth()

ggplot(data=mpg,mapping = aes(x=displ, y=hwy))
	+ geom_smooth()
	+ geom_point()


}