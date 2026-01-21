library(ggplot2) 
mytheme<-theme(
   plot.background = element_rect(fill = 'white', color = 'white', linewidth = 1), # 전체 플롯배경: '#843E3D1C'   
   panel.background = element_rect(fill = 'white', color = 'gray'), # 그래프 영역 배경 색     
   panel.grid.major = element_line(color = '#0300031C', linewidth = 0.5), # 주요 그리드 선       
   panel.grid.minor = element_line(color = 'gray', linewidth = 0.25, linetype = 'dashed'), # 보조 그리드 선       
   panel.grid = element_line(color = 'gray', linewidth = 0.5), # 전체 그리드 선 색과 두께       
   axis.title.x = element_text(size = 20, color = 'black', hjust=0.5, vjust= -3), # X축 제목 스타일     
   axis.title.y = element_text(size = 20, color = 'black', hjust=0.5), # Y축 제목 스타일     
   axis.text.x = element_text(size = 14, color = 'black', hjust=0.5), # X축 제목 스타일     
   axis.text.y = element_text(size = 14, color = 'black', hjust=0.5), # Y축 제목 스타일     
   plot.title = element_text(size = 20, color = 'black', hjust = 0, vjust = 2, face = 'bold'), # Title 조정     
   plot.margin = margin(t = 40, r = 40, b = 40, l = 40), # Title 조정     
   legend.text = element_text(size = 14),     
   legend.title = element_text(size = 14),     
   legend.position = 'right'  ## legend.position = 'none' OR 'bottom'     
 )   


