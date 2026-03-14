if (!require(tcltk)) install.packages("tcltk")
if (!require(rstudioapi)) install.packages("rstudioapi")

selvar <- function(df_obj) {
  # 데이터프레임의 이름을 문자열로 추출 (새로고침 시 get()으로 최신 상태를 불러오기 위함)
  df_name <- deparse(substitute(df_obj))
  
  # 1. 메인 윈도우 설정
  tt <- tktoplevel()
  tkwm.title(tt, paste("변수 선택기 -", df_name))
  tkwm.geometry(tt, "280x500")
  
  # 창을 항상 위로 설정
  tcl("wm", "attributes", tt, "-topmost", TRUE)
  
  # 2. 리스트박스와 스크롤바 배치
  tl <- tklistbox(tt, height = 18, selectmode = "multiple", 
                  background = "white", highlightthickness = 2)
  scr <- tkscrollbar(tt, repeatinterval = 5, command = function(...) tkyview(tl, ...))
  tkconfigure(tl, yscrollcommand = function(...) tkset(scr, ...))
  
  # [기능 추가] 변수 목록 갱신 함수 (새 변수 반영)
  refresh_vars <- function() {
    # .GlobalEnv에서 최신 데이터프레임을 가져옴
    current_df <- get(df_name, envir = .GlobalEnv)
    vars_raw <- colnames(current_df)
    if (is.null(vars_raw)) vars_raw <- names(current_df)
    
    # 기존 리스트 삭제 후 다시 삽입
    tkdelete(tl, 0, "end")
    for (v in vars_raw) {
      tkinsert(tl, "end", paste0("  ", v)) # 2칸 들여쓰기 유지
    }
  }
  
  # 초기 변수 목록 로드
  refresh_vars()
  
  # 3. 화면 입력 및 리셋 로직
  insert_and_reset <- function() {
    selection <- as.numeric(tkcurselection(tl))
    
    if (length(selection) > 0) {
      # 입력 시점의 최신 변수명 리스트 가져오기
      current_df <- get(df_name, envir = .GlobalEnv)
      vars_raw <- colnames(current_df)
      if (is.null(vars_raw)) vars_raw <- names(current_df)
      
      selected_vars <- vars_raw[selection + 1]
      
      # 변수들 사이를 공백 2개("  ")로 연결
      var_string <- paste(selected_vars, collapse = "  ")
      
      # RStudio 커서 위치에 즉시 삽입
      if (rstudioapi::isAvailable()) {
        rstudioapi::insertText(var_string)
      }
      
      # 선택 항목 리셋
      tkselection.clear(tl, 0, "end")
    }
  }

  # 4. 레이아웃 배치
  tkpack(tl, side = "left", fill = "both", expand = TRUE)
  tkpack(scr, side = "right", fill = "y")
  
  btn_frame <- tkframe(tt)
  tkpack(btn_frame, side = "bottom", fill = "x")
  
  # [버튼 1] 입력 버튼
  copy_btn <- tkbutton(btn_frame, text = "✅Copy & Reset", 
                       command = insert_and_reset, 
                       height = 2, background = "#f0f0f0")
  
  # [버튼 2] 새로고침 버튼 (Upload new variable)
  refresh_btn <- tkbutton(btn_frame, text = "🔄Update Variable List", 
                         command = refresh_vars, 
                         background = "#e8e8e8")
  
  # [버튼 3] 닫기 버튼
  close_btn <- tkbutton(btn_frame, text = "❌Close", 
                        command = function() tkdestroy(tt))
  
  tkpack(copy_btn, side = "top", fill = "x", padx = 10, pady = 5)
  tkpack(refresh_btn, side = "top", fill = "x", padx = 10, pady = 2)
  tkpack(close_btn, side = "bottom", fill = "x", padx = 10, pady = 5)
  
  return(invisible(NULL))
}
