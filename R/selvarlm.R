if (!require(tcltk)) install.packages("tcltk")
if (!require(rstudioapi)) install.packages("rstudioapi")

selvar <- function(df_obj) {
  # 데이터프레임 객체의 이름을 문자열로 추출 (새로고침 시 get() 사용을 위함)
  df_name <- deparse(substitute(df_obj))
  
  # 1. 메인 윈도우 설정
  tt <- tktoplevel()
  tkwm.title(tt, paste("회귀분석 변수 선택기 -", df_name))
  tkwm.geometry(tt, "320x550")
  tcl("wm", "attributes", tt, "-topmost", TRUE)
  
  # 안내 라벨
  label_text <- "첫 번째 선택: 종속변수(y)\n이후 선택: 독립변수(x)"
  tkpack(tklabel(tt, text = label_text, justify = "left", fg = "blue"), pady = 5)
  
  # 2. 리스트박스와 스크롤바
  tl <- tklistbox(tt, height = 18, selectmode = "multiple", 
                  background = "white", highlightthickness = 2)
  scr <- tkscrollbar(tt, repeatinterval = 5, command = function(...) tkyview(tl, ...))
  tkconfigure(tl, yscrollcommand = function(...) tkset(scr, ...))
  
  # [기능 추가] 변수 목록 갱신 함수 (Upload new variable)
  refresh_vars <- function() {
    # Global Environment에서 최신 데이터프레임 상태를 가져옴
    current_df <- get(df_name, envir = .GlobalEnv)
    new_vars <- colnames(current_df)
    if (is.null(new_vars)) new_vars <- names(current_df)
    
    tkdelete(tl, 0, "end") # 기존 목록 삭제
    for (v in new_vars) {
      tkinsert(tl, "end", paste0("  ", v)) # 2칸 들여쓰기
    }
  }
  
  # 초기 로드
  refresh_vars()
  
  # 3. 회귀식 생성 및 리셋 로직
  insert_formula_and_reset <- function() {
    selection <- as.numeric(tkcurselection(tl))
    
    if (length(selection) > 0) {
      # 현재 표시된 리스트 기준이 아닌, 실제 데이터의 최신 인덱스 참조
      current_df <- get(df_name, envir = .GlobalEnv)
      vars_raw <- colnames(current_df)
      if (is.null(vars_raw)) vars_raw <- names(current_df)
      
      selected_vars <- vars_raw[selection + 1]
      
      # [수정] 모든 변수 간 간격을 공백 2개("  ")로 연결
      if (length(selected_vars) == 1) {
        var_string <- selected_vars[1]
      } else {
        # ~ 와 + 앞뒤로 공백 2개씩 적용
        target <- selected_vars[1]
        features <- paste(selected_vars[-1], collapse = "  +  ")
        var_string <- paste(target, "  ~  ", features)
      }
      
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
                       command = insert_formula_and_reset, 
                       height = 2, background = "#e1f5fe")
  
  # [버튼 2] 변수 업데이트 버튼 (새로고침)
  upload_btn <- tkbutton(btn_frame, text = "🔄Update Variable List", 
                         command = refresh_vars, 
                         background = "#f0f0f0")
  
  # [버튼 3] 닫기 버튼
  close_btn <- tkbutton(btn_frame, text = "❌Close", 
                        command = function() tkdestroy(tt))
  
  tkpack(copy_btn, side = "top", fill = "x", padx = 10, pady = 5)
  tkpack(upload_btn, side = "top", fill = "x", padx = 10, pady = 2)
  tkpack(close_btn, side = "bottom", fill = "x", padx = 10, pady = 5)
  
  return(invisible(NULL))
}