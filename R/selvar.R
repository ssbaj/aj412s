selvar <- function(df_obj) {
  
  # 1. 패키지 설치 여부 확인 및 중단 로직
  missing_pkgs <- c()
  if (!requireNamespace("tcltk", quietly = TRUE)) missing_pkgs <- c(missing_pkgs, "tcltk")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) missing_pkgs <- c(missing_pkgs, "rstudioapi")
  
  if (length(missing_pkgs) > 0) {
    stop(paste0("\n[설치 필요] 아래 패키지가 없습니다. 설치 후 다시 실행해 주세요:\n",
                "install.packages(c('", paste(missing_pkgs, collapse = "', '"), "'))"))
  }

  # 패키지 로드
  library(tcltk)
  library(rstudioapi)

  # 데이터프레임 이름 추출
  df_name <- deparse(substitute(df_obj))
  
  # 2. 메인 윈도우 설정
  tt <- tktoplevel()
  tkwm.title(tt, paste("통합 변수 선택기 -", df_name))
  tkwm.geometry(tt, "350x650")
  tcl("wm", "attributes", tt, "-topmost", TRUE)
  
  # 폰트 설정 (10pt)
  default_font <- tkfont.create(family = "helvetica", size = 10)
  bold_font <- tkfont.create(family = "helvetica", size = 10, weight = "bold")
  
  # 상단 레이블
  tkpack(tklabel(tt, text = paste("Data:", df_name), font = bold_font, fg = "#2c3e50"), pady = 5)
  tkpack(tklabel(tt, text = "회귀식(LM): 첫 선택=y, 이후=x", 
                 font = tkfont.create(size = 9), fg = "#e67e22"))

  # 3. 리스트박스 및 스크롤바
  list_frame <- tkframe(tt)
  tkpack(list_frame, fill = "both", expand = TRUE, padx = 10, pady = 5)
  
  tl <- tklistbox(list_frame, height = 15, selectmode = "multiple", 
                  background = "white", font = default_font, highlightthickness = 1)
  scr <- tkscrollbar(list_frame, repeatinterval = 5, command = function(...) tkyview(tl, ...))
  tkconfigure(tl, yscrollcommand = function(...) tkset(scr, ...))
  
  tkpack(tl, side = "left", fill = "both", expand = TRUE)
  tkpack(scr, side = "right", fill = "y")

  # 변수 목록 갱신 함수
  refresh_vars <- function() {
    if (!exists(df_name, envir = .GlobalEnv)) {
      tkmessageBox(message = "데이터프레임을 찾을 수 없습니다.", icon = "error")
      return()
    }
    current_df <- get(df_name, envir = .GlobalEnv)
    vars_raw <- colnames(current_df)
    if (is.null(vars_raw)) vars_raw <- names(current_df)
    
    tkdelete(tl, 0, "end")
    for (v in vars_raw) {
      tkinsert(tl, "end", paste0("  ", v)) # 리스트박스 내부 들여쓰기는 유지
    }
  }
  
  refresh_vars()

  # 4. 삽입 로직 (공백 1개로 수정된 부분)
  
  # [기능 1] 일반 변수 나열 (Simple List)
  insert_simple <- function() {
    selection <- as.numeric(tkcurselection(tl))
    if (length(selection) > 0) {
      current_df <- get(df_name, envir = .GlobalEnv)
      vars_raw <- if(!is.null(colnames(current_df))) colnames(current_df) else names(current_df)
      selected_vars <- vars_raw[selection + 1]
      
      # 변수 간 간격을 공백 1개(" ")로 설정
      var_string <- paste(selected_vars, collapse = " ")
      if (rstudioapi::isAvailable()) rstudioapi::insertText(var_string)
      tkselection.clear(tl, 0, "end")
    }
  }

  # [기능 2] 회귀 식 생성 (Regression Formula)
  insert_lm <- function() {
    selection <- as.numeric(tkcurselection(tl))
    if (length(selection) > 0) {
      current_df <- get(df_name, envir = .GlobalEnv)
      vars_raw <- if(!is.null(colnames(current_df))) colnames(current_df) else names(current_df)
      selected_vars <- vars_raw[selection + 1]
      
      if (length(selected_vars) == 1) {
        var_string <- selected_vars[1]
      } else {
        target <- selected_vars[1]
        # + 와 ~ 앞뒤 공백을 1개(" ")로 설정
        features <- paste(selected_vars[-1], collapse = " + ")
        var_string <- paste0(target, " ~ ", features)
      }
      
      if (rstudioapi::isAvailable()) rstudioapi::insertText(var_string)
      tkselection.clear(tl, 0, "end")
    }
  }

  # 5. 버튼 레이아웃
  btn_frame <- tkframe(tt)
  tkpack(btn_frame, side = "bottom", fill = "x", padx = 10, pady = 10)

  tkpack(tkbutton(btn_frame, text = "📄 Simple List (selvar)", 
                  command = insert_simple, font = default_font, height = 1), fill = "x", pady = 2)
  
  tkpack(tkbutton(btn_frame, text = "📈 Regression Formula (lm)", 
                  command = insert_lm, font = default_font, height = 1, background = "#e1f5fe"), fill = "x", pady = 2)
  
  tkpack(tklabel(btn_frame, text = ""), pady = 2)
  
  tkpack(tkbutton(btn_frame, text = "🔄 Update Variable List", 
                  command = refresh_vars, font = default_font), fill = "x", pady = 2)
  
  tkpack(tkbutton(btn_frame, text = "❌ Close", 
                  command = function() tkdestroy(tt), font = default_font), fill = "x", pady = 5)

  return(invisible(NULL))
}