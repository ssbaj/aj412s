files22 <- function() {
  # 1. 필수 패키지 확인 (인코딩 분석을 위해 readr 포함)
  pkg_list <- c("tcltk", "readxl", "haven", "data.table", "readr")
  for (pkg in pkg_list) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("\n[설치 필요] %s 패키지가 없습니다. install.packages('%s')를 실행하세요.", pkg, pkg))
      stop(sprintf("\n[설치 필요] %s 패키지가 없습니다. install.packages('%s')를 실행하세요.", pkg, pkg))
    }
  }
  library(tcltk)
  
  # 2. 파일 선택 (최상단 고정)
  tt_parent <- tktoplevel()
  tkwm.withdraw(tt_parent)
  tcl("wm", "attributes", tt_parent, "-topmost", TRUE)
  
  f_path <- tclvalue(tkgetOpenFile(parent = tt_parent, 
                                   title = "Select Data File",
                                   filetypes = "{{All Files} {*}} {{Files} {.xlsx .xls .csv .sav .dat .txt}}"))
  tkdestroy(tt_parent)
  
  if (f_path == "") return(message("File selection canceled."))
  
  # 3. 확장자 판독
  ext <- tolower(tools::file_ext(f_path))
  supported_ext <- c("xlsx", "xls", "csv", "sav", "dat", "txt")
  
  if (!(ext %in% supported_ext)) {
    tkmessageBox(type = "ok", icon = "warning", 
                 message = "Warning: file22 only supports xlsx, xls, csv, sav, dat, and txt files.")
    return(invisible(NULL))
  }
  
  # 파일 인코딩 분석 및 추천 로직
  rec_enc <- "Unknown"
  if (ext %in% c("csv", "txt")) {
    guess <- readr::guess_encoding(f_path, n_max = 1000)
    if (nrow(guess) > 0) {
      rec_enc <- guess$encoding[1] # 가장 확률이 높은 인코딩 추출
    }
  }
  
  # 4. 공통 옵션 입력창
  opt_win <- tktoplevel()
  tkwm.title(opt_win, paste("Import Options:", basename(f_path)))
  tkwm.geometry(opt_win, "350x580") # 항목 추가로 창 세로 길이 소폭 확대
  tcl("wm", "attributes", opt_win, "-topmost", TRUE)
  
  # Skip 입력
  skip_var <- tclVar("0")
  tkpack(tklabel(opt_win, text = "1. Number of Rows to Skip", font = "helvetica 10 bold"), pady = c(15, 2))
  tkpack(tkentry(opt_win, textvariable = skip_var, width = 10, justify = "center"), pady = 5)
  
  # Header 체크박스
  header_var <- tclVar("1")
  header_cb <- tkcheckbutton(opt_win, text = " Header (First Row as Names)", font = "helvetica 10 bold", variable = header_var)
  tkpack(header_cb, pady = 10)
  
  # 5. CSV/TXT 인코딩 추천 및 선택 아이콘 (ASCII 추가)
  enc_var <- tclVar("UTF-8") # 디폴트 UTF-8
  
  if (ext %in% c("csv", "txt")) {
    tkpack(tklabel(opt_win, text = "2. Select Encoding", font = "helvetica 10 bold"), pady = c(10, 2))
    
    # 추천 인코딩 표시
    rec_label <- paste0("💡 Recommended Encoding: ", rec_enc)
    tkpack(tklabel(opt_win, text = rec_label, fg = "blue"), pady = c(0, 5))
    
    # 인코딩 선택 라디오 버튼
    enc_frame <- tkframe(opt_win)
    tkpack(tkradiobutton(enc_frame, text = "UTF-8", variable = enc_var, value = "UTF-8"), anchor = "w")
    tkpack(tkradiobutton(enc_frame, text = "EUC-KR", variable = enc_var, value = "EUC-KR"), anchor = "w")
    tkpack(tkradiobutton(enc_frame, text = "CP949", variable = enc_var, value = "CP949"), anchor = "w")
    tkpack(tkradiobutton(enc_frame, text = "ASCII", variable = enc_var, value = "ASCII"), anchor = "w") # ASCII 추가
    tkpack(enc_frame, pady = 5)
  }
  
  # 6. TXT 전용 구분자 선택
  sep_var <- tclVar(",") # 기본값
  if (ext == "txt") {
    tkpack(tklabel(opt_win, text = "3. Select Text Separator", font = "helvetica 10 bold"), pady = c(10, 2))
    rb_frame <- tkframe(opt_win)
    tkpack(tkradiobutton(rb_frame, text = "Comma (,)", variable = sep_var, value = ","), anchor = "w")
    tkpack(tkradiobutton(rb_frame, text = "Dot (.)", variable = sep_var, value = "."), anchor = "w")
    tkpack(tkradiobutton(rb_frame, text = "Tab", variable = sep_var, value = "\t"), anchor = "w")
    tkpack(tkradiobutton(rb_frame, text = "Blank (Space)", variable = sep_var, value = " "), anchor = "w")
    tkpack(rb_frame, pady = 5)
  }
  
  result_data <- NULL
  
  # 7. 로딩 실행 함수
  load_action <- function() {
    s_val <- as.numeric(tclvalue(skip_var))
    h_val <- as.logical(as.numeric(tclvalue(header_var)))
    current_sep <- tclvalue(sep_var)
    current_enc <- tclvalue(enc_var) 
    
    tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        result_data <<- readxl::read_excel(f_path, skip = s_val, col_names = h_val)
      } else if (ext == "csv") {
        result_data <<- read.csv(f_path, skip = s_val, header = h_val, 
                                 fileEncoding = current_enc, stringsAsFactors = FALSE)
      } else if (ext == "txt") {
        result_data <<- read.table(f_path, skip = s_val, header = h_val, sep = current_sep, 
                                   fileEncoding = current_enc, fill = TRUE, blank.lines.skip = TRUE,
                                   stringsAsFactors = FALSE, check.names = FALSE)
      } else if (ext == "sav") {
        result_data <<- haven::read_sav(f_path)
        if (s_val > 0) result_data <<- result_data[-(1:s_val), ]
      } else if (ext == "dat") {
        result_data <<- haven::read_dta(f_path)
        if (s_val > 0) result_data <<- result_data[-(1:s_val), ]
      }
      
      # Header=FALSE일 경우 변수명 강제 지정 (V1, V2...)
      if (!h_val && !is.null(result_data)) {
        colnames(result_data) <<- paste0("V", 1:ncol(result_data))
      }
      
      tkdestroy(opt_win)
      message(paste("Successfully loaded:", basename(f_path), "| Encoding:", current_enc))
    }, error = function(e) {
      tkmessageBox(message = paste("Error loading file:", e$message), icon = "error")
    })
  }
  
  tkpack(tkbutton(opt_win, text = "🚀 Load Data", command = load_action, width = 20, bg = "#e1f5fe"), pady = 25)
  tkwait.window(opt_win)
  
  return(result_data)
}
