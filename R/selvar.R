selvar <- function(df_obj) {
    missing_pkgs <- c()
    if (!requireNamespace("tcltk", quietly = TRUE)) 
        missing_pkgs <- c(missing_pkgs, "tcltk")
    if (!requireNamespace("rstudioapi", quietly = TRUE)) 
        missing_pkgs <- c(missing_pkgs, "rstudioapi")
    if (length(missing_pkgs) > 0) {
        stop(paste0("\n[Required] Packages not found. Please install them and try again:\n", 
            "install.packages(c('", paste(missing_pkgs, collapse = "', '"), 
            "'))"))
    }
    
    library(tcltk)
    library(rstudioapi)
    
    df_name <- deparse(substitute(df_obj))
    # 클릭 순서를 저장할 환경 변수 생성 (클로저 역할)
    click_order <- tclVar("") 
    ordered_indices <- c() 

    tt <- tktoplevel()
    tkwm.title(tt, "Variable Selector")
    tkwm.geometry(tt, "350x650")
    tcl("wm", "attributes", tt, "-topmost", TRUE)
    
    default_font <- tkfont.create(family = "helvetica", size = 10)
    
    tkpack(tklabel(tt, text = paste("DATA FRAME :", df_name), fg = "black"), pady = 5)
    
    list_frame <- tkframe(tt)
    tkpack(list_frame, fill = "both", expand = TRUE, padx = 10, pady = 5)
    
    tl <- tklistbox(list_frame, height = 15, selectmode = "multiple", 
                    background = "white", font = default_font, highlightthickness = 1)
    scr <- tkscrollbar(list_frame, repeatinterval = 5, command = function(...) tkyview(tl, ...))
    tkconfigure(tl, yscrollcommand = function(...) tkset(scr, ...))
    tkpack(tl, side = "left", fill = "both", expand = TRUE)
    tkpack(scr, side = "right", fill = "y")

    # [핵심 수정] 클릭할 때마다 순서를 기록하는 함수
    on_select <- function() {
        # 현재 선택된 전체 인덱스(오름차순)
        current_sel <- as.numeric(tkcurselection(tl))
        
        if (length(current_sel) == 0) {
            ordered_indices <<- c()
        } else if (length(current_sel) > length(ordered_indices)) {
            # 새로 추가된 인덱스 찾아서 뒤에 붙임
            new_idx <- setdiff(current_sel, ordered_indices)
            ordered_indices <<- c(ordered_indices, new_idx)
        } else if (length(current_sel) < length(ordered_indices)) {
            # 해제된 인덱스 제거
            ordered_indices <<- intersect(ordered_indices, current_sel)
        }
    }
    
    # 리스트박스 클릭 이벤트 바인딩
    tkbind(tl, "<<ListboxSelect>>", on_select)

    refresh_vars <- function() {
        if (!exists(df_name, envir = .GlobalEnv)) {
            tkmessageBox(message = "DataFrame not found", icon = "error")
            return()
        }
        current_df <- get(df_name, envir = .GlobalEnv)
        vars_raw <- colnames(current_df)
        if (is.null(vars_raw)) vars_raw <- names(current_df)
        
        tkdelete(tl, 0, "end")
        ordered_indices <<- c() # 초기화
        for (v in vars_raw) {
            tkinsert(tl, "end", paste0("  ", v))
        }
    }
    
    refresh_vars()

    insert_simple <- function() {
        if (length(ordered_indices) > 0) {
            current_df <- get(df_name, envir = .GlobalEnv)
            vars_raw <- if (!is.null(colnames(current_df))) colnames(current_df) else names(current_df)
            
            # 기록된 순서대로 변수 추출
            selected_vars <- vars_raw[ordered_indices + 1]
            var_string <- paste(selected_vars, collapse = " ")
            
            if (rstudioapi::isAvailable()) rstudioapi::insertText(var_string)
            tkselection.clear(tl, 0, "end")
            ordered_indices <<- c() # 입력 후 초기화
        }
    }

    insert_lm <- function() {
        if (length(ordered_indices) > 0) {
            current_df <- get(df_name, envir = .GlobalEnv)
            vars_raw <- if (!is.null(colnames(current_df))) colnames(current_df) else names(current_df)
            
            # 기록된 순서대로 변수 추출 (첫 번째 클릭이 종속변수)
            selected_vars <- vars_raw[ordered_indices + 1]
            
            if (length(selected_vars) == 1) {
                var_string <- selected_vars[1]
            } else {
                target <- selected_vars[1]
                features <- paste(selected_vars[-1], collapse = " + ")
                var_string <- paste0(target, " ~ ", features, ", data = ", df_name)
            }
            
            if (rstudioapi::isAvailable()) rstudioapi::insertText(var_string)
            tkselection.clear(tl, 0, "end")
            ordered_indices <<- c() # 입력 후 초기화
        }
    }

    btn_frame <- tkframe(tt)
    tkpack(btn_frame, side = "bottom", fill = "x", padx = 10, pady = 10)
    
    tkpack(tkbutton(btn_frame, text = "📄 Simple List (selvar)", command = insert_simple, font = default_font), fill = "x", pady = 2)
    tkpack(tkbutton(btn_frame, text = "📈 Regression Formula (lm)", command = insert_lm, font = default_font, background = "#e1f5fe"), fill = "x", pady = 2)
    tkpack(tkbutton(btn_frame, text = "🔄 Update Variable List", command = refresh_vars, font = default_font), fill = "x", pady = 2)
    tkpack(tkbutton(btn_frame, text = "❌ Close", command = function() tkdestroy(tt), font = default_font), fill = "x", pady = 5)
    
    return(invisible(NULL))
}
