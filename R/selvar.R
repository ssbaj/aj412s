selvar <- function (df_obj) 
{
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
    click_order <- tclVar("")
    ordered_indices <- c()
    
    tt <- tktoplevel()
    tkwm.title(tt, "Variable Selector")
    # tkwm.geometry(tt, "350x650") # 버튼이 추가되어 잘릴 수 있으므로 크기 자동 조절을 위해 주석 처리/삭제
    tcl("wm", "attributes", tt, "-topmost", TRUE)
    default_font <- tkfont.create(family = "helvetica", size = 10)
    
    tkpack(tklabel(tt, text = paste("DATA FRAME :", df_name), 
        fg = "black"), pady = 5)
        
    list_frame <- tkframe(tt)
    tkpack(list_frame, fill = "both", expand = TRUE, padx = 10, pady = 5)
    
    tl <- tklistbox(list_frame, height = 15, selectmode = "multiple", 
        background = "white", font = default_font, highlightthickness = 1)
    scr <- tkscrollbar(list_frame, repeatinterval = 5, command = function(...) tkyview(tl, ...))
    tkconfigure(tl, yscrollcommand = function(...) tkset(scr, ...))
    tkpack(tl, side = "left", fill = "both", expand = TRUE)
    tkpack(scr, side = "right", fill = "y")
    
    on_select <- function() {
        current_sel <- as.numeric(tkcurselection(tl))
        if (length(current_sel) == 0) {
            ordered_indices <<- c()
        }
        else if (length(current_sel) > length(ordered_indices)) {
            new_idx <- setdiff(current_sel, ordered_indices)
            ordered_indices <<- c(ordered_indices, new_idx)
        }
        else if (length(current_sel) < length(ordered_indices)) {
            ordered_indices <<- intersect(ordered_indices, current_sel)
        }
    }
    tkbind(tl, "<<ListboxSelect>>", on_select)
    
    refresh_vars <- function() {
        if (!exists(df_name, envir = .GlobalEnv)) {
            tkmessageBox(message = "DataFrame not found", icon = "error")
            return()
        }
        current_df <- get(df_name, envir = .GlobalEnv)
        vars_raw <- colnames(current_df)
        if (is.null(vars_raw)) 
            vars_raw <- names(current_df)
        tkdelete(tl, 0, "end")
        ordered_indices <<- c()
        for (v in vars_raw) {
            tkinsert(tl, "end", paste0("  ", v))
        }
    }
    refresh_vars()
    
    insert_simple <- function() {
        if (length(ordered_indices) > 0) {
            current_df <- get(df_name, envir = .GlobalEnv)
            vars_raw <- if (!is.null(colnames(current_df))) 
                colnames(current_df)
            else names(current_df)
            selected_vars <- vars_raw[ordered_indices + 1]
            var_string <- paste(selected_vars, collapse = " ")
            if (rstudioapi::isAvailable()) 
                rstudioapi::insertText(var_string)
            tkselection.clear(tl, 0, "end")
            ordered_indices <<- c()
        }
    }
    
    insert_lm <- function() {
        if (length(ordered_indices) > 0) {
            current_df <- get(df_name, envir = .GlobalEnv)
            vars_raw <- if (!is.null(colnames(current_df))) 
                colnames(current_df)
            else names(current_df)
            selected_vars <- vars_raw[ordered_indices + 1]
            if (length(selected_vars) == 1) {
                var_string <- selected_vars[1]
            }
            else {
                target <- selected_vars[1]
                features <- paste(selected_vars[-1], collapse = " + ")
                var_string <- paste0(target, " ~ ", features, 
                  ", data = ", df_name)
            }
            if (rstudioapi::isAvailable()) 
                rstudioapi::insertText(var_string)
            tkselection.clear(tl, 0, "end")
            ordered_indices <<- c()
        }
    }

    # === [추가된 부분] 데이터 프레임에서 변수 삭제 함수 ===
    delete_vars <- function() {
        if (length(ordered_indices) > 0) {
            current_df <- get(df_name, envir = .GlobalEnv)
            vars_raw <- if (!is.null(colnames(current_df))) colnames(current_df) else names(current_df)
            selected_vars <- vars_raw[ordered_indices + 1]
            
            # 실수로 삭제하는 것을 방지하기 위한 확인 창
            confirm_msg <- paste0("정말로 다음 변수를 데이터 프레임에서 삭제하시겠습니까?\n\n", 
                                  paste(selected_vars, collapse = ", "))
            ans <- tclvalue(tkmessageBox(message = confirm_msg, icon = "warning", type = "yesno", title = "변수 삭제 확인"))
            
            if (ans == "yes") {
                # 선택된 컬럼들을 NULL로 할당하여 삭제
                current_df[selected_vars] <- NULL
                # 변경된 데이터 프레임을 글로벌 환경에 덮어쓰기
                assign(df_name, current_df, envir = .GlobalEnv)
                cat("\n[Deleted] 다음과 같은 변수가 삭제되었습니다:", paste(selected_vars, collapse = ", "), "\n")
                
                # 리스트 박스 업데이트
                refresh_vars()
            }
        } else {
            tkmessageBox(message = "삭제할 변수를 먼저 선택해주세요.", icon = "info", title = "알림")
        }
    }
    # =======================================================

    btn_frame <- tkframe(tt)
    tkpack(btn_frame, side = "bottom", fill = "x", padx = 10, pady = 10)
    
    tkpack(tkbutton(btn_frame, text = "📄 Simple List (selvar)", 
        command = insert_simple, font = default_font), fill = "x", pady = 2)
        
    tkpack(tkbutton(btn_frame, text = "📈 Regression Formula (lm)", 
        command = insert_lm, font = default_font, background = "#e1f5fe"), 
        fill = "x", pady = 2)
        
    # === [추가된 부분] 삭제 버튼 UI ===
    tkpack(tkbutton(btn_frame, text = "🗑️ Delete Selected Variables", 
        command = delete_vars, font = default_font, background = "#ffebee"), 
        fill = "x", pady = 2)
    # ==================================
        
    tkpack(tkbutton(btn_frame, text = "🔄 Update Variable List", 
        command = refresh_vars, font = default_font), fill = "x", pady = 2)
        
    tkpack(tkbutton(btn_frame, text = "❌ Close", command = function() tkdestroy(tt), 
        font = default_font), fill = "x", pady = 5)
        
    return(invisible(NULL))
}