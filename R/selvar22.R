selvar22<-function (df_obj) {
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
    tt <- tktoplevel()
    tkwm.title(tt, paste("      "))
    tkwm.geometry(tt, "350x650")
    tcl("wm", "attributes", tt, "-topmost", TRUE)
    default_font <- tkfont.create(family = "helvetica", size = 10)
    bold_font <- tkfont.create(family = "helvetica", size = 10, 
        weight = "bold")
    tkpack(tklabel(tt, text = paste("DATA FRAME :", df_name), fg = "black"), pady = 5)
    list_frame <- tkframe(tt)
    tkpack(list_frame, fill = "both", expand = TRUE, padx = 10, 
        pady = 5)
    tl <- tklistbox(list_frame, height = 15, selectmode = "multiple", 
        background = "white", font = default_font, highlightthickness = 1)
    scr <- tkscrollbar(list_frame, repeatinterval = 5, command = function(...) tkyview(tl, 
        ...))
    tkconfigure(tl, yscrollcommand = function(...) tkset(scr, 
        ...))
    tkpack(tl, side = "left", fill = "both", expand = TRUE)
    tkpack(scr, side = "right", fill = "y")
    refresh_vars <- function() {
        if (!exists(df_name, envir = .GlobalEnv)) {
            tkmessageBox(message = "DataFrame not found", 
                icon = "error")
            return()
        }
        current_df <- get(df_name, envir = .GlobalEnv)
        vars_raw <- colnames(current_df)
        if (is.null(vars_raw)) 
            vars_raw <- names(current_df)
        tkdelete(tl, 0, "end")
        for (v in vars_raw) {
            tkinsert(tl, "end", paste0("  ", v))
        }
    }
    refresh_vars()
    insert_simple <- function() {
        selection <- as.numeric(tkcurselection(tl))
        if (length(selection) > 0) {
            current_df <- get(df_name, envir = .GlobalEnv)
            vars_raw <- if (!is.null(colnames(current_df))) 
                colnames(current_df)
            else names(current_df)
            selected_vars <- vars_raw[selection + 1]
            var_string <- paste(selected_vars, collapse = " ")
            if (rstudioapi::isAvailable()) 
                rstudioapi::insertText(var_string)
            tkselection.clear(tl, 0, "end")
        }
    }
    insert_lm <- function() {
        selection <- as.numeric(tkcurselection(tl))
        if (length(selection) > 0) {
            current_df <- get(df_name, envir = .GlobalEnv)
            vars_raw <- if (!is.null(colnames(current_df))) 
                colnames(current_df)
            else names(current_df)
            selected_vars <- vars_raw[selection + 1]
            if (length(selected_vars) == 1) {
                var_string <- selected_vars[1]
            }
            else {
                target <- selected_vars[1]
                features <- paste(selected_vars[-1], collapse = " + ")
                var_string <- paste0(target, " ~ ", features, ", data= ")
            }
            if (rstudioapi::isAvailable()) 
                rstudioapi::insertText(var_string)
            tkselection.clear(tl, 0, "end")
        }
    }
    btn_frame <- tkframe(tt)
    tkpack(btn_frame, side = "bottom", fill = "x", padx = 10, 
        pady = 10)
    tkpack(tkbutton(btn_frame, text = "📄 Simple List (selvar)", 
        command = insert_simple, font = default_font, height = 1), 
        fill = "x", pady = 2)
    tkpack(tkbutton(btn_frame, text = "📈 Regression Formula (lm)", 
        command = insert_lm, font = default_font, height = 1, 
        background = "#e1f5fe"), fill = "x", pady = 2)
    tkpack(tklabel(btn_frame, text = ""), pady = 2)
    tkpack(tkbutton(btn_frame, text = "🔄 Update Variable List", 
        command = refresh_vars, font = default_font), fill = "x", 
        pady = 2)
    tkpack(tkbutton(btn_frame, text = "❌ Close", command = function() tkdestroy(tt), 
        font = default_font), fill = "x", pady = 5)
    return(invisible(NULL))
}
