chk22 <- function (data, exclude = NULL) 
{
    # --- [1] 도움말 및 사용법 출력 (입력 데이터 없을 시) ---
    if (base::missing(data)) {
        cat("   ", "\n")
        cat("  \033[1;33m사용예: chk22(data)  #조사를 위한 dataset 지정 \033[0m", "\n")
        cat("  \033[1;33m사용예: chk22(data, exclude=c(변수1, 변수2)) #따옴표 없이 변수명 입력 가능 \033[0m", "\n")
        cat("  \033[1;33m사용예: chk22(data, exclude=c(\"V1\", \"V2\")) #기존 방식(따옴표)도 가능 \033[0m", "\n")
        cat("  \033[1;33m#------------------------------------ \033[0m", "\n")
        cat("  \033[1;33m        \033[0m", "\n")
        cat("  \033[1;33m# numeric cell이 아닌 것을 모두 NA로 변경하기\033[0m", "\n")
        cat("  \033[1;32m data <- data %>% \033[0m", "\n")
        cat("  \033[1;32m       mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.))))) \033[0m", "\n")
        cat("  \033[1;33m \033[0m", "\n")
        return(invisible(NULL))
    }

    # --- [2] 데이터 프레임 변환 ---
    if (is.data.frame(data)) {
        df_check <- data
    } else {
        var_name <- deparse(substitute(data))
        if (length(var_name) > 1) 
            var_name <- "Vector"
        df_check <- data.frame(data, stringsAsFactors = FALSE)
        names(df_check) <- var_name
    }

    # --- [3] 제외할 변수 처리 (따옴표 없는 입력 지원 추가) ---
    # 사용자가 exclude 인자를 입력했는지 확인 (!missing 사용)
    if (!missing(exclude)) {
        
        # 1. 사용자의 입력을 표현식으로 캡처 (예: c(V1, V2))
        ex_expr <- substitute(exclude)
        
        # 2. 표현식을 문자열 벡터로 변환
        # c(V1, V2) -> c("c", "V1", "V2") 형태로 변환됨
        ex_str <- as.character(ex_expr)
        
        # 3. "c" 함수 호출 부분이 포함되어 있다면 제거하여 변수명만 남김
        if (length(ex_str) > 1 && ex_str[1] == "c") {
            target_excludes <- ex_str[-1]
        } else {
            target_excludes <- ex_str
        }
        
        # 4. 실제 컬럼 제외 로직 수행
        existing_cols <- names(df_check)
        
        # 입력된 변수명 중 실제 데이터에 있는 것만 제외 (오타 방지용 intersect 권장이나, 여기선 setdiff로 바로 처리)
        target_cols <- setdiff(existing_cols, target_excludes)
        
        if (length(target_cols) == 0) {
            cat(">>> [경고] 모든 변수가 제외되었습니다. 검사할 항목이 없습니다.\n")
            return(NULL)
        }
        df_check <- df_check[, target_cols, drop = FALSE]
    }

    # --- [4] 비수치 데이터 검사 ---
    report <- data.frame(Row = integer(), Column = character(), 
        Value = character(), stringsAsFactors = FALSE)
        
    for (col in names(df_check)) {
        orig <- as.character(df_check[[col]])
        num_val <- suppressWarnings(as.numeric(orig))
        bad_idx <- which(is.na(num_val) & !is.na(orig))
        
        if (length(bad_idx) > 0) {
            report <- rbind(report, data.frame(Row = bad_idx, 
                Column = col, Value = orig[bad_idx], stringsAsFactors = FALSE))
        }
    }

    # --- [5] 결과 출력 ---
    if (nrow(report) > 0) {
        report <- report[order(report$Row, report$Column), ]
        disp <- report
        disp$Row <- as.character(disp$Row)
        
        if (nrow(disp) > 1) {
            for (i in 2:nrow(disp)) {
                if (report$Row[i] == report$Row[i - 1]) {
                  disp$Row[i] <- ""
                }
            }
        }
        cat(paste0(">>> [발견] 수치가 아닌 값 ", nrow(report), 
            "개 (행 기준 정렬) <<<\n"))
        
        return(disp)
    } else {
        cat(">>> [정상] 제외된 열을 뺀 나머지 모든 데이터가 수치 변환 가능합니다.\n")
        return(NULL)
    }
}
