# mktable.R
# 사용법:  mktable(print(result))
#   -> print(result)의 화면 출력을 표로 변환하여 result_YYYYMMDD_HHMMSS.xlsx 로 저장
#   지원: (1) ┌─┐ 박스 표  (2) 공백 정렬 표(요약통계, 상관행렬, data.frame 등)
#         (3) lm 회귀분석 summary  (+) tibble  (4) glm(logit) summary  (그 외는 줄 단위로 저장)

mktable <- function(x, file = NULL, dir = getwd(), quiet = FALSE) {

    if (base::missing(x)) {
    cat("  \033[1;31m  # ---------------------------------- ", '\n')
    cat("  \033[1;31m  mktable(print(result)) ", '\n')
    cat("  \033[1;31m  print(result)의 화면 출력을 표로 변환하여 result_YYYYMMDD_HHMMSS.xlsx 로 저장 ", '\n')
    cat("  \033[1;31m  # ---------------------------------- ", '\n')
    return( cat("   ", '\n') )  }

  
  
  expr <- substitute(x)
  # 저장 이름: print(result) / result 에서 가장 안쪽 변수명 추출
  nm <- expr
  while (is.call(nm) && length(nm) >= 2) nm <- nm[[2]]
  nm <- if (is.name(nm)) as.character(nm) else "result"
  if (is.null(file)) file <- file.path(dir, paste0(nm, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))

  # 식을 평가하면서 출력 캡처 (print(result)이면 print 출력, 그냥 result이면 print 해서 캡처)
  out <- utils::capture.output({
    v <- withVisible(eval(expr, parent.frame()))
    if (v$visible) print(v$value)
  })
  if (!quiet) cat(out, sep = "\n")

  m <- .mk_parse(out)
  .mk_write_xlsx(m, file)
  if (!quiet) message("저장: ", normalizePath(file, mustWork = FALSE))
  invisible(m)
}

# ---- 파서 -------------------------------------------------------------
.mk_parse <- function(lines) {
  lines <- enc2utf8(sub("\r$", "", lines))
  lines <- lines[nzchar(trimws(lines)) | TRUE]
  txt <- paste(lines, collapse = "\n")
  if (grepl("Std. Error", txt, fixed = TRUE) && grepl("Coefficients:", txt, fixed = TRUE))
    return(.mk_coef(lines))
  if (any(grepl("^# A tibble:", lines))) return(.mk_tibble(lines))
  if (any(grepl("^\\s*[┌├]", lines))) return(.mk_box(lines))
  m <- .mk_ws(lines)
  if (!is.null(m)) return(m)
  .mk_lines(lines)
}

.mk_pad <- function(rows) {
  n <- max(lengths(rows))
  t(vapply(rows, function(r) c(r, rep("", n - length(r))), character(n)))
}

# (1) 박스 표: 여러 줄로 나뉜 셀은 공백으로 이어붙임
.mk_box <- function(lines) {
  rows <- list(); cur <- NULL
  flush <- function() if (!is.null(cur)) { rows[[length(rows) + 1]] <<- cur; cur <<- NULL }
  for (ln in lines) {
    ln <- trimws(ln)
    if (grepl("^[┌├└]", ln)) { flush(); next }
    if (!grepl("^│", ln)) next
    cells <- trimws(strsplit(ln, "│", fixed = TRUE)[[1]][-1])
    if (grepl("│$", ln)) { # 마지막 구분자 뒤는 strsplit이 버림 -> 정상
    }
    if (is.null(cur)) cur <- cells
    else {
      k <- max(length(cur), length(cells))
      cur <- c(cur, rep("", k - length(cur))); cells <- c(cells, rep("", k - length(cells)))
      cur <- trimws(paste(cur, cells))
    }
  }
  flush()
  .mk_pad(rows)
}

# tibble: "# A tibble" 줄, <chr> 타입 줄, 맨 앞 행번호 열, "# ..." 꼬리말 제거
.mk_tibble <- function(lines) {
  lines <- gsub("\033\\[[0-9;]*m", "", lines)
  lines <- lines[nzchar(trimws(lines)) & !grepl("^\\s*#", lines)]
  tok <- lapply(lines, function(l) gsub("`", "", regmatches(l, gregexpr("`[^`]*`|\\S+", l))[[1]]))
  hdr <- tok[[1]]
  rows <- tok[-1]
  rows <- rows[!vapply(rows, function(r) grepl("^<.*>$", r[1]), NA)]          # 타입 줄(<chr> <dbl>) 제거
  .mk_pad(c(list(hdr), lapply(rows, function(r) r[-1])))  # 행번호 열 제거
}

# (2) 공백 정렬 표: 헤더 토큰 수 n, 데이터 행은 (행이름 + 최대 n개 값)
.mk_ws <- function(lines) {
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) < 2) return(NULL)
  tok <- strsplit(trimws(lines), "[ \t]+")
  n <- length(tok[[1]])
  k <- lengths(tok[-1])
  if (!all(k >= 1 & k <= n + 1)) return(NULL)
  rows <- c(list(c("", tok[[1]])),
            lapply(tok[-1], function(r) c(r[1], r[-1])))
  .mk_pad(rows)
}

# (3)(4) lm / glm summary
.mk_coef <- function(lines) {
  hi <- grep("Std. Error", lines, fixed = TRUE)[1]
  hdr <- lines[hi]
  stat <- if (grepl("z value", hdr, fixed = TRUE)) "z-value" else "t-value"
  pn <- regmatches(hdr, regexpr("Pr\\(>\\|[tz]\\|\\)", hdr))
  num <- "[<>]?\\s*-?[0-9.]+(?:[eE][-+]?[0-9]+)?"
  re <- sprintf("^(.*?)\\s+(%s)\\s+(%s)\\s+(%s)\\s+(%s)\\s*([.*]*)\\s*$", num, num, num, num)
  rows <- list(); star <- FALSE
  for (ln in lines[-(1:hi)]) {
    if (!nzchar(trimws(ln)) || grepl("^---", ln)) break
    g <- regmatches(ln, regexec(re, ln, perl = TRUE))[[1]]
    if (length(g) == 0) break
    g <- trimws(g[-1]); if (nzchar(g[6])) star <- TRUE
    rows[[length(rows) + 1]] <- g
  }
  hd <- c("", "Coef", "SE", stat, pn, if (star) "")
  out <- list(hd)
  for (r in rows) out[[length(out) + 1]] <- if (star) r else r[1:5]
  txt <- paste(lines, collapse = "\n")
  grab <- function(p) { g <- regmatches(txt, regexec(p, txt, perl = TRUE))[[1]]; if (length(g)) trimws(g[2]) }
  r2 <- grab("Multiple R-squared:\\s*([-0-9.eE+]+)")
  ar2 <- grab("Adjusted R-squared:\\s*([-0-9.eE+]+)")
  fs <- grab("F-statistic:\\s*([-0-9.eE+]+)")
  fp <- grab("F-statistic:[^\n]*p-value:\\s*([^\n]+)")
  aic <- grab("AIC:\\s*([-0-9.eE+]+)")
  if (!is.null(r2))  out[[length(out) + 1]] <- c("R2", r2)
  if (!is.null(ar2)) out[[length(out) + 1]] <- c("Adjusted R2", ar2)
  if (!is.null(fs))  out[[length(out) + 1]] <- c("F-test", sprintf("%s(p:%s)", fs, sub("^([<>])\\s+", "\\1", fp)))
  if (!is.null(aic)) out[[length(out) + 1]] <- c("AIC", aic)
  .mk_pad(out)
}

# 그 외: 한 줄 = 한 행
.mk_lines <- function(lines) matrix(lines, ncol = 1)

# ---- XLSX 쓰기 (writexl) ------------------------------------------------
# 첫 행은 열 제목, 나머지는 본문. 본문 열이 모두 숫자 형태이면 숫자 셀로 저장
.mk_write_xlsx <- function(m, file) {
  if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
  m <- enc2utf8(m)
  hdr <- m[1, ]
  body <- m[-1, , drop = FALSE]
  num <- "^[<>]?\\s*-?[0-9.]+(?:[eE][-+]?[0-9]+)?$"
  df <- lapply(seq_len(ncol(body)), function(j) {
    v <- trimws(body[, j])
    nz <- v[nzchar(v)]
    if (length(nz) > 0 && all(grepl(num, nz, perl = TRUE)) && !any(grepl("[<>]", nz))) {
      out <- suppressWarnings(as.numeric(v)); out[!nzchar(v)] <- NA; out
    } else {
      v[!nzchar(v)] <- NA; v
    }
  })
  names(df) <- ifelse(nzchar(hdr), hdr, paste0("V", seq_along(hdr)))
  names(df) <- make.unique(names(df))
  writexl::write_xlsx(as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE), file)
}
