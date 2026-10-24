# 사용법: source("table22.R"); table22("기술통계.txt"); table22("상관계수.txt")
# 워킹디렉토리에 입력파일명과 같은 이름의 .xlsx 파일을 만든다.
# 파일 형태를 자동 판별한다.
#  - 상관계수: 하삼각행렬(행마다 값의 개수가 다름) -> 상삼각은 빈칸
#  - 기술통계: 변수별 행, 통계량별 열
#  - 회귀분석(lm summary): Coefficients 표 + R^2, F-value, p-value
table22 <- function(file) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  library(openxlsx)

  raw <- readLines(file, warn = FALSE, encoding = "UTF-8")
  out_file <- paste0(tools::file_path_sans_ext(basename(file)), ".xlsx")

  if (any(grepl("^Coefficients:", raw))) {
    return(invisible(table22_reg(raw, out_file)))
  }

  lines <- trimws(raw)
  lines <- lines[nzchar(lines)]
  fields <- strsplit(lines, "[[:space:]]+")
  header <- fields[[1]]
  rows   <- fields[-1]
  nval   <- vapply(rows, length, 1L) - 1L   # 행 이름을 뺀 값의 개수

  if (!all(nval == length(header))) {
    # 상관계수 형태: i번째 행은 i개의 값만 있음
    sheet <- "상관계수"
    dat <- matrix("", length(rows), length(header),
                  dimnames = list(NULL, header))
    for (i in seq_along(rows)) dat[i, seq_len(nval[i])] <- rows[[i]][-1]
    dat <- data.frame(변수 = vapply(rows, `[`, "", 1), dat,
                      check.names = FALSE, stringsAsFactors = FALSE)
    for (j in seq_along(header) + 1L) {
      v <- dat[[j]]
      dat[[j]] <- ifelse(v == "", NA, suppressWarnings(as.numeric(v)))
    }
  } else {
    # 기술통계 형태
    sheet <- "기술통계"
    dat <- read.table(file, header = TRUE, check.names = FALSE)
    dat <- cbind(변수 = rownames(dat), dat)
    rownames(dat) <- NULL
  }

  wb <- createWorkbook()
  addWorksheet(wb, sheet)
  writeData(wb, sheet, dat, headerStyle = createStyle(
    textDecoration = "bold", halign = "center", fgFill = "#D9E1F2",
    border = "TopBottom"))
  setColWidths(wb, sheet, cols = 1:ncol(dat), widths = "auto")
  freezePane(wb, sheet, firstRow = TRUE, firstCol = TRUE)

  saveWorkbook(wb, out_file, overwrite = TRUE)
  cat("저장 완료:", file.path(getwd(), out_file), "\n")
  invisible(out_file)
}

# 회귀분석 출력 -> 엑셀
table22_reg <- function(raw, out_file) {
  library(openxlsx)
  raw <- trimws(raw)
  s <- grep("^Coefficients:", raw)
  e <- grep("^---", raw)
  e <- e[e > s][1]
  hdr  <- c("", "Estimate", "Std. Error", "t value", "Pr(>|t|)", "")
  body <- raw[(s + 2):(e - 1)]

  tab <- do.call(rbind, lapply(body, function(l) {
    f <- strsplit(l, "[[:space:]]+")[[1]]
    c(f[1:5], if (length(f) >= 6) f[6] else "")
  }))
  dat <- data.frame(tab[, 1], lapply(2:5, function(j) as.numeric(tab[, j])),
                    tab[, 6], stringsAsFactors = FALSE)
  names(dat) <- hdr

  r2 <- as.numeric(sub(".*Multiple R-squared:\\s*([0-9.eE+-]+).*", "\\1",
                       grep("Multiple R-squared", raw, value = TRUE)))
  fl <- grep("F-statistic", raw, value = TRUE)
  fv <- as.numeric(sub("F-statistic:\\s*([0-9.eE+-]+).*", "\\1", fl))
  pv <- as.numeric(sub(".*p-value:\\s*([<0-9.eE+-]+).*", "\\1", fl))

  sheet <- "회귀분석"
  wb <- createWorkbook()
  addWorksheet(wb, sheet)
  writeData(wb, sheet, dat, headerStyle = createStyle(
    textDecoration = "bold", halign = "center", fgFill = "#D9E1F2",
    border = "TopBottom"))
  r <- nrow(dat) + 2                       # 표 바로 아래 행
  writeData(wb, sheet, "R^2", startCol = 1, startRow = r)
  writeData(wb, sheet, r2,    startCol = 2, startRow = r)
  writeData(wb, sheet, "F-value", startCol = 1, startRow = r + 1)
  writeData(wb, sheet, fv,        startCol = 2, startRow = r + 1)
  writeData(wb, sheet, "p-value", startCol = 4, startRow = r + 1)  # 한 칸 띄움
  writeData(wb, sheet, pv,        startCol = 5, startRow = r + 1)
  setColWidths(wb, sheet, cols = 1:6, widths = "auto")

  saveWorkbook(wb, out_file, overwrite = TRUE)
  cat("저장 완료:", file.path(getwd(), out_file), "\n")
  out_file
}
