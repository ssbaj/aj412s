## desc22(df) : 변수 선택 팝업 + 기술통계 출력 + 엑셀 저장
##
##  1) desc22(df) 를 실행하면 팝업이 뜬다.
##     - 왼쪽  : 전체 변수
##     - 오른쪽 위 : "범주형" 변수 박스 / 오른쪽 아래 : "연속형" 변수 박스
##     - 가운데 화살표(→, ←)로 선택한 변수를 이동 (여러 개 선택 가능: Ctrl/Shift+클릭)
##  2) OK 를 누르면 결과창이 뜨고 콘솔에도 같은 내용이 출력된다.
##     - 범주형 : 변수별로 값이 몇 개(개수)이고 몇 %인지
##     - 연속형 : Ndata, NAs, Zero, nonzero, Median, Mean, St.Dev., Q1, Q2, Q3, Min, Max, St.Dev/Mean
##  3) 결과창 위쪽의 엑셀 아이콘(엑셀 저장)을 누르면 .xlsx 파일로 저장된다.
##     (openxlsx 또는 writexl 패키지가 설치되어 있어야 함)
##
##  반환값 : list(categorical=, continuous=)  (눈에 보이지 않게 반환)

## maxlines : 결과 팝업창에 표시할 줄 수(팝업창 높이). 결과가 이보다 길면 잘라서 표시하고
##            마지막 줄에 안내 문구 출력 (콘솔(화면 스크린)에는 항상 전체가 출력됨)
## catcols  : 범주형 변수를 가로로 나란히 출력할 변수 개수 (기본 5)
desc22 <- function(A0data, digitx = 3, maxlines = 35, catcols = 5){
options(scipen = 100)
options(digits = digitx)

if (base::missing(A0data)) {
	return(cat("  desc22(df) \n")) }

if (!requireNamespace("tcltk", quietly = TRUE))
	stop("tcltk 패키지를 사용할 수 없습니다. (Mac은 XQuartz 설치 필요)")
suppressPackageStartupMessages(library(tcltk))

A0data <- as.data.frame(A0data, stringsAsFactors = FALSE)
vars   <- colnames(A0data)
if (length(vars) == 0) return(cat("  변수가 없습니다. \n"))

## ---------------------------------------------------------------
## 계산 함수들
## ---------------------------------------------------------------

## 문자 -> 숫자 코드 (기존 c2n 과 동일)
c2n <- function(x){
	groups <- sort(unique(x))
	as.numeric(factor(x, levels = groups))
}

## 범주형 : 한 변수의 값별 개수 / 비율(NA 제외 유효값 기준)
cat_freq <- function(x, vname){
	## 값의 순서는 원본 형태 기준으로 정렬 (숫자는 1,2,10 순서 / factor는 level 순서)
	groups <- unique(as.character(sort(unique(x[!is.na(x)]))))
	x      <- as.character(x)
	nvalid <- sum(!is.na(x))
	nNA    <- sum(is.na(x))
	cnt    <- as.integer(table(factor(x, levels = groups)))
	pct    <- if (nvalid > 0) round(100 * cnt / nvalid, 2) else rep(NA_real_, length(cnt))
	out <- data.frame(Variable = rep(vname, length(groups)), Value = groups,
	                  Count = cnt, Percent = pct, stringsAsFactors = FALSE)
	if (nNA > 0)
		out <- rbind(out, data.frame(Variable = vname, Value = "(NA)",
		                  Count = nNA, Percent = NA_real_, stringsAsFactors = FALSE))
	out
}

## 연속형 : 기존 출력 양식 유지
cont_desc <- function(D){
	D <- as.data.frame(D, stringsAsFactors = FALSE)
	for (j in seq_len(ncol(D))){
		if (is.factor(D[[j]])) D[[j]] <- as.character(D[[j]])
		if (is.character(D[[j]])) D[[j]] <- c2n(D[[j]])
		D[[j]] <- as.numeric(D[[j]])
	}
	stats <- sapply(D, function(x){
		suppressWarnings({
		c(sum(!is.na(x)),
		  sum(is.na(x)),
		  sum(x == 0, na.rm = TRUE),
		  sum(x != 0, na.rm = TRUE),
		  median(x, na.rm = TRUE),
		  round(mean(x, na.rm = TRUE), digitx),
		  sd(x, na.rm = TRUE),
		  unname(quantile(x, .25, na.rm = TRUE)),
		  unname(quantile(x, .50, na.rm = TRUE)),
		  unname(quantile(x, .75, na.rm = TRUE)),
		  min(x, na.rm = TRUE),
		  max(x, na.rm = TRUE),
		  round(sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE), digitx))
		})
	})
	if (is.null(dim(stats))) stats <- matrix(stats, ncol = 1)
	Bdata <- as.data.frame(t(stats))
	rownames(Bdata) <- colnames(D)
	colnames(Bdata) <- c('  Ndata', '  NAs', '  Zero', '  nonzero', '  Median', '  Mean',
	                     'St.Dev.', 'Q1', 'Q2', 'Q3', '  Min', '  Max', '  St.Dev/Mean')
	Bdata
}

## ---------------------------------------------------------------
## 1단계 : 변수 선택 팝업
## ---------------------------------------------------------------
left  <- vars
catv  <- character(0)
contv <- character(0)
chosen <- NULL

tt <- tktoplevel()
tkwm.title(tt, "desc22 - 변수 선택")

make_box <- function(parent){
	f   <- tkframe(parent)
	scr <- tkscrollbar(f, orient = "vertical")
	lb  <- tklistbox(f, selectmode = "extended", exportselection = FALSE,
	                 height = 8, width = 24,
	                 yscrollcommand = function(...) tkset(scr, ...))
	tkconfigure(scr, command = function(...) tkyview(lb, ...))
	tkpack(lb, side = "left", fill = "both", expand = TRUE)
	tkpack(scr, side = "right", fill = "y")
	list(frame = f, lb = lb)
}
boxAll  <- make_box(tt)
boxCat  <- make_box(tt)
boxCont <- make_box(tt)
tkconfigure(boxAll$lb, height = 18)

refresh <- function(){
	fill <- function(lb, v){
		tkdelete(lb, 0, "end")
		for (s in v) tkinsert(lb, "end", s)
	}
	fill(boxAll$lb, left); fill(boxCat$lb, catv); fill(boxCont$lb, contv)
}
get_sel <- function(lb){
	s <- tclvalue(tkcurselection(lb))
	if (s == "") return(integer(0))
	as.integer(strsplit(s, " ")[[1]]) + 1L
}
move_in <- function(target){            # 전체 -> 범주형/연속형
	idx <- get_sel(boxAll$lb)
	if (length(idx) == 0) return(invisible())
	v <- left[idx]
	left <<- left[-idx]
	if (target == "cat") catv <<- c(catv, v) else contv <<- c(contv, v)
	refresh()
}
move_out <- function(target){           # 범주형/연속형 -> 전체
	lb  <- if (target == "cat") boxCat$lb else boxCont$lb
	idx <- get_sel(lb)
	if (length(idx) == 0) return(invisible())
	if (target == "cat"){ v <- catv[idx];  catv  <<- catv[-idx] }
	else                 { v <- contv[idx]; contv <<- contv[-idx] }
	left <<- vars[vars %in% c(left, v)]     # 원래 순서 유지
	refresh()
}

mid1 <- tkframe(tt); mid2 <- tkframe(tt)
tkpack(tkbutton(mid1, text = "  →  ", command = function() move_in("cat")),  pady = 3)
tkpack(tkbutton(mid1, text = "  ←  ", command = function() move_out("cat")), pady = 3)
tkpack(tkbutton(mid2, text = "  →  ", command = function() move_in("cont")),  pady = 3)
tkpack(tkbutton(mid2, text = "  ←  ", command = function() move_out("cont")), pady = 3)

tkgrid(tklabel(tt, text = "전체 변수"),      row = 0, column = 0, pady = 4)
tkgrid(tklabel(tt, text = "범주형 변수"),    row = 0, column = 2, pady = 4)
tkgrid(boxAll$frame,  row = 1, column = 0, rowspan = 3, padx = 8, sticky = "ns")
tkgrid(mid1,          row = 1, column = 1, padx = 6)
tkgrid(boxCat$frame,  row = 1, column = 2, padx = 8)
tkgrid(tklabel(tt, text = "연속형 변수"),    row = 2, column = 2, pady = 4)
tkgrid(mid2,          row = 3, column = 1, padx = 6)
tkgrid(boxCont$frame, row = 3, column = 2, padx = 8)

btn <- tkframe(tt)
on_ok <- function(){
	if (length(catv) + length(contv) == 0){
		tkmessageBox(parent = tt, title = "desc22", icon = "warning",
		             message = "범주형 또는 연속형 박스로 변수를 이동시키세요.")
		return(invisible())
	}
	chosen <<- list(cat = catv, cont = contv)
	tkdestroy(tt)
}
tkpack(tkbutton(btn, text = "  OK  ",     width = 8, command = on_ok),         side = "left", padx = 10)
tkpack(tkbutton(btn, text = " Cancel ", width = 8, command = function() tkdestroy(tt)), side = "left", padx = 10)
tkgrid(btn, row = 4, column = 0, columnspan = 3, pady = 10)

refresh()
tkfocus(tt)
tkwait.window(tt)                         # 팝업이 닫힐 때까지 대기

if (is.null(chosen)) return(invisible(NULL))

## ---------------------------------------------------------------
## 2단계 : 기술통계 계산
## ---------------------------------------------------------------
Cdata <- NULL     # 범주형 결과 (long format)
if (length(chosen$cat) > 0){
	Cdata <- do.call(rbind, lapply(chosen$cat, function(v) cat_freq(A0data[[v]], v)))
	rownames(Cdata) <- NULL
}
Bdata <- NULL     # 연속형 결과
if (length(chosen$cont) > 0)
	Bdata <- cont_desc(A0data[, chosen$cont, drop = FALSE])

## 화면 출력용 문자열 : 범주형 변수는 가로로 catcols(기본 5)개씩 나란히 출력한 뒤 다음 묶음을 출력
## 각 변수는 [변수명] 아래에 값 / 개수 / 비율(%) 3개 열
cat_display <- function(C, per = catcols){
	padr <- function(s, w) paste0(s, strrep(" ", max(0, w - nchar(s, type = "width"))))
	padl <- function(s, w) paste0(strrep(" ", max(0, w - nchar(s, type = "width"))), s)
	vs <- unique(C$Variable)
	cells <- lapply(vs, function(v){
		s <- C[C$Variable == v, , drop = FALSE]
		list(v   = v,
		     val = c("값", s$Value),
		     cnt = c("개수", as.character(s$Count)),
		     pct = c("비율(%)", ifelse(is.na(s$Percent), "", sprintf("%.2f", s$Percent))))
	})
	out <- character(0)
	for (st in seq(1, length(cells), by = per)){
		grp <- cells[st:min(st + per - 1, length(cells))]
		nr  <- max(sapply(grp, function(g) length(g$val)))
		cols <- lapply(grp, function(g){
			w1 <- max(nchar(g$val, type = "width"))
			w2 <- max(nchar(g$cnt, type = "width"))
			w3 <- max(nchar(g$pct, type = "width"))
			n  <- length(g$val)
			rows <- vapply(seq_len(nr), function(i){
				if (i <= n) paste(padr(g$val[i], w1), padl(g$cnt[i], w2), padl(g$pct[i], w3), sep = "  ")
				else "" }, "")
			title <- paste0("[", g$v, "]")
			w <- max(nchar(rows, type = "width"), nchar(title, type = "width"))
			list(title = padr(title, w), rows = vapply(rows, function(r) padr(r, w), "", USE.NAMES = FALSE))
		})
		out <- c(out,
		         sub("\\s+$", "", paste(sapply(cols, function(k) k$title), collapse = "    ")),
		         sapply(seq_len(nr), function(i)
		                sub("\\s+$", "", paste(sapply(cols, function(k) k$rows[i]), collapse = "    "))),
		         "")
	}
	out
}

out_lines <- character(0)
if (!is.null(Cdata)){
	out_lines <- c(out_lines, "[범주형 변수]",
	               cat_display(Cdata))
}
if (!is.null(Bdata)){
	out_lines <- c(out_lines, "[연속형 변수]", capture.output(print(Bdata)))
}
cat(paste(out_lines, collapse = "\n"), "\n")      # 콘솔 출력

## ---------------------------------------------------------------
## 3단계 : 결과창 + 엑셀 저장 아이콘
## ---------------------------------------------------------------
save_excel <- function(parent){
	f <- tclvalue(tkgetSaveFile(parent = parent, initialfile = "desc22_result.xlsx",
	              defaultextension = ".xlsx", filetypes = "{{Excel 파일} {.xlsx}}"))
	if (f == "") return(invisible())

	## 연속형 시트 (기존 양식 그대로)
	Y <- NULL
	if (!is.null(Bdata)){
		Y <- cbind(Variable = rownames(Bdata), Bdata, stringsAsFactors = FALSE)
		colnames(Y) <- trimws(colnames(Y))
		rownames(Y) <- NULL
	}

	## 범주형 시트 : 팝업창과 같은 규칙 - 변수 catcols(기본 5)개씩 가로로 배치
	##   변수마다 [변수명] / 값 / 개수 / 비율(%) 3열 + 빈 열 1개, 5개 변수를 쓴 뒤 아래쪽에 다음 묶음
	lay <- NULL
	if (!is.null(Cdata)){
		vs   <- unique(Cdata$Variable)
		nrv  <- sapply(vs, function(v) sum(Cdata$Variable == v))
		blk  <- (seq_along(vs) - 1) %/% catcols
		pos  <- (seq_along(vs) - 1) %% catcols
		hts  <- as.numeric(tapply(nrv, blk, max)) + 3          # 제목 + 머리글 + 자료 + 빈 줄
		row0 <- c(1, 1 + cumsum(hts))[blk + 1]
		col0 <- pos * 4 + 1
		tabs <- lapply(vs, function(v){
			t <- Cdata[Cdata$Variable == v, c("Value", "Count", "Percent"), drop = FALSE]
			colnames(t) <- c("값", "개수", "비율(%)")
			rownames(t) <- NULL
			t })
		lay <- list(vs = vs, nrv = nrv, pos = pos, hts = hts, row0 = row0, col0 = col0, tabs = tabs)
	}

	ok <- tryCatch({
		if (requireNamespace("openxlsx", quietly = TRUE)){
			wb <- openxlsx::createWorkbook()
			hs <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DDEBF7")
			ts <- openxlsx::createStyle(textDecoration = "bold")
			if (!is.null(lay)){
				sh <- "범주형"
				openxlsx::addWorksheet(wb, sh)
				for (k in seq_along(lay$vs)){
					openxlsx::writeData(wb, sh, paste0("[", lay$vs[k], "]"),
					                    startCol = lay$col0[k], startRow = lay$row0[k], colNames = FALSE)
					openxlsx::addStyle(wb, sh, ts, rows = lay$row0[k], cols = lay$col0[k])
					openxlsx::writeData(wb, sh, lay$tabs[[k]], startCol = lay$col0[k],
					                    startRow = lay$row0[k] + 1, headerStyle = hs)
				}
				for (ps in sort(unique(lay$pos))){
					ks <- which(lay$pos == ps)
					wv <- max(10, nchar(unlist(lapply(lay$tabs[ks], function(t) t[[1]])), type = "width") + 2)
					openxlsx::setColWidths(wb, sh, cols = ps * 4 + 1, widths = wv)
					openxlsx::setColWidths(wb, sh, cols = ps * 4 + 2, widths = 8)
					openxlsx::setColWidths(wb, sh, cols = ps * 4 + 3, widths = 10)
					openxlsx::setColWidths(wb, sh, cols = ps * 4 + 4, widths = 3)
				}
			}
			if (!is.null(Y)){
				openxlsx::addWorksheet(wb, "연속형")
				openxlsx::writeData(wb, "연속형", Y, headerStyle = hs)
				openxlsx::setColWidths(wb, "연속형", cols = seq_len(ncol(Y)), widths = "auto")
			}
			openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
			TRUE
		} else if (requireNamespace("writexl", quietly = TRUE)){
			sheets <- list()
			if (!is.null(lay)){            # 같은 배치를 문자 격자로 만들어 저장 (숫자도 문자로 저장됨)
				ncolg <- min(catcols, length(lay$vs)) * 4
				grid  <- matrix("", nrow = sum(lay$hts), ncol = ncolg)
				for (k in seq_along(lay$vs)){
					r <- lay$row0[k]; cc <- lay$col0[k]; t <- lay$tabs[[k]]
					grid[r, cc] <- paste0("[", lay$vs[k], "]")
					grid[r + 1, cc + 0:2] <- colnames(t)
					for (i in seq_len(nrow(t)))
						grid[r + 1 + i, cc + 0:2] <- c(t[i, 1], as.character(t[i, 2]),
						                              ifelse(is.na(t[i, 3]), "", sprintf("%.2f", t[i, 3])))
				}
				g <- as.data.frame(grid, stringsAsFactors = FALSE)
				colnames(g) <- strrep(" ", seq_len(ncolg))
				sheets[["범주형"]] <- g
			}
			if (!is.null(Y)) sheets[["연속형"]] <- Y
			writexl::write_xlsx(sheets, f)
			TRUE
		} else {
			tkmessageBox(parent = parent, title = "desc22", icon = "error",
			  message = "엑셀 저장에는 openxlsx 또는 writexl 패키지가 필요합니다.\ninstall.packages(\"openxlsx\") 후 다시 시도하세요.")
			FALSE
		}
	}, error = function(e){
		tkmessageBox(parent = parent, title = "desc22", icon = "error",
		             message = paste("저장 실패:", conditionMessage(e)))
		FALSE
	})
	if (isTRUE(ok))
		tkmessageBox(parent = parent, title = "desc22", message = paste("저장되었습니다:\n", f))
}

res <- tktoplevel()
tkwm.title(res, "desc22 - 기술통계 결과")

## 엑셀 아이콘 (20x20 GIF, base64)
icon_b64 <- "R0lGODdhFAAUAIEAAP///yFzRgAAAAAAACwAAAAAFAAUAEAIXQABCBxIsKDBAAgJBlDIEADChw8bClzoEGLEgRQnTkyIUaLDjRZDWtxIsmDGjxU5ihxZsaRHiitjwpQpEiRNiCU5vgSpUKZNlyg1ppy5M2XPmDlPChV6M6TBp1ADAgA7"
icon <- tryCatch(tkimage.create("photo", data = icon_b64), error = function(e) NULL)

tb <- tkframe(res)
if (!is.null(icon)){
	tkpack(tkbutton(tb, image = icon, text = " 엑셀 저장", compound = "left",
	                command = function() save_excel(res)), side = "left", padx = 6, pady = 4)
} else {
	tkpack(tkbutton(tb, text = "엑셀 저장",
	                command = function() save_excel(res)), side = "left", padx = 6, pady = 4)
}
tkpack(tb, side = "top", fill = "x")

tf   <- tkframe(res)
ysc  <- tkscrollbar(tf, orient = "vertical")
xsc  <- tkscrollbar(tf, orient = "horizontal")
txt  <- tktext(tf, width = 120, height = maxlines, wrap = "none", font = "Courier 10",
               yscrollcommand = function(...) tkset(ysc, ...),
               xscrollcommand = function(...) tkset(xsc, ...))
tkconfigure(ysc, command = function(...) tkyview(txt, ...))
tkconfigure(xsc, command = function(...) tkxview(txt, ...))
tkgrid(txt, row = 0, column = 0, sticky = "nsew")
tkgrid(ysc, row = 0, column = 1, sticky = "ns")
tkgrid(xsc, row = 1, column = 0, sticky = "ew")
tkgrid.rowconfigure(tf, 0, weight = 1)
tkgrid.columnconfigure(tf, 0, weight = 1)
tkpack(tf, side = "top", fill = "both", expand = TRUE)

## 팝업창에 표시할 수 있는 줄 수(maxlines)를 넘으면 앞부분만 표시하고 마지막 줄에 안내 문구 추가
if (length(out_lines) > maxlines){
	popup_lines <- c(out_lines[seq_len(maxlines - 2)],
	                 "",
	                 "※ 전체 결과가 출력되지 않았습니다. 나머지 결과는 화면 스크린을 참조하세요.")
} else {
	popup_lines <- out_lines
}
tkinsert(txt, "end", paste(popup_lines, collapse = "\n"))
tkconfigure(txt, state = "disabled")

invisible(list(categorical = Cdata, continuous = Bdata))
}
