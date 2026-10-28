# mygraph.R
# 사용법:
#   source("mygraph.R")
#   mygraph(df)
#
# 팝업(RStudio에서는 dialog, 그 외에는 브라우저)에 변수 리스트가 뜨고,
# x, y 와 그래프 유형(line / scatter / line+scatter / lm / lm+scatter / spline / spline+scatter / ci)을 고르면
# ggplot2 그래프와 그 아래에 해당 ggplot2 R 코드가 표시된다.

for (pkg in c("shiny", "ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

mygraph <- function(df) {
  stopifnot(is.data.frame(df))
  library(shiny)
  library(ggplot2)

  dname <- make.names(deparse(substitute(df))[1])
  if (!grepl("^[A-Za-z.][A-Za-z0-9._]*$", dname) || dname == "") dname <- "df"
  vars  <- names(df)
  types <- c("line", "scatter", "line+scatter", "lm", "lm+scatter",
             "spline", "spline+scatter", "ci")

  # 선택값 -> ggplot2 코드 문자열
  point_types <- c("scatter", "line+scatter", "lm+scatter", "spline+scatter", "ci")

  build_code <- function(x, y, type, method) {
    head <- sprintf("ggplot(%s, aes(x = `%s`, y = `%s`))", dname, x, y)
    layers <- switch(type,
      "line"         = "geom_line()",
      "scatter"      = "geom_point()",
      "line+scatter" = c("geom_line()", "geom_point()"),
      "lm"             = "geom_smooth(method = \"lm\", formula = y ~ x, se = FALSE)",
      "lm+scatter"     = c("geom_point(alpha = 0.5)",
                           "geom_smooth(method = \"lm\", formula = y ~ x, se = FALSE)"),
      "spline"         = "geom_smooth(method = \"lm\", formula = y ~ splines::ns(x, df = 5), se = FALSE)",
      "spline+scatter" = c("geom_point(alpha = 0.5)",
                           "geom_smooth(method = \"lm\", formula = y ~ splines::ns(x, df = 5), se = FALSE)"),
      "ci"           = c("geom_point(alpha = 0.5)",
                         sprintf("geom_smooth(method = \"%s\", formula = y ~ x, level = 0.95, se = TRUE)", method))
    )
    paste(c(head, layers, "theme_minimal()"), collapse = " +\n  ")
  }

  ui <- fluidPage(
    titlePanel(sprintf("mygraph : %s", dname)),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5("변수 리스트"),
        selectInput("x", "X 변수", choices = vars, selected = vars[1]),
        selectInput("y", "Y 변수", choices = vars,
                    selected = vars[min(2, length(vars))]),
        radioButtons("type", "그래프 유형", choices = types),
        conditionalPanel(
          sprintf("[%s].indexOf(input.type) >= 0",
                  paste0("'", point_types, "'", collapse = ", ")),
          checkboxInput("ident", "ident (점을 클릭하면 행번호 보기)", FALSE)
        ),
        conditionalPanel(
          "input.type == 'ci'",
          selectInput("method", "ci 방법", choices = c("lm", "loess"))
        ),
        actionButton("done", "닫기", class = "btn-primary")
      ),
      mainPanel(
        width = 9,
        plotOutput("plot", height = "420px", click = "plot_click"),
        h5("ggplot2 R 코드"),
        verbatimTextOutput("code"),
        h5("선택한 점의 행번호 (ident)"),
        verbatimTextOutput("ident_info")
      )
    )
  )

  server <- function(input, output, session) {
    clicked <- reactiveVal(integer(0))

    # 변수/유형/ident 옵션이 바뀌면 이전 선택을 초기화한다
    observeEvent(list(input$x, input$y, input$type, input$ident),
                 clicked(integer(0)))

    # 클릭한 점과 가장 가까운 점의 행번호를 저장한다
    observeEvent(input$plot_click, {
      req(isTRUE(input$ident), input$type %in% point_types)
      hit <- tryCatch(
        nearPoints(df, input$plot_click, xvar = input$x, yvar = input$y,
                   threshold = 15, maxpoints = 1, allRows = TRUE),
        error = function(e) NULL)
      i <- if (is.null(hit)) integer(0) else which(hit$selected_)
      clicked(i)
    })

    code <- reactive({
      req(input$x, input$y, input$type)
      build_code(input$x, input$y, input$type, input$method %||% "lm")
    })

    output$ident_info <- renderText({
      if (!isTRUE(input$ident)) return("ident 옵션을 체크하고 그래프의 점을 클릭하세요.")
      i <- clicked()
      if (length(i) == 0) return("선택된 점이 없습니다. 점을 클릭하세요.")
      sprintf("행번호: %d   (%s = %s, %s = %s)", i,
              input$x, format(df[[input$x]][i]),
              input$y, format(df[[input$y]][i]))
    })

    output$code <- renderText(code())

    # 화면에 보이는 코드를 그대로 실행하여 그래프를 그린다
    output$plot <- renderPlot({
      env <- new.env(parent = globalenv())
      assign(dname, df, envir = env)
      eval(parse(text = code()), envir = env)
    })

    observeEvent(input$done, stopApp())
  }

  `%||%` <- function(a, b) if (is.null(a)) b else a

  viewer <- tryCatch(dialogViewer("mygraph", width = 1000, height = 800),
                     error = function(e) browserViewer())
  runGadget(ui, server, viewer = viewer)
  invisible(NULL)
}
