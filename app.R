# Lotto analysis Shiny app
if (dir.exists("C:/R_project/Codex_fun")) {
  setwd("C:/R_project/Codex_fun")
}
suppressPackageStartupMessages({
  library(shiny)
  library(httr)
  library(jsonlite)
})

#' Load last lotto results
#' Attempts to download official data. If that fails, falls back to bundled sample data.
load_lotto_results <- function(count = 50) {
  url <- sprintf("https://www.lotto.pl/api/lotteries/draw-results/lotto/last/%d", count)
  res <- try(httr::GET(url), silent = TRUE)
  if (!inherits(res, "try-error") && httr::status_code(res) == 200) {
    txt <- httr::content(res, as = "text", encoding = "UTF-8")
    json <- jsonlite::fromJSON(txt)
    if (!is.null(json$items)) {
      draws <- lapply(json$items, function(x) x$results$primary)
      dates <- vapply(json$items, function(x) x$drawDate, character(1))
      df <- as.data.frame(do.call(rbind, draws))
      colnames(df) <- paste0("n", 1:6)
      df$draw_date <- as.Date(dates)
      df <- df[, c("draw_date", paste0("n", 1:6))]
      return(df)
    }
  }
  df <- read.csv("data/lotto_last50.csv")
  if (count < nrow(df)) {
    df <- head(df, count)
  }
  df
}

#' Calculate frequency of drawn numbers
calculate_frequency <- function(df) {
  nums <- as.numeric(unlist(df[, -1]))
  sort(table(nums), decreasing = TRUE)
}

#' Suggest next draw numbers based on frequency
suggest_numbers <- function(freq, n = 6) {
  sample(
    as.integer(names(freq)),
    size = n,
    replace = FALSE,
    prob = as.numeric(freq)
  )
}

#' Create Shiny application
lotto_app <- function() {
  ui <- fluidPage(
    titlePanel("Lotto frequency analyzer"),
    sidebarLayout(
      sidebarPanel(
        numericInput("draws", "Number of recent draws", value = 50, min = 1),
        actionButton("suggest", "Suggest numbers")
      ),
      mainPanel(
        tableOutput("freqTable"),
        plotOutput("freqPlot"),
        verbatimTextOutput("suggested")
      )
    )
  )
  server <- function(input, output, session) {
    data <- reactive(load_lotto_results(input$draws))
    freq <- reactive(calculate_frequency(data()))
    output$freqTable <- renderTable({
      data.frame(
        Number = as.integer(names(freq())),
        Frequency = as.integer(freq())
      )
    })
    output$freqPlot <- renderPlot({
      barplot(
        freq(),
        names.arg = names(freq()),
        xlab = "Number",
        ylab = "Frequency",
        main = paste("Frequency in last", input$draws, "draws")
      )
    })
    observeEvent(input$suggest, {
      nums <- sort(suggest_numbers(freq()))
      output$suggested <- renderText(
        paste("Suggested numbers:", paste(nums, collapse = ", "))
      )
    })
  }
  shinyApp(ui, server)
}

if (interactive()) {
  lotto_app()
}
