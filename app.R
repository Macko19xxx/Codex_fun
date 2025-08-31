# Lotto analysis Shiny app
if (dir.exists("C:/R_project/Codex_fun")) {
  setwd("C:/R_project/Codex_fun")
}
suppressPackageStartupMessages({
  library(shiny)
  library(httr)
  library(jsonlite)
})

#' Load last 50 lotto results
#' Attempts to download official data. If that fails, falls back to bundled sample data.
load_lotto_results <- function() {
  url <- "https://www.lotto.pl/api/lotteries/draw-results/lotto/last/50"
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
  read.csv("data/lotto_last50.csv")
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
lotto_app <- function(data = load_lotto_results()) {
  freq <- calculate_frequency(data)
  ui <- fluidPage(
    titlePanel("Lotto frequency analyzer"),
    sidebarLayout(
      sidebarPanel(actionButton("suggest", "Suggest numbers")),
      mainPanel(
        tableOutput("freqTable"),
        verbatimTextOutput("suggested")
      )
    )
  )
  server <- function(input, output, session) {
    output$freqTable <- renderTable({
      data.frame(
        Number = as.integer(names(freq)),
        Frequency = as.integer(freq)
      )
    })
    observeEvent(input$suggest, {
      nums <- sort(suggest_numbers(freq))
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
