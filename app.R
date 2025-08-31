# Lotto analysis Shiny app

suppressPackageStartupMessages({
  library(shiny)
  library(httr)
  library(jsonlite)
  library(ggplot2)
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
  as.integer(names(freq)[seq_len(n)])
}

#' Create Shiny application
lotto_app <- function(data = load_lotto_results()) {
  freq <- calculate_frequency(data)
  ui <- fluidPage(
    titlePanel("Lotto frequency analyzer"),
    sidebarLayout(
      sidebarPanel(actionButton("suggest", "Suggest numbers")),
      mainPanel(
        plotOutput("freqPlot"),
        verbatimTextOutput("suggested")
      )
    )
  )
  server <- function(input, output, session) {
    output$freqPlot <- renderPlot({
      df <- data.frame(number = as.integer(names(freq)),
                       count = as.vector(freq))
      ggplot(df, aes(number, count)) +
        geom_col(fill = "steelblue") +
        theme_minimal() +
        labs(x = "Number", y = "Frequency")
    })
    observeEvent(input$suggest, {
      nums <- suggest_numbers(freq)
      output$suggested <- renderText(paste("Suggested numbers:",
                                           paste(nums, collapse = ", ")))
    })
  }
  shinyApp(ui, server)
}

if (interactive()) {
  lotto_app()
}
