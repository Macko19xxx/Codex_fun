# Codex_fun

Simple repository demonstrating a Lotto analysis app written in R. The app lets you choose how many recent Lotto draws to analyze, lists number frequencies from most to least common, plots their distribution, and suggests the most likely numbers for the next draw.

## Running

1. Ensure required packages are installed: `shiny`, `httr`, `jsonlite`.
2. Start the app with:
   ```r
   shiny::runApp()
   ```
3. If the online data source is unavailable, the app falls back to the sample data in `data/lotto_last50.csv` (up to 50 draws).
