## Load Required Packages
library(dplyr)
library(plotly)

## Data work
# Import data from NASA
url <- "https://data.giss.nasa.gov/gistemp/"
giss_raw <- read.csv(paste0(url, "tabledata_v4/GLB.Ts+dSST.csv"), skip = 1)

# Clean data
giss_avg <- as.data.frame(sapply(giss_raw[, 1:13], as.numeric))
giss_cln <- giss_avg |>
  tidyr::pivot_longer(cols = !Year, names_to = "month") |>
  rename(year = Year, month_lbl = month) |>
  mutate(
    month = match(tolower(month_lbl), tolower(month.abb)),
    date = as.Date(paste(year, month, "01", sep = "-"))
  ) |>
  select(year, date, month, month_lbl, value)


## Plot data
# Heatmap
p1 <- giss_cln |>
  plot_ly(
    x =~ year, y =~ month_lbl, z =~ value,
    type = "heatmap", colors = "inferno"
  ) |>
  layout(
    yaxis = list(
      title = "",
      categoryorder = "array",
      categoryarray = unique(giss_cln$month_lbl),
      autorange = "reversed"
    ),
    margin = c(0, 0, 0, 0)
  )

# Time Series
p2 <- giss_cln |>
  plot_ly(
    x =~ date, y =~ value, color =~ "black",
    type = "scatter", mode = "line", colors = "black"
  ) |>
  layout(
    xaxis = list(showticklabels = FALSE),
    margin = c(0, 0, 0, 0)
  )

# Final subplot
ttl <- "Global Temperature Anomaly"
subttl <- " - Difference from Long-Term Mean (\u00B0C)"
fig <- subplot(p1, p2, nrows = 2, heights = c(0.75, 0.25)) |>
  hide_colorbar() |>
  layout(
    title = list(
      text = paste(ttl, subttl, sep = "\n"),
      x = 0.05,
      y = 0.9
    ),
    margin = list(l = 50, r = 50, b = 50, t = 100, pad = 5),
    annotations = list(
      text = paste("Source:", url),
      xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "auto",
      x = 1, y = 0, showarrow = FALSE
    ),
    showlegend = FALSE, margin = 0.01
  )

# Print final plot
fig
# Need to manually save plot to png.
# png plot dims: w = 1255; h = 668
