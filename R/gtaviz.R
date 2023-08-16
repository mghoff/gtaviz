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
  dplyr::rename(year = Year, month_lbl = month) |>
  dplyr::mutate(
    month = match(tolower(month_lbl), tolower(month.abb)),
    date = as.Date(paste(year, month, "01", sep = "-"))
  ) |>
  dplyr::select(year, date, month, month_lbl, value)
giss_cln$hover <- with(
  giss_cln, paste("Year: ", year, "<br>Month: ", month_lbl, "<br>Dev.: ", value)
)

## Plot data
# Heatmap
p1 <- giss_cln |>
  plotly::plot_ly(
    x =~ year, y =~ month_lbl, z =~ value, text =~ hover,
    type = "heatmap", colors = "inferno", hoverinfo = "text"
    # width = 750, height = 600
  ) |>
  plotly::layout(
    yaxis = list(
      title = "",
      categoryorder = "array",
      categoryarray = unique(giss_cln$month_lbl),
      autorange = "reversed"
    ),
    margin = c(0, 0, 0, 0),
    showlegend = FALSE
  )

# Time Series
p2 <- giss_cln |>
  plotly::plot_ly(
    x =~ date, y =~ value, text =~ hover,
    hoverinfo = "text", color = "black", colors = "black",
    type = "scatter", mode = "line"
    # width = 1000, height = 600
  ) |>
  plotly::layout(
    xaxis = list(showticklabels = FALSE),
    margin = c(0, 0, 0, 0),
    showlegend = FALSE
  )

# Final subplot
ttl <- "Global Temperature Anomaly"
subttl <- " - Difference from Long-Term Mean (\u00B0C)"
fig <- plotly::subplot(p1, p2, nrows = 2, heights = c(0.7, 0.3)) |>
  plotly::hide_colorbar() |>
  plotly::layout(
    title = list(
      x = 0.05, y = 0.9,
      text = paste(ttl, subttl, sep = "\n")
    ),
    annotations = list(
      text = paste("Source: ", url),
      xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "auto",
      x = 1, y = 0, showarrow = FALSE
    ),
    margin = list(l = 50, r = 50, b = 50, t = 100, pad = 5),
    showlegend = FALSE
  )
