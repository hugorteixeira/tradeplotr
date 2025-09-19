# rTradingPlots

Simple plotting for trading/backtesting workflows in R. The package centers around a single function, `tplot()`, that tries to “just show the chart”, whatever is that you put into it, be a quantstrat/blotter portfolio backtest, an xts object, a string of a name of an object, a quantmod ticker, a rSenhorMercadoAPI ticker (to be released), so you can focus on your ideas instead of wiring up plots.

This is a VERY early-stage software with a lot of rough edges. Use with care and common sense.


## Why tplot()

- Simple defaults: call `tplot()` with a portfolio name, ticker(s), or xts/data.frame series and you get a full performance view (cumulative, periodic returns, drawdowns, stats, optional OHLC - first ticker).
- Quantstrat-friendly: if you pass a portfolio name, `tplot()` tries to pick up your blotter/quantstrat data (mktdata/trades/returns) automatically.
- Multiple outputs: interactive viewer, standalone HTML, and static PNG/JPG (to be improved).
- Themes: ship with light/dark and a few alternative styles, plus an easy way to create your own.
- Future modularity: more flexible composition of sections/modules will be added.
- Highcharter: adds a more interesting way to look at charts (not optimized yet, big datasets are slow)

## Install

```r
# From GitHub (development)
install.packages("remotes")
remotes::install_github("hugorteixeira/rTradingPlots")

# Recommended (for crisp images)
install.packages("ragg")
```

Requirements: R >= 4.5 recommended. For quantstrat/blotter integration, install those packages and set up your portfolio as usual.


## Quick Start

The one function you need is `tplot()`.

```r
library(rTradingPlots)

# 1) Quick view of a ticker with a benchmark (interactive viewer)
tplot("AAPL", "SPY", init = "2018-01-01", format = "viewer")

or even just

tplot("GOOG")

# 2) Standalone HTML report
path_html <- tplot("AAPL", "SPY", init = "2018-01-01", format = "html", output_dir = "tplots")
cat("Saved:", path_html, "\n")

# 3) Static image (PNG/JPG) — fast-ish plots via ggplot2
path_png <- tplot("AAPL", "SPY", init = "2018-01-01", format = "png",  output_dir = "tplots")
path_jpg <- tplot("AAPL", "SPY", init = "2018-01-01", format = "jpg",  output_dir = "tplots")

# 4) With quantstrat/blotter portfolio name
# If a portfolio named "myPort" exists, tplot will try to use its mktdata/trades/returns
tplot("myPort", format = "viewer")
```

You can also pass xts/data.frame objects directly (single- or multi-column). The first series is the main asset; others are benchmarks.

```r
# xts returns series
xts_main <- your_xts_returns
xts_bench <- your_benchmark_returns
tplot(xts_main, xts_bench, init = "2019-01-01", finit = "2024-01-01", format = "viewer")
```

Output options for `format`:

- `"viewer"`: opens an interactive view (RStudio viewer/browser)
- `"html"`: saves a self-contained HTML (returns the file path)
- `"png"`, `"jpg"`: saves a static image to `output_dir` (returns the file path)
- `"json"`: returns core data as JSON (not a chart)

`tplot()` tries to be robust with inputs and will fall back to simpler behaviors when some data are missing.


## Examples

```r
# Basic: main ticker only
tplot("AAPL", init = "2015-01-01", format = "viewer")

# Main + multiple benchmarks
tplot("AAPL", c("SPY", "QQQ"), init = "2018-01-01", finit = Sys.Date(), format = "html")

# Static image with a different theme
tplot("AAPL", "SPY", init = "2018-01-01", format = "png", theme = dark_theme(), output_dir = "tplots")

# Using a blotter/quantstrat portfolio (if loaded in your session)
tplot("myPort", init = "2021-01-01", finit = Sys.Date(), format = "viewer")
```


## Creating a Theme

Themes are lightweight lists that control colors, fonts, margins, and some chart settings. Start by modifying an existing theme:

```r
my_theme <- function(){
  modifyList(dark_theme(), list(
    palette = c("#00d175", "#5991ff", "#ffa600", "#ff4d4d", "#8a2be2"),
    font_family = "Inter",
    font_sizes  = list(axis = 12, title = 14, legend = 12, table = 12),
    colors = modifyList(dark_theme()$colors, list(
      title_txt = "#FFFFFF",
      page_bg   = "#1E1E1E",
      chart_bg  = "#1E1E1E"
    )),
    candles = modifyList(dark_theme()$candles, list(
      point_width = 5
    )),
    footer_text = "Whatever is that you want to write here"
  ))
}

tplot("AAPL", "SPY", format = "viewer", theme = my_theme())
```

Or build one from scratch (must return a list with at least: `palette`, `font_family`, `font_sizes`, `colors`, `hc_margin`, `hc_spacing`, `candles`, `footer_text`). See `R/themes.R` for examples.


## Roadmap (short)

- More modular composition for charts and tables (plug-and-play modules)
- Performance and layout polish
- API cleanup and more consistent naming
- Documentation and examples


## Status and Caveats

This is very much a work in progress. There are a lot of bugs and edge cases. Expect breaking changes. Please file issues with minimal reproducible examples.


## About the Author

Hi, I’m Hugo. I build tools around trading and backtesting in R to streamline workflow and help iterate on strategies faster. If you find rTradingPlots useful (or frustrating!), feedback is welcome.


## License

GPL-3. See `LICENSE.md`.

