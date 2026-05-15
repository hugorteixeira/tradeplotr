<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R](https://img.shields.io/badge/R-%E2%89%A54.5-blue)](https://www.r-project.org/)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

<h1 align="center">tradeplotr 📊✨</h1>

<p align="center">
  <strong>Forge interactive performance charts for financial assets and backtesting</strong>
</p>

<p align="center">
  <em>Simple. Modular. Experimental.</em>
</p>

## 🎯 What is tradeplotr?

`tradeplotr` is a cutting-edge R library for creating sophisticated trading performance visualizations. Forge beautiful, interactive charts for financial assets, trading strategies, and portfolio analysis with minimal code. Designed for quants and algo traders who demand both functionality and aesthetics.

Built for the **backtestforge** ecosystem, with native support for quantstrat strategies, native backtest objects, and xts time series objects.

> ⚠️ **Experimental**: This package is in active development. Expect bugs and API changes.

### 🌟 Features

- **One-command plotting**: Single function `tplot()` creates comprehensive visualizations
- **Interactive charts**: Rich, dynamic visualizations in HTML format
- **JSON export**: Core data export for custom processing
- **Theme engine**: Light, dark, and custom themes with full styling control
- **Quantstrat native**: Direct plotting of trading strategy results (equity curves, trade markers, etc.)
- **Native backtest input**: Pass a backtest object directly when it contains `rets$Discrete` or `rets$Log`
- **Backtest diagnostics**: Optional costs/friction and trade-quality modules when the object carries trade details
- **Rolling correlation**: Compare how multiple assets or backtests correlate through time
- **backtestforge integration**: Part of a complete R-based backtesting ecosystem
- **xts optimized**: Built from the ground up for xts time series objects
- **Modular architecture**: Highly extensible chart components

## 📊 Chart Types

### 📉 Quantstrat Strategy Charts
Visualize complete trading strategies with equity curves, trade markers, position sizing, and performance metrics. Native support for quantstrat portfolios with automatic detection of trades, positions, and performance data.

### 🔄 Rolling Returns & Risk Metrics
Analyze performance consistency with dynamic rolling windows. Visualize volatility, Sharpe ratios, and other risk-adjusted metrics across time horizons.

### 📋 Performance Analytics Tables
Comprehensive performance statistics in interactive tables. Alpha, beta, maximum drawdown, volatility, and more - all in a clean, exportable format.

### 📈 Correlation & Portfolio Analysis
Visualize relationships between assets and strategies. Understand diversification benefits, correlation matrices, and portfolio composition.

## 🎨 Custom Themes

Personalize your charts with custom themes:
- Light theme for bright environments
- Dark theme for low-light work
- Create your own custom themes

### Creating Custom Themes

Create custom themes with the `theme_` functions:

```r
# Example custom theme
my_theme <- function() {
  modifyList(dark_theme(), list(
    palette = c("#00d175", "#5991ff", "#ffa600", "#ff4d4d", "#8a2be2"),
    font_family = "Inter",
    font_sizes  = list(axis = 12, title = 14, legend = 12, table = 12),
    colors = modifyList(dark_theme()$colors, list(
      title_txt = "#FFFFFF",
      page_bg   = "#1E1E1E",
      chart_bg  = "#1E1E1E"
    )),
    footer_text = "Made with tradeplotr"
  ))
}

tplot("AAPL", theme = my_theme())
```

## 🚀 Quick Start

### 📦 Installation

```r
# Install from GitHub (development version)
install.packages("remotes")
remotes::install_github("hugorteixeira/tradeplotr")

# Recommended for crisp images
install.packages("ragg")
```

⚠️ **Note**: This is experimental software. Use with caution.

### 🏃 Usage

Start with the `tplot()` function:

```r
library(tradeplotr)

# Simple chart of a ticker
tplot("AAPL")

# Interactive viewer with benchmark
tplot("AAPL", "SPY", format = "viewer")

# Save as HTML report
path_html <- tplot("AAPL", format = "html", output_dir = "plots")
cat("Saved:", path_html, "\n")

# Quantstrat strategy visualization
tplot("my_portfolio_name", format = "viewer")  # Automatically detects strategy data

# Native backtest object with $rets
tplot(tick_a, format = "viewer")

# Add a grouped line from all selected tickers
tplot("AAPL", "SPY", "QQQ", group_lines = "all")

# Group only selected ticker positions; repeated positions act as weights
tplot("AAPL", "SPY", "QQQ", "IWM", group_lines = c(2, 2, 4))

# Risk-normalize regular tickers only
tplot("AAPL", "SPY", "QQQ", normalize_risk = 10)

# Keep regular tickers raw, but risk-normalize the grouped line to 10% vol
tplot("AAPL", "SPY", "QQQ", group_lines = "all", normalize_group_risk = 10)

# With custom theme
tplot("AAPL", theme = dark_theme())
```

### 📊 Output Formats

| Format    | Description                              | Output                    |
|-----------|------------------------------------------|---------------------------|
| `viewer`  | Interactive view in RStudio/browser      | Opens in viewer           |
| `html`    | Self-contained HTML file                 | Returns file path         |
| `json`    | Core data as JSON                        | Returns JSON data         |
| `png`     | Static image report                      | Returns file path(s)      |
| `jpg`     | Static image report                      | Returns file path(s)      |

### Internal Organization

The public API is intentionally small: `tplot()` and the theme helpers. Internally, the code is split by responsibility:

- input/source detection: `R/input-sources.R`
- return preparation and risk normalization: `R/data-prepare.R`, `R/returns-risk.R`
- report assembly: `R/tplot-prepare.R`
- renderers: `R/render.R`, `R/json-utils.R`
- chart modules: `R/modules-tables.R`, `R/modules-market.R`, `R/modules-performance.R`
- themes: `R/themes.R`

### Native Backtest Objects

`tplot()` can read a backtest-like list directly. It looks for returns in this order:

1. `obj$rets`
2. `obj$rets_acct`
3. `obj$raw_rets`

Inside the chosen return object, `Discrete` is preferred. If only `Log` exists, it is converted to discrete returns with `exp(Log) - 1`. When the object also includes `symbol`, `stats`, `trades`, `mktdata`, or `info_blocks`, tradeplotr uses those fields for the asset label, candle trade markers, position chart, and a small hover info marker in the stats table.

Backtest objects can also unlock extra modules:

- `costs`: summarizes fees, slippage, total friction, net/gross P&L, and cost impact.
- `trade_quality`: summarizes win rate, profit factor, R multiples, MFE/MAE, and plots MFE vs final R per trade when excursions are available.
- `rolling_corr`: plots rolling pairwise correlation for multi-series reports. Series with different native frequencies are compared after the normal return-preparation step, which aggregates intraday data to a common compatible period.

### Risk Normalization and Grouped Lines

`normalize_risk` scales only the regular ticker lines. Use it when you want all
input series displayed at the same annualized volatility target:

```r
tplot("AAPL", "SPY", "QQQ", normalize_risk = 10)
```

`group_lines` can add a synthetic return line called `Grupo de Tickers`. It
averages the selected series in log-return space and adds the result as an extra
series. Use `group_lines = "all"` for all tickers, or a numeric vector of
1-based ticker positions for a subset. Duplicates are preserved, so
`group_lines = c(2, 2, 6)` gives ticker 2 twice the weight of ticker 6.

`normalize_group_risk` scales only the grouped line. This lets you keep regular
tickers raw while making `Grupo de Tickers` comparable at a chosen risk target:

```r
tplot("AAPL", "SPY", "QQQ", group_lines = "all", normalize_group_risk = 10)
```

For large candle datasets, the HTML renderer caps chart payloads before sending them to Highcharts. You can tune this with:

```r
options(tplot.max_candles = 6000)
options(tplot.max_volume_points = 6000)
options(tplot.max_position_points = 6000)
```

### 🔄 Ecosystem Integration

Part of the complete **backtestforge** ecosystem for professional algorithmic trading development:

- **Quantstrat native**: Direct visualization of strategy results with trade markers and performance metrics
- **Portfolio analysis**: Complete portfolio performance and risk analytics
- **Backtesting workflow**: End-to-end strategy development, testing, and visualization
- **xts optimization**: Full compatibility with xts time series objects

## 🛠️ Current Status

This package is actively under development:
- 🔄 Early-stage experimental library
- 🐛 Expect bugs and API changes
- 📈 Core functionality in place
- 📚 More documentation coming

## 🤝 Contributing

This is an experimental project in active development. Feedback and contributions are welcome!

## 📄 License

GPL-3 © Hugo Rzepian Teixeira

---

<p align="center">
  <em>Forging better financial visualizations 🧰✨</em>
</p>
