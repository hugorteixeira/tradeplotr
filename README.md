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

Built for the **backtestforge** ecosystem, with native support for quantstrat strategies and xts time series objects.

> ⚠️ **Experimental**: This package is in active development. Expect bugs and API changes.

### 🌟 Features

- **One-command plotting**: Single function `tplot()` creates comprehensive visualizations
- **Interactive charts**: Rich, dynamic visualizations in HTML format
- **JSON export**: Core data export for custom processing
- **Theme engine**: Light, dark, and custom themes with full styling control
- **Quantstrat native**: Direct plotting of trading strategy results (equity curves, trade markers, etc.)
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

# With custom theme
tplot("AAPL", theme = dark_theme())
```

### 📊 Output Formats

| Format    | Description                              | Output                    |
|-----------|------------------------------------------|---------------------------|
| `viewer`  | Interactive view in RStudio/browser      | Opens in viewer           |
| `html`    | Self-contained HTML file                 | Returns file path         |
| `json`    | Core data as JSON                        | Returns JSON data         |

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
