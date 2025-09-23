<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R](https://img.shields.io/badge/R-%E2%89%A54.5-blue)](https://www.r-project.org/)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

<p align="center">
  <img src="man/figures/logo.png" alt="rTradingPlots Logo" width="200">
</p>

<h1 align="center">rTradingPlots 📈</h1>

<p align="center">
  <strong>Generate beautiful, interactive performance reports for financial assets and trading strategies</strong>
</p>

<p align="center">
  <img src="man/figures/demo-chart.png" alt="Example Chart" width="600">
</p>

## 🌟 Why rTradingPlots?

Tired of spending hours wiring up plots for your trading strategies? `rTradingPlots` is here to help!

The package centers around a single powerful function, `tplot()`, that **"just shows the chart"** - whatever you throw at it. Whether it's a quantstrat/blotter portfolio backtest, an xts object, a quantmod ticker, or a string with an object name, you get a comprehensive visualization instantly.

### ✨ Key Features

- **Simple defaults**: One function call and you get a full performance view
- **Quantstrat-friendly**: Automatically picks up your blotter/quantstrat data
- **Multiple outputs**: Interactive viewer, standalone HTML, and static PNG/JPG
- **Custom themes**: Light/dark modes and easy theme creation
- **Flexible composition**: Modular design for customizable charts
- **Highcharter integration**: Interactive, engaging visualizations

> ⚠️ **Note**: This is a VERY early-stage software with rough edges. Use with care and common sense.

## 📊 Chart Types

### 📉 Drawdown Analysis
Understand your strategy's risk profile with comprehensive drawdown visualization. See peak-to-trough declines over time to identify periods of loss and recovery.

<p align="center">
  <img src="man/figures/drawdowns.png" alt="Drawdown Analysis" width="600">
</p>

### 🔄 Rolling Returns
Analyze performance consistency with rolling window returns. Identify periods of strong or weak performance across different time horizons.

<p align="center">
  <img src="man/figures/rolling-returns.png" alt="Rolling Returns" width="600">
</p>

### 📋 Returns Tables
Get detailed performance statistics in an easy-to-read tabular format. Perfect for comparing performance across periods or assets.

<p align="center">
  <img src="man/figures/returns-table.png" alt="Returns Table" width="600">
</p>

### 📈 Correlation Matrix
Visualize relationships between different assets or strategies to understand diversification benefits.

<p align="center">
  <img src="man/figures/correlations.png" alt="Correlation Matrix" width="600">
</p>

## 🎨 Custom Themes

Personalize your charts with custom themes. Here's how different themes can transform the look and feel of your visualizations:

<p align="center">
  <img src="man/figures/theme-light.png" alt="Light Theme" width="250">
  <img src="man/figures/theme-dark.png" alt="Dark Theme" width="250">
  <img src="man/figures/theme-colorful.png" alt="Colorful Theme" width="250">
</p>

### Creating Custom Themes

Themes are lightweight lists that control colors, fonts, margins, and chart settings:

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

## 🚀 Quick Start

### 📦 Installation

```r
# Install from GitHub (development version)
install.packages("remotes")
remotes::install_github("hugorteixeira/rTradingPlots")

# Recommended for crisp images
install.packages("ragg")
```

**Requirements**: R >= 4.5 recommended. For quantstrat/blotter integration, install those packages and set up your portfolio as usual.

### 🏃 Usage

The one function you need is `tplot()`:

```r
library(rTradingPlots)

# 1) Quick view of a ticker with a benchmark (interactive viewer)
tplot("AAPL", "SPY", init = "2018-01-01", format = "viewer")

# Or even simpler
tplot("GOOG")

# 2) Standalone HTML report
path_html <- tplot("AAPL", "SPY", init = "2018-01-01", format = "html", output_dir = "tplots")
cat("Saved:", path_html, "\n")

# 3) Static image (PNG/JPG) - fast plots via ggplot2
path_png <- tplot("AAPL", "SPY", init = "2018-01-01", format = "png",  output_dir = "tplots")
path_jpg <- tplot("AAPL", "SPY", init = "2018-01-01", format = "jpg",  output_dir = "tplots")

# 4) With quantstrat/blotter portfolio name
# If a portfolio named "myPort" exists, tplot will try to use its mktdata/trades/returns
tplot("myPort", format = "viewer")
```

### 📊 Output Formats

| Format    | Description                              | Output                         |
|-----------|------------------------------------------|--------------------------------|
| `viewer`  | Interactive view in RStudio/browser      | Opens in viewer                |
| `html`    | Self-contained HTML file                 | Returns file path              |
| `png`     | Static PNG image                         | Saves to `output_dir`          |
| `jpg`     | Static JPG image                         | Saves to `output_dir`          |
| `json`    | Core data as JSON                        | Returns JSON data              |

## 🛠️ Roadmap

- [ ] More modular composition for charts and tables (plug-and-play modules)
- [ ] Performance and layout polish
- [ ] API cleanup and more consistent naming
- [ ] Comprehensive documentation and examples

## 👨‍💻 About the Author

Hi, I'm **Hugo Rzepian Teixeira**! I build tools around trading and backtesting in R to streamline workflow and help iterate on strategies faster. 

If you find `rTradingPlots` useful (or frustrating!), feedback is always welcome.

## 📄 License

[GPL-3](LICENSE.md) © Hugo Rzepian Teixeira

---

<p align="center">
  <strong>Made with ❤️ for the R trading community</strong>
</p>