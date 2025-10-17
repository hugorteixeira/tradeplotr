#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom PerformanceAnalytics Drawdowns Return.annualized Return.cumulative SortinoRatio table.CalendarReturns maxDrawdown
#' @importFrom ggplot2 aes element_rect element_text geom_line ggplot labs scale_color_manual scale_y_continuous theme theme_minimal
#' @importFrom grDevices colorRampPalette dev.off
#' @importFrom grid gpar textGrob
#' @importFrom gridExtra grid.arrange tableGrob
#' @importFrom highcharter JS hchart hc_add_series hc_add_yAxis hc_chart hc_legend hc_navigator hc_plotOptions hc_rangeSelector hc_scrollbar hc_size hc_tooltip hc_xAxis hc_yAxis highchart list_parse2
#' @importFrom htmltools HTML htmlDependencies<- html_print save_html tagAppendChild tagList tags findDependencies
#' @importFrom htmlwidgets onRender
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom scales label_number
#' @importFrom shiny isolate
#' @importFrom stats na.omit sd
#' @importFrom tidyr pivot_longer
#' @importFrom xts apply.monthly first last xts
#' @importFrom zoo coredata index
#' @importFrom lubridate interval
## usethis namespace: end
NULL

# Suppress R CMD check notes for non-standard evaluation
utils::globalVariables(c(".", "value", "series"))
