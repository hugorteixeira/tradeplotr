# tradeplotr trash shelf
#
# This file is intentionally outside R/ and ignored by R builds. It keeps small
# legacy snippets removed during the 2026-05 cleanup so they are easy to recover
# during the refactor if needed.
#
# Full removed functions can live in separate trash-* files. The old Shiny
# tplot_interactive() implementation is preserved in trash-tplot_interactive.R.
#
# 1) Unused exact/prefix helper that was superseded by the vectorized
# prefix-aware find_col_idx() inside R/data-prepare.R.
#
# find_col_idx_old <- function(cols, base_name) {
#   idx <- grep(paste0("(^|\\.)", base_name, "$"), cols, ignore.case = TRUE)
#   if (length(idx) == 0) NA_integer_ else idx[1]
# }
#
# 2) Duplicate timestamp dedupe removed from volume_module(); the first pass was
# already keeping the last occurrence.
#
# std <- std[!duplicated(index(std), fromLast = TRUE), ]
#
# 3) Unused date label in .tplot_render_html(). It was calculated and then kept
# commented out inside the HTML output.
#
# data_inicial <- format((prep$start_date %||% prep$init_date), "%d-%m-%Y")
# data_final <- format((prep$end_date %||% prep$finit_date), "%d-%m-%Y")
# paste(data_inicial, "a", data_final)
#
# 4) Older .data_prepare return loop. It was fully replaced by the DI-aware
# implementation that follows it.
#
# ativos_data_returns <- ativos_data
# for (i in seq_len(NCOL(ativos_data))) {
#   if (!use_discrete[i]) {
#     r_i <- PerformanceAnalytics::Return.calculate(ativos_data[, i], method = "discrete")
#     if (NROW(r_i) > 0) {
#       fi <- which(!is.na(r_i[, 1]))
#       if (length(fi)) r_i[fi[1], 1] <- 0
#     }
#     ativos_data_returns[, i] <- r_i
#   } else {
#     r_i <- ativos_data[, i, drop = FALSE]
#     fi <- which(!is.na(r_i[, 1]))
#     if (length(fi)) r_i[fi[1], 1] <- 0
#     ativos_data_returns[, i] <- r_i
#   }
# }
#
# 5) Old returns-table pre-aggregation. It summed returns before
# PerformanceAnalytics::table.CalendarReturns(), which is not correct for
# geometric returns.
#
# return_man_mensal <- apply.monthly(series_object, colSums)
# colnames(return_man_mensal) <- "Ano"
# return_man_mensal <- table.CalendarReturns(return_man_mensal, digits = 2, geometric = geometric)
# return_man_mensal_tabela <- as.data.frame(return_man_mensal)
