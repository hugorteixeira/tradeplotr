#' Renders the Performance Statistics Table Module
#' @param stats_df The data.frame with performance statistics.
#' @param asset The name of the main asset.
#' @param benchmarks A character vector with benchmark names.
#' @param theme The theme list object.
#' @param asset_info Optional named list with tooltip text for asset names.
#' @return An HTML object containing the statistics table.
#' @keywords internal
stats_module <- function(stats_df, asset, benchmarks, theme, asset_info = NULL) {
  comeca <- Sys.time()
  pal <- theme$palette
  cl <- theme$colors
  todos <- c(asset, benchmarks)
  asset_cell <- function(value) {
    label <- as.character(value)
    label_safe <- htmltools::htmlEscape(label)
    label_js <- jsonlite::toJSON(label, auto_unbox = TRUE)
    badge_style <- paste0(
      "display:inline-flex;align-items:center;justify-content:center;",
      "width:14px;height:14px;margin-left:5px;border-radius:50%;",
      "font-size:10px;line-height:14px;font-weight:bold;background:", cl$table_header_bg,
      ";color:", cl$table_header_txt, ";border:1px solid rgba(255,255,255,0.35);",
      "vertical-align:middle;"
    )
    eye_button <- paste0(
      " <button type=\"button\" data-tplot-eye=\"", htmltools::htmlEscape(label, attribute = TRUE),
      "\" aria-pressed=\"false\" title=\"Hide asset in charts\" onclick='window.tplotToggleAsset(",
      label_js,
      ");' style=\"", badge_style,
      "padding:0;cursor:pointer;\">",
      "<span style=\"position:relative;width:10px;height:6px;border:1px solid currentColor;",
      "border-radius:50%;display:inline-block;box-sizing:border-box;background:transparent;\">",
      "<span style=\"position:absolute;left:3px;top:1px;width:2px;height:2px;",
      "background:currentColor;border-radius:50%;display:block;\"></span>",
      "</span></button>"
    )
    info <- NULL
    if (!is.null(asset_info) && length(asset_info) && !is.null(asset_info[[label]])) {
      info <- as.character(asset_info[[label]])
      info <- info[!is.na(info) & nzchar(info)]
    }
    if (is.null(info) || !length(info)) {
      return(paste0(label_safe, eye_button))
    }
    tooltip <- htmltools::htmlEscape(paste(info, collapse = "\n"), attribute = TRUE)
    paste0(
      label_safe,
      " <span title=\"", tooltip, "\" style=\"", badge_style,
      "cursor:help;\">i</span>"
      , eye_button
    )
  }
  value_cell <- function(value, is_asset = FALSE) {
    if (is_asset) {
      return(asset_cell(value))
    }
    htmltools::htmlEscape(as.character(value))
  }
  html <- paste0(
    # "<h3 style='font-family:",theme$font_family,
    # ";font-size:",theme$font_sizes$title,"px;font-weight:bold;margin:20px 0;",
    # "color:",cl$title_txt,";'>Performance Stats</h3>",
    # Container with horizontal overflow
    "<div style='width:100%;max-width:100%;margin:0;padding:0;",
    "overflow-x:auto;overflow-y:visible;'>",
    "<table style='min-width:100%;font-family:", theme$font_family,
    ";table-layout:auto;white-space:nowrap;", # changed to nowrap
    "background-color:", cl$page_bg, ";border-collapse:collapse;'>"
  )
  html <- paste0(html, "<tr>")
  for (coluna in colnames(stats_df)) {
    coluna_safe <- htmltools::htmlEscape(coluna)
    html <- paste0(
      html,
      "<th style='background-color:", cl$table_header_bg,
      ";color:", cl$table_header_txt,
      ";padding:8px 12px;font-family:", theme$font_family,
      ";font-size:", theme$font_sizes$table, "px;font-weight:bold;",
      "border:1px solid rgba(0,0,0,0.1);'>",
      coluna_safe, "</th>"
    )
  }
  html <- paste0(html, "</tr>")
  for (i in seq_len(nrow(stats_df))) {
    # safe color lookup: match ativo against palette, fallback to first color
    pal_idx <- match(stats_df$Asset[i], todos)
    pal_col <- if (!is.na(pal_idx) && pal_idx <= length(pal)) pal[pal_idx] else pal[1]
    rgb_col <- tryCatch(grDevices::col2rgb(pal_col), error = function(e) matrix(c(200, 200, 200), ncol = 1))
    cor <- paste0("rgba(", paste(rgb_col[, 1], collapse = ","), ",0.2)")
    html <- paste0(html, "<tr style='background-color:", cor, ";'>")
    for (j in seq_len(ncol(stats_df))) {
      cell <- value_cell(stats_df[i, j], colnames(stats_df)[j] == "Asset")
      html <- paste0(
        html,
        "<td style='padding:8px 12px;font-family:", theme$font_family,
        ";font-size:", theme$font_sizes$table,
        "px;color:", cl$table_row_txt,
        ";border:1px solid rgba(0,0,0,0.1);'>", # subtle border
        cell, "</td>"
      )
    }
    html <- paste0(html, "</tr>")
  }
  html <- paste0(html, "</table></div>")
  termina <- Sys.time()
  message(sprintf(
    "Module 'stats' rendered in %.2f seconds.",
    as.numeric(difftime(termina, comeca, units = "secs"))
  ))
  HTML(html)
}

#' Renders the Monthly/Annual Returns Table Module
#' @param returns_tables A list of returns data.frames, one for each asset.
#' @param asset The name of the main asset.
#' @param benchmarks A character vector with benchmark names.
#' @param theme The theme list object.
#' @return An HTML object containing the returns tables.
#' @keywords internal
rentab_table_module <- function(returns_tables, asset, benchmarks, theme) {
  comeca <- Sys.time()
  pal <- theme$palette
  cl <- theme$colors
  todos <- c(asset, benchmarks)
  html <- ""
  for (nome in names(returns_tables)) {
    dados <- returns_tables[[nome]]
    anos <- nrow(dados)
    # dynamic name cell: wrap long names into two lines and reduce font only for the name
    name_cell <- (function(nm) {
      base_style <- paste0(
        "background-color:", cl$table_header_bg,
        ";color:", cl$table_header_txt,
        ";padding:8px 12px;font-family:", theme$font_family,
        ";font-size:", theme$font_sizes$table, "px;font-weight:bold;",
        "border:1px solid rgba(0,0,0,0.1);"
      )
      if (!is.character(nm) || nchar(nm) <= 16) {
        return(paste0('<th style="', base_style, '">', nm, "</th>"))
      }
      # Try to split at underscore near 16 chars; fallback to hard split
      split_pos <- regexpr("_", substr(nm, 8, 20))
      if (split_pos[1] > 0) {
        cut_at <- 7 + split_pos[1] - 1
      } else {
        cut_at <- 16
      }
      first <- substr(nm, 1, cut_at)
      second <- substr(nm, cut_at + 1, nchar(nm))
      small <- max(8, theme$font_sizes$table - 2)
      paste0(
        '<th style="', base_style, '"><div style="white-space:normal;word-break:break-word;line-height:1.1;font-size:', small, 'px;">',
        first, "<br>", second, "</div></th>"
      )
    })(nome)
    header <- paste0(
      name_cell,
      paste(
        sprintf(
          '<th style="background-color:%s;color:%s;padding:8px 12px;font-family:%s;font-size:%dpx;font-weight:bold;border:1px solid rgba(0,0,0,0.1);">%s</th>',
          cl$table_header_bg, cl$table_header_txt,
          theme$font_family, theme$font_sizes$table,
          colnames(dados)
        ),
        collapse = ""
      )
    )
    linhas <- character(anos)
    for (i in seq_len(anos)) {
      # safe color lookup for this table
      pal_idx <- match(nome, todos)
      pal_col <- if (!is.na(pal_idx) && pal_idx <= length(pal)) pal[pal_idx] else pal[1]
      rgb_col <- tryCatch(grDevices::col2rgb(pal_col), error = function(e) matrix(c(200, 200, 200), ncol = 1))
      cor <- paste0("rgba(", paste(rgb_col[, 1], collapse = ","), ",0.2)")
      cel <- sapply(dados[i, ], function(v) {
        if (is.na(v)) {
          sprintf(
            '<td style="padding:8px 12px;font-family:%s;font-size:%dpx;color:%s;border:1px solid rgba(0,0,0,0.1);">-</td>',
            theme$font_family, theme$font_sizes$table, cl$table_row_txt
          )
        } else if (is.character(v)) {
          sprintf(
            '<td style="padding:8px 12px;font-family:%s;font-size:%dpx;color:%s;border:1px solid rgba(0,0,0,0.1);">%s</td>',
            theme$font_family, theme$font_sizes$table, cl$table_row_txt, v
          )
        } else {
          sprintf(
            '<td style="padding:8px 12px;font-family:%s;font-size:%dpx;color:%s;border:1px solid rgba(0,0,0,0.1);">%.2f%%</td>',
            theme$font_family, theme$font_sizes$table, cl$table_row_txt, as.numeric(v)
          )
        }
      })
      linhas[i] <- sprintf(
        '<tr style="background-color:%s;"><td style="padding:8px 12px;font-family:%s;font-size:%dpx;color:%s;border:1px solid rgba(0,0,0,0.1);">%s</td>%s</tr>',
        cor, theme$font_family, theme$font_sizes$table, cl$table_row_txt,
        rownames(dados)[i], paste(cel, collapse = "")
      )
    }
    html <- paste0(
      html, sprintf(
        '
      <div id="tabela-%s" class="tabela-retornos" style="margin:20px 0 0 0;width:100%%;max-width:100%%;overflow-x:auto;overflow-y:visible;">
        <table style="min-width:100%%;font-family:%s;table-layout:auto;white-space:nowrap;border-collapse:collapse;background-color:%s;">
          <tr>%s</tr>%s
        </table>
      </div>',
        nome, theme$font_family, cl$page_bg, header, paste(linhas, collapse = "\n")
      )
    )
  }
  termina <- Sys.time()
  message(sprintf(
    "Module 'returns_table' rendered in %.2f seconds.",
    as.numeric(difftime(termina, comeca, units = "secs"))
  ))
  HTML(html)
}

#' Calculates the Monthly and Annual Returns Table
#' @param series_object An xts object with a single column of returns.
#' @param retornar Logical, whether to return the table.
#' @param geometric Logical, whether to use geometric returns.
#' @return A data.frame with the calendar returns.
#' @keywords internal
rentab_table_calc <- function(series_object, retornar = TRUE, geometric = geometric) {
  return_man_mensal_tabela <- as.data.frame(
    table.CalendarReturns(series_object, digits = 2, geometric = geometric)
  )

  return_man_mensal_tabela[is.na(return_man_mensal_tabela)] <- ""

  for (col in colnames(return_man_mensal_tabela)) {
    return_man_mensal_tabela[[col]] <- sapply(return_man_mensal_tabela[[col]], function(x) {
      if (x != "") {
        paste0(x, "%")
      } else {
        x
      }
    })
  }

  month_names_en <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Total")
  colnames(return_man_mensal_tabela) <- month_names_en

  if (retornar) {
    return(return_man_mensal_tabela)
  }
}

#' Renders the Footer Module
#' @param theme The theme list object.
#' @return An HTML object containing the footer text.
#' @keywords internal
footer_module <- function(theme) {
  comeca <- Sys.time()
  cl <- theme$colors
  ui <- tags$div(
    style = paste0(
      "text-align:center;font-family:", theme$ft_font_family,
      ";font-size:", theme$ft_font_size, ";font-weight:bold;margin:", theme$ft_margin,
      ";color:", cl$footer_txt, ";"
    ),
    theme$footer_text
  )
  termina <- Sys.time()
  message(sprintf(
    "Module 'footer' rendered in %.2f seconds.",
    as.numeric(difftime(termina, comeca, units = "secs"))
  ))
  ui
}
