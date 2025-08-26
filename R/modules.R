#' Renders the Performance Statistics Table Module
#' @param carteira_df The data.frame with performance statistics.
#' @param ativo The name of the main asset.
#' @param benchs A character vector with benchmark names.
#' @param theme The theme list object.
#' @return An HTML object containing the statistics table.
#' @keywords internal
stats_module <- function(carteira_df, ativo, benchs, theme){
  comeca <- Sys.time()
  pal <- theme$palette; cl <- theme$colors; todos <- c(ativo, benchs)
  html <- paste0(
    #"<h3 style='font-family:",theme$font_family,
    #";font-size:",theme$font_sizes$title,"px;font-weight:bold;margin:20px 0;",
    #"color:",cl$title_txt,";'>Performance Stats</h3>",
    # Container com overflow horizontal
    "<div style='width:100%;max-width:100%;margin:0;padding:0;",
    "overflow-x:auto;overflow-y:visible;'>",
    "<table style='min-width:100%;font-family:",theme$font_family,
    ";table-layout:auto;white-space:nowrap;", # mudado para nowrap
    "background-color:",cl$page_bg,";border-collapse:collapse;'>"
  )
  html <- paste0(html,"<tr>")
  for(coluna in colnames(carteira_df)){
    html <- paste0(
      html,
      "<th style='background-color:",cl$table_header_bg,
      ";color:",cl$table_header_txt,
      ";padding:8px 12px;font-family:",theme$font_family,
      ";font-size:",theme$font_sizes$table,"px;font-weight:bold;",
      "border:1px solid rgba(0,0,0,0.1);'>",
      coluna,"</th>"
    )
  }
  html <- paste0(html,"</tr>")
  for(i in seq_len(nrow(carteira_df))){
    # safe color lookup: match ativo against palette, fallback to first color
    pal_idx <- match(carteira_df$Ativos[i], todos)
    pal_col <- if (!is.na(pal_idx) && pal_idx <= length(pal)) pal[pal_idx] else pal[1]
    rgb_col <- tryCatch(grDevices::col2rgb(pal_col), error = function(e) matrix(c(200,200,200), ncol = 1))
    cor <- paste0("rgba(", paste(rgb_col[,1], collapse = ","), ",0.2)")
    html <- paste0(html,"<tr style='background-color:",cor,";'>")
    for(j in seq_len(ncol(carteira_df))){
      html <- paste0(
        html,
        "<td style='padding:8px 12px;font-family:",theme$font_family,
        ";font-size:",theme$font_sizes$table,
        "px;color:",cl$table_row_txt,
        ";border:1px solid rgba(0,0,0,0.1);'>", # borda sutil
        carteira_df[i,j],"</td>"
      )
    }
    html <- paste0(html,"</tr>")
  }
  html <- paste0(html,"</table></div>")
  termina <- Sys.time()
  message(sprintf("Module 'stats' rendered in %.2f seconds.",
                  as.numeric(difftime(termina, comeca, units = "secs"))))
  HTML(html)
}

#' Renders the Monthly/Annual Returns Table Module
#' @param lista_tabelas A list of returns data.frames, one for each asset.
#' @param ativo The name of the main asset.
#' @param benchs A character vector with benchmark names.
#' @param theme The theme list object.
#' @return An HTML object containing the returns tables.
#' @keywords internal
rentab_table_module <- function(lista_tabelas, ativo, benchs, theme){
  comeca <- Sys.time()
  pal <- theme$palette; cl <- theme$colors; todos <- c(ativo, benchs)
  html <- ""
  for(nome in names(lista_tabelas)){
    dados <- lista_tabelas[[nome]]; anos <- nrow(dados)
    header <- paste0(
      '<th style="background-color:',cl$table_header_bg,
      ';color:',cl$table_header_txt,
      ';padding:8px 12px;font-family:',theme$font_family,
      ';font-size:',theme$font_sizes$table,'px;font-weight:bold;',
      'border:1px solid rgba(0,0,0,0.1);">',
      nome,'</th>',
      paste(
        sprintf(
          '<th style="background-color:%s;color:%s;padding:8px 12px;font-family:%s;font-size:%dpx;font-weight:bold;border:1px solid rgba(0,0,0,0.1);">%s</th>',
          cl$table_header_bg,cl$table_header_txt,
          theme$font_family,theme$font_sizes$table,
          colnames(dados)
        ),
        collapse=""
      )
    )
    linhas <- character(anos)
    for(i in seq_len(anos)){
      # safe color lookup for this table
      pal_idx <- match(nome, todos)
      pal_col <- if (!is.na(pal_idx) && pal_idx <= length(pal)) pal[pal_idx] else pal[1]
      rgb_col <- tryCatch(grDevices::col2rgb(pal_col), error = function(e) matrix(c(200,200,200), ncol = 1))
      cor <- paste0("rgba(", paste(rgb_col[,1], collapse = ","), ",0.2)")
      cel <- sapply(dados[i, ], function(v){
        if(is.na(v)){
          sprintf(
            '<td style="padding:8px 12px;font-family:%s;font-size:%dpx;color:%s;border:1px solid rgba(0,0,0,0.1);">-</td>',
            theme$font_family,theme$font_sizes$table,cl$table_row_txt
          )
        } else if(is.character(v)){
          sprintf(
            '<td style="padding:8px 12px;font-family:%s;font-size:%dpx;color:%s;border:1px solid rgba(0,0,0,0.1);">%s</td>',
            theme$font_family,theme$font_sizes$table,cl$table_row_txt,v
          )
        } else {
          sprintf(
            '<td style="padding:8px 12px;font-family:%s;font-size:%dpx;color:%s;border:1px solid rgba(0,0,0,0.1);">%.2f%%</td>',
            theme$font_family,theme$font_sizes$table,cl$table_row_txt,as.numeric(v)
          )
        }
      })
      linhas[i] <- sprintf(
        '<tr style="background-color:%s;"><td style="padding:8px 12px;font-family:%s;font-size:%dpx;color:%s;border:1px solid rgba(0,0,0,0.1);">%s</td>%s</tr>',
        cor,theme$font_family,theme$font_sizes$table,cl$table_row_txt,
        rownames(dados)[i], paste(cel, collapse="")
      )
    }
    html <- paste0(
      html, sprintf('
      <div id="tabela-%s" class="tabela-retornos" style="margin:20px 0 0 0;width:100%%;max-width:100%%;overflow-x:auto;overflow-y:visible;">
        <table style="min-width:100%%;font-family:%s;table-layout:auto;white-space:nowrap;border-collapse:collapse;background-color:%s;">
          <tr>%s</tr>%s
        </table>
      </div>',
                    nome, theme$font_family, cl$page_bg, header, paste(linhas, collapse="\n")
      ))
  }
  termina <- Sys.time()
  message(sprintf("Module 'rentab_table' rendered in %.2f seconds.",
                  as.numeric(difftime(termina, comeca, units = "secs"))))
  HTML(html)
}

#' Calculates the Monthly and Annual Returns Table
#' @param nome_do_objeto An xts object with a single column of returns.
#' @param retornar Logical, whether to return the table.
#' @param geometric Logical, whether to use geometric returns.
#' @return A data.frame with the calendar returns.
#' @keywords internal
rentab_table_calc <- function(nome_do_objeto, retornar = TRUE, geometric = TRUE) {
  # Calcular os retornos mensais acumulados
  return_man_mensal <- apply.monthly(nome_do_objeto, colSums)
  colnames(return_man_mensal) <- "Ano"
  return_man_mensal <- table.CalendarReturns(return_man_mensal, digits = 2, geometric = geometric)

  return_man_mensal_tabela <- as.data.frame(return_man_mensal)

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

  nomes_meses_pt <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez", "Total")
  colnames(return_man_mensal_tabela) <- nomes_meses_pt

  if (retornar) {
    return(return_man_mensal_tabela)
  }
}

#' Renders the Footer Module
#' @param theme The theme list object.
#' @return An HTML object containing the footer text.
#' @keywords internal
footer_module <- function(theme){
  comeca <- Sys.time()
  cl <- theme$colors
  ui <- tags$div(
    style=paste0(
      "text-align:center;font-family:",theme$ft_font_family,
      ";font-size:",theme$ft_font_size,";font-weight:bold;margin:",theme$ft_margin,
      ";color:",cl$footer_txt,";"
    ),
    theme$footer_text
  )
  termina <- Sys.time()
  message(sprintf("Module 'footer' rendered in %.2f seconds.",
                  as.numeric(difftime(termina, comeca, units = "secs"))))
  ui
}

#' Renders the Candlestick Chart Module with Trades
#' @param mktdata An xts object with OHLC market data.
#' @param txns An xts object with the transactions (trades).
#' @param theme The theme list object.
#' @return A highchart object.
#' @keywords internal
candles_module <- function(mktdata, txns, theme){
  comeca <- Sys.time()
  # Require valid OHLC market data; transactions are optional
  if (is.null(mktdata) || !xts::is.xts(mktdata)) return(NULL)
  # Standardize OHLC column names to ensure hchart candlesticks work reliably
  std <- try(.to_ohlc_standard(mktdata), silent = TRUE)
  if (inherits(std, "try-error") || is.null(std)) return(NULL)
  mktdata <- std
  has_txns <- !is.null(txns) && xts::is.xts(txns) && nrow(txns) > 0

  ## ---- Paleta de cores
  pal  <- theme$palette
  corx <- pal[1]
  cory <- pal[3]
  cl   <- theme$colors

  # Resolve candle visual options from theme with safe fallbacks
  candle_cfg <- if (!is.null(theme$candles)) theme$candles else list()
  cw    <- if (!is.null(candle_cfg$point_width)) candle_cfg$point_width else 4
  cgrp  <- isTRUE(candle_cfg$grouping)
  cup   <- if (!is.null(candle_cfg$up_color))   candle_cfg$up_color   else "#00c176"
  cdown <- if (!is.null(candle_cfg$down_color)) candle_cfg$down_color else "#ff4d4d"
  cline <- if (!is.null(candle_cfg$line_color)) candle_cfg$line_color else "#cccccc"
  clw   <- if (!is.null(candle_cfg$line_width)) candle_cfg$line_width else 1
  # Range selector styles from theme colors
  rs_txt  <- if (!is.null(cl$range_selector_txt))    cl$range_selector_txt    else cl$axis_txt
  rs_fill <- if (!is.null(cl$range_selector_bg))     cl$range_selector_bg     else cl$chart_bg
  rs_stk  <- if (!is.null(cl$range_selector_border)) cl$range_selector_border else cl$axis_txt

  # Candle style with theme-configurable options (provide safe fallbacks)
  candle_cfg <- if (!is.null(theme$candles)) theme$candles else list()
  cw    <- if (!is.null(candle_cfg$point_width)) candle_cfg$point_width else 4
  cgrp  <- isTRUE(candle_cfg$grouping)
  cup   <- if (!is.null(candle_cfg$up_color))   candle_cfg$up_color   else "#00c176"
  cdown <- if (!is.null(candle_cfg$down_color)) candle_cfg$down_color else "#ff4d4d"
  cline <- if (!is.null(candle_cfg$line_color)) candle_cfg$line_color else "#cccccc"
  clw   <- if (!is.null(candle_cfg$line_width)) candle_cfg$line_width else 1
  # Range selector style from theme$colors
  rs_txt  <- if (!is.null(cl$range_selector_txt))    cl$range_selector_txt    else cl$axis_txt
  rs_fill <- if (!is.null(cl$range_selector_bg))     cl$range_selector_bg     else cl$chart_bg
  rs_stk  <- if (!is.null(cl$range_selector_border)) cl$range_selector_border else cl$axis_txt
  # Candle style with theme fallbacks
  candle_cfg <- theme$candles %||% list()
  cw   <- candle_cfg$point_width %||% 4
  cgrp <- candle_cfg$grouping %||% FALSE
  cup  <- candle_cfg$up_color %||% "#00c176"
  cdown<- candle_cfg$down_color %||% "#ff4d4d"
  cline<- candle_cfg$line_color %||% "#cccccc"
  clw  <- candle_cfg$line_width %||% 1
  # Range selector styles
  rs_txt   <- cl$range_selector_txt %||% cl$axis_txt
  rs_fill  <- cl$range_selector_bg  %||% cl$chart_bg
  rs_stk   <- cl$range_selector_border %||% cl$axis_txt

  if (has_txns) {
    buys  <- txns[ txns$Txn.Qty >  0 , ]
    sells <- txns[ txns$Txn.Qty <  0 , ]
  }

  di_flag       <- isDI(mktdata)
  maturity_date <- attr(mktdata, "maturity")
  if (has_txns && di_flag && !is.null(maturity_date)){
    buys$Txn.Price  <- mapply(get_DI_price, buys$Txn.Price, index(buys), MoreArgs = list(maturity = maturity_date))
    sells$Txn.Price <- mapply(get_DI_price, sells$Txn.Price, index(sells), MoreArgs = list(maturity = maturity_date))
  }

  ## ---- Inverte o sentido para DI
  if (has_txns) {
    if (di_flag){
      buys_plot  <- sells
      sells_plot <- buys
    } else{
      buys_plot  <- buys
      sells_plot <- sells
    }
  }

  if (has_txns) {
    buys_data <- lapply(seq_len(nrow(buys_plot)), function(i){
      list(
        x = as.numeric(as.POSIXct(index(buys_plot)[i])) * 1000,
        y = as.numeric(buys_plot$Txn.Price[i]),
        z = as.numeric(buys_plot$Txn.Qty[i])
      )
    })
    sells_data <- lapply(seq_len(nrow(sells_plot)), function(i){
      list(
        x = as.numeric(as.POSIXct(index(sells_plot)[i])) * 1000,
        y = as.numeric(sells_plot$Txn.Price[i]),
        z = as.numeric(sells_plot$Txn.Qty[i])
      )
    })
  }

  abrev3 <- JS("
  function () {

    var raw     = this.value + '';
    var hasPerc = raw.indexOf('%') !== -1;
    if (hasPerc) raw = raw.replace('%','');

    var v = parseFloat(raw);
    if (isNaN(v)) return this.value;

    var neg = v < 0 ? '-' : '';
    v       = Math.abs(v);

    var txt;
    if (v >= 1e6) {
      txt = (v/1e6).toFixed(v >= 1e7 ? 0 : 1) + 'M';

    } else if (v >= 1e3) {
      txt = (v/1e3).toFixed(v >= 1e4 ? 0 : 1) + 'k';

    } else if (v >= 100) {
      txt = v.toFixed(0);

    } else {
      txt = v.toFixed(2);
    }

    return neg + txt + (hasPerc ? '%' : '');
  }
")

  hc <- hchart(mktdata[, c("Open","High","Low","Close")], type = "candlestick", name = "Ativo") %>%
    hc_size(height = 500) %>%
    hc_chart(
      spacing = theme$hc_spacing,
      margin  = c(theme$hc_margin[1],theme$hc_margin[2],theme$hc_margin[3], theme$hc_margin[4]),
      backgroundColor = cl$chart_bg
    ) %>%
    {
      tmp <- .
      yid <- if (has_txns) "a" else 0
      if ("X.el" %in% names(mktdata)) {
        tmp <- tmp %>% hc_add_series(round(mktdata$X.el, 3), yAxis = yid, name = "X", color = "darkgray", lineWidth = 1)
      }
      if ("Y.el" %in% names(mktdata)) {
        tmp <- tmp %>% hc_add_series(round(mktdata$Y.el, 3), yAxis = yid, name = "Y", color = "darkgray", lineWidth = 1)
      }
      tmp
    } %>%
    {
      if ("Volume" %in% names(mktdata)) {

        (.) %>%
          hc_add_yAxis(id="b",
                       title = list(text = "Volume",style = list(
                         color      = cl$title_txt,
                         fontFamily = theme$font_family,
                         fontSize   = paste0(theme$font_sizes$title,"px"),
                         fontWeight = "bold"
                       )), labels = list(
                         formatter = abrev3,
                         style  = list(
                           color      = cl$axis_txt,
                           fontFamily = theme$font_family,
                           fontSize   = paste0(theme$font_sizes$axis, "px"),
                           fontWeight = "bold"
                         )
                       ),
                       relative = 1, opposite = FALSE) %>%
          hc_add_series(.,mktdata$Volume, yAxis = "b",
                        type = "column", color = corx, name = "Volume", showInLegend = FALSE)
      } else {
        .
      }
    }  %>%
    {
      if("PU_c" %in% names(mktdata)) {
        (.) %>%
          hc_add_yAxis(id="c", title = list(text = "PU",style = list(
            color      = cl$title_txt,
            fontFamily = theme$font_family,
            fontSize   = paste0(theme$font_sizes$title,"px"),
            fontWeight = "bold"
          )), labels = list(
            formatter = abrev3,
            style  = list(
              color      = cl$axis_txt,
              fontFamily = theme$font_family,
              fontSize   = paste0(theme$font_sizes$axis, "px"),
              fontWeight = "bold"
            )
          ), relative = 1, opposite = FALSE) %>%
          hc_add_series(.,round(mktdata$PU_c,2), yAxis = "c", name = "PU", type = "line", color = cory, showInLegend = FALSE)
      } else {
        .
      }
    } %>%
    hc_plotOptions(
      candlestick = list(dataGrouping = list(enabled = FALSE)),
      series = list(dataGrouping = list(enabled = FALSE))
    ) %>%
    hc_xAxis(
      type = "datetime",
      labels = list(style = list(color = cl$axis_txt,
                                 fontFamily = theme$font_family,
                                 fontSize = paste0(theme$font_sizes$axis,"px"),
                                 fontWeight = "bold")),
      events = list(
        afterSetExtremes = JS(
          "function(e) {
             var thisChart = this.chart;
             if (e.trigger !== 'syncExtremes') {
               Highcharts.each(Highcharts.charts, function(chart) {
                 if (chart && chart !== thisChart && chart.options.chart.renderTo &&
                     (chart.options.chart.renderTo.includes('cumret') ||
                      chart.options.chart.renderTo.includes('position') ||
                      chart.options.chart.renderTo.includes('rolling') ||
                      chart.options.chart.renderTo.includes('period') ||
                      chart.options.chart.renderTo.includes('drawdown'))) {
                   if (chart.xAxis[0].setExtremes) {
                     chart.xAxis[0].setExtremes(e.min, e.max, undefined, false, {trigger: 'syncExtremes'});
                   }
                 }
               });
             }
           }"
        )
      )
    ) %>%
    hc_rangeSelector(enabled = TRUE) %>%
    hc_navigator(
      outlineWidth = 1,
      series = list(
        color = pal[1],
        lineWidth = 2,
        type = "areaspline", # you can change the type
        fillColor = "white"
      ),
      handles = list(
        backgroundColor = pal[4],
        borderColor = pal[3]
      )
    ) %>%
    hc_scrollbar(
      barBackgroundColor = "lightgray",
      barBorderRadius = 7,
      barBorderWidth = 0,
      buttonBackgroundColor = "lightgray",
      buttonBorderWidth = 0,
      buttonArrowColor = "yellow",
      buttonBorderRadius = 7,
      rifleColor = "yellow",
      trackBackgroundColor = "white",
      trackBorderWidth = 1,
      trackBorderColor = "silver",
      trackBorderRadius = 7
    ) %>%
    hc_legend(enabled = FALSE, verticalAlign = "bottom") %>%
    onRender("
      function(el, x) {
        Highcharts.setOptions({
          global: {
            useUTC: false
          }
        });
      }
    ")

  # Conditionally add the auxiliary axis and trade markers only when transactions are available
  if (has_txns) {
    hc <- hc %>%
      hc_add_yAxis(id="a",  startOnTick = FALSE,
                   endOnTick = FALSE,title = list(text = "Buys and Sells",style = list(
                     color      = cl$title_txt,
                     fontFamily = theme$font_family,
                     fontSize   = paste0(theme$font_sizes$title,"px"),
                     fontWeight = "bold"
                   )), labels = list(
                     formatter = abrev3,
                     style  = list(
                       color      = cl$axis_txt,
                       fontFamily = theme$font_family,
                       fontSize   = paste0(theme$font_sizes$axis, "px"),
                       fontWeight = "bold"
                     )
                   ), relative = 4, gridLineColor = "rgba(255,255,255,0.1)",
                   opposite=FALSE) %>%
      hc_add_series(
        data = buys_data,
        type = "scatter",
        name = "Compras",
        color = corx,
        marker = list(enabled = TRUE, symbol = "triangle", radius = 5,
                      fillColor = corx, lineColor = corx, lineWidth = 2),
        tooltip = list(headerFormat = "",
                       pointFormat = "Buy<br>Data: {point.x:%Y-%m-%d %H:%M}<br>Price: {point.y:.2f}<br>Quantity: {point.z:.2f}"),
        showInLegend = TRUE
      ) %>%
      hc_add_series(
        data = sells_data,
        type = "scatter",
        name = "Vendas",
        color = cory,
        marker = list(enabled = TRUE, symbol = "triangle-down", radius = 5,
                      fillColor = cory, lineColor = cory, lineWidth = 1),
        tooltip = list(headerFormat = "",
                       pointFormat = "Sell<br>Data: {point.x:%Y-%m-%d %H:%M}<br>Price: {point.y:.2f}<br>Quantity: {point.z:.2f}"),
        showInLegend = TRUE
      )
  }

  termina <- Sys.time()
  message(sprintf("Module 'candles' rendered in %.2f seconds.",
                  as.numeric(difftime(termina, comeca, units = "secs"))))
  hc
}

# Simplified Volume module (separate from candles)
#' @keywords internal
volume_module <- function(mktdata, theme){
  if (is.null(mktdata) || !xts::is.xts(mktdata)) return(NULL)
  std <- try(.to_ohlc_standard(mktdata), silent = TRUE)
  if (inherits(std, "try-error") || is.null(std)) return(NULL)
  if (!("Volume" %in% colnames(std))) return(NULL)

  pal <- theme$palette; cl <- theme$colors
  idx_ms <- as.numeric(as.POSIXct(index(std))) * 1000
  vol_data <- lapply(seq_len(nrow(std)), function(i){ list(x = idx_ms[i], y = as.numeric(std$Volume[i])) })

  hc <- highcharter::highchart() %>%
    hc_size(height = 150) %>%
    hc_chart(
      spacing = theme$hc_spacing,
      margin  = theme$hc_margin,
      backgroundColor = cl$chart_bg,
      renderTo = "volume-chart"
    ) %>%
    hc_xAxis(type = "datetime", labels = list(style = list(color = cl$axis_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$axis, "px"), fontWeight = "bold"))) %>%
    hc_yAxis(title = list(text = "Volume", style = list(color = cl$title_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$title, "px"), fontWeight = "bold")),
             labels = list(style = list(color = cl$axis_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$axis, "px"), fontWeight = "bold"))) %>%
    hc_plotOptions(column = list(dataGrouping = list(enabled = FALSE))) %>%
    hc_add_series(data = vol_data, type = "column", color = pal[1], name = "Volume", showInLegend = FALSE) %>%
    hc_xAxis(
      type = "datetime",
      events = list(
        afterSetExtremes = JS(
          "function(e) { var thisChart = this.chart; if (e.trigger !== 'syncExtremes') { Highcharts.each(Highcharts.charts, function(chart) { if (chart && chart !== thisChart && chart.options.chart.renderTo && (chart.options.chart.renderTo.includes('cumret') || chart.options.chart.renderTo.includes('position') || chart.options.chart.renderTo.includes('rolling') || chart.options.chart.renderTo.includes('period') || chart.options.chart.renderTo.includes('drawdown') || chart.options.chart.renderTo.includes('candles'))) { if (chart.xAxis[0].setExtremes) { chart.xAxis[0].setExtremes(e.min, e.max, undefined, false, {trigger: 'syncExtremes'}); } } }); } }"
        )
      )
    )
  hc
}

# Override candles module with axis-safe, volume-free version
#' @keywords internal
candles_module <- function(mktdata, txns, theme){
  comeca <- Sys.time()
  if (is.null(mktdata) || !xts::is.xts(mktdata)) return(NULL)
  std <- try(.to_ohlc_standard(mktdata), silent = TRUE)
  if (inherits(std, "try-error") || is.null(std)) return(NULL)

  pal  <- theme$palette
  corx <- pal[1]
  cory <- pal[3]
  cl   <- theme$colors

  # Candle + range selector style with safe fallbacks
  candle_cfg <- theme$candles %||% list()
  cw    <- candle_cfg$point_width %||% 4
  cgrp  <- candle_cfg$grouping %||% FALSE
  cup   <- candle_cfg$up_color %||% "#00c176"
  cdown <- candle_cfg$down_color %||% "#ff4d4d"
  cline <- candle_cfg$line_color %||% "#cccccc"
  clw   <- candle_cfg$line_width %||% 1
  rs_txt  <- cl$range_selector_txt %||% cl$axis_txt
  rs_fill <- cl$range_selector_bg  %||% cl$chart_bg
  rs_stk  <- cl$range_selector_border %||% cl$axis_txt

  has_txns <- !is.null(txns) && xts::is.xts(txns) && nrow(txns) > 0
  if (has_txns) {
    buys  <- txns[ txns$Txn.Qty >  0 , ]
    sells <- txns[ txns$Txn.Qty <  0 , ]
  }

  di_flag       <- isDI(std)
  maturity_date <- attr(std, "maturity")
  if (has_txns && di_flag && !is.null(maturity_date)){
    buys$Txn.Price  <- mapply(get_DI_price, buys$Txn.Price, index(buys), MoreArgs = list(maturity = maturity_date))
    sells$Txn.Price <- mapply(get_DI_price, sells$Txn.Price, index(sells), MoreArgs = list(maturity = maturity_date))
  }
  if (has_txns) {
    if (di_flag){ tmp <- buys; buys <- sells; sells <- tmp }
    buys_data <- lapply(seq_len(nrow(buys)), function(i){ list(x = as.numeric(as.POSIXct(index(buys)[i])) * 1000,  y = as.numeric(buys$Txn.Price[i]),  z = as.numeric(buys$Txn.Qty[i])) })
    sells_data <- lapply(seq_len(nrow(sells)), function(i){ list(x = as.numeric(as.POSIXct(index(sells)[i])) * 1000, y = as.numeric(sells$Txn.Price[i]), z = as.numeric(sells$Txn.Qty[i])) })
  }

  idx_ms <- as.numeric(as.POSIXct(index(std))) * 1000
  ohlc_data <- lapply(seq_len(nrow(std)), function(i){ list(x = idx_ms[i], open = as.numeric(std$Open[i]), high = as.numeric(std$High[i]), low = as.numeric(std$Low[i]), close = as.numeric(std$Close[i])) })

  abrev3 <- JS("function () { var raw=this.value+''; var hasPerc=raw.indexOf('%')!==-1; if(hasPerc) raw=raw.replace('%',''); var v=parseFloat(raw); if(isNaN(v)) return this.value; var neg=v<0?'-':''; v=Math.abs(v); var txt; if(v>=1e6){ txt=(v/1e6).toFixed(v>=1e7?0:1)+'M'; } else if(v>=1e3){ txt=(v/1e3).toFixed(v>=1e4?0:1)+'k'; } else if(v>=100){ txt=v.toFixed(0);} else { txt=v.toFixed(2);} return neg+txt+(hasPerc?'%':''); }")

  hc <- highcharter::highchart() %>%
    hc_size(height = 500) %>%
    hc_chart(spacing = theme$hc_spacing, margin = c(theme$hc_margin[1], theme$hc_margin[2], theme$hc_margin[3], theme$hc_margin[4]), backgroundColor = cl$chart_bg, renderTo = "candles-chart") %>%
    hc_add_yAxis(id = "price", startOnTick = FALSE, endOnTick = FALSE,
                 title = list(text = "Price", style = list(color = cl$title_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$title, "px"), fontWeight = "bold")),
                 labels = list(style = list(color = cl$axis_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$axis, "px"), fontWeight = "bold")),
                 relative = 1, opposite = FALSE)

  # Add candlestick series with pre-evaluated style to avoid NSE scoping issues
  ohlc_style <- list(upColor = cup, color = cdown, lineColor = cline, lineWidth = clw, pointWidth = cw)
  hc <- do.call(highcharter::hc_add_series,
                c(list(hc = hc, data = ohlc_data, type = "candlestick", name = "Ativo", yAxis = "price"),
                  ohlc_style)) %>%
    hc_plotOptions(candlestick = list(dataGrouping = list(enabled = cgrp)), series = list(dataGrouping = list(enabled = FALSE))) %>%
    hc_xAxis(type = "datetime", labels = list(style = list(color = cl$axis_txt, fontFamily = theme$font_family, fontSize = paste0(theme$font_sizes$axis, "px"), fontWeight = "bold")),
             events = list(afterSetExtremes = JS("function(e){ var thisChart=this.chart; if(e.trigger!=='syncExtremes'){ Highcharts.each(Highcharts.charts,function(chart){ if(chart && chart!==thisChart && chart.options.chart.renderTo && (chart.options.chart.renderTo.includes('cumret')||chart.options.chart.renderTo.includes('position')||chart.options.chart.renderTo.includes('rolling')||chart.options.chart.renderTo.includes('period')||chart.options.chart.renderTo.includes('drawdown')||chart.options.chart.renderTo.includes('volume'))){ if(chart.xAxis[0].setExtremes){ chart.xAxis[0].setExtremes(e.min,e.max,undefined,false,{trigger:'syncExtremes'}); } } }); }}"))) %>%
    hc_rangeSelector(enabled = TRUE,
                     buttonTheme = list(style = list(color = rs_txt), fill = rs_fill, stroke = rs_stk,
                                        states = list(hover = list(fill = rs_fill, style = list(color = rs_txt)),
                                                      select = list(fill = rs_fill, style = list(color = rs_txt)))),
                     inputStyle = list(color = rs_txt), labelStyle = list(color = rs_txt)) %>%
    hc_navigator(outlineWidth = 1, series = list(color = pal[1], lineWidth = 2, type = "areaspline", fillColor = "white"), handles = list(backgroundColor = pal[4], borderColor = pal[3])) %>%
    hc_scrollbar(barBackgroundColor = "lightgray", barBorderRadius = 7, barBorderWidth = 0, buttonBackgroundColor = "lightgray", buttonBorderWidth = 0, buttonArrowColor = "yellow", buttonBorderRadius = 7, rifleColor = "yellow", trackBackgroundColor = "white", trackBorderWidth = 1, trackBorderColor = "silver", trackBorderRadius = 7) %>%
    hc_legend(enabled = FALSE, verticalAlign = "bottom") %>%
    onRender("function(el,x){ Highcharts.setOptions({global:{useUTC:false}}); }")

  if (has_txns) {
    hc <- hc %>%
      hc_add_series(data = buys_data, yAxis = "price", type = "scatter", name = "Compras", color = corx,
                    marker = list(enabled = TRUE, symbol = "triangle", radius = 5, fillColor = corx, lineColor = corx, lineWidth = 2),
                    tooltip = list(headerFormat = "", pointFormat = "Buy<br>Data: {point.x:%Y-%m-%d %H:%M}<br>Price: {point.y:.2f}<br>Quantity: {point.z:.2f}"), showInLegend = TRUE) %>%
      hc_add_series(data = sells_data, yAxis = "price", type = "scatter", name = "Vendas", color = cory,
                    marker = list(enabled = TRUE, symbol = "triangle-down", radius = 5, fillColor = cory, lineColor = cory, lineWidth = 1),
                    tooltip = list(headerFormat = "", pointFormat = "Sell<br>Data: {point.x:%Y-%m-%d %H:%M}<br>Price: {point.y:.2f}<br>Quantity: {point.z:.2f}"), showInLegend = TRUE)
  }

  termina <- Sys.time()
  message(sprintf("Module 'candles' rendered in %.2f seconds.", as.numeric(difftime(termina, comeca, units = 'secs'))))
  hc
}

#' Renders the Position Chart Module
#' @param mktdata An xts object with market data.
#' @param txns An xts object with transactions.
#' @param theme The theme list object.
#' @param sync_with_candles Logical, whether to sync with the candlestick chart.
#' @return A highchart object.
#' @keywords internal
position_module <- function(mktdata, txns, theme, sync_with_candles = FALSE) {
  comeca <- Sys.time()

  if (is.null(mktdata) || is.null(txns) || nrow(txns) == 0)
    return(NULL)

  pos <- xts(rep(NA_real_, NROW(mktdata)), order.by = index(mktdata))
  pos[index(txns)] <- cumsum(txns$Txn.Qty)
  pos <- zoo::na.locf(pos, na.rm = FALSE)
  pos[is.na(pos)] <- 0
  colnames(pos) <- "Pos.Qty"

  pos_data <- highcharter::list_parse2(
    data.frame(
      x = as.numeric(as.POSIXct(index(pos))) * 1000,
      y = as.numeric(coredata(pos))
    )
  )

  pal   <- theme$palette
  zones <- list(
    list(value = 0, color = pal[3]),
    list(color      = pal[1])
  )

  hc <- highcharter::highchart() %>%
    hc_size(height = 150) %>%
    hc_chart(
      spacing         = theme$hc_spacing,
      margin          = theme$hc_margin,
      backgroundColor = theme$colors$chart_bg,
      renderTo        = if (sync_with_candles) "position-chart" else NULL
    ) %>%
    hc_xAxis(type = "datetime",     lineWidth = 0,    tickLength = 0,labels = list(enabled = FALSE)) %>%
    hc_yAxis( startOnTick = FALSE,
              endOnTick = FALSE,
              title  = list(
                text  = "Position",
                style = list(
                  color      = theme$colors$title_txt,
                  fontFamily = theme$font_family,
                  fontSize   = paste0(theme$font_sizes$title, "px"),
                  fontWeight = "bold"
                )
              ),
              labels = list(
                style = list(
                  color      = theme$colors$axis_txt,
                  fontFamily = theme$font_family,
                  fontSize   = paste0(theme$font_sizes$axis, "px"),
                  fontWeight = "bold"
                )
              )
    ) %>%
    # impede que o Highcharts aglutine barras
    hc_plotOptions(column = list(dataGrouping = list(enabled = FALSE))) %>%
    hc_add_series(
      data         = pos_data,
      type         = "area",
      name         = "Position",
      zones        = zones,
      showInLegend = FALSE
    )
  termina <- Sys.time()
  message(sprintf(
    "Module 'position' rendered in %.2f seconds.",
    as.numeric(difftime(termina, comeca, units = "secs"))
  ))
  hc
}

#' Renders the Cumulative Returns Chart Module
#' @param ret_cum An xts object with cumulative returns.
#' @param datas A numeric vector of dates in milliseconds.
#' @param ativo The name of the main asset.
#' @param benchs A character vector of benchmark names.
#' @param theme The theme list object.
#' @param link_charts Logical, whether to link legends across charts.
#' @param sync_with_candles Logical, whether to sync with the candlestick chart.
#' @return A highchart object.
#' @keywords internal
cumret_module <- function(ret_cum, datas, ativo, benchs, theme, link_charts=FALSE, sync_with_candles=FALSE){
  comeca <- Sys.time()
  pal <- theme$palette; cl <- theme$colors
  hc <- highchart() %>%
    hc_size(height = 350) %>%
    hc_chart(
      spacing = theme$hc_spacing,
      margin  = c(theme$hc_margin[1],theme$hc_margin[2],theme$hc_margin[3]+45, theme$hc_margin[4]),
      backgroundColor = cl$chart_bg
    ) %>%
    hc_xAxis(
      type = "datetime",
      labels = list(
        style = list(
          color      = cl$axis_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$axis,"px"),
          fontWeight = "bold"
        )
      )
    ) %>%
    hc_yAxis(
      startOnTick = FALSE,
      endOnTick = FALSE,
      title = list(
        text = "Cumulative Returns",
        style = list(
          color      = cl$title_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$title,"px"),
          fontWeight = "bold"
        )
      ),
      labels = list(
        format= "{value}%",
        style = list(
          color      = cl$axis_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$axis,"px"),
          fontWeight = "bold"
        )
      ),
      min = min(ret_cum, na.rm=TRUE),
      max = max(ret_cum, na.rm=TRUE)
    ) %>%
    hc_tooltip(
      pointFormat = "<b>{series.name}</b>: {point.y:.2f}%<br>",
      valueDecimals=2
    ) %>%
    hc_legend(
      enabled      = TRUE,
      align        = "center",
      verticalAlign= "bottom",
      layout       = "horizontal",
      itemStyle    = list(
        color      = cl$legend_txt,
        fontFamily = theme$font_family,
        fontSize   = paste0(theme$font_sizes$legend,"px"),
        fontWeight = "bold"
      )
    )

  if(sync_with_candles) {
    hc <- hc %>% hc_chart(
      spacing = theme$hc_spacing,
      margin  = c(theme$hc_margin[1],theme$hc_margin[2],theme$hc_margin[3]+45, theme$hc_margin[4]),
      backgroundColor = cl$chart_bg,
      renderTo = "cumret-chart"
    )
  }

  hc <- hc %>% hc_add_series(
    data = list_parse2(data.frame(x = datas, y = ret_cum[, ativo])),
    type  = "line", name = ativo, color = pal[1], id = ativo
  )
  for(i in seq_along(benchs)){
    hc <- hc %>% hc_add_series(
      data = list_parse2(data.frame(x = datas, y = ret_cum[, benchs[i]])),
      type  = "line", name = benchs[i], color = pal[i+1], id = benchs[i]
    )
  }
  if(link_charts){
    hc <- hc %>% hc_plotOptions(
      series = list(events = list(
        legendItemClick = JS(
          "function(){
             var id=this.options.id;
             Highcharts.charts.forEach(function(c){
               if(!c) return;
               var s = c.get(id);
               if(s){ s.visible? s.hide(): s.show(); }
             });
             return false;
           }"
        )
      ))
    )
  }
  termina <- Sys.time()
  message(sprintf("Module 'cumret' rendered in %.2f seconds.",
                  as.numeric(difftime(termina, comeca, units = "secs"))))
  hc <- hc %>%
    hc_xAxis(
      type="datetime",
      events = list(
        afterSetExtremes = JS("
        function(e){
          var minX = e.min, maxX = e.max,
              yMin = Infinity, yMax = -Infinity,
              chart = this.chart;

          Highcharts.each(chart.series, function(s){
            Highcharts.each(s.points, function(p){
              if(p.x >= minX && p.x <= maxX){
                yMin = Math.min(yMin, p.y);
                yMax = Math.max(yMax, p.y);
              }
            });
          });

          chart.yAxis[0].setExtremes(yMin, yMax);
        }
      ")
      )
    )
  hc
}

#' Renders the Rolling Returns Chart Module
#' @param ret_cum An xts object with cumulative returns.
#' @param datas A numeric vector of dates in milliseconds.
#' @param ativo The name of the main asset.
#' @param benchs A character vector of benchmark names.
#' @param theme The theme list object.
#' @param sync_with_candles Logical, whether to sync with the candlestick chart.
#' @return A highchart object.
#' @keywords internal
rollingret_module <- function(ret_cum, datas, ativo, benchs, theme, sync_with_candles=FALSE) {
  comeca <- Sys.time()
  if (inherits(ret_cum, c("xts", "zoo"))) {
    M <- coredata(ret_cum)
  } else if (is.data.frame(ret_cum)) {
    M <- as.matrix(ret_cum)
  } else if (is.matrix(ret_cum)) {
    M <- ret_cum
  } else {
    stop("ret_cum deve ser xts, zoo, data.frame ou matrix")
  }
  colnames(M) <- colnames(ret_cum)
  N <- 1 + M/100
  k <- 22
  n <- nrow(N); p <- ncol(N)
  R <- matrix(NA_real_, n, p,
              dimnames = list(rownames(N), colnames(N)))
  if (n > k) {
    R[(k+1):n, ] <- (N[(k+1):n, ,drop=FALSE] /
                       N[1:(n-k),   ,drop=FALSE] - 1) * 100
  }
  # 3) paleta e cores
  pal <- theme$palette
  cl  <- theme$colors
  hc <- highchart() %>%
    hc_size(height = 150) %>%
    hc_chart(
      spacing         = theme$hc_spacing,
      margin          = theme$hc_margin,
      backgroundColor = cl$chart_bg
    ) %>%
    hc_xAxis(
      type   = "datetime",
      lineWidth = 0,
      tickLength = 0,
      labels = list(enabled = FALSE)
    ) %>%
    hc_yAxis(
      startOnTick = FALSE,
      endOnTick = FALSE,
      title = list(
        text  = paste0("Rolling Rets. ", k, " p"),
        style = list(
          color      = cl$title_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$title, "px"),
          fontWeight = "bold"
        )
      ),
      labels = list(
        format = "{value}%",
        style  = list(
          color      = cl$axis_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$axis, "px"),
          fontWeight = "bold"
        )
      ),
      min = min(R, na.rm = TRUE),
      max = max(R, na.rm = TRUE)
    ) %>%
    hc_tooltip(
      pointFormat  = "<b>{series.name}</b>: {point.y:.2f}%<br>",
      valueDecimals = 2
    ) %>%
    hc_legend(enabled = FALSE)

  if(sync_with_candles) {
    hc <- hc %>% hc_chart(
      spacing         = theme$hc_spacing,
      margin          = theme$hc_margin,
      backgroundColor = cl$chart_bg,
      renderTo = "rolling-chart"
    )
  }

  # ativo principal
  hc <- hc %>% hc_add_series(
    data  = list_parse2(data.frame(x = datas, y = R[, ativo])),
    type  = "line", name = ativo, color = pal[1], id = ativo
  )
  # benchmarks
  for (i in seq_along(benchs)) {
    nm <- benchs[i]
    hc <- hc %>% hc_add_series(
      data  = list_parse2(data.frame(x = datas, y = R[, nm])),
      type  = "line", name = nm,     color = pal[i+1], id = nm
    )
  }
  # 6) mensagem de tempo e retorno
  termina <- Sys.time()
  message(sprintf(
    "Module 'rollingret' rendered in %.2f seconds.",
    as.numeric(difftime(termina, comeca, units = "secs"))
  ))
  hc <- hc %>%
    hc_xAxis(
      type="datetime",
      events = list(
        afterSetExtremes = JS("
        function(e){
          var minX = e.min, maxX = e.max,
              yMin = Infinity, yMax = -Infinity,
              chart = this.chart;

          Highcharts.each(chart.series, function(s){
            Highcharts.each(s.points, function(p){
              if(p.x >= minX && p.x <= maxX){
                yMin = Math.min(yMin, p.y);
                yMax = Math.max(yMax, p.y);
              }
            });
          });

          chart.yAxis[0].setExtremes(yMin, yMax);
        }
      ")
      )
    )
  hc
}

#' Renders the Period Returns Chart Module
#' @param ret_simple An xts object with simple returns.
#' @param datas A numeric vector of dates in milliseconds.
#' @param ativo The name of the main asset.
#' @param benchs A character vector of benchmark names.
#' @param theme The theme list object.
#' @param sync_with_candles Logical, whether to sync with the candlestick chart.
#' @return A highchart object.
#' @keywords internal
periodret_module <- function(ret_simple, datas, ativo, benchs, theme, sync_with_candles=FALSE){
  comeca <- Sys.time()
  pal <- theme$palette; cl <- theme$colors
  hc <- highchart() %>%
    hc_size(height = 150) %>%
    hc_chart(
      spacing = theme$hc_spacing,
      margin  = theme$hc_margin,
      backgroundColor = cl$chart_bg
    ) %>%
    hc_xAxis(type="datetime",      tickLength = 0,     lineWidth = 0,labels=list(enabled=FALSE)) %>%
    hc_yAxis(
      startOnTick = FALSE,
      endOnTick = FALSE,
      title = list(
        text = "Period Rets",
        style= list(
          color      = cl$title_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$title,"px"),
          fontWeight = "bold"
        )
      ),
      labels = list(
        format="{value}%",
        style = list(
          color      = cl$axis_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$axis,"px"),
          fontWeight = "bold"
        )
      ),
      min = min(ret_simple, na.rm=TRUE),
      max = max(ret_simple, na.rm=TRUE)
    ) %>%
    hc_tooltip(
      pointFormat = "<b>{series.name}</b>: {point.y:.2f}%<br>",
      valueDecimals=2
    ) %>%
    hc_legend(enabled=FALSE)

  if(sync_with_candles) {
    hc <- hc %>% hc_chart(
      spacing = theme$hc_spacing,
      margin  = theme$hc_margin,
      backgroundColor = cl$chart_bg,
      renderTo = "period-chart"
    )
  }

  hc <- hc %>% hc_add_series(
    data = list_parse2(data.frame(x=datas, y=ret_simple[, ativo])),
    type="line", name=ativo, color=pal[1], id=ativo
  )
  for(i in seq_along(benchs)){
    hc <- hc %>% hc_add_series(
      data = list_parse2(data.frame(x=datas, y=ret_simple[, benchs[i]])),
      type="line", name=benchs[i], color=pal[i+1], id=benchs[i]
    )
  }
  termina <- Sys.time()
  message(sprintf("Module 'periodret' rendered in %.2f seconds.",
                  as.numeric(difftime(termina, comeca, units = "secs"))))
  hc <- hc %>%
    hc_xAxis(
      type="datetime",
      events = list(
        afterSetExtremes = JS("
        function(e){
          var minX = e.min, maxX = e.max,
              yMin = Infinity, yMax = -Infinity,
              chart = this.chart;

          Highcharts.each(chart.series, function(s){
            Highcharts.each(s.points, function(p){
              if(p.x >= minX && p.x <= maxX){
                yMin = Math.min(yMin, p.y);
                yMax = Math.max(yMax, p.y);
              }
            });
          });

          chart.yAxis[0].setExtremes(yMin, yMax);
        }
      ")
      )
    )
  hc
}

#' Renders the Drawdowns Chart Module
#' @param drawdowns An xts object with the drawdown series.
#' @param datas A numeric vector of dates in milliseconds.
#' @param ativo The name of the main asset.
#' @param benchs A character vector of benchmark names.
#' @param theme The theme list object.
#' @param sync_with_candles Logical, whether to sync with the candlestick chart.
#' @return A highchart object.
#' @keywords internal
drawdown_module <- function(drawdowns, datas, ativo, benchs, theme, sync_with_candles=FALSE){
  comeca <- Sys.time()
  pal <- theme$palette; cl <- theme$colors
  hc <- highchart() %>%
    hc_size(height = 150) %>%
    hc_chart(
      spacing        = theme$hc_spacing,
      margin         = theme$hc_margin,
      backgroundColor= cl$chart_bg
    ) %>%
    hc_xAxis(type="datetime",    lineWidth = 0,     tickLength = 0,   labels=list(enabled=FALSE)) %>%
    hc_yAxis(
      title = list(
        text = "Drawdowns",
        style= list(
          color      = cl$title_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$title,"px"),
          fontWeight = "bold"
        )
      ),
      labels = list(
        format="{value}%",
        style = list(
          color      = cl$axis_txt,
          fontFamily = theme$font_family,
          fontSize   = paste0(theme$font_sizes$axis,"px"),
          fontWeight = "bold"
        )
      ),
      min = min(drawdowns, na.rm=TRUE),
      max = 0
    ) %>%
    hc_tooltip(
      pointFormat = "<b>{series.name}</b>: {point.y:.2f}%<br>",
      valueDecimals=2
    ) %>%
    hc_legend(enabled=FALSE)

  if(sync_with_candles) {
    hc <- hc %>% hc_chart(
      spacing        = theme$hc_spacing,
      margin         = theme$hc_margin,
      backgroundColor= cl$chart_bg,
      renderTo = "drawdown-chart"
    )
  }

  hc <- hc %>% hc_add_series(
    data=list_parse2(data.frame(x=datas,y=drawdowns[, ativo])),
    type="line", name=ativo, color=pal[1], id=ativo
  )
  for(i in seq_along(benchs)){
    hc <- hc %>% hc_add_series(
      data=list_parse2(data.frame(x=datas,y=drawdowns[, benchs[i]])),
      type="line", name=benchs[i], color=pal[i+1], id=benchs[i]
    )
  }
  termina <- Sys.time()
  message(sprintf("Module 'drawdown' rendered in %.2f seconds.",
                  as.numeric(difftime(termina, comeca, units = "secs"))))
  hc <- hc %>%
    hc_xAxis(
      type="datetime",
      events = list(
        afterSetExtremes = JS("
        function(e){
          var minX = e.min, maxX = e.max,
              yMin = Infinity, yMax = -Infinity,
              chart = this.chart;

          Highcharts.each(chart.series, function(s){
            Highcharts.each(s.points, function(p){
              if(p.x >= minX && p.x <= maxX){
                yMin = Math.min(yMin, p.y);
                yMax = Math.max(yMax, p.y);
              }
            });
          });

          chart.yAxis[0].setExtremes(yMin, yMax);
        }
      ")
      )
    )
  hc
}
