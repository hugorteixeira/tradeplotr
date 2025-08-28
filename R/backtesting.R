remotes::install_github("hugorteixeira/rSenhorMercadoAPI")

source("~/rTradingPlots/R/functions.R")
source("~/rTradingPlots/R/helpers.R")
source("~/rTradingPlots/R/modules.R")
source("~/rTradingPlots/R/rTradingPlots-package.R")
source("~/rTradingPlots/R/themes.R")
source("~/rTradingPlots/R/utils-pipe.R")

library(PerformanceAnalytics)
library(FinancialInstrument)
library(PortfolioAnalytics)
library(rSenhorMercadoAPI)
library(conflicted)
library(quantstrat)
library(quantmod)
library(bizdays)
library(rb3)
library(TTR)
library(blotter)
library(htmltools)
library(htmlwidgets)
library(highcharter)

conflicts_prefer(stats::lag)
conflicts_prefer(PerformanceAnalytics::legend)
conflicts_prefer(xts::first)

options(scipen = 999)

ttz<-Sys.getenv('TZ')
Sys.setenv(TZ='BRT')

#objetos_no_ambiente <- ls()
#objetos_a_manter <- objetos_no_ambiente[!grepl("^(MT5|meuip)", objetos_no_ambiente)]
#rm(list = objetos_a_manter)

.blotter <- new.env()
.strategy <- new.env()

# Funções elDoc, elData e txfeeFUN
#source("~/carteira_funcoes.R")
#source("~/tool_obter_dados.R")
#source("~/sm_hplot/tool_tplot.R")

cal_b3 <- create.calendar(
  name      = "Brazil/ANBIMA",
  holidays  = holidays("Brazil/ANBIMA"),
  weekdays  = c("saturday","sunday")
)

dbg <- function(...) cat(format(Sys.time(), "%T"), "-", ..., "\n")
usar_close <- function(objeto_nome) {
  objeto <- get(objeto_nome, envir = .GlobalEnv)

  if(is.character(objeto)) {
    objeto <- get(objeto, envir = .GlobalEnv)
  }

  colunas <- tolower(colnames(objeto))

  tem_open  <- "open"  %in% colunas
  tem_high  <- "high"  %in% colunas
  tem_low   <- "low"   %in% colunas
  tem_close <- "close" %in% colunas

  if (tem_open && tem_high && tem_low && tem_close) {
    return(objeto)
  }

  if (tem_close) {
    col_close <- which(colunas == "close")
    if(!tem_open) {
      objeto$Open <- objeto[, col_close]
    }
    if(!tem_high) {
      objeto$High <- objeto[, col_close]
    }
    if(!tem_low) {
      objeto$Low <- objeto[, col_close]
    }

    desired_order <- c("Open", "High", "Low", "Close")
    colunas_atualizadas <- colnames(objeto)
    lower_map <- tolower(colunas_atualizadas)
    desired_indices <- match(tolower(desired_order), lower_map)
    desired_indices <- desired_indices[!is.na(desired_indices)]
    other_cols <- setdiff(seq_along(colunas_atualizadas), desired_indices)
    new_order <- c(desired_indices, other_cols)
    objeto <- objeto[, new_order]
  }

  assign(objeto_nome, objeto, envir = .GlobalEnv)
  return(objeto)
}
converter_posixct <- function(objeto_xts) {
  if (inherits(objeto_xts, "xts")) {

    dados <- as.POSIXct(format(index(objeto_xts), "%Y-%m-%d 00:00:00"),
                        tz = "America/Sao_Paulo")

    return(dados)
  } else {
    stop("O objeto fornecido não é do tipo xts.")
  }
}
txfeeFUN <- function(TxnQty, TxnPrice, Symbol) {
  # 1. Verificação de segurança para inputs NA
  # Se o preço ou a quantidade não estiverem disponíveis, não há taxa.
  if (is.na(TxnPrice) || is.na(TxnQty)) {
    return(0)
  }
  return(0)
  # Tenta obter as informações do instrumento usando getInstrument
  instrument_info <- tryCatch({
    suppressWarnings(getInstrument(Symbol))
  }, error = function(e) {
    NULL  # Se getInstrument falhar, retorna NULL
  })

  # Se getInstrument retornar informações válidas, calcula as taxas
  if (!is.null(instrument_info) && !is.na(instrument_info)) {
    # Extrai as informações relevantes
    print(instrument_info)
    print(str(instrument_info))
    symbol_id <- instrument_info$primary_id

    # Função auxiliar para verificar o início da string
    startsWith_any <- function(string, patterns) {
      if(!is.character(string) || length(string) == 0) return(NULL)
      for (pattern in patterns) {
        if (startsWith(string, pattern)) return(pattern)
      }
      return(NULL)
    }

    # Lista de padrões a serem verificados
    patterns1 <- c("CCM","BGI","DOL","GOLD","WDO","WIN","IND","COCOA","CORN","NATURAL_GAS")

    # Encontrar o padrão correspondente
    matched_pattern <- startsWith_any(symbol_id, patterns1)

    slippage <- instrument_info$identifiers$slippage
    fees <- instrument_info$identifiers$fees
    multiplier <- instrument_info$multiplier

    # Verifica se os identificadores foram carregados corretamente
    if (is.null(slippage) || is.null(fees) || is.null(multiplier)) {
      warning(paste("Identificadores (slippage, fees, multiplier) não encontrados para o símbolo:", Symbol, ". Retornando taxa 0."))
      return(0)
    }

    # Definir as taxas baseadas no padrão encontrado
    if (!is.null(matched_pattern) && matched_pattern == "BGI") { # Adicionado para ser mais específico
      # Use as.numeric para garantir que os valores são números
      return(-1 * (as.numeric(slippage) * as.numeric(TxnPrice) * (as.numeric(multiplier)/100) + as.numeric(fees) * abs(as.numeric(TxnQty))))
    } else {
      # Fallback para outros tipos de ativos
      return(-1 * (as.numeric(slippage) * as.numeric(TxnPrice) * abs(as.numeric(TxnQty))))
    }

  } else {
    # 2. Correção do valor de retorno
    # Se getInstrument falhar, a taxa é 0, não 1.
    warning(paste("Instrumento não encontrado:", Symbol, ". Retornando taxa 0."))
    return(0)
  }
}
eldoc <- function(ticker, name_to_use = NULL, x = 25, y = 25,  hi.col = "High", lo.col = "Low", type = "chart") {
  sm <- chart_theme()
  sm$col$line.col <- "blue"
  sm$col$dn.col <- "firebrick2"
  sm$col$up.col <- "forestgreen"
  sm$col$dn.border <- "black"
  sm$col$up.border <- "black"
  sm$lylab = FALSE

  smpars <- chart_pars()
  smpars$cex <- 1

  hi <- ticker[, hi.col]         # usa colunas passadas
  lo <- ticker[, lo.col]

  high <- runMax(hi, x)
  low <- runMin(lo, y)
  result <- cbind(high, low)
  colnames(result) <- c("X", "Y")

  if (is.null(name_to_use)) {
    name_to_use <- toupper(deparse(substitute(ticker)))
  }

  if (type == "chart") {
    p <- format(median(diff(index(ticker))), format = "%H:%M:%S")
    lines <- "add_Series(high, on = 1, type = 'line'); add_Series(low, on = 1, type='line'); add_Vo()"
    chart_Series(ticker, pars = smpars, theme = sm, TA = lines,
                 name = paste0(name_to_use, ", ", p, ", ", "ElDoc ", x, "/", y,
                               ", X = ", tail(high, 1), ", Y = ", tail(low, 1)))
  } else if (type == "data") {
    res <- lag.xts(result)
    return(res)
  } else if (type == "fulldata") {
    ticker_subset <- ticker[, 1:5, drop = FALSE]
    final <- cbind(ticker_subset, result)
    final <- as.xts(final)
    return(final)
  }
}
calcular_pu_futuro <- function(taxa,vencimento,data_base = Sys.Date(),cal = cal_b3) {
  cal_b3 <- create.calendar(
    name      = "Brazil/ANBIMA",
    holidays  = holidays("Brazil/ANBIMA"),
    weekdays  = c("saturday", "sunday")
  )
  # ------------------------------------------------------------------
  # 1) n = nº de dias úteis restantes
  # ------------------------------------------------------------------
  if (inherits(vencimento, "Date")) {
    n  <- bizdays(data_base, vencimento, cal)
    mm <- interval(data_base, vencimento) %/% months(1)   # meses corridos
  } else if (is.numeric(vencimento)) {
    n  <- as.integer(vencimento)
    mm <- n / 21                                           # aprox. meses
  } else {
    stop("'vencimento' deve ser número de dias úteis (numeric) ou objeto Date.")
  }

  # ------------------------------------------------------------------
  # 2) define o tick-size de acordo com o prazo em meses
  # ------------------------------------------------------------------
  tick_size <- if      (mm <=  3) 0.001
  else if (mm <= 60) 0.005
  else               0.010

  # ------------------------------------------------------------------
  # 3) PU
  # ------------------------------------------------------------------
  pu <- 1e5 / (1 + taxa/100)^(n/252)

  # ------------------------------------------------------------------
  # 4) tick-value = |dPU/d(i)| · tick_size
  # ------------------------------------------------------------------
  deriv <- (n/252) * 1e5/100 * (1 + taxa/100)^(-(n/252) - 1)
  tick_value <- deriv * tick_size   # já é módulo (deriv > 0)

  # ------------------------------------------------------------------
  # 5) devolve lista
  # ------------------------------------------------------------------
  return(list(
    dias_uteis  = n,
    pu          = as.numeric(pu),
    tick_size   = tick_size,
    tick_value  = as.numeric(tick_value)
  ))
}
calcular_taxa_futuro <- function(pu, vencimento, data_base = Sys.Date(), cal = cal_b3) {
  # -------------------------------------------------------------
  # 0) calendário padrão (se o usuário não passar um explícito)
  # -------------------------------------------------------------
  if (is.null(cal)) {
    cal <- create.calendar(
      name      = "Brazil/ANBIMA",
      holidays  = holidays("Brazil/ANBIMA"),
      weekdays  = c("saturday", "sunday")
    )
  }

  # -------------------------------------------------------------
  # 1) número de dias úteis (n) e prazo em meses corridos (mm)
  # -------------------------------------------------------------
  if (inherits(vencimento, "Date")) {
    n  <- bizdays(data_base, vencimento, cal)           # dias úteis
    mm <- lubridate::interval(data_base, vencimento) %/% months(1)
  } else if (is.numeric(vencimento)) {
    n  <- as.integer(vencimento)
    mm <- n / 21                                        # aprox. meses
  } else {
    stop("'vencimento' deve ser número de dias úteis (numeric) ou objeto Date.")
  }

  # -------------------------------------------------------------
  # 2) tick-size
  # -------------------------------------------------------------
  tick_size <- if      (mm <=  3) 0.001
  else if (mm <= 60) 0.005
  else               0.010

  # -------------------------------------------------------------
  # 3) taxa implícita
  #        PU = 100000 / (1 + i)^(n/252)
  #  ==>   i  = (100000/Pu)^(252/n) - 1
  #        (i em pontos-percentuais: × 100)
  # -------------------------------------------------------------
  taxa <- 100 * ( (1e5 / pu)^(252 / n) - 1 )

  # -------------------------------------------------------------
  # 4) tick-value (= |dPU/d(i)| · tick_size)
  #        dPU/d(i) = (n/252)*100000/100*(1 + i)^{-(n/252)-1}
  # -------------------------------------------------------------
  deriv       <- (n/252) * 1e5/100 * (1 + taxa/100)^(-(n/252) - 1)
  tick_value  <- deriv * tick_size        # já em módulo

  # -------------------------------------------------------------
  # 5) saída
  # -------------------------------------------------------------
  return(list(
    dias_uteis  = n,
    taxa        = round(as.numeric(taxa),3),
    tick_size   = tick_size,
    tick_value  = round(as.numeric(tick_value),3)
  ))
}
psQuantiadeContratosFixa <- function(timestamp,buyorderqty,sellorderqty,orderside,portfolio,symbol,...) {
  pos <- getPosQty(portfolio, symbol, timestamp)
  if (orderside == "short" && pos < 0) {
    return(0)
  } else if (orderside == "long" && pos > 0) {
    return(0)
  } else {
    if (orderside == "short") {
      return(sellorderqty)
    } else {
      return(buyorderqty)
    }
  }
}
psPorcentagemDoCapital <- function(data,timestamp,orderqty,ordertype,orderside,portfolio,symbol,tradeSize,maxSize,integerQty = TRUE,initEq=NULL,...) {
  pos <- getPosQty(portfolio, symbol, timestamp)
  datePos <- format(timestamp, "%Y-%m-%d")
  if (orderside == "short" && pos < 0) {
    return(0)
  } else if (orderside == "long" && pos > 0) {
    return(0)
  } else {
    if (orderside == "short") {
      updatePortf(
        Portfolio = portfolio,
        Symbol = symbol,
        Dates = paste0(start(data), "/", datePos)
      )
      pl <- sum(.getPortfolio(portfolio)$summary$Net.Trading.PL)
    } else {
      updatePortf(
        Portfolio = portfolio,
        Symbol = symbol,
        Dates = paste0(start(data), "/", datePos)
      )
      pl <- sum(.getPortfolio(portfolio)$summary$Net.Trading.PL)
    }
    qty <- ((initEq + pl) * 0.02) / as.numeric(Cl(data[datePos, ]))
    return(qty)
  }
}
psRiscopctDonchian <- function(data, timestamp,
                               orderqty, ordertype, orderside,
                               portfolio, symbol,
                               tradeSize , maxSize,
                               integerQty = TRUE, prefer = "Close", risk = 2, reinvest = FALSE, start_capital = 1000000, ...){
  pos <- getPosQty(portfolio, symbol, timestamp)
  if ((orderside=="long"  && pos>0) ||
      (orderside=="short" && pos<0))
    return(0)

  # -------- preço de entrada e stop
  prc   <- tryCatch(as.numeric(data[timestamp, prefer]),
                    error = function(e) NA)
  if (is.na(prc)) prc <- as.numeric(Cl(data[timestamp, ]))

  upper <- as.numeric(data[timestamp,"X.el"])
  lower <- as.numeric(data[timestamp,"Y.el"])
  if (anyNA(c(prc, upper, lower))) return(0)

  stopPrice  <- if (orderside=="long") lower else upper
  mult       <- tryCatch(getInstrument(symbol)$multiplier,
                         error = function(e) 1)

  riscoContr <- abs(prc - stopPrice) * mult
  if (riscoContr <= 0 || is.na(riscoContr)) return(0)

  # -------- patrimônio e perda máxima

  if(reinvest){
    print("Lucros Reinvestidos")
    updatePortf(portfolio)
    updateAcct(portfolio)
    updateEndEq(portfolio)
    datePos <- format(timestamp, "%Y-%m-%d")
    eqty <- getEndEq(portfolio,datePos)
    riscoPermitido  <- (risk/100) * eqty
  }
  else {
    print("Lucros Não Reinvestidos")
    riscoPermitido  <- (risk/100) * start_capital

  }
  print(riscoPermitido)
  # -------- quantidade
  qty <- floor(riscoPermitido / riscoContr)
  if (qty <= 0) return(0)

  if (orderside=="short") qty <- -qty
  print(qty)
  return(qty)
}
psRiscopctDonchian_DI <- function(data, timestamp,
                                  orderqty, ordertype, orderside,
                                  portfolio, symbol,
                                  tradeSize , maxSize,
                                  integerQty = TRUE, prefer = "Close", risk = 2, reinvest = FALSE, start_capital = 1000000,verbose=TRUE,...){
  pos <- getPosQty(portfolio, symbol, timestamp)
  if ((orderside=="long"  && pos>0) ||
      (orderside=="short" && pos<0))
    return(0)
  verbose = TRUE
  # -------- taxa de entrada e taxa do stop
  taxaEnt <- as.numeric(data[timestamp,prefer])
  upper   <- as.numeric(data[timestamp,"X.el"])
  lower   <- as.numeric(data[timestamp,"Y.el"])
  if(verbose) {
    print(paste("taxa de entrada:", taxaEnt))
    print(paste("upper:", upper))
    print(paste("lower:", lower))
  }
  if (anyNA(c(taxaEnt, upper, lower))) return(0)

  tipo_entrada <- if (orderside=="long") lower else upper
  if(verbose) print(tipo_entrada)

  # -------- dias úteis p/ vencimento
  vencimento_di <- attr(data, "maturity")
  dados_di <- calcular_pu_futuro(tipo_entrada,
                                 vencimento = vencimento_di,
                                 data_base = timestamp,
                                 cal       = cal_b3)
  dias_ate_vencimento <- dados_di$dias_uteis
  pu_entrada <- round(dados_di$pu,2)
  ticksize <- dados_di$tick_size
  tickvalue <- round(dados_di$tick_value,2)

  if(verbose) {
    print(paste("PU de entrada:", pu_entrada))
    print(paste("Dias Até Exp.:", dias_ate_vencimento))
    print(paste("Tick Size:", ticksize))
    print(paste("Tick Value:", tickvalue))
  }

  qty <- (((start_capital*(risk/100))/(upper-lower)/100)/tickvalue)/2
  qty <- floor(qty)
  if (qty <= 0) return(0)
  if (orderside=="short") qty <- -qty
  if(verbose)     print(paste("Contratos:", qty))
  return(qty)
}
eldoc_backtest <- function(ticker, verde_x = 40, vermelho_y = 40, ps = "pct", fee = "normal", inicio = "2000-01-01", final = Sys.Date(), verb = FALSE, only_returns = FALSE) {

  #rm.strat("elDoc")
  #rm.strat("elDoc")
  #rm.strat("elDoc")

  if(exists(ticker, envir = .GlobalEnv)) {
    dados <- get(ticker)
  } else {
    dados <- sm_get_data(ticker,start_date=inicio,end_date=final,future_history=FALSE, single_xts = TRUE, local_data=FALSE)
    print(str(dados))
  }
  #usar_close(ticker)

  bt_ticker <- paste0(ticker,"_BT_",verde_x,"_",vermelho_y,"_PS_",ps)
  print(bt_ticker)
  instrument_attr(ticker, "primary_id", bt_ticker)
  ticker <- bt_ticker
  print(ls_futures())

  verbose = TRUE
  initEq <- 1000000
  path.dependence <- TRUE

  portfolio.st = 'elDoc'
  account.st = 'elDoc'
  initPortf(portfolio.st, symbols = ticker)
  initAcct(account.st, portfolios = portfolio.st, initEq = initEq)
  initOrders(portfolio = portfolio.st)
  estrategia <- strategy(portfolio.st)

  tradeSize <- 999999
  bcontracts <- 1
  scontracts <- -1

  con <- quote(psQuantiadeContratosFixa)
  per <- quote(psPorcentagemDoCapital)
  pct <- quote(psRiscopctDonchian)
  di <- quote(psRiscopctDonchian_DI)

  isDI <- startsWith(ticker,"DI1")

  if(isDI){
    PositionSizing <- eval(di)
  } else {
    PositionSizing <- eval(pct)
  }

  TipodeOrdem <- 'market'

  LongEnabled <- TRUE
  ShortEnabled <- TRUE
  if(startsWith(ticker, "WIN")) ShortEnabled <- FALSE
  if(startsWith(ticker, "HASH")) ShortEnabled <- FALSE

  HighCol <- "High"
  LowCol  <- "Low"
  if ("PU_o" %in% colnames(dados)) Preference <- "PU_o" else Preference <- "Open"

  assign(ticker, dados, envir = .GlobalEnv)   # devolve a série renomeada
  ReplaceBuy <- FALSE
  ReplaceSell <- FALSE
  ReplaceShort <- FALSE
  ReplaceCover <- FALSE

  docx = verde_x
  docy = vermelho_y

  # Define TxnFeesVal dependendo de 'fee'
  if(fee == "nofee"){
    TxnFeesVal <- 0
  } else {
    TxnFeesVal <- "txfeeFUN"
  }

  estrategia <- add.indicator(
    estrategia, "eldoc",
    arguments = list(
      ticker = quote(mktdata),
      x      = docx,
      y      = docy,
      hi.col = HighCol,    # <- PU_h se for DI
      lo.col = LowCol,     # <- PU_l se for DI
      type   = "data"),
    label = "el")

  coisa <- applyIndicators(strategy = estrategia, mktdata = get(ticker))

  estrategia <-   add.signal(strategy = estrategia,
                             name = "sigCrossover",
                             arguments = list(
                               data = quote(mktdata),
                               columns = c(HighCol, "X.el"),
                               relationship = "gte"),
                             label = "Entrada")

  estrategia <-  add.signal(strategy = estrategia,
                            name = "sigCrossover",
                            arguments = list(
                              data = quote(mktdata),
                              columns = c(LowCol, "Y.el"),
                              relationship = "lte"),
                            label = "Saida")

  coisa <- applySignals(strategy = estrategia, mktdata = coisa)

  n.ent  <- sum(coisa$Entrada == 1, na.rm = TRUE)
  n.saida<- sum(coisa$Saida   == 1, na.rm = TRUE)
  dbg("sinais   Entrada:", n.ent, " Saída:", n.saida)

  # Regras Long (Entrada quando High >= X.el, saída quando Low <= Y.el)
  estrategia <-  add.rule(strategy = estrategia,
                          name = 'ruleSignal',
                          arguments = list(
                            sigcol = "Entrada",
                            sigval = TRUE,
                            datax = mktdata,
                            initEq = initEq,
                            orderqty = tradeSize,
                            portfolio = portfolio.st,
                            ordertype = TipodeOrdem,
                            orderside = if(!isDI) 'long' else 'short',
                            osFUN = eval(PositionSizing),
                            tradeSize = tradeSize,
                            buyorderqty = bcontracts,
                            sellorderqty = scontracts,
                            maxSize = 999999,
                            prefer = Preference,
                            replace = ReplaceBuy,
                            TxnFees = TxnFeesVal
                          ),
                          type = 'enter',
                          label = 'enterLong',
                          storefun = TRUE,
                          path.dep = path.dependence,
                          enabled = LongEnabled)

  estrategia <-  add.rule(strategy = estrategia,
                          name = 'ruleSignal',
                          arguments = list(
                            sigcol = "Saida",
                            sigval = TRUE,
                            orderqty = "all",
                            ordertype = TipodeOrdem,
                            orderside = if(!isDI) 'long' else 'short',
                            prefer = Preference,
                            replace = ReplaceSell,
                            TxnFees = TxnFeesVal
                          ),
                          type = 'exit',
                          label = 'exitLong',
                          storefun = TRUE,
                          path.dep = path.dependence,
                          enabled = LongEnabled)

  # Regras Short (Entrada quando Low <= Y.el, saída quando High >= X.el)
  estrategia <-  add.rule(strategy = estrategia,
                          name = 'ruleSignal',
                          arguments = list(
                            sigcol = "Saida",
                            sigval = TRUE,
                            datax = mktdata,
                            initEq = initEq,
                            orderqty = tradeSize,
                            portfolio = portfolio.st,
                            ordertype = TipodeOrdem,
                            orderside = if(isDI) 'long' else 'short',
                            osFUN = eval(PositionSizing),
                            tradeSize = -tradeSize,
                            buyorderqty = bcontracts,
                            sellorderqty = scontracts,
                            maxSize = -999999,
                            prefer = Preference,
                            replace = ReplaceShort,
                            TxnFees = TxnFeesVal
                          ),
                          type = 'enter',
                          label = 'enterShort',
                          storefun = TRUE,
                          path.dep = path.dependence,
                          enabled = ShortEnabled)

  estrategia <-  add.rule(strategy = estrategia,
                          name = 'ruleSignal',
                          arguments = list(
                            sigcol = "Entrada",
                            sigval = TRUE,
                            orderqty = "all",
                            ordertype = TipodeOrdem,
                            orderside = if(isDI) 'long' else 'short',
                            prefer = Preference,
                            replace = ReplaceCover,
                            TxnFees = TxnFeesVal
                          ),
                          type = 'exit',
                          label = 'exitShort',
                          storefun = TRUE,
                          path.dep = path.dependence,
                          enabled = ShortEnabled)

  start_t <- Sys.time()
  getInstrument(ticker)               # deve mostrar multiplier < 0
  # Roda Backtest
  applyStrategy(strategy = estrategia, portfolios = portfolio.st, verbose = FALSE, initEq = initEq)

  # depois de applyStrategy
  table(mktdata$Entrada, mktdata$Saida)
  # deve mostrar algum TRUE
  tx <- getTxns(portfolio.st, ticker)
  dbg("ordens geradas:", nrow(tx))
  if (nrow(tx) == 0) {
    warning("Nenhuma ordem criada.  Verifique coluna indicada em 'prefer' ",
            "e se o instrumento tem multiplier/tick_size definidos.")
    return(invisible(NULL))
  }
  # deve listar compras/vendas
  updatePortf(Portfolio = 'elDoc', prefer = Preference)
  updateAcct(name = 'elDoc')
  updateEndEq(Account = 'elDoc')

  getTxns('elDoc', ticker)

  port = getPortfolio('elDoc')
  book    = getOrderBook('elDoc')
  stats   = tradeStats('elDoc')
  ptstats = perTradeStats('elDoc')
  ptrets  = PortfReturns('elDoc')
  acrets  = AcctReturns('elDoc')
  txns    = getTxns('elDoc', ticker)
  Fee.n.Slip <- sum(txns$Txn.Fees)
  stats$Fee.n.Slip <- Fee.n.Slip

  # Imprimir resultados
  cat(paste0("Resultados para ", ticker, " - elDoc ", verde_x,"/",vermelho_y,"\n\n"))
  if(verb == TRUE) {
    print(stats)
    cat("\n")
    print(txns)
    cat("\n")
  }
  tab <- tabela_retornos_mensais(ptrets, retornar = TRUE, geometric = FALSE)
  print(tab)
  tab_rs <- tabela_lucro_mensal(port)
  print(tab_rs)

  cat("\nRetorno Anualizado:", Return.annualized(ptrets, geometric = FALSE), "\n")
  cat("Retorno Cumulativo:", Return.cumulative(ptrets, geometric = FALSE), "\n")
  index(ptrets) <- converter_posixct(ptrets)
  colnames(ptrets) <- "Discrete"
  stop_t <- Sys.time()
  cat(paste("\nTempo de execução:", stop_t - start_t))
  cat("\n----------------------------------------\n\n")

  if(only_returns) {
    return(ptrets)
  }

  stats$elDoc <- paste0(verde_x,"/",vermelho_y)
  stats$PosSiz <- ps
  stats$Multiplier <- dados$multiplier
  stats$TickSize <- dados$tick_size
  if(fee == "nofee"){
    stats$Slippage <- 0
    stats$Fees <- 0
  } else {
    stats$Slippage <- dados$identifiers$slippage
    stats$Fees <- dados$identifiers$fees
  }

  attr(ptrets, "backtest") <- TRUE
  attr(ptrets, "local") <- TRUE

  nomes_elementos <- c("rets",
                       #paste0(ticker,"_DC_",verde_x,"_",vermelho_y,"_PS_",ps),
                       #   paste0(ticker,"_stats_",verde_x,"_",vermelho_y,"_PS_",ps),
                       #  paste0(ticker,"_trades_",verde_x,"_",vermelho_y,"_PS_",ps),
                       # paste0(ticker,"_rets_acct_",verde_x,"_",vermelho_y,"_PS_",ps),
                       #paste0(ticker,"_mktdata_",verde_x,"_",vermelho_y,"_PS_",ps),
                       "stats","trades","rets_acct","mktdata")

  resultados <- setNames(
    list(ptrets, stats, txns, acrets, mktdata),
    nomes_elementos
  )
  rm(list = ticker, envir = .GlobalEnv)  # limpa o dado grande
  #rm(mktdata, envir = .GlobalEnv)
  #  assign("resultados",resultados, envir = .GlobalEnv)
  # assign("resultados",resultados, envir = .GlobalEnv)
  tplot(portfolio.st)

  #tplot(resultados[[1]],benchs="DOLAR")

  return(resultados)
}
tabela_retornos_mensais <- function(nome_do_objeto, retornar = TRUE, geometric = TRUE) {
  # Calcular os retornos mensais acumulados
  return_man_mensal <- apply.monthly(nome_do_objeto, colSums)
  colnames(return_man_mensal) <- "Ano"
  return_man_mensal <- table.CalendarReturns(return_man_mensal, digits = 2, geometric = geometric)

  # Converta o objeto em um data frame
  return_man_mensal_tabela <- as.data.frame(return_man_mensal)

  # Substitua os NA por espaços vazios
  return_man_mensal_tabela[is.na(return_man_mensal_tabela)] <- ""

  # Adicione o símbolo "%" após cada valor, incluindo para os anos
  for (col in colnames(return_man_mensal_tabela)) {
    return_man_mensal_tabela[[col]] <- sapply(return_man_mensal_tabela[[col]], function(x) {
      if (x != "") {
        paste0(x, "%")
      } else {
        x
      }
    })
  }

  # Renomeie os meses para português
  nomes_meses_pt <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez", "Total")
  colnames(return_man_mensal_tabela) <- nomes_meses_pt

  # Retornar a tabela se solicitado
  if (retornar) {
    return(return_man_mensal_tabela)
  }
}
tabela_lucro_mensal <- function(nome_do_objeto, retornar = TRUE) {
  # Extrair a coluna Net.Trading.PL do componente posPL
  lucro_diario <- nome_do_objeto$summary[, "Net.Trading.PL"]
  lucro_diario <- lucro_diario[-1]

  # Calcular os lucros mensais acumulados (soma por mês)
  lucro_mensal <- apply.monthly(lucro_diario, sum)

  # Extrair anos e meses
  anos <- format(index(lucro_mensal), "%Y")
  meses <- as.numeric(format(index(lucro_mensal), "%m"))

  # Criar matriz para a tabela (anos x 13 colunas: 12 meses + total)
  anos_unicos <- unique(anos)
  lucro_tabela <- matrix(NA, nrow = length(anos_unicos), ncol = 13)
  rownames(lucro_tabela) <- anos_unicos
  colnames(lucro_tabela) <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                              "Jul", "Ago", "Set", "Out", "Nov", "Dez", "Total")

  # Preencher a matriz com os lucros mensais
  for (i in 1:length(lucro_mensal)) {
    ano_idx <- which(anos_unicos == anos[i])
    mes_idx <- meses[i]
    lucro_tabela[ano_idx, mes_idx] <- as.numeric(lucro_mensal[i])
  }

  # Calcular totais anuais
  for (i in 1:nrow(lucro_tabela)) {
    lucro_tabela[i, 13] <- sum(lucro_tabela[i, 1:12], na.rm = TRUE)
  }

  # Converter para data frame
  lucro_mensal_tabela <- as.data.frame(lucro_tabela)

  # Formatar para notação brasileira
  for (col in colnames(lucro_mensal_tabela)) {
    lucro_mensal_tabela[[col]] <- sapply(lucro_mensal_tabela[[col]], function(x) {
      if (is.na(x)) {
        ""
      } else {
        format(round(x), big.mark = ".", decimal.mark = ",", nsmall = 0)
      }
    })
  }

  # Retornar a tabela se solicitado
  if (retornar) {
    return(lucro_mensal_tabela)
  }
}
# ES - mini SP500
# ZO - Oats (Aveia)
# SR3 - 3 month SOFR (Taxa SOFR de 3 meses)
# ZN - 10-year T-Note (Tesouro Americano 10 anos)
# ZF - 5-year T-Note (Tesouro Americano 5 anos)
# ZT - 2-year T-Note (Tesouro Americano 2 anos)
# TN - Ultra 10-year T-Note (Tesouro Ultra 10 anos)
# SR1 - 1 month SOFR (Taxa SOFR de 1 mês)
# CL - Crude Oil (Petróleo Bruto)
# ZC - Corn (Milho)
# NG - Natural Gas (Gás Natural)
# ZS - Soybeans (Soja)
# GC - Gold (Ouro)
# ZL - Soybean Oil (Óleo de Soja)
# MGC - Micro Gold (Micro Contratos de Ouro)
# ZM - Soybean Meal (Farelo de Soja)
# ZW - Wheat (Trigo)
# RB - RBOB Gasoline (Gasolina)
# SI - Silver (Prata)
# HG - Copper (Cobre)
# 6L - Brazilian Real (Real Brasileiro)
# LE - Live Cattle (Boi Gordo)
# PL - Platinum (Platina)
# HE - Lean Hogs (Porco Magro)
# KE - KC Wheat (Trigo KC)
# SIL - Micro Silver (Micro Contratos de Prata)
# GF - Feeder Cattle (Gado de Alimentação)
# NGT - Natural Gas Last Day (Gás Natural Último Dia)
# PA - Palladium (Paládio)
# QG - E-mini Natural Gas (Mini Gás Natural)
# QM - E-mini Crude Oil (Mini Petróleo)
# XK - Mini-sized Soybeans (Mini Soja)
# DC - Class III Milk (Leite Classe III)
# ALI - Aluminum (Alumínio)
# LBR - Lumber (Madeira)
# ZR - Rough Rice (Arroz em Casca)
# MTN - Montreal Exchange (Bolsa de Montreal)
# XC - Mini-sized Corn (Mini Milho)
# QO - E-mini Gold (Mini Ouro)
# XW - Mini-sized Wheat (Mini Trigo)
# XAV - Mini-sized Soybeans (Mini Soja)
# QI - E-mini Silver (Mini Prata)

inicio <- "2000-01-01"
final <- Sys.Date()#"2024-12-04"

ativo = "ZC_"
tf <- "1H"  # ou "4H" ou "1D", etc.

ex <- 120
ey <- 30

a <- eldoc_backtest("WDOFUT_4H", verde_x = 50, vermelho_y = 50, verb = TRUE, only_returns = FALSE)
b <- eldoc_backtest("WDOFUT_1H", verde_x = 50, vermelho_y = 50, verb = TRUE, only_returns = FALSE)
