#' @title Default Theme for tplot Charts
#' @description Returns a list with color and font settings for the light theme.
#' @return A list of theme settings.
#' @export
default_theme <- function(){
  list(
    palette     = c("limegreen","royalblue","orange","lightpink","red","lightgreen"),
    font_family = "Montserrat",
    font_sizes  = list(axis=10,title=12,legend=10,table=10),
    colors = list(
      page_bg="#ffffff", page_txt="#000000", chart_bg="#ffffff",
      axis_txt="#000000", title_txt="#000000", legend_txt="#000000",
      table_header_bg="#ffffff", table_header_txt="#000000",
      table_row_txt="#000000", footer_txt="#000000"
    ),
    hc_margin  = c(15,0,20,75),
    hc_spacing = c(0,0,0,0),
    ft_font_size = "12px",
    ft_margin = "5px",
    ft_font_family = "Montserrat",
    footer_text = "rTradingPlots package by Hugo Rzepian Teixeira | Licensed under GPL-3 | www.senhormercado.com.br"
    )
}

#' @title Dark Theme for tplot Charts
#' @description Returns a list with color and font settings for the dark theme.
#' @return A list of theme settings.
#' @export
dark_theme <- function(){
  list(
    palette     = c("#00d175","#5991ff","#ffa600","#ff4d4d"),
    font_family = "Montserrat",
    font_sizes  = list(axis=12,title=14,legend=12,table=12),
    colors = list(
      page_bg="#1E1E1E", page_txt="#E0E0E0", chart_bg="#1E1E1E",
      axis_txt="#E0E0E0", title_txt="#FFFFFF", legend_txt="#E0E0E0",
      table_header_bg="#2A2A2A", table_header_txt="#FFFFFF",
      table_row_txt="#CCCCCC", footer_txt="#AAAAAA"
    ),
    hc_margin  = c(15,0,20,65),
    hc_spacing = c(5,0,0,0),
    ft_font_size = "12px",
    ft_margin = "5px",
    ft_font_family = "Montserrat",
    footer_text = "rTradingPlots package by Hugo Rzepian Teixeira | Licensed under GPL-3 | www.senhormercado.com.br"
  )
}
