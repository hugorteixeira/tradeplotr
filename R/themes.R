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
      table_row_txt="#000000", footer_txt="#000000",
      range_selector_txt="#222222", range_selector_bg="#f2f2f2", range_selector_border="#cccccc"
    ),
    hc_margin  = c(15,0,20,75),
    hc_spacing = c(0,0,0,0),
    candles = list(
      up_color   = "#1a9f4b",
      down_color = "#cc3d3d",
      line_color = "#555555",
      line_width = 0.5,
      point_width= 3,
      height = 300,
      grouping   = FALSE
    ),
    volume = list(
      height = 70
    ),
    position = list(
      height = 150
    ),
    cumret = list(
      height = 200
    ),
    rollingret = list(
      height = 100
    ),
    periodret = list(
      height = 100
    ),
    drawdown = list(
      height = 100
    ),
    ft_font_size = "12px",
    ft_margin = "5px",
    ft_font_family = "Montserrat",
    footer_text = "rTradingPlots | Licensed under GPL-3 | github.com/hugorteixeira"
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
      table_row_txt="#CCCCCC", footer_txt="#AAAAAA",
      range_selector_txt="#E0E0E0", range_selector_bg="#2A2A2A", range_selector_border="#444444"
    ),
    hc_margin  = c(15,0,20,65),
    hc_spacing = c(5,0,0,0),
    candles = list(
      up_color   = "#00d175",
      down_color = "#ff4d4d",
      line_color = "#bbbbbb",
      line_width = 0.5,
      point_width= 3,
      height = 400,
      grouping   = FALSE
    ),
    volume = list(
      height = 130
    ),
    position = list(
      height = 130
    ),
    cumret = list(
      height = 130
    ),
    rollingret = list(
      height = 130
    ),
    periodret = list(
      height = 130
    ),
    drawdown = list(
      height = 130
    ),
    ft_font_size = "12px",
    ft_margin = "5px",
    ft_font_family = "Montserrat",
    footer_text = "rTradingPlots | Licensed under GPL-3 | github.com/hugorteixeira"
  )
}

#' @title Fancy Theme (super chic)
#' @export
fancy_theme <- function(){
  list(
    palette     = c("#7F00FF","#FF00FF","#00E5FF","#FFC300","#FF5733"),
    font_family = "Poppins",
    font_sizes  = list(axis=12,title=16,legend=12,table=12),
    colors = list(
      page_bg="#FAFAFD", page_txt="#2B2B2B", chart_bg="#FFFFFF",
      axis_txt="#444444", title_txt="#1F1F1F", legend_txt="#333333",
      table_header_bg="#EFEFFA", table_header_txt="#1F1F1F",
      table_row_txt="#2B2B2B", footer_txt="#666666",
      range_selector_txt="#333333", range_selector_bg="#EFEFFA", range_selector_border="#BBAAFF"
    ),
    hc_margin  = c(20,10,20,80),
    hc_spacing = c(8,4,0,4),
    candles = list(
      up_color   = "#20BF55",
      down_color = "#F54748",
      line_color = "#7A7A7A",
      line_width = 0.5,
      point_width= 3,
      height = 400,
      grouping   = FALSE
    ),
    volume = list(
      height = 130
    ),
    position = list(
      height = 130
    ),
    cumret = list(
      height = 130
    ),
    rollingret = list(
      height = 130
    ),
    periodret = list(
      height = 130
    ),
    drawdown = list(
      height = 130
    ),
    ft_font_size = "12px",
    ft_margin = "5px",
    ft_font_family = "Poppins",
    footer_text = "rTradingPlots | Licensed under GPL-3 | github.com/hugorteixeira"
  )
}

#' @title Pro Theme (super professional)
#' @export
pro_theme <- function(){
  list(
    palette     = c("#2F80ED","#27AE60","#EB5757","#F2C94C","#9B51E0"),
    font_family = "Inter",
    font_sizes  = list(axis=11,title=14,legend=11,table=11),
    colors = list(
      page_bg="#F6F7F9", page_txt="#1B1E23", chart_bg="#FFFFFF",
      axis_txt="#4F5969", title_txt="#1B1E23", legend_txt="#4F5969",
      table_header_bg="#F0F2F5", table_header_txt="#1B1E23",
      table_row_txt="#1B1E23", footer_txt="#6B7280",
      range_selector_txt="#1B1E23", range_selector_bg="#F0F2F5", range_selector_border="#D1D5DB"
    ),
    hc_margin  = c(18,8,18,70),
    hc_spacing = c(6,4,0,4),
    candles = list(
      up_color   = "#27AE60",
      down_color = "#EB5757",
      line_color = "#B0B7C3",
      line_width = 0.5,
      point_width= 3,
      height = 400,
      grouping   = FALSE
    ),
    volume = list(
      height = 130
    ),
    position = list(
      height = 130
    ),
    cumret = list(
      height = 130
    ),
    rollingret = list(
      height = 130
    ),
    periodret = list(
      height = 130
    ),
    drawdown = list(
      height = 130
    ),
    ft_font_size = "12px",
    ft_margin = "5px",
    ft_font_family = "Inter",
    footer_text = "rTradingPlots | Licensed under GPL-3 | github.com/hugorteixeira"
  )
}

#' @title Hacker Theme (stylish hacker)
#' @export
hacker_theme <- function(){
  list(
    palette     = c("#33FF99","#00CCFF","#FF33CC","#FFFF66","#FF6666"),
    font_family = "Fira Code",
    font_sizes  = list(axis=12,title=15,legend=12,table=12),
    colors = list(
      page_bg="#0B0F10", page_txt="#C5D1D3", chart_bg="#0E1416",
      axis_txt="#7A8C8E", title_txt="#EAF2F3", legend_txt="#9FB1B3",
      table_header_bg="#0F1A1C", table_header_txt="#9FB1B3",
      table_row_txt="#C5D1D3", footer_txt="#7A8C8E",
      range_selector_txt="#C5D1D3", range_selector_bg="#102227", range_selector_border="#1C3439"
    ),
    hc_margin  = c(15,8,20,70),
    hc_spacing = c(6,2,0,2),
    candles = list(
      up_color   = "#33FF99",
      down_color = "#FF6666",
      line_color = "#4A5C5E",
      line_width = 0.5,
      point_width= 3,
      height = 400,
      grouping   = FALSE
    ),
    volume = list(
      height = 130
    ),
    position = list(
      height = 130
    ),
    cumret = list(
      height = 130
    ),
    rollingret = list(
      height = 130
    ),
    periodret = list(
      height = 130
    ),
    drawdown = list(
      height = 130
    ),
    ft_font_size = "12px",
    ft_margin = "5px",
    ft_font_family = "Fira Code",
    footer_text = "rTradingPlots | Licensed under GPL-3 | github.com/hugorteixeira"
  )
}
