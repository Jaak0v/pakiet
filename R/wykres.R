#' Stworzenie wykresu
#'
#'
#' @return
#' @import dplyr tidyverse ggplot2 magrittr
#' @export
#'
#' @examples


wykres <- function(out,x,y) {

  x <- enquo(x)
  y <- enquo(y)

  ggplot()+ geom_boxplot(out, mapping = aes(!!x, !!y)) + theme_bw() }

