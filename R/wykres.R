#' Stworzenie wykresu
#'
#'
#' @return plot
#' @param x double
#' @param y double
#' @param out double
#' @import dplyr tidyverse ggplot2 magrittr
#' @export
#'
#' @examples

wykres <- function(out,x,y,z) {

  x <- enquo(x)
  y <- enquo(y)
  z <- enquo(z)

  ggplot(out,aes(!!x, !!y, colour = !!z)) +
    geom_point() +
    theme_bw() -> p

  return(p)
  }

