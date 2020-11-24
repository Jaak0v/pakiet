
wykres <- function(out,x,y) {

  x <- enquo(x)
  y <- enquo(y)

  ggplot(out)+ geom_boxplot(out, mapping = aes(!!x, !!y)) }

