fun_pack <- function(a = 10, b = 20, d = 2){

  out <- sqrt(a*b*d/2 + 100)
  out <- rnorm(n = round(out, 0)) %>%
    as.data.frame()

  out <- out %>%
    summarise(mean = mean(., na.rm = T))

  return(out)
}
