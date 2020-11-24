#' Obliczenia emisji spalin
#'
#' @param dane
#' @param kategoria
#' @param euro
#' @param mode
#' @param substancja
#'
#' @return
#' @import dplyr tidyverse ggplot2
#' @export
#'
#' @examples
fun_pack <- function(dane = input,
                     kategoria = "Passenger Cars",
                     #paliwo = "Petrol",
                     #segment = "Mini",
                     euro = "Euro 5",
                     #technologia = "",
                     mode = "",
                     substancja = c("CO", "EC")) {
  out <- wskazniki %>%
    filter(Category %in% kategoria) %>%
    filter(Euro.Standard %in% euro) %>%
    filter(Pollutant %in% substancja) %>%
    filter(Mode %in% mode)
    #filter(Fuel %in% paliwo)

  out <- inner_join(x = out, y = input, by = c("Segment","Fuel","Technology"))

  out <- out %>%
    mutate(Emisja = Nat * ((Alpha * Procent ^ 2 + Beta * Procent + Gamma + (Delta/Procent))/
                             (Epsilon * Procent ^ 2 + Zita * Procent + Hta) * (1-Reduction))
    ) %>%
    select(Category, Fuel, Euro.Standard, Technology, Pollutant, Mode, Segment, Emisja, Nat)

  return(out)

}

wykres <- function(dane = input,
                     kategoria = "Passenger Cars",
                     #paliwo = "Petrol",
                     #segment = "Mini",
                     euro = "Euro 5",
                     #technologia = "",
                     mode = "",
                     substancja = c("CO", "EC")) {
  emi <- wskazniki %>%
    filter(Category %in% kategoria) %>%
    filter(Euro.Standard %in% euro) %>%
    filter(Pollutant %in% substancja) %>%
    filter(Mode %in% mode)
    #filter(Fuel %in% paliwo)

  emi <- inner_join(x = emi, y = input, by = c("Segment","Fuel","Technology"))

  emi <- emi %>%
    mutate(Emisja = Nat * ((Alpha * Procent ^ 2 + Beta * Procent + Gamma + (Delta/Procent))/
                             (Epsilon * Procent ^ 2 + Zita * Procent + Hta) * (1-Reduction))
    ) %>%
    select(Fuel)


  return(emi)

}
