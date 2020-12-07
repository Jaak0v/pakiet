#' Wskazniki emisji z transportu drogowego EEA
#'
#'@source Europejska Agencja Srodowiska (EEA) \url{https://www.eea.europa.eu/}
#' @format Ramka danych zawiera:
#' \describe{
#'  \item{Category}{character, zawiera informacje o typie samochody np. Passenger Cars}
#'  \item{Fuel}{character, rodzaj paliwa np. petrol}
#'  \item{Segment}{character, klasa samochodu np. mini}
#'  \item{Euro.Standard}{character, Europejski standard emisji spalin np. Euro 6 up to 2016}
#'  \item{Technology}{character, technologia użyta w silniku np. GDI}
#'  \item{Pollutant}{character, rodzaj zanieczyszczenia np. CH4}
#'  \item{Mode}{character, zawiera rodzaj drogi np. Highway}
#'  \item{Alpha}{numeric, parametr potrzebny do szacowania emisji}
#'  \item{Beta}{numeric, parametr potrzebny do szacowania emisji}
#'  \item{Gamma}{numeric, parametr potrzebny do szacowania emisji}
#'  \item{Delta}{numeric, parametr potrzebny do szacowania emisji}
#'  \item{Epsilon}{numeric, parametr potrzebny do szacowania emisji}
#'  \item{Zita}{numeric, parametr potrzebny do szacowania emisji}
#'  \item{Hta}{numeric, parametr potrzebny do szacowania emisji}
#'  \item{Reduction}{numeric,wspólczynnik redukcji wyrazony w %}
#'  \item{Bio}{numeric, wspólczynnik redykcji BIO wyrazony w %}
#'  \item{Procent}{numeric, wspolczynnik procent}
#' }
#' @examples
#' \dontrun{
#'  wskazniki
#'
#'
#' }
"wskazniki"

