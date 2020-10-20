#' Title
#'
#' @param numero
#' @param name
#' @param iniciosVacios
#' @param enunciadosEquivalentes
#' @param validas
#' @param invalidas
#' @param OK
#' @param KO
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
maquetaHTML <- function(numero,name,iniciosVacios,enunciadosEquivalentes, finalesVacios, validas, invalidas,OK,KO,marcadas=TRUE,...){
  if(marcadas) validas=sprintf("<b>%s</b>",validas)
  opciones=c(validas,invalidas) %>% sample()
  str_c("<hr>\n<p><b>",numero,"</b>) ",iniciosVacios, " ", enunciadosEquivalentes, " ",finalesVacios,"</p>\n<ol type = \"a\">\n",
        #paste0(sprintf("<br>%s. %s<br>",letters[1:length(opciones)],opciones),collapse="\n")
        paste0(sprintf("\t<li>%s</li>",opciones),collapse="\n"),"\n</ol>"
        )
}
