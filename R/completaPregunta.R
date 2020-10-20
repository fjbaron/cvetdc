#' Title
#'
#' @param preg
#' @param preferencia
#'
#' @return
#' @export
#'
#' @examples
completaPregunta <- function(preg,preferencia=prefieroFijarnumValidas){
  print(preg)
  predefinido=preferencia(preg)
  if(is.null(preg$numOpciones) || length(preg$numOpciones)==0 || preg$numOpciones[1]=="" ||
     is.null(preg$numValidas)  || length(preg$numValidas)==0  ||  preg$numValidas[1]=="") {
    preg$numValidas=predefinido$numValidas
    preg$numOpciones=predefinido$numOpciones
  }
  preg$numValidas=as.integer(preg$numValidas)
  preg$numOpciones=as.integer(preg$numOpciones)
    preg
}
