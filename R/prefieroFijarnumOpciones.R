#' Title
#'
#' @param numOpciones
#' @param numValidasDeseadas
#' @param preg
#'
#' @return
#' @export
#'
#' @examples
prefieroFijarnumOpciones=function(preg,numOpciones=5,numValidasDeseadas=1:4){
  list( numOpciones=min(numOpciones,length(preg$opcionesValidas)+length(preg$opcionesInvalidas)),
        numValidas= intersect(numOpciones-(1:length(preg$opcionesInvalidas)),numValidasDeseadas)
  )
}
