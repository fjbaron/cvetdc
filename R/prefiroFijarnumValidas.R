#' Title
#'
#' @param numValidas
#' @param opcionesDeseadas
#' @param preg
#'
#' @return
#' @export
#'
#' @examples
prefieroFijarnumValidas=function(preg,numValidas=1,opcionesDeseadas=5){
  list(       numOpciones=min(opcionesDeseadas,
                              min(numValidas,length(preg$opcionesValidas))+
                                length(preg$opcionesInvalidas)),
              numValidas=numValidas
)
}

