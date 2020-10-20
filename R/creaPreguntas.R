#' Title
#'
#' @param iniciosVacios
#' @param enunciadosEquivalentes
#' @param opcionesValidas
#' @param opcionesInvalidas
#' @param numOpciones
#' @param numValidas
#' @param noSe
#'
#' @return
#' @export
#'
#' @examples
creaPreguntas <- function(.l){
  referencia=.l$referencia
  iniciosVacios=.l$iniciosVacios
  finalesVacios=.l$finalesVacios
  enunciadosEquivalentes=.l$enunciadosEquivalentes
  opcionesValidas=.l$opcionesValidas
  opcionesInvalidas=.l$opcionesInvalidas

  if(is.null(iniciosVacios)||length(iniciosVacios)==0) iniciosVacios=""
  if(is.null(finalesVacios)||length(finalesVacios)==0) finalesVacios=""

  numOpciones=as.integer(.l$numOpciones)
  numValidas=as.integer(.l$numValidas)
  if (is.null(numValidas) || length(numValidas)==0 || is.na(numValidas[1]) ||   numValidas[1]<1 ||
      is.null(numOpciones)|| length(numOpciones)==0 ||is.na(numOpciones[1]) || numOpciones[1] < 2){
    cat("Esto deberia haberse decidido antes: numOpciones=",numOpciones," numValidas=",numValidas)
    return(data.frame())
  }


#  dfEnunciados=crossing(referencia,enunciadosEquivalentes,iniciosVacios) %>% mutate(finalesVacios=sample(finalesVacios,size =nrow(.),replace = TRUE))

  dfEnunciados=crossing(referencia,enunciadosEquivalentes)


  dfOpciones=numValidas %>% map_df(todasLasValidas %>% partial(opciones=opcionesValidas))%>%
    as_tibble() %>%
    rename(validas=elegidas) %>%
#    filter(map_int(validas,length)+map_int(opcionesInvalidas,length)>=numOpciones) %>%
    mutate(invalidas=pmap(.,function(validas,...)sample(opcionesInvalidas,size = numOpciones-length(validas)))) %>%
    mutate(OK = map_dbl(validas,puntos),
           KO = - map_dbl(invalidas,puntos),
           numValidas=map_int(validas,length),
           numOpciones=numOpciones)

  #print(dfOpciones)
  dfEnunciados %>% crossing(dfOpciones)   %>% mutate(numero=row_number())   %>%
    mutate(iniciosVacios=sample(iniciosVacios,size =nrow(.), replace = TRUE),
          finalesVacios=sample(finalesVacios,size =nrow(.),replace = TRUE))

}
