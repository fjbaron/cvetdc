#' Title
#'
#' @param .l
#'
#' @return
#' @export
#'
#' @examples
lista2tibble <- function(.l){
  creaPreguntas(.l$iniciosVacios,
                .l$enunciadosEquivalentes,
                .l$finalesVacios,
                .l$opcionesValidas,
                .l$opcionesInvalidas,
                .l$numOpciones,
                .l$numValidas  ) %>% sample_frac(1)
  }
