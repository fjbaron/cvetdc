#' Title
#'
#' @param input
#'
#' @return
#' @export
#'
#' @examples
maquetaTXT <- function(input){
  sprintf(
    "###referencia
%s

###iniciosVacios
%s

###enunciadosEquivalentes
%s

###finalesVacios
%s

###opcionesValidas
%s

###opcionesInvalidas
%s

###numOpciones
%s

###numValidas
%s
",input$referencia,
    paste0(input$iniciosVacios,collapse="\n"),
    paste0(input$enunciadosEquivalentes,collapse="\n"),
    paste0(input$finalesVacios,collapse="\n"),
    paste0(input$opcionesValidas,collapse="\n"),
    paste0(input$opcionesInvalidas,collapse="\n"),
    paste0(input$numOpciones,collapse="\n"),
    paste0(input$numValidas,collapse="\n"))
}
