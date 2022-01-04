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
maquetaXML <- function(numero,name,iniciosVacios,enunciadosEquivalentes, finalesVacios, validas, invalidas,OK,KO,...){

  opcionesXML=maquetaOpcionXML(validas,invalidas,OK,KO)

  isSingle=as.character(ifelse(length(validas)>1,"false","true"))

  sprintf("
<!-- question: %s  -->
<question type=\"multichoice\">
    <name>
      <text>%s</text>
    </name>
    <questiontext format=\"html\">
      <text><![CDATA[%s %s %s]]></text>
    </questiontext>
    <generalfeedback format=\"html\">
      <text></text>
    </generalfeedback>
    <defaultgrade>1.0000000</defaultgrade>
    <penalty>0.3333333</penalty>
    <hidden>0</hidden>
    <single>%s</single>
    <shuffleanswers>false</shuffleanswers>
    <answernumbering>abc</answernumbering>
    <correctfeedback format=\"html\">
      <text>Respuesta correcta</text>
    </correctfeedback>
    <partiallycorrectfeedback format=\"html\">
      <text>Respuesta parcialmente correcta.</text>
    </partiallycorrectfeedback>
    <incorrectfeedback format=\"html\">
      <text>Respuesta incorrecta.</text>
    </incorrectfeedback>
    <shownumcorrect/>
    %s
  </question>
",  name,name,iniciosVacios,enunciadosEquivalentes,finalesVacios,isSingle,opcionesXML)
}
