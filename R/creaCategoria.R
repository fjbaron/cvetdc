#' Title
#'
#' @param category
#' @param XML
#'
#' @return
#' @export
#'
#' @examples
creaCategoria <- function(category,XML){
sprintf("<!-- question: 0  -->\n<question type=\"category\"><category><text>%s</text></category></question>\n%s",
        category,XML)
}
