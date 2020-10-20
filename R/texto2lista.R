#' Title
#'
#' @param text
#'
#' @return
#' @export
#'
#' @examples
texto2lista <- function(text){
  text %>%
    str_split("###referencia") %>%
    unlist() %>%
    map_chr(str_trim) %>%
    .[str_length(.)>=10] %>%
    map(extraePregunta) %>%
    unlist(recursive=FALSE)
}
