#' Title
#'
#' @param texto
#'
#' @return
#' @export
#'
#' @examples
quizMoodle <- function(texto){
sprintf(
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<quiz>
%s

</quiz>",
  texto)
}
