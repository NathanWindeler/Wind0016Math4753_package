#' Calculate z values of a vector
#'
#' @param vector A data vector
#' @return A vector containing the Z values of the given data vector
#' @examples
#' z <- zValues(c(1, 2, 3))
#'
#' @export
zValues <- function(vector) {
  z <- (vector-mean(vector))/sd(vector)
  return(z)
}
