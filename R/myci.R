#' Calculates the (1-a)100\% confidence interval for the mean of a single sample x
#'
#' @param x the sample set
#' @param a alpha in the (1-a)100\% confidence interval formula
#'
#' @return A named list containing the lower and upper bounds of the interval, as well as the given alpha value
#'
#' @examples
#' myci(c(1,2,3,4), a = 0.10)
#'
#' @export
myci = function(x, a = 0.05) {
  mu = mean(x)
  t = qt(p = 1 - a/2, df = (length(x)-1))
  margin = t * sd(x) / sqrt(length(x))

  out <- list(lower = (mu - margin),
              upper = (mu + margin),
              alpha = a
              )
  return(out)
}
