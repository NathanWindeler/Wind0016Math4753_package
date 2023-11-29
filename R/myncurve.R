#' Displays the normal curve, with the shaded area from -Inf to x=a
#' and calculates the area
#'
#' @param mu Mean of the normal curve
#' @param sigma Standard deviation of the normal curve
#' @param a Function returns P(X<=a)
#' @return Area of the shaded region, the probability of X <= a. Rounded to 4 digits
#' @examples
#' myncurve(mu = 10, sigma = 5, a = 6)
#'
#' @export
myncurve = function(mu, sigma, a){
  # fix annoying check problem, hides note: "no visible binding for global variable 'x'"
  x <- NULL

  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3*sigma, mu + 3*sigma))
  # 'from' has to be a finite number so we just choose an arbitrarily large number instead of -Inf
  # with an appropriately increased length
  xcurve=seq(-99999, a, length=100000)
  ycurve=dnorm(xcurve, mean = mu, sd = sigma)
  polygon(c(-Inf, xcurve, a), c(0, ycurve, 0), col="Red")
  prob=pnorm(a, mean = mu, sd = sigma)
  prob=round(prob, 4)
  return(prob)
}
