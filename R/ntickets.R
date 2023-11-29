#' Solves the "airline overbooking problem"
#'
#'     Given the number of seats available, acceptable overbooking rate, and the
#' expected attendance rate, calculates the number of tickets that should be sold.
#' Calculates the number of tickets using both a binomial distribution and an
#' equivalent normal distribution.
#'
#'     Only checks the range [N, N*1.1]
#'
#' @param N Number of available seats (integer)
#' @param gamma Acceptable overbooking rate (between 0.0 and 1.0)
#' @param p Expected rate of attendance (between 0.0 and 1.0)
#'
#' @return Named list containing number of tickets calculated with continuous distribution (cn), discrete (dn), and the given inputs
#'
#' @examples
#' ntickets(200, 0.02, 0.95)
#'
#' @export
ntickets = function(N, gamma, p) {
  # N = number of seats in flight
  # p = probability a ticket-buyer shows up for the flight
  # gamma = probability the plane will be overbooked

  # First, get some numbers to start and end looking at
  nrange = N:(N * 1.1)

  # Discrete Distribution Objective Function
  dobjective = function(n){
    # 0 = 1 - gamma - pbinom()
    return(1 - pbinom(N, n, p))
  }

  # Get overbooking risk for each number in range
  vector = dobjective(nrange)
  # Find highest number of tickets under the acceptable risk gamma
  nd = N + which.max(vector[vector <= gamma])

  # Continuous Distribution Objective Function
  cobjective = function(n){
    # 0 = 1 - gamma - pnorm()
    # mean = np, var = npq
    return (1 - pnorm(N+0.5, mean = n*p, sd = sqrt(n*p*(1-p))))
  }

  # Find number of tickets with continuous distribution
  temp = function(n){
    return(abs(cobjective(n) - gamma))
  }
  nc = optimize(temp, nrange)$minimum

  # Now that we have nd and nc, construct the named list
  output = list(nc = nc, nd = nd, N = N, p = p, gamma = gamma)

  # Now make the two graphs
  # Plot discrete numbers
  newdf = data.frame(n = nrange, Objective = dobjective(nrange))
  title = paste0("Objective Vs n to find optimal tickets sold\n", "(", nd, ") gamma = ", gamma, " N = ", N, " discrete")
  plot(newdf, bg = "Blue", pch = 21, cex = 1.2, type = "b", main = title, xlab = "n", ylab = "Objective")
  # Crossing lines
  abline(v = nd, col = "red", lwd = 3)
  abline(h = dobjective(nd), col = "red", lwd = 3)

  # Plot continuous numbers
  newdf = data.frame(n = nrange, Objective = cobjective(nrange))
  title = paste0("Objective Vs n to find optimal tickets sold\n", "(", nc, ") gamma = ", gamma, " N = ", N, " continuous")
  plot(newdf, bg = "Blue", pch = 21, cex = 1.2, type = "l", main = title, xlab = "n", ylab = "Objective", ylim = c(0, 1))
  # Crossing lines
  abline(v = nc, col = "blue")
  abline(h = cobjective(nc), col = "blue")

  return(output)
}
