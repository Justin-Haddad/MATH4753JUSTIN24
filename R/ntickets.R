#' Title
#'
#' @param N iterations
#' @param gamma gamma distruvution
#' @param p probability
#' @importFrom graphics abline curve hist segments text
#' @importFrom qbinom
#'
#'
#' @return nd, ns, and discrete and normal graphs
#' @export
#'
#'
#' @examples
ntickets <- function(N, gamma, p) {
  # Calculate nd using the appropriate discrete distribution
  nd = qbinom(p=1-gamma,size=N, prob=p)+1

  # Calculate nc using normal approximation
  mean <- (N / p)
  sd <- sqrt(N * p * (1 - p))
  nc = qnorm(p=1-gamma, mean=N*p, sd=sqrt(N*p*(1-p)))


  ## Print named list containing nd, nc, N, p, and gamma
  print(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))

  # Create a plot of Objective function Vs n for discrete case
  n_values <- 1:N
  objective_function_discrete <- 1 - gamma - ppois(n_values, lambda = N)
  plot(n_values, rev(objective_function_discrete), type = "l",
       xlab = "Number of tickets sold", ylab = "Objective function",
       main = "Objective function Vs n (Discrete Case)")

  # Create a plot of Objective function Vs n for continuous case
  objective_function_continuous <- 1 - gamma - pnorm(n_values, mean = mean, sd = sd)
  plot(n_values, rev(objective_function_continuous), type = "l",
       xlab = "Number of tickets sold", ylab = "Objective function",
       main = "Objective function Vs n (Continuous Case)")

  # Return a list containing both objective functions
  return(list(discrete = objective_function_discrete, continuous = objective_function_continuous))
}
