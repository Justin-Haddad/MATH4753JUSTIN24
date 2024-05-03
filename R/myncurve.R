#' Title
#'
#' @param mu mean
#' @param sigma standard deviation
#'
#' @return display of curve with probability area
#' @export
#'
#' @examples
myncurve = function(mu, sigma){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu +
                                              3*sigma))
  list(mu = mu, sigma = sigma)
}
