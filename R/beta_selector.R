#' Beta Selector
#'
#' @param t A numeric vector of angles to calculate the depth.
#' @param beta Locality parameter.
#'
#' @return a list width the beta selected, the p.value and the vector of standard deviations.
#'
#' @examples
#'
#' library(circular)
#' tita = as.numeric(rmixvonmises(1000, mu1=pi, mu2=0, kappa1=10, kappa2=10, prop=0.3))
#' beta_selector(t=tita)
#'

library(trend)

beta_selector = function(t, alpha=0.05, beta_grid=seq(0.1,1,0.05)) {
  q = length(beta_grid)
  n = length(t)
  # Desvios
  desvio = numeric(q-1)
  for (b in 1:(q-1)) desvio[b] = sd(cld(t, beta_grid[b+1]) - cld(t,beta_grid[b]))
  pet = pettitt.test(desvio)
  if (pet$p.value < alpha) {
    beta = min(beta_grid[pet$estimate[1]], 1-beta_grid[pet$estimate[1]])
  } else {
    beta = 0.2
  }
  salida = list(beta, pet$p.value, desvio)
  names(salida) = c("beta", "p.value", "desvios")
  return(salida)
}


