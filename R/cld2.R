#' Circular Local Depth
#'
#' @param x A numeric vector of angles to calculate the depth.
#' @param data A numeric vector use to calculate depth.
#' @param beta Locality parameter.
#'
#' @return a numeric vector with the circular depth for each value of t.
#' @examples
#'
#' library(circular)
#' tita = as.numeric(rvonmises(100, mu=pi, kappa=5))
#' cld(t=tita, beta=0.2)

## AUXILIARY FUNCTIONS

# lambda plus
lambda_plus = function(pos, lambda, n) {
  if ((pos+lambda) <= n) return(floor(pos+lambda)) else return(floor(pos-n+lambda))
}

# lambda minus
lambda_minus = function(pos, lambda, n) {
  if ((pos-lambda) >= 1) return(floor(pos-lambda)) else return(floor(pos+n-lambda))
}

# Arc correction
arc_correction = function(a,b) {
  if (a-b>=0) return(a-b) else return(2*pi + a -b)
}

## MAIN FUNCTION

cld2 = function(x, data, beta) {
  n = length(x)
  data_length = sum(is.na(data)==FALSE)
  depth = numeric(length(x))
  t_length = data_length + 1
  lambda = floor(data_length*beta/2)
  for (i in 1:n) {
    if (is.na(x[i])) {
      depth[i] = NA
    } else {
      t = c(as.numeric(data[is.na(data)==FALSE]),x[i])
      t_ranks = floor(rank(t, ties.method="random"))
      x_position = t_ranks[t_length]
      l_cc = floor(lambda_plus(x_position, lambda, t_length))
      l_mm = floor(lambda_minus(x_position, lambda,t_length))
      arc_plus = arc_correction(t[which(t_ranks==l_cc)],t[t_length])
      arc_minus = arc_correction(t[t_length],t[which(t_ranks==l_mm)])
      #depth[i] = 1/(1+((arc_plus*arc_minus/(pi^2))))
      depth[i] = (pi^2)/((pi^2)+(arc_plus*arc_minus))
    }
  }
  return(depth)
}

