#' Circular Local Depth
#'
#' Calculates the circular depth for a sample.
#'
#' @param t A numeric vector of angles to calculate the depth.
#' @param beta Locality parameter.
#'
#' @return a numeric vector with the circular depth for each value of t.
#' @examples
#'
#' library(circular)
#' tita = as.numeric(rvonmises(100, mu=pi, kappa=5))
#' cld(t=tita, beta=0.2)

cld = function(t,beta) {
  ## Esta rutina funciona bien si quiero asignar profundidades locales a un conjunto de datos
  ## respecto de si mismo pero no puedo poner un nuevo punto y calcularle la profundidad local
  # INPUT
  # t es el vector con los datos en S1 a los que se les asigna profundidad


  # OUTPUT
  # depth: numeric vector with the depth of t

  n = length(t)
  t_ord = sort(t, index.return=TRUE)
  t_ind = t_ord$ix; t_ord = t_ord$x
  quant = floor(n*beta/2)

  # Aplico la permutacion
  quant_clock = shift(t_ord,quant)
  quant_clock[1:quant] = t_ord[(n-quant+1):n]
  quant_anti = shift(t_ord,-quant)
  quant_anti[(n-quant+1):n] = t_ord[1:quant]

  # Longitudes de arco
  arc_clock = vector(length=n)
  arc_anti = vector(length=n)

  arc_clock[(quant+1):n] = t_ord[(quant+1):n] - quant_clock[(quant+1):n]
  arc_clock[1:quant] = 2*pi+t_ord[1:quant] - quant_clock[1:quant]
  arc_anti[1:(n-quant)] = quant_anti[1:(n-quant)] - t_ord[1:(n-quant)]
  arc_anti[(n-quant+1):n] = 2*pi+quant_anti[(n-quant+1):n] - t_ord[(n-quant+1):n]

  depth = 1/(1+(arc_clock*arc_anti))#/pi^2)
  depth_df = data.frame(ind=t_ind, t=t_ord, depth=depth)
  depth_df = depth_df[order(t_ind),]

  return(depth_df$depth)
}

