#' Integrated Circular Depth (version 4)
#'
#' @param traj A list which contains the trajectories.
#' @param beta Locality level.
#' @param probs Probabilities for the grid construction.
#' @param polar If TRUE is assumed that trajectories are in polar coordinates. Contrary, is assumed long/lat coordinates.
#'
#' @return a numeric vector which contains the depth for each trajectory.


icd_v4 = function(traj, beta=0.2, probs=seq(0,1,0.1), type="geographical") {
  ## Calcula la profundidad local integrada para un conjunto de trayectorias
  # INPUT
  # traj: list, lista con las trayectorias.
  # beta: numeric, parametro de localidad.
  # probs: numeric, vector de probabilidades para construir los cuantiles.
  # type: character, the trajectory type of coordinates: cartesian or geographical

  # OUTPUT
  # depth: numeric, devuelve el vector con las profundidades para cada curva.

  ## Transformacion de trayectorias
  n = length(traj)
  traj_polar = vector(mode="list", length=n)
    for (i in 1:n) traj_polar[[i]] = polar_coordinates(as.matrix(traj[[i]]), type=type)

  ## Grilla de radios basada en los percentiles
  radios = traj_polar[[1]]$radius; radio_max = max(traj_polar[[1]]$radius)
  for (i in 2:n) {
    radios = c(radios, traj_polar[[i]]$radius)
    radio_max = c(radio_max, max(traj_polar[[i]]$radius))
  }
  grilla = as.numeric(quantile(radios, probs = probs))
  grilla_length = length(probs)

  ## Angulos para cada radio percentil
  titas_matrix = matrix(NA, nrow=n, ncol=grilla_length)
  for (i in 1:n) {
    t = traj_polar[[i]]
    for (j in 1:grilla_length) {
      selector = which(t$radius>grilla[j])[1]
      if (length(selector)==0) {
        next
      } else {
        d12 = abs(t$radius[selector] - t$radius[selector-1])
        d10 = abs(grilla[j] - t$radius[selector-1])
        d02 = abs(t$radius[selector] - grilla[j])
        titas_matrix[i,j] = (d10/d12)*t$angle[selector] + (d02/d12)*t$angle[selector-1]
      }
    }
  }

  ## Depth
  # Depth matrix
  depth_matrix = matrix(0, nrow=n, ncol=grilla_length)
  for (j in 1:grilla_length) {
    aux = which(is.na(titas_matrix[,j])==FALSE)
    if (floor(length(aux)*beta)<2) {
      next
    } else {
      depth_matrix[aux,j] = cld(titas_matrix[aux,j], beta)
    }
  }
  # Depth calculus
  depth = numeric(n)
  for (i in 1:n) depth[i] = mean(depth_matrix[i,])

  # Return
  salida = list(depth, grilla, traj_polar)
  names(salida) = c("depth", "grid", "polar")
  return(salida)
}


