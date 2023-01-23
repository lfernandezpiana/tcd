#' Integrated Circular Depth (version 3)
#'
#' @param traj A list which contains the trajectories.
#' @param beta Locality level.
#' @param probs Probabilities for the grid construction.
#' @param polar If TRUE is assumed that trajectories are in polar coordinates. Contrary, is assumed long/lat coordinates.
#'
#' @return a numeric vector which contains the depth for each trajectory.

icd_v3 = function(traj, beta=0.2, probs=seq(0,1,0.1), polar=FALSE) {
  ## Calcula la profundidad local integrada para un conjunto de trayectorias
  # INPUT
  # traj: list, lista con las trayectorias.
  # beta: numeric, parametro de localidad.
  # probs: numeric, vector de probabilidades para construir los cuantiles.
  # polar: bool, si las trayectorias estan en coordenadas polares no transforma.

  # OUTPUT
  # depth: numeric, devuelve el vector con las profundidades para cada curva.

  ## Transformacion de trayectorias
  n = length(traj)
  traj_polar = vector(mode="list", length=n)
  if (polar==FALSE) {
    for (i in 1:n) traj_polar[[i]] = smooth.spline(cart2pol(as.matrix(traj[[i]])))
  } else {
    for (i in 1:n) traj_polar[[i]] = smooth.spline(as.matrix(traj[[i]]))
  }

  ## Grilla de radios basada en los percentiles
  radios = traj_polar[[1]]$x; radio_max = max(traj_polar[[1]]$x)
  for (i in 2:n) {
    radios = c(radios, traj_polar[[i]]$x)
    radio_max = c(radio_max, max(traj_polar[[i]]$x))
  }

  grilla = as.numeric(quantile(radios, probs = probs))
  grilla_length = length(probs)

  ## Pesos
  pesos_bool = matrix(0, nrow=grilla_length, ncol=n)
  for (i in 1:grilla_length) {
    for (j in 1:n) {
      if (grilla[i]<=radio_max[j]) pesos_bool[i,j] = 1
    }
  }
  pesos = matrix(0, nrow=grilla_length, ncol=n)
  for (i in 1:grilla_length) {
    for (j in 1:n) {
      pesos[i,j] = sum(pesos_bool[i,])/sum(pesos_bool)
    }
  }

  ## Angulos para cada radio percentil
  # Interpolo
  titas_matrix = matrix(NA, nrow=grilla_length, ncol=n)
  for (i in 1:n) {
    cond = which(grilla<=radio_max[i])
    titas_matrix[cond,i] = predict(traj_polar[[i]], x=grilla[cond])$y
  }

  ## Depth
  # Depth matrix
  depth_matrix = matrix(0, nrow=grilla_length, ncol=n)
  for (i in 1:nrow(titas_matrix)) {
    aux = which(is.na(titas_matrix[i,])==FALSE)
    if (floor(length(aux)*beta)<2) {
      next
    } else {
      depth_matrix[i,aux] = cld(titas_matrix[i,aux], beta)
    }
  }
  # Depth con peso
  depth = numeric(n)
  for (i in 1:n) depth[i] = sum(depth_matrix[,i]*pesos[,i])

  return(depth)
}



