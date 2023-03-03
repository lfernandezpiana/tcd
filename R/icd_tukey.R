#' Integrated Circular Depth (version Tukey)
#'
#' @param traj A list which contains the trajectories.
#' @param probs Sequense of probabilities for grid construction.
#' @param type The trajectory type of coordinates "geographical" or "cartesian". If "geographical" made correction for lat and long coordinates.
#' @param weight If TRUE a weight in each radius is applied.
#'
#' @return a numeric vector which contains the depth for each trajectory.



icd_tukey = function(traj, beta="automatic", probs=seq(0,1,0.1), type="geographical", weight=TRUE) {
  ## Calcula la profundidad local integrada para un conjunto de trayectorias utiliza la prondidad de Tukey en el circulo
  ## la funcion sale de la libreria depth y se llama sdepth
  # INPUT
  # traj: list, lista con las trayectorias.
  # probs: numeric, vector de probabilidades para construir los cuantiles.
  # type: character, the trajectory type of coordinates: cartesian or geographical.
  # weights: bool, if TRUE a weigth depth is applied.
  
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
      pesos[i,j] = pesos_bool[i,j]/(sum(pesos_bool[i,], na.rm = TRUE)*grilla_length)
    }
  }
  
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
    if (floor(length(aux)*0.2)<2) {
      next
    } else {
      for (a in 1:length(aux)) {
        # Esto hubo que hacerlo asi porque no entra de otra forma en sdepth  
        b=circular(titas_matrix[aux,j], type="angles", units="radians", zero=0, modulo="2pi")
        b=as.numeric(b)
        depth_matrix[aux[a],j] = sdepth(theta=b[a], P=b)
      }
    } 
  }
  # Depth calculus
  depth = numeric(n)
  if (weight==TRUE) {
    for (i in 1:n) depth[i] = sum(pesos[,i]*depth_matrix[i,])
  } else {
    for (i in 1:n) depth[i] = mean(depth_matrix[i,])
  }
  
  # Return
  salida = list(depth, grilla, traj_polar)
  names(salida) = c("depth", "grid", "polar")
  return(salida)
}
