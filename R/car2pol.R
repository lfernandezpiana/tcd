#### CONVERT CARTESIAN COORDINATES TO POLAR COORDINATES


cart2pol = function(trayectory)
  # Trayectory: n x 2 matrix. Each column has a coordinate.
  # Returns: n x 2 matrix. The first column is radius and the second is angle.
{
  x = as.numeric(trayectory[,1]) - trayectory[1,1] ; y = as.numeric(trayectory[,2]) - trayectory[1,2]
  radius = sqrt(x^2 + y^2)
  gamma = atan2(y,x)

  return(cbind(radius,gamma))
}
