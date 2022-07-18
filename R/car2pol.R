#### CONVERT CARTESIAN OR GEOGRAPHICAL COORDINATES TO POLAR COORDINATES


# Calculates harvesing distance between points in km
harvesing_dist = function(lon1, lat1, lon2, lat2) {
  # distance between latitudes and longitudes
  dlat = (lat2 - lat1)*pi/180
  dlon = (lon2 - lon1)*pi/180

  # convert to radians
  lat1 = (lat1)*pi/180
  lat2 = (lat2)*pi/180

  # apply formulae
  a = sin(dlat/2)^2 + (sin(dlon/2)^2)*cos(lat1)*cos(lat2)
  rad = 6371
  c = 2*sin(sqrt(a))

  return(rad*c)
}


# Convert to polar coordinates
polar_coordinates = function(trajectory, type="cartesian") {
  # Trajectory: n x 2 matrix. Each column has a coordinate.
  # Type: character. If "cartesian" trajectories are in cartesian coordinates;
  #       if "geografical" coordiantes are in long/lat format.
  # Returns: n x 2 dataframe. The first column is radius and the second is angle.

  if (type=="cartesian") {
    x = as.numeric(trajectory[,1]) - trajectory[1,1] ; y = as.numeric(trajectory[,2]) - trajectory[1,2] # move to (0,0)
    radius = sqrt(x^2 + y^2)
    gamma = atan2(y,x)
    return(data.frame(radius=radius,angle=gamma))
  }

  else if (type=="geographical") {
    n = nrow(trajectory)
    x = as.numeric(trajectory[,1]) - trajectory[1,1] ; y = as.numeric(trajectory[,2]) - trajectory[1,2] # move to (0,0)
    gamma = atan2(y,x)
    radius = numeric(n)
    for (i in 1:n) radius[i] = harvesing_dist(0,0,x[i],y[i])
    return(data.frame(radius=radius,angle=gamma))
  }

  else {
    print("Choose type as geographical or cartesian")
  }

}



