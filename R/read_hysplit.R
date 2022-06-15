read_hysplit = function(file) {
  
  ## Esta funcion toma una archivo de hysplit y devuelve una lista de trajectorias.
  ## Parametros:
  # file: archivo de salida de hysplit
  
  header1 = read.table(file, header=FALSE, nrows=1)
  n_skip = 1 + header1$V1
  header2 = read.table(file, header=FALSE, skip=n_skip, nrows=1)
  n_traj = header2$V1
  print(paste("Trajectory class:", as.character(header2$V2), sep=" "))
  n_skip = 1 + n_skip + n_traj
  var_file = as.character(read.table(file, header=FALSE, skip=n_skip,nrows = 1))[-1]
  var_names = c("Id","Year","Month","Day","Hr","Lat","Long","Alt")
  vars = c(var_names, var_file)
  n_skip = n_skip + 1
  traj_data = read.table(file, header=FALSE, skip=n_skip)
  traj_data = traj_data[,-c(2,7:9)]
  colnames(traj_data) = vars
  
  # Create trajectory list
  trajectories = vector(mode="list", length=n_traj)
  for (i in 1:n_traj)
    trajectories[[i]] = traj_data[traj_data$Id==i,-1]
  
  return(trajectories)
}


