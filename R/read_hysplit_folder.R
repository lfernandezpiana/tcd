#### READ HYSPLIT FOLDER


read_hysplit_folder = function(folder) {
  files = list.files(folder)
  n = length(files)
  trajectories = vector(mode = "list", length=0)
  registry = data.frame(id=0, year=0, month=0, day=0, hour=0)
  ## Progress bar
  pb = txtProgressBar(min = 0, # Valor mínimo de la barra de progreso
                      max = n, # Valor máximo de la barra de progreso
                      style = 3, # Estilo de la barra (también style = 1 y style = 2)
                      width = getOption("width"), # Ancho de la barra.
                      char = "=") # Caracter usado para crear la barra
  
  for (i in 1:n) {
    # Trajectory extraction
    file_path = paste(folder,files[i],sep="/")
    traj_aux = read_hysplit_file(file_path)
    trajectories = c(trajectories, traj_aux)
    setTxtProgressBar(pb, i)
    
    # Registry extraction
    reg = sub(pattern="EXP101_", replacement="", x=files[i], fixed=TRUE)
    reg = unlist(strsplit(reg, ""))
    registry = rbind(registry,
                     data.frame(id = i,
                                year = as.numeric(paste(reg[1:4], collapse = '')),
                                month = as.numeric(paste(reg[5:6], collapse = '')),
                                day = as.numeric(paste(reg[7:8], collapse = '')),
                                hour = as.numeric(paste(reg[9:10], collapse = ''))
                     )
    )
    
  }
  registry = registry[-1,]
  ret = list(registry, trajectories)
  names(ret) = c("registry", "trajectories")
  
  close(pb)
  return(ret)
}