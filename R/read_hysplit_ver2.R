#### READ HYSPLIT


read_hysplit_file = function(file) {
  ## Usage: read a file from hysplit
  ## return: a list containing a df for each trajectory in the file 
  
  ## File header
  # number of lines to jump "GDAS"
  first_line = scan(file = file, nlines = 1, quiet = TRUE)[1] 
  # number of trayectories in the same file
  second_line = as.numeric(scan(file = file, what = "character", nlines=1, skip=first_line + 1, quiet = TRUE)[1])
  # meteo variables names
  meteo_names = scan(file = file, what = "character", nlines = 1, skip=first_line + second_line + 2, quiet = TRUE) 
  meteo_names = meteo_names[as.numeric(meteo_names[1]) - length(meteo_names)]
  meteo_names = tolower(meteo_names)
  
  ## Read trajectory hysplit file
  trajectories_df = read.table(file, skip = first_line + second_line + 3, header=FALSE)
  colnames_trajectories_df = c(c("id","unknown1","year","month","day","hr","unknown2","unknown3","unknown4","lat","lon",
                              "alt"), meteo_names)
  colnames(trajectories_df) = colnames_trajectories_df
  trajectories_df = trajectories_df[,grepl("unknown",colnames_trajectories_df)==FALSE]
  
  ## Create list of trajectories
  trajectories = vector(mode = "list", length=second_line)
  for (i in 1:second_line) {
    selector_id = which(trajectories_df$id==i)
    df = trajectories_df[selector_id, c(6,7,8)]
    selector_alt = which(df$Alt<=0)[1]
    if (length(selector_alt)==0) {
      trajectories[[i]] = df[selector_alt,1:2]
    } else {
      trajectories[[i]] = data.frame(lon = df$lon, lat=df$lat)
    }
  }
  
  return(trajectories)
}


read_hysplit_folder = function(folder) {
  files = list.files(folder)
  n = length(files)
  trajectories = vector(mode = "list", length=0)
  
  ## Progress bar
  pb = txtProgressBar(min = 0, # Valor mínimo de la barra de progreso
                       max = n, # Valor máximo de la barra de progreso
                       style = 3, # Estilo de la barra (también style = 1 y style = 2)
                       width = getOption("width"), # Ancho de la barra.
                       char = "=") # Caracter usado para crear la barra
  
  for (i in 1:n) {
    #Sys.sleep(0.0001)
    traj_aux = read_hysplit_file(paste(folder,files[i],sep="/"))
    trajectories = c(trajectories, traj_aux)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  return(trajectories)
}




