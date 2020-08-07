# Radiometrics (eU - resolution 50m)
## Download Radiometric data [*Tellus radiometric merged grd, tiff and gxf (981.9 MB)*] 
## from [Tellus](https://www.gsi.ie/en-ie/data-and-maps/Pages/Geophysics.aspx)
## and save it in *Rdata*.
  
  library(tidyverse)
  library(sf)
  library(stars)
  
  Grids1km <- read_sf("Rdata/Grids1km/IR_NI_Grids_1km.shp")
  
  Ireland <- Grids1km %>%
    group_by(Id) %>%
    summarise(N = n()) 
  
  eU <- "Rdata/Radiometrics/RAD_MERGED_GRIDS/GXF/Tellus_RAD_eU_MERGE_2019.gxf"
  eU  <- read_stars(eU) %>% st_set_crs(st_crs(Grids1km)) 
  
# Divide Grids1km in cells of 40x40 km
  Parts <- st_make_grid(Grids1km,
                        c(40000, 40000),
                        offset = st_bbox(Grids1km)[1:2],
                        n = c(9, 11),
                        what = "polygons"
  ) 
  
# Select Parts (of 40km x 40km) with data (Grids 1km x 1 km)
  Parts_1km <- st_intersection(Grids1km, Parts)
  Parts_Grids <- list()
  for (i in 1:length(Parts)) { 
    if (length(Parts_1km[Parts[i], ]$geometry) > 0) {Parts_Grids[[i]] <- Parts[i]}
  }
  Parts_Grids <- Parts_Grids[!sapply(Parts_Grids, is.null)] %>%
    map(st_sf)
  PA <- mapedit:::combine_list_of_sf(Parts_Grids) 
  plot(PA) 
  
# Function for averaging (AM and SD) the eU values by grid cells of 1km x 1km
  average_grids <- function(df) {  
    crop <- st_bbox(df, crs = st_crs(Grids1km))
    eU_crop <- st_crop(eU, crop)
    IE_crop <- st_crop(Grids1km, crop) %>% st_buffer(0)
    eU_AM <- eU_crop %>%
      aggregate(by = IE_crop, FUN = mean) %>%
      st_as_sf(as_points = TRUE, merge = FALSE) %>%
      rename(eU_AM = Tellus_RAD_eU_MERGE_2019.gxf)
    eU_SD <- eU_crop %>%
      aggregate(by = IE_crop, FUN = sd) %>%
      st_as_sf(as_points = TRUE, merge = FALSE) %>%
      rename(eU_SD = Tellus_RAD_eU_MERGE_2019.gxf) 
    eU_Grids1k <- eU_AM %>%
      mutate(eU_SD = eU_SD$eU_SD,
             SP_ID = IE_crop$SP_ID) %>%
      drop_na()
    return(eU_Grids1k)
  }
# Function for working in parallel (1 core for each Part of 40km x 40km)
  library(parallel)
  Parallel_fun <- function(df_parts, num_cores){
    cl <- makeCluster(num_cores)
    clusterExport(cl = cl, varlist = c("eU", "Grids1km", "df_parts"), envir = .GlobalEnv)
    clusterEvalQ(cl = cl, expr = c(library(tidyverse), library(sf), library(stars)))
    parallelX <- parLapply(cl = cl, df_parts, fun = average_grids)
    stopCluster(cl)
    df_Grids1k <- mapedit:::combine_list_of_sf(parallelX)
  }
# Average values (eU) by grid cells of 1km x 1km in each Part
  start_time <- Sys.time()
  
    PA <- PA %>% mutate(cl = c(rep(1:19, each = 4), rep(20, 2)))
    eU_Grids1km_list <- list()
    for (i in 4:20) {
      df <- PA[PA$cl == i, ]
      num_cores <- dim(df)[1]
      df_parts <- split(x = df, f = 1:num_cores)
      eU_Grids1km_list[[i]] <- Parallel_fun(df_parts, num_cores)
    }
    
  end_time <- Sys.time()
  end_time - start_time
 
# Merge values in ons data frame
  eU_Grids1km <- eU_Grids1km_list[!sapply(eU_Grids1km_list, is.null)] 
  eU_Grids1km <- mapedit:::combine_list_of_sf(eU_Grids1km) 
  
# Save results
  dir.create("Rresults")
  write_sf(eU_Grids1km, "Rresults/eU_Grids1km.shp")
