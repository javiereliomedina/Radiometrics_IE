
  library(tidyverse)
  library(sf)
  library(rmapshaper)

######################################################################
#### GWSP (Soil permeability from Groundwater recharge map - GSI) ####
######################################################################

# Download from "https://secure.dccae.gov.ie/GSI_DOWNLOAD/Groundwater/Data/GSI_Groundwater_Subsoil_Permeability.zip"
# Fix geometries and dissolve permeability (PERM) in QGIS

# Load data
  GWSP <- read_sf("Rdata/GSI_Groundwater_Subsoil_Permeability/GWSP_FG_Dissolve.shp") %>%
    select(PERM) %>%
    filter(PERM != "Water") %>%
    st_set_crs("+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=0.99982 +x_0=600000 +y_0=750000 +ellps=GRS80 +units=m +no_defs")
  
# Simplify polygons
  GWSP <- GWSP %>% 
    ms_simplify(keep = 0.01, keep_shapes = TRUE, sys = TRUE) %>%
    st_make_valid() %>%
    st_buffer(dist = 0)
  
# Export map 
  st_write(GWSP, "Rresults/Soil_Permeability/GWSP.shp")

######################################################################  
####        QGSP (Permeability map from Quaternary Geology)       ####
####        All-Ireland Quaternary map (scale 1:500.000)          ####
######################################################################

# Load QG map
  QG <- read_sf("Rdata/GSI_Quaternary_500k/QG_500k.shp") %>% 
    mutate(Sediment_5 = as_factor(Sediment_5)) %>%
    group_by(Sediment_5) %>%
    summarise(N = n(),
              area_sq_km = sum(area_sq_km),
              label = first(label)) %>%
    ungroup()

# Subsoil permeability QG
# Classification (based on the all-Ireland Quaternary map scale 1:500.000):
#  + Alluvium: Low
#  + Glaciofluvial/glaciolacustrine sand and gravel: High
#  + Glaciomarine sediments: Low
#  + Lacustrine sediments: Low
#  + Marine and estuarine deposits: Low
#  + Peat: Low
#  + Slope deposits: Moderate
#  + Till derived from acidic volcanic rocks: Moderate
#  + Till derived from basic igneous rocks: Moderate
#  + Till derived from Devonian sandstones: Moderate
#  + Till derived from granites: Moderate
#  + Till derived from limestones: Moderate
#  + Till derived from metamorphic rocks: Moderate
#  + Till derived from sandstones: Moderate
#  + Till derived from sandstones and shales: Moderate
#  + Wind blown sand: High (after GSI - R1)   
  QGSP <- QG %>%
    mutate(Perm = case_when(
      Sediment_5 == "Alluvium"                                ~ "Low",
      Sediment_5 == 
        "Glaciofluvial/glaciolacustrine sand and gravel"      ~ "High",
      Sediment_5 == "Glaciomarine sediments"                  ~ "Low",
      Sediment_5 == "Lacustrine sediments"                    ~ "Low",                 
      Sediment_5 == "Marine and estuarine deposits"           ~ "Low",                
      Sediment_5 == "Peat"                                    ~ "Low",              
      Sediment_5 == "Slope deposits"                          ~ "Moderate",
      Sediment_5 == "Till derived from acidic volcanic rocks" ~ "Moderate",       
      Sediment_5 == "Till derived from basic igneous rocks"   ~ "Moderate",      
      Sediment_5 == "Till derived from Devonian sandstones"   ~ "Moderate",     
      Sediment_5 == "Till derived from granites"              ~ "Moderate",    
      Sediment_5 == "Till derived from limestones"            ~ "Moderate",   
      Sediment_5 == "Till derived from metamorphic rocks"     ~ "Moderate",  
      Sediment_5 == "Till derived from sandstones"            ~ "Moderate", 
      Sediment_5 == "Till derived from sandstones and shales" ~ "Moderate",
      Sediment_5 == "Wind blown sand"                         ~ "High",
      TRUE ~ as.character(Sediment_5))
    ) %>%
    rmapshaper::ms_simplify(keep = 0.01, keep_shapes = TRUE) %>%
    lwgeom::st_make_valid() %>%
    group_by(Perm) %>%
    summarise(N = n(),
              Area = sum(area_sq_km)) %>%
    ungroup() %>%
    mutate(Perm = factor(Perm, 
                         levels = c("High", "Moderate", "Low"),
                         ordered=TRUE))

# In the areas of the Quaternary map where there is no datum,
# "Bedrock" was assigned as soil type, and then two types of permeability
# depending if the Aquifer bedrock is karstified or not
# based on the "Groundwater Resources (Aquifers) - Aquifer Bedrock" map of the GSI):
#  + Bedrock (No Karstified): Moderate
#  + Bedrock (Karstified): High

# AQ_URL <- "https://secure.dccae.gov.ie/GSI_DOWNLOAD/Groundwater/Data/GSI_Groundwater_Bedrock_Aquifers.zip"
# download.file(AQ_URL, destfile = "Rdata/Groundwater.zip")
# unzip(zipfile = "Rdata/Groundwater.zip", exdir   = "Rdata/Groundwater")
  AQ <- read_sf("Rdata/Groundwater/IRL_AQUIFER_BEDROCK_ITM.shp") %>%
    rmapshaper::ms_simplify(keep = 0.01, keep_shapes = TRUE) %>%
    st_make_valid()
  
  Karst <- AQ %>% 
    group_by(AQUIFERCAT) %>%
    summarize(AQUIFER_DE = first(AQUIFER_DE)) %>%
    ungroup() %>%
    filter(AQUIFERCAT != "Lake" & AQUIFERCAT != "Unclas") %>%
    mutate(Karst = case_when(
      AQUIFERCAT == "Lk"     ~ "TRUE",
      AQUIFERCAT == "LkNI"   ~ "TRUE",
      AQUIFERCAT == "Ll"     ~ "FALSE",                         
      AQUIFERCAT == "LlNI"   ~ "FALSE",                 
      AQUIFERCAT == "Lm"     ~ "FALSE",                
      AQUIFERCAT == "LmNI"   ~ "FALSE",              
      AQUIFERCAT == "NI_Un*" ~ "FALSE",                  
      AQUIFERCAT == "PINIl"  ~ "FALSE",       
      AQUIFERCAT == "Pl"     ~ "FALSE",      
      AQUIFERCAT == "PlNI"   ~ "FALSE",     
      AQUIFERCAT == "Pu"     ~ "FALSE",    
      AQUIFERCAT == "PuNI"   ~ "FALSE",   
      AQUIFERCAT == "Rf"     ~ "FALSE",  
      AQUIFERCAT == "Rf/Rk"  ~ "TRUE" , 
      AQUIFERCAT == "RfNI"   ~ "FALSE",
      AQUIFERCAT == "Rk"     ~ "TRUE",
      AQUIFERCAT == "Rkc"    ~ "TRUE",
      AQUIFERCAT == "RkcNI"  ~ "TRUE",
      AQUIFERCAT == "Rkd"    ~ "TRUE",
      AQUIFERCAT == "RkNI"   ~ "TRUE",
      TRUE ~ as.character(AQUIFERCAT))
    ) %>%
    group_by(Karst) %>%
    summarise(N = n()) %>%
    ungroup()

# Difference Karst y QGSP:
#  + Difference
#  + Remove Lough Neagh
#    - plot(st_geometry(Karst_QGSP))
#    - identify(st_geometry(Karst_QGSP))
  diff <- st_difference(Karst, st_union(QGSP)) %>%
    st_cast("POLYGON") %>%
    slice(-2919) %>% # Remove "Lough Neagh"
    group_by(Karst) %>%
    summarise(N = n()) %>%
    ungroup() %>%
    lwgeom::st_make_valid()

# Add the polygons to Soil Permeability (and save)
  A <- select(QGSP, Perm) 
  
  B <- diff %>%
    mutate(Perm = case_when(
      Karst == TRUE  ~ "High",
      Karst == FALSE ~ "Moderate")
    ) %>%
    select(Perm) 
  
  QGSP <- rbind(A, B) %>%
    st_collection_extract("POLYGON") %>%
    st_buffer(0.001) %>%
    group_by(Perm) %>%
    summarise(N = n())

# Export map
  st_write(QGSP, "Rresults/Soil_Permeability/QGSP.shp")

######################################################################
####              All-Ireland Soil permeability map               ####
######################################################################

# Map taking into account both 
# Groundwater Recharge Map and the Quaternary Map of Ireland

# Where SP = N/A in GSI - Soil Permeability (Grownwater recharge - GWSP)
# estimate it based on the QG map (QGSP)

# load data 
  GWSP <- read_sf("Rresults/Soil_Permeability/GWSP.shp")
  QGSP <- read_sf("Rresults/Soil_Permeability/QGSP.shp") 
  
# Plot maps
  plot(QGSP["Perm"], border = NA)
  plot(GWSP["PERM"], border = NA)

# Intersect GWSP and QGSP
  SP_Int <- st_intersection(GWSP, QGSP)

# Add permeability derived from QG (Perm) where there is N/A in GW permeability (PERM)
  SP_Int <- SP_Int %>%
    mutate(PERM = ifelse(PERM == "N/A", Perm, PERM),
           PERM = case_when(PERM == "High"     ~ "H",
                            PERM == "Moderate" ~ "M",
                            PERM == "Low"      ~ "L",
                            TRUE ~ as.character(PERM)))

# This results is the SP of RoI, we'd need to add NI 
# Because we do not have GWSP in NI, the SP is based on the QG
# Intersect NI with QGSP, and add the resulting map to SP_Int

# Load World map, and extract NI (scale 1:1Million)
  # World_URL <- "https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/countries/download/ref-countries-2016-01m.shp.zip"
  # download.file(World_URL, destfile = "Rdata/World1M.zip")
  # unzip(zipfile = "Rdata/World1M.zip", exdir   = "Rdata/World1M")
  # unzip(zipfile = "Rdata/World/CNTR_RG_01M_2016_4326.shp.zip",
  #       exdir   = "Rdata/World1M_SHP")
  World <- read_sf("Rdata/World1M_SHP/CNTR_RG_01M_2016_4326.shp") %>%
    st_transform(crs = st_crs(QGSP))

# Ireland (all Iseland)
  Grids1km <- read_sf("Rdata/Grids1km/IR_NI_Grids_1km.shp")
  
  Ireland <- Grids1km %>%
    group_by(Id) %>%
    summarise(N = n()) %>%
    ungroup() %>% 
    st_transform(crs = st_crs(QGSP))
  
# Republic of Ireland
  IE <- filter(World, CNTR_ID == "IE")
  
# Northerm Ireland
  NI <- st_difference(Ireland, IE) %>%
    st_cast("POLYGON") 
  
  NI <- NI %>%
    mutate(Area = units::set_units(st_area(NI), km^2)) %>%
    arrange(desc(Area)) %>%
    filter(Area > units::set_units(13000, km^2))

# Intersect QGSP and NI 
  QGSP_NI <- st_intersection(QGSP, NI) 

# Soil permeabilities (IE and NI)
  SP_IE <- select(SP_Int, "PERM") 
  
  SP_NI <- select(QGSP_NI, "Perm") %>%
    rename(PERM = Perm) %>%
    mutate(PERM = case_when(PERM == "High"     ~ "H",
                            PERM == "Moderate" ~ "M",
                            PERM == "Low"      ~ "L",
                            TRUE ~ as.character(PERM))) 
  
# Merge shapefiles
  SP <- rbind(SP_IE, SP_NI) %>%
    mutate(PERM = factor(PERM, 
                         levels = c("H", "M", "L"),
                         ordered = TRUE)) %>%
    arrange(PERM)

# Save final permeability map  
  st_write(SP, "Rresults/Soil_Permeability/SP_GW_naQG.shp", append=FALSE)
  