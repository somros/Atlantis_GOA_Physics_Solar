# This code creates the solar.ts file from ROMS output.
# NEP ROMS is forced by solar shortwave radiation (swrad) from the Climate Forecast System Reanalysis (and Operational Analysis, Al Hermann pers. comm.).
# This is also returned by ROMS as an output - swrad in the nc files. 
# We map swrad from ROMS to the Atlantis geometry and take an average at each time step. This is conceptually different from taking data from a
# single station and may result in different primary productivity dynamics. Note that the solar forcing in Atlantis is not spatially explicit.

library(tidyverse)
library(tidync)
library(sf)
library(raster)
library(data.table)
library(rbgm)

select <- dplyr::select

# Data --------------------------------------------------------------------

bgm_file <- 'data/GOA_WGS84_V4_final.bgm' # Atlantis model geometry
roms_grid_file <- 'data/NEP_grid_5a.nc' # ROMS grid

# Prepare the spatial domain ----------------------------------------------

#read in Atlantis BGM
atlantis_bgm <- read_bgm(bgm_file)
atlantis_box <- atlantis_bgm %>% box_sf()
atlantis_box <- atlantis_box %>% filter(boundary!=TRUE)

# added 4/12/2022
atlantis_crs <- atlantis_bgm$extra$projection

mask <- atlantis_box %>% st_union() %>% st_as_sf(crs = atlantis_crs)

# read in ROMS grid 
roms_grid <- tidync(roms_grid_file)

#grid info
grid_variables <- hyper_grids(roms_grid) %>% # all available grids in the ROMS ncdf
  pluck("grid") %>% # for each grid, pull out all the variables associated with that grid and make a reference table
  purrr::map_df(function(x){
    roms_grid %>% activate(x) %>% hyper_vars() %>% 
      mutate(grd=x)
  })

# find appropriate ROMS ncdf grid for the rho points
latlon_rhogrd <- grid_variables %>% filter(name=="lat_rho") %>% pluck('grd')
# pull the lon/lats
roms_rho <- roms_grid %>% activate(latlon_rhogrd) %>% hyper_tibble() %>% select(lon_rho,lat_rho,xi_rho,eta_rho) %>% 
  mutate(rhoidx=row_number()) # add index

# Add coordinates in the CRS used by the Atlantis mask.
append_xy_coords <- function(lonlatdat, xyproj=atlantis_crs, lon_col="lon_rho", lat_col="lat_rho"){
  lonlatdat %>% 
    st_as_sf(coords=c(lon_col, lat_col), crs=4326, remove=F) %>%  # convert to spatial object
    st_transform(xyproj) %>%  # convert to Atlantis coords
    mutate(x = st_coordinates(.)[,1],
           y = st_coordinates(.)[,2]) # grab x and y coordinates and add them as attributes
}

rhoxy<- append_xy_coords(roms_rho,lon_col="lon_rho",lat_col="lat_rho") %>% mutate(rhoidx=row_number())

#match rho points to the Atlantis geometry
rho_join <- mask %>% st_join(rhoxy) %>% na.omit()

# Get indeces of rho, u, and v points that overlap with Atlantis geometry, to subset large ROMS files and reduce memory chokes
min_xi_rho <- min(rho_join$xi_rho, na.rm = TRUE)
max_xi_rho <- max(rho_join$xi_rho, na.rm = TRUE)
min_eta_rho <- min(rho_join$eta_rho, na.rm = TRUE)
max_eta_rho <- max(rho_join$eta_rho, na.rm = TRUE)

# Extract solar -----------------------------------------------------------

# Write a function that pulls swrad at the time step of interest.
get_swrad <- function(variable, time_step){
  
  grd <- roms_variables %>% filter(name==variable) %>% pluck('grd')
  # pull the env data
  # interpolate the env data
  # do this step conditional to join with the appropriate depth data frame depending on the variable
  dat <- roms_vars %>% activate(grd) %>%
    hyper_tibble(select_var=variable, 
                 xi_rho = between(xi_rho, min_xi_rho, max_xi_rho), 
                 eta_rho = between(eta_rho, min_eta_rho, max_eta_rho),
                 ocean_time = ocean_time == time_step)
  
  # join to rho_join set to subset to mask only
  dat <- rho_join %>%
    st_set_geometry(NULL) %>%
    left_join(dat,by=c('xi_rho','eta_rho'))
  
  # return(interp_dat) # if you want to see what things look like spatially, this is a good place to return an output for visualisation
  # dat %>% ggplot()+geom_point(aes(x=x.y,y=y,color=swrad))+scale_color_viridis()+theme_bw()
  
  # take averages over the entire model domain
  # this step is problematic for solar, but this is how Atlantis works for now
  swrad_value <- mean(dat$swrad, na.rm=T)
  return(swrad_value)
}

all_years <- list.dirs("/home/arovel/../../ahr0/hermann/goa-output/NEP_Data/", recursive = F)

for(y in 1:length(all_years)){
  
  roms_files <- list.files(all_years[y], pattern='nep5', full.names = T) # ROMS NetCDF output for one year
  
  # prepare a list with as many entries as we have input files
  solar_frames_list <- vector(mode = 'list', length = length(roms_files))
  
  for(i in 1:length(solar_frames_list)){
    # repeat for each file
    
    romsfile <- roms_files[i]
    roms_vars <- tidync(romsfile)
    
    #variables
    roms_variables <- hyper_grids(roms_vars) %>% # all available grids in the ROMS ncdf
      pluck("grid") %>% # for each grid, pull out all the variables asssociated with that grid and make a reference table
      purrr::map_df(function(x){
        roms_vars %>% activate(x) %>% hyper_vars() %>% 
          mutate(grd=x)
      })
    
    # get time steps from the output file
    time_grd <- roms_variables %>% filter(name=='ocean_time') %>% pluck('grd')
    epoch <- "1900-01-01 00:00:00" #important, check that this is your correct start - keeping this generic as it changes a lot ROMS by ROMS
    
    # pull time steps from the output file
    roms_time <- roms_vars %>% activate(time_grd) %>% hyper_tibble() %>% pull()
    
    solar_frame <- data.frame(variable = 'swrad', time_step=roms_time)
    
    solar_frame <- solar_frame %>% 
      mutate(swrad_value = purrr::pmap_dbl(list(as.character(variable),time_step),possibly(get_swrad,NA)))
    
    solar_frames_list[[i]] <- solar_frame %>% mutate(date=as.POSIXct(time_step, origin = epoch, tz='UTC')) %>%
      arrange(time_step) %>%
      select(time_step,date,swrad_value) %>%
      drop_na()
  }
  
  solar_frame <- rbindlist(solar_frames_list)
  
  # Fill missing values -----------------------------------------------------
  
  ts <- unique(solar_frame$time_step)
  
  # get year for this directory
  this_year <- solar_frame %>% mutate( year = year(date)) %>% group_by(year) %>% tally() %>% filter(n == max(n)) %>% pull(year)
  
  epoch <- "1900-01-01 00:00:00" #important, check that this is your correct start - keeping this generic as it changes a lot ROMS by ROMS
  
  # drop time steps that belong to other years
  solar_frame <- solar_frame %>%
    filter(date >= as.POSIXct(paste0(this_year, '-01-01 00:00:00'),tz='UTC'), 
           date <= as.POSIXct(paste0(this_year, '-12-31 12:00:00'),tz='UTC'))
  
  # and use as t_0 and t_end Jan 1 and Dec 31, but they need to be in seconds
  t_0 <- as.POSIXct(paste0(this_year, '-01-01 12:00:00'),origin=epoch,tz='UTC') # this has to be specific to your ROMS, so check your origin and tz
  t_end <- as.POSIXct(paste0(this_year, '-12-31 12:00:00'),origin=epoch,tz='UTC')
  complete <- seq(from=t_0,to=t_end,by=60*60*24) # 
  
  solar_complete <- data.frame(time=complete,
                               value=approx(solar_frame$date,solar_frame$swrad_value,xout = complete, rule = 2)$y)
  
  # change dates to days from 1990-01-01 (so in the end your first sime step will be 0)
  
  model_origin <- as.POSIXct(0,origin='1990-01-01 12:00:00',tz='UTC') # may have to revisit this based on ROMS start
  
  # change time column to days since origin, and format values correctly
  solar_complete <- solar_complete %>%
    mutate(time=as.numeric(difftime(solar_complete[,1],model_origin)),
           value=as.numeric(formatC(value, format = 'f', digits = 5)))
  
  # added 4/12/2022
  # time column needs to be set to 0,1,2,...,364
  # the start time of solar is really important - will need to rework this when we have the final files
  # solar_complete <- solar_complete %>% mutate(time = 0:364)
  
  # write out
  write.csv(solar_complete, paste0('solar_goa_test', this_year , '.csv'), row.names = F)
}
