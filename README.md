# Solar_from_ROMS_GOA

This repo contains code that takes ROMS NetCDF output (NEP 10 km ROMS) and transforms it into a solar forcing file for Atlantis GOA. Solar forcnings are not spatially explicit in Atlantis, so this code just pulls solar shortwave radiation (W m-2) from ROMS output, masks the rho points with the Atlantis GOA geometry, and takes an average across space for the time step.
