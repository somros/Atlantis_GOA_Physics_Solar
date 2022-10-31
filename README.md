# Solar forcings for Atlantis GOA from NEP 10K ROMS model

This repo contains code that takes ROMS NetCDF output (NEP 10 km ROMS) and transforms it into a solar forcing file for Atlantis GOA. Solar forcnings are not spatially explicit in Atlantis, so this code just pulls solar shortwave radiation (W m<sup>-2</sup>) from ROMS output, masks the rho points with the Atlantis GOA geometry, and takes an average across space for the time step.

See code for details.
