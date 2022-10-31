# Make .ts files from the loon annual output

library(data.table)
library(tidyverse)

select <- dplyr::select

all_files <- list.files('../output_loon/', pattern = '.csv', full.names = T)

solar_complete <- rbindlist(lapply(all_files, read.csv))

# add the first 5 years at the start
temp <- read.csv(all_files[2])
first_5 <- do.call("rbind", replicate(5, temp, simplify = FALSE))
first_5[,1] <- 0:(nrow(first_5)-1)

solar_complete

header_file <- paste0('../output_loon/solar_historical.ts')

cat("# Solar radiation data as average for Atlantis GOA from NEP 10km ROMS\n", file = header_file, append = T)
cat("#\n", file = header_file, append = T)
cat("# Missing values have been deleted when gaps are small. They were filled by linear interpolation.\n", file = header_file, append = T)
cat("#\n", file = header_file, append = T)
cat("# This data is 1995-2020; we replicate 1995 for 1990-1994. \n", file = header_file, append = T)
cat("#\n", file = header_file, append = T)
cat("# Alberto Rovellini\n", file = header_file, append = T)
cat("# AFSC/UW\n", file = header_file, append = T)
cat("# October 31 2022\n", file = header_file, append = T)
cat("#\n", file = header_file, append = T)
cat("# daily data in the 2nd column\n", file = header_file, append = T)
cat("#\n", file = header_file, append = T)
cat("## COLUMNS 2\n", file = header_file, append = T)
cat("##\n", file = header_file, append = T)
cat("## COLUMN1.name  Time\n", file = header_file, append = T)
cat("## COLUMN1.long_name  Time\n", file = header_file, append = T)
cat("## COLUMN1.units  days since 1990-01-01 12:00:00\n", file = header_file, append = T)
cat("## COLUMN1.missing_value  -999.000000\n", file = header_file, append = T)
cat("##\n", file = header_file, append = T)
cat("## COLUMN2.name  swr\n", file = header_file, append = T)
cat("## COLUMN2.long_name  Short wave radiation\n", file = header_file, append = T)
cat("## COLUMN2.units  W m-2\n", file = header_file, append = T)
cat("## COLUMN2.missing_value  -999.000000\n", file = header_file, append = T)
cat("##\n", file = header_file, append = T)

write.table(solar_complete, file = header_file, append = T, sep = " ", row.names = FALSE, col.names = FALSE)

# View --------------------------------------------------------------------

# place to have a look that the time series seems reasonable

plot(solar_complete$time,solar_complete$value,type='l')