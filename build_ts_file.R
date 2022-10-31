# Make .ts files from the loon annual output

library(data.table)
library(tidyverse)

select <- dplyr::select

all_files <- list.files('../output_loon/', pattern = '.csv', full.names = T)

solar_list <- lapply(all_files, read.csv)

# check that length of the files checks out - 365 or 366 (for leap years)
check <- unlist(lapply(solar_list, nrow))

#TODO: decide what to do with leap years

# tie them all together
solar_complete <- rbindlist(solar_list)

# add the first 5 years at the start - replicate the first file
temp <- read.csv(all_files[1])
first_5 <- do.call("rbind", replicate(5, temp, simplify = FALSE))
first_5[,1] <- 1:nrow(first_5)

solar_complete <- rbind(first_5, solar_complete)

# start from 0
solar_complete[,1] <- solar_complete[,1]-1

# write ts file
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


# Check 2017 --------------------------------------------------------------
# DO NOT RUN

# # this section reads the loon output for 2017 and compares it to the original ts file we have been using
# this_file <- all_files[grep('2017', all_files)]
# 
# solar_2017 <- read.csv(this_file)
# 
# solar_2017[,1] <- 0:(nrow(solar_2017)-1)
# 
# header_file <- paste0('../output_loon/solar_2017_check.ts')
# 
# cat("# Solar radiation data as average for Atlantis GOA from NEP 10km ROMS\n", file = header_file, append = T)
# cat("#\n", file = header_file, append = T)
# cat("# Missing values have been deleted when gaps are small. They were filled by linear interpolation.\n", file = header_file, append = T)
# cat("#\n", file = header_file, append = T)
# cat("# This data is 1995-2020; we replicate 1995 for 1990-1994. \n", file = header_file, append = T)
# cat("#\n", file = header_file, append = T)
# cat("# Alberto Rovellini\n", file = header_file, append = T)
# cat("# AFSC/UW\n", file = header_file, append = T)
# cat("# October 31 2022\n", file = header_file, append = T)
# cat("#\n", file = header_file, append = T)
# cat("# daily data in the 2nd column\n", file = header_file, append = T)
# cat("#\n", file = header_file, append = T)
# cat("## COLUMNS 2\n", file = header_file, append = T)
# cat("##\n", file = header_file, append = T)
# cat("## COLUMN1.name  Time\n", file = header_file, append = T)
# cat("## COLUMN1.long_name  Time\n", file = header_file, append = T)
# cat("## COLUMN1.units  days since 1990-01-01 12:00:00\n", file = header_file, append = T)
# cat("## COLUMN1.missing_value  -999.000000\n", file = header_file, append = T)
# cat("##\n", file = header_file, append = T)
# cat("## COLUMN2.name  swr\n", file = header_file, append = T)
# cat("## COLUMN2.long_name  Short wave radiation\n", file = header_file, append = T)
# cat("## COLUMN2.units  W m-2\n", file = header_file, append = T)
# cat("## COLUMN2.missing_value  -999.000000\n", file = header_file, append = T)
# cat("##\n", file = header_file, append = T)
# 
# write.table(solar_2017, file = header_file, append = T, sep = " ", row.names = FALSE, col.names = FALSE)
