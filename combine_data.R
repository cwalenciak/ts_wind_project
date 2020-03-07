library(tswge)
library(ncdf4)


setwd("C:/Users/cwale/OneDrive/Desktop/TS Project")

# Call support function file
source("C:/Users/cwale/OneDrive/Desktop/TS Project/ts_wind_project/Support/wind_support_functions.R")

# retrieve a list of nc files in my data folder
flist <- list.files(path = "Data", pattern = "^.*\\.(nc|NC|Nc|Nc)$")


keep_cols <- c( "horizontal_wspd", 
                "horizontal_wdir", 
                "air_pressure", 
                "relative_humidity"
                )


time_span = (24 * 60 * 60)

# flist, fname, time_col, time_span, time_interval, keep_cols
df <- combine_files(flist, "Data", "time_offset", time_span, 60, 4, keep_cols)


hist(df$horizontal_wspd)
plot(df$horizontal_wspd, type = "l")
