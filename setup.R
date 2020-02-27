
# Sets up for other scripts, including loading packages and setting connections.
library(tidyverse)
library(lubridate)
library(readxl)
library(chron)
library(fs)


# Run all the custom functions stored in folder.
rfiles <- list.files('3_Rfunctions', pattern = "\\.[Rr]$", full.names = TRUE)
for (f in rfiles){source(f)}
