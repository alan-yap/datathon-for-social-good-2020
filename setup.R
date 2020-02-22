
# Sets up for other scripts, including loading packages and setting connections.

library (nousutils) # need to install these again
#library (nousstyle)
library (xlsx)
library (tidyverse)
library (janitor) # mass clean variable name
library (readxl)
library (data.table) # setnames(dt, 'old name', 'new name')


# Run all the custom functions stored in folder.

rfiles <- list.files ('3_Rfunctions', pattern = "\\.[Rr]$", full.names = TRUE)
for (f in rfiles){source(f)}

keepers <- c ('keepers',
              'cleanup',
              'run_all_r_scripts')

cleanup(keepers)

# If require DAWN
# DAWN <- connect_dawn()

