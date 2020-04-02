
# Sets up for other scripts, including loading packages and setting connections.
library(tidyverse)
library(lubridate)
library(readxl)
library(chron)
library(fs)
library(factoextra)
library(stats)
library(cluster)
library(nousstyle)
library(nousutils)
library(nousstyle)
library(plotly)



# Run all the custom functions stored in folder.
rfiles <- list.files('3_Rfunctions', pattern = "\\.[Rr]$", full.names = TRUE)
for (f in rfiles){source(f)}

build_rmd_report(report_name = "markdown_template", subdir = "2_Code")