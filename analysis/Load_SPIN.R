# Load data from Social Policy Indicators 
# http://www.spin.su.se/datasets

# Packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio)

# Data from http://www.spin.su.se/datasets

# Read in policy indicators
# (1) Get files
files <- list.files(path = "./data", pattern = "*.xlsx|.xls", full.names = TRUE) %>%
  .[-2]

# (2) Map over files while reading
spin.df <- data_frame(
  filename = str_extract_all(files, pattern = "[:upper:]+")) %>%
  mutate(data = 
           map2(.x = files, 
                .y = c(0,0,1,0,0),
               ~import(., col_names = TRUE, skip = .y))
  )

# Data cleaning

  
