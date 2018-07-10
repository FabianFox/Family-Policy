# Load data family policies

# From Social Policy Indicators 
# Link: http://www.spin.su.se/datasets

# Packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, countrycode)

# Data from http://www.spin.su.se/datasets
### ------------------------------------------------------------------------------------------------ ###

# Read in policy indicators
# (1) Get files
files <- list.files(path = "./data", pattern = "*.xlsx|.xls", full.names = TRUE) %>%
  .[c(-2,-4,-5)] # Certain aren't needed

# (2) Map over files while reading
spin.df <- data_frame(
  filename = str_extract_all(files, pattern = "[:upper:]+")) %>%
  mutate(data = 
           map2(.x = files, 
                .y = c(0,0,1,0,0), # One file has two headings
               ~import(., col_names = TRUE, skip = .y))
  )

# From OECD.Stat "SOCX_AGG"
### ------------------------------------------------------------------------------------------------ ###

# Would need GDP p.c. to proceed further
socialexp.df <- import("./data/SOCX_AGG_09072018142527039.csv") %>%
  filter(Branch == "Family")

ecec.df <- socialexp.df %>%
  filter(`Type of Programme` == "Family - Early childhood education and care (ECEC)",
         Year >= 1990)

# Other option: OECD "Family Database"
### ------------------------------------------------------------------------------------------------ ###

# Link: http://www.oecd.org/els/soc/PF3_1_Public_spending_on_childcare_and_early_education.xlsx
ecec.df <- import("./data/PF3_1_Public_spending_on_childcare_and_early_education.xlsx",
                  range = "A4:AJ39", sheet = 3) %>%
  select(-Note) %>%
  gather(key = year, value = expenditure, -Country) %>%
  mutate(ISO3 = countrycode(Country, origin = "country.name", destination = "iso3c"),
         EU28 = countrycode(Country, origin = "country.name", destination = "eu28"),
         expenditure = as.numeric(expenditure),
         year = as.numeric(year)) %>%
  filter(!is.na(EU28))

# Link: http://www.oecd.org/els/soc/PF3_2_Enrolment_childcare_preschool.xlsx
enrol.df <- import("./data/PF3_2_Enrolment_childcare_preschool.xlsx",
                   range = "A4:V45", sheet = 7) %>%
  select(-Note) %>%
  gather(key = year, value = enrolment, -Country) %>%
  mutate(enrolment = as.numeric(enrolment),
         year = as.numeric(year))

ecec.df <- ecec.df %>%
  left_join(enrol.df, by = c("Country", "year"))