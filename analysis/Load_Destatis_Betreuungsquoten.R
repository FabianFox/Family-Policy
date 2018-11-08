# Load data from Destatis (Genesis)
# Population: 12411-0012
# Attendance of "Tageseinrichtung": 22541-0002

!!!!!!!!!!
# ADD: Attendance of "Kindertagespflege": 22543-0002
!!!!!!!!!
  
# Packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio)

# Stichtag: 31.12.
pop <- import("./data/12411-0012.xls", range = "A4:R95", col_names = TRUE) %>%
  gather(key = Bundesland, value = Anzahl, c(-Altersjahre, -Jahr)) %>%
  mutate(Altersjahre = case_when(
    Altersjahre %in% c("unter 1 Jahr", "1-Jährige", "2-Jährige") ~ "unter 3 Jahre",
    Altersjahre %in% c("3-Jährige", "4-Jährige", "5-Jährige") ~ "3 bis unter 6 Jahre",
    TRUE ~ "Insgesamt"
  )) %>%
  filter(Altersjahre != "Insgesamt")

# Stichtag: 01.03.
care <- import("./data/22541-0002.xls", range = "A5:D213", col_names = TRUE) %>%
  gather(key = Altersjahre, value = Betreuung, c(-Bundesland, -Jahr)) %>%
  mutate(Jahr = Jahr-1) # to match "Stichtag" in pop

# Join data for further analysis
popcare <- pop %>%
  left_join(care) %>%
  mutate(Region = case_when(
    Bundesland %in% c("Berlin", "Brandenburg", "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", 
                      "Thüringen") ~ "Neue Bundesländer",
    TRUE ~ "Alte Bundesländer"
  ))

# Childcare attendance over time by Bundesland
attendance.bund <- popcare %>%
  group_by(Altersjahre, Jahr, Bundesland) %>%
  mutate(Anzahl = sum(Anzahl)) %>%
  distinct(.keep_all = TRUE) %>%
  group_by(Altersjahre, Jahr, Bundesland) %>%
  mutate(Betreuungsquote = Betreuung/Anzahl*100) %>%
  filter(Bundesland == "Nordrhein-Westfalen")

# Childcare attendance over time by "Ost/West"
attendance.region <- popcare %>%
  group_by(Altersjahre, Jahr, Bundesland) %>%
  mutate(Anzahl = sum(Anzahl)) %>%
  distinct(.keep_all = TRUE) %>%
  group_by(Altersjahre, Jahr, Region) %>%
  mutate(Anzahl = sum(Anzahl),
         Betreuung = sum(Betreuung)) %>%
  select(-Bundesland) %>%
  distinct() %>%
  summarise(Betreuungsquote = Betreuung/Anzahl*100)
