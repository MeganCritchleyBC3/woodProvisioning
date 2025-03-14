##### install packages 

install.packages("ggplot2")
library("sf")
options(stringsAsFactors = FALSE)
library(tidyverse)
library(sf)
library(dplyr)
library(ggplot2)

###### Spain NFI wood provisioning #########

###### load shapefile downloaded direct from NFI 
## layername=fotofija2021__ff21_pb

spainNFI_navarre <- st_read("C:/Users/megan.critchley/Documents/PYRENEES4CLIMA/Data/Landcover/Spain forest/FotoFija2021.gdb/Navarra_fotofija2021.gpkg",
                    layer = "fotofija2021__ff21_pb")
#spainNFI = read_sf("data/countries/countries.shp")
# Read the CSV file
species_data <- read.csv("C:/Users/megan.critchley/Documents/PYRENEES4CLIMA/Data/Landcover/Spain forest/SpainNFI_2021_navarre_SPECIES_input.csv")

####### Add columns for the service values provided by the species 
spainNFI_navarre_SPX <- spainNFI_navarre %>%
  left_join(species_data, by = c("ID_SP1" = "ID_SPX")) %>%
  rename_with(~ paste0(.x, "_SP1"), .cols = all_of(setdiff(names(species_data), "ID_SPX")))

spainNFI_navarre_SPX <- spainNFI_navarre_SPX %>%
  left_join(species_data, by = c("ID_SP2" = "ID_SPX")) %>%
  rename_with(~ paste0(.x, "_SP2"), .cols = all_of(setdiff(names(species_data), "ID_SPX")))

spainNFI_navarre_SPX <- spainNFI_navarre_SPX %>%
  left_join(species_data, by = c("ID_SP3" = "ID_SPX")) %>%
  rename_with(~ paste0(.x, "_SP3"), .cols = all_of(setdiff(names(species_data), "ID_SPX")))

####### make new columns for the importance of the services

### NTFP

spainNFI_navarre_NTFP_s1 <- spainNFI_navarre_SPX %>%
  mutate(NTFP_fruit_level_s1 = case_when(
    Service_fruitHumans_SP1 == 1 & NM_O1 >= 7 ~ 1,
    Service_fruitHumans_SP1 == 1 & NM_O1 >= 5 & NM_O1 < 7 ~ 2,
    Service_fruitHumans_SP2 == 1 & NM_O2 >= 3 & NM_O2 < 5 ~ 3,
    Service_fruitHumans_SP2 == 1 & NM_O2 >= 1 & NM_O2 < 3 ~ 4,
    Service_fruitHumans_SP3 == 1 & NM_O3 >= 1 & NM_O3 < 3 ~ 4,
    TRUE ~ 0
  ))

#### not sure how the level 5 should be handled, using paper logic either nothing 
# or all 0s should be assigned 5 
spainNFI_navarre_NTFP <- spainNFI_navarre_NTFP_s1 %>%
  mutate(NTFP_fruit_level = case_when(
    NTFP_fruit_level_s1 > 0  ~ NTFP_fruit_level_s1,
    NTFP_fruit_level_s1 == 0 & Service_fruitHumans_SP2 == 1 &  NM_O2 > 0 & NM_O2 <= 1 ~ 5,
    NTFP_fruit_level_s1 == 0 & Service_fruitHumans_SP3 == 1 &  NM_O3 > 0 & NM_O3 <= 1 ~ 5,
    TRUE ~ 0
  ))


spainNFI_navarre_NTFP <- spainNFI_navarre_NTFP_test %>%
  relocate(NTFP_fruit_level, .before = geom) %>%
  mutate(across(NTFP_fruit_level, 
                ~ na_if(., 0)))# Convert 0s to NA
glimpse(spainNFI_navarre_NTFP_s1)  # Confirm new column is added
colnames(spainNFI_navarre_NTFP) 
##View(as.data.frame(spainNFI_navarre_NTFP))
head(spainNFI_navarre_NTFP)

### add timber columns 

## level 1 =IF(AND(S2215=1,L2215>=5),1,IF(AND(T2215=1,M2215>=5),1
spainNFI_navarre_timber1 <- spainNFI_navarre_NTFP %>%
  mutate(timber_level1 = case_when(
    Service_timber_SP1 == 1 & NM_O1 >= 5 ~ 1,
    Service_timber_SP2 == 1 & NM_O2 >= 5 ~ 1,
    TRUE ~ 0
  ))
glimpse(spainNFI_navarre_timber1) 

## level 2 
spainNFI_navarre_timber2 <- spainNFI_navarre_timber1 %>%
  mutate(timber_level2 = case_when(
    Service_timber_SP1 == 2 & NM_O1 >= 5 ~ 2,
    Service_timber_SP2 == 2 & NM_O2 >= 5 ~ 2,
    Service_timber_SP1 == 1 & NM_O1 > 0 & NM_O1 < 5 ~ 2,
    Service_timber_SP2 == 1 & NM_O2 > 0 & NM_O2 < 5 ~ 2,
    Service_timber_SP3 == 1 & NM_O3 > 0 & NM_O3 < 5 ~ 2,
    TRUE ~ 0
  ))
glimpse(spainNFI_navarre_timber2) 

## level 3 

spainNFI_navarre_timber3 <- spainNFI_navarre_timber2 %>%
  mutate(timber_level3 = case_when(
    Service_timber_SP1 == 3 & NM_O1 >= 5 ~ 3,
    Service_timber_SP2 == 3 & NM_O2 >= 5 ~ 3,
    Service_timber_SP1 == 2 & NM_O1 > 0 & NM_O1 < 5 ~ 3,
    Service_timber_SP2 == 2 & NM_O2 > 0 & NM_O2 < 5 ~ 3,
    Service_timber_SP3 == 2 & NM_O3 > 0 & NM_O3 < 5 ~ 3,
    TRUE ~ 0
  ))
glimpse(spainNFI_navarre_timber3) 

## level 4 

spainNFI_navarre_timber4 <- spainNFI_navarre_timber3 %>%
  mutate(timber_level4 = case_when(
    Service_timber_SP1 == 4 & NM_O1 >= 5 ~ 4,
    Service_timber_SP2 == 4 & NM_O2 >= 5 ~ 4,
    Service_timber_SP1 == 3 & NM_O1 > 0 & NM_O1 < 5 ~ 4,
    Service_timber_SP2 == 3 & NM_O2 > 0 & NM_O2 < 5 ~ 4,
    Service_timber_SP3 == 3 & NM_O3 > 0 & NM_O3 < 5 ~ 4,
    TRUE ~ 0
  ))
glimpse(spainNFI_navarre_timber4) 

## level 5

spainNFI_navarre_timber5 <- spainNFI_navarre_timber4 %>%
  mutate(timber_level5 = case_when(
    Service_timber_SP1 == 4 & NM_O1 > 0 & NM_O1 < 5 ~ 5,
    Service_timber_SP2 == 4 & NM_O2 > 0 & NM_O2 < 5 ~ 5,
    Service_timber_SP3 == 4 & NM_O3 > 0 & NM_O3 < 5 ~ 5,
    TRUE ~ 0
  ))
glimpse(spainNFI_navarre_timber5) 

### add final column with the highest species level 

spainNFI_navarre_NTFP_timber <- spainNFI_navarre_timber5 %>%
  mutate(across(c(timber_level1, timber_level2, timber_level3, timber_level4, timber_level5), 
                ~ na_if(., 0))) %>% # Convert 0s to NA
  mutate(timber_level = pmin(timber_level1, 
                             timber_level2, 
                             timber_level3,
                             timber_level4,
                             timber_level5,
                                na.rm = TRUE))
glimpse(spainNFI_navarre_NTFP_timber)

###### fuelwood #########

### level 1 

spainNFI_navarre_fuelwood1 <- spainNFI_navarre_NTFP_timber %>%
  mutate(fuelwood_level1 = case_when(
    Service_fuelwood_SP1 == 1 & NM_O1 >= 5 ~ 1,
    Service_fuelwood_SP2 == 1 & NM_O2 >= 5 ~ 1,
    TRUE ~ 0
  ))
glimpse(spainNFI_navarre_fuelwood1) 

## level 2 
spainNFI_navarre_fuelwood2 <- spainNFI_navarre_fuelwood1 %>%
  mutate(fuelwood_level2 = case_when(
    Service_fuelwood_SP1 == 2 & NM_O1 >= 5 ~ 2,
    Service_fuelwood_SP2 == 2 & NM_O2 >= 5 ~ 2,
    Service_fuelwood_SP1 == 1 & NM_O1 > 0 & NM_O1 < 5 ~ 2,
    Service_fuelwood_SP2 == 1 & NM_O2 > 0 & NM_O2 < 5 ~ 2,
    Service_fuelwood_SP3 == 1 & NM_O3 > 0 & NM_O3 < 5 ~ 2,
    TRUE ~ 0
  ))
glimpse(spainNFI_navarre_fuelwood2) 

## level 3 

spainNFI_navarre_fuelwood3 <- spainNFI_navarre_fuelwood2 %>%
  mutate(fuelwood_level3 = case_when(
    Service_fuelwood_SP1 == 3 & NM_O1 >= 5 ~ 3,
    Service_fuelwood_SP2 == 3 & NM_O2 >= 5 ~ 3,
    Service_fuelwood_SP1 == 2 & NM_O1 > 0 & NM_O1 < 5 ~ 3,
    Service_fuelwood_SP2 == 2 & NM_O2 > 0 & NM_O2 < 5 ~ 3,
    Service_fuelwood_SP3 == 2 & NM_O3 > 0 & NM_O3 < 5 ~ 3,
    TRUE ~ 0
  ))
glimpse(spainNFI_navarre_fuelwood3) 

## level 4 

spainNFI_navarre_fuelwood4 <- spainNFI_navarre_fuelwood3 %>%
  mutate(fuelwood_level4 = case_when(
    Service_fuelwood_SP1 == 4 & NM_O1 >= 5 ~ 4,
    Service_fuelwood_SP2 == 4 & NM_O2 >= 5 ~ 4,
    Service_fuelwood_SP1 == 3 & NM_O1 > 0 & NM_O1 < 5 ~ 4,
    Service_fuelwood_SP2 == 3 & NM_O2 > 0 & NM_O2 < 5 ~ 4,
    Service_fuelwood_SP3 == 3 & NM_O3 > 0 & NM_O3 < 5 ~ 4,
    TRUE ~ 0
  ))
glimpse(spainNFI_navarre_fuelwood4) 

## level 5

spainNFI_navarre_fuelwood5 <- spainNFI_navarre_fuelwood4 %>%
  mutate(fuelwood_level5 = case_when(
    Service_fuelwood_SP1 == 4 & NM_O1 > 0 & NM_O1 < 5 ~ 5,
    Service_fuelwood_SP2 == 4 & NM_O2 > 0 & NM_O2 < 5 ~ 5,
    Service_fuelwood_SP3 == 4 & NM_O3 > 0 & NM_O3 < 5 ~ 5,
    TRUE ~ 0
  ))
glimpse(spainNFI_navarre_fuelwood5) 

### add final column with the highest species level 

spainNFI_navarre_NTFP_timber_fuelwood <- spainNFI_navarre_fuelwood5 %>%
  mutate(across(c(fuelwood_level1, fuelwood_level2, fuelwood_level3, fuelwood_level4, fuelwood_level5), 
                ~ na_if(., 0))) %>% # Convert 0s to NA
  mutate(fuelwood_level = pmin(fuelwood_level1, 
                             fuelwood_level2, 
                             fuelwood_level3,
                             fuelwood_level4,
                             fuelwood_level5,
                             na.rm = TRUE))
glimpse(spainNFI_navarre_NTFP_timber_fuelwood)
str(spainNFI_navarre_NTFP_timber_fuelwood)
###### visualise to check #######

# Define custom colors for the 5 classes
class_colors <- c("1" = "darkgreen",   # Class 1
                  "2" = "lightgreen",  # Class 2
                  "3" = "yellow",      # Class 3
                  "4" = "orange",      # Class 4
                  "5" = "red")         # Class 5

# Plot the spatial data
ggplot(spainNFI_navarre_NTFP_timber_fuelwood) +
  geom_sf(aes(fill = factor(fuelwood_level)), color = "black", size = 0.1) +  # Use factor to ensure discrete colors
  scale_fill_manual(values = class_colors, name = "Fuelwood importance Level") +  # Assign colors to classes
  theme_minimal() +
  labs(title = "Spatial Map of Fuelwood Species Importance Level",
      # subtitle = "Categorized into 5 classes",
       fill = " Level") +
  theme(legend.position = "right")

ggplot(spainNFI_navarre_NTFP_timber_fuelwood) +
  geom_sf(aes(fill = factor(fuelwood_level)), color = NA, size = 0) +  # No borders = faster!
  scale_fill_manual(values = c("1" = "darkgreen", "2" = "lightgreen",
                               "3" = "yellow", "4" = "orange", "5" = "red")) +
  theme_void() +  # Lighter theme
  coord_sf(datum = NA) + # Avoid unnecessary transformations 
  labs(title = "Spatial Map of Fuelwood Species Importance Level",
       # subtitle = "Categorized into 5 classes",
       fill = "Level") +
  theme(legend.position = "right")
