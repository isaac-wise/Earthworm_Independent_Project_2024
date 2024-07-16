# This  file will contain the following statistical information. A 
# Wilcoxon-Mann-Whitney Test of the earthworm abundance along with 
# potentially more statistical test for other data such as CN 
# ratio or potentially further data.

library(tidyverse)
library(readxl)

Earthworms <- read_excel("Worm Survey_v18Jun24.xlsx")
Nutrient_Data <- read_excel("Soil CN and Mass_1Jun24.xlsx")


# Creates a new data frame of the mean size of all Earthworms between sites

Earthworm_Abundance <- Earthworms |>
  group_by(Site, Quadrat) |>
  count(Quadrat) |>
  ungroup(Site, Quadrat) |>
  add_row(Site = "RBP", Quadrat = "0105", n = 0) |>
  add_row(Site = "RBP", Quadrat = "0107", n = 0) |>
  add_row(Site = "TFO", Quadrat = "0204", n = 0) |>
  rename(Abundance = n)

wilcox.test(Earthworm_Abundance$Abundance ~ Earthworm_Abundance$Site)

# Removes the useless columns, and fixes the Quadrat numbers

Clean_Nutrient <- Nutrient_Data|>
  select(-ShortID, -`Mass_(g)_60`, -`Mass_(g)_100`) |>
  mutate(Quadrat = str_pad(Quadrat, width = 4, pad = "0"))

# Filters out the unneeded variables, and creates Carbon Nitrogen Ratio of the depth ranging 0-20 cm

CNRatio_Nutrient <- Clean_Nutrient |>
  filter(Site == "Robbins" | Site == "TFO") |>
  filter(`Depth_(cm)`== "0-10" | `Depth_(cm)` == "10-20" | `Depth_(cm)` == "20+") |>
  filter(Quadrat == "0101" | Quadrat == "0103"| Quadrat == "0105"| Quadrat == "0107"| Quadrat == "0117"| Quadrat == "0204"| Quadrat == "0214"| Quadrat == "0300"| Quadrat == "0306") |>
  group_by(Site, Quadrat) |> 
  mutate(C_N_Ratio = `%C`/ `%N`) |> 
  summarise(`CN Ratio` = mean(C_N_Ratio))

wilcox.test(CNRatio_Nutrient$`CN Ratio` ~ CNRatio_Nutrient$Site)

