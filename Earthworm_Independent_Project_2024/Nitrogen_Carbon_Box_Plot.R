# Import of tidyverse, and excel reader

library(readxl)
library(tidyverse)

# Creates a data frame of the excel containing the earthworm data

Nutrient_Data <- read_excel("Soil CN and Mass_1Jun24.xlsx")

# Removes the useless columns, and fixes the Quadrat numbers

Clean_Nutrient <- Nutrient_Data|>
  select(-ShortID, -`Mass_(g)_60`, -`Mass_(g)_100`) |>
  mutate(Quadrat = str_pad(Quadrat, width = 4, pad = "0"))

# Filters out the unneeded variables, and creates Carbon Nitrogen Ratio of the depth ranging 0-20 cm

CNRatio_Nutrient <- Clean_Nutrient |>
  filter(Site == "Robbins" | Site == "TFO") |>
  filter(`Depth_(cm)` == "0-10" | `Depth_(cm)`== "10-20") |> 
  group_by(Site, Quadrat) |> 
  mutate(C_N_Ratio = `%C`/ `%N`) |> 
  summarise(`CN Ratio` = mean(C_N_Ratio))

# Creates a box plot showing the data from CNRatio_Nutrient

ggplot(CNRatio_Nutrient, aes(x = Site, y = `CN Ratio`)) +
  labs(title = "Carbon Nitrogen Ratio") + 
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  ) +
  geom_boxplot(fill = "#A2B283") + 
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = .5)


  

