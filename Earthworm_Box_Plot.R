# Import of tidyverse, and excel reader

library(readxl)
library(tidyverse)

# Creates a data frame of the excel containing the earthworm data

Earthworms <- read_excel("Worm Survey_v18Jun24.xlsx")


# Creates a new data frame of the mean size of all Earthworms between sites

Earthworm_Abundance <- Earthworms |>
  group_by(Site, Quadrat) |>
  count(Quadrat) |>
  ungroup(Site, Quadrat) |>
  add_row(Site = "RBP", Quadrat = "0105", n = 0) |>
  add_row(Site = "RBP", Quadrat = "0107", n = 0) |>
  add_row(Site = "TFO", Quadrat = "0204", n = 0) |>
  rename(Abundance = n)

# Creates a box plot that shows 

ggplot(Earthworm_Abundance, aes(x = Site, y = Abundance)) +
  labs(title = "Earthworm Abundance") + 
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
    ) +
  geom_boxplot(fill = "#A2B283") + 
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = .5)
  
