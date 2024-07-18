# This code will create the two regression plots for my comparison of earthworm
# abundance with mean temperature and mean water content

library(tidyverse)
library(readxl)
library(patchwork)

# load data from soil sensors and earthworm collection

Soil_Data <- read_csv("Worm Soil Data_v8Jul24.csv")
Earthworms <- read_excel("Worm Survey_v18Jun24.xlsx")

# Cleans the data sets for usage

Soil_Data$Date <- lubridate::mdy_hm(Soil_Data$Date)

mean_Soil_Temp <- Soil_Data |> 
  select(contains("Temperature")) |>
  summarise(across(everything(), mean, na.rm = TRUE))

mean_Soil_Water <- Soil_Data |> 
  select(contains("Water")) |>
  summarise(across(everything(), mean, na.rm = TRUE))

Earthworm_Abundance <- Earthworms |>
  group_by(Site, Quadrat) |>
  count(Quadrat) |>
  ungroup(Site, Quadrat) |>
  add_row(Site = "RBP", Quadrat = "0105", n = 0) |>
  add_row(Site = "RBP", Quadrat = "0107", n = 0) |>
  add_row(Site = "TFO", Quadrat = "0204", n = 0) |>
  rename(Abundance = n)


# pivot long from each column 

LongTempSoil <- mean_Soil_Temp |> 
  pivot_longer(cols = contains("Temperature"), values_to = "Temperature")

LongWaterSoil <- mean_Soil_Water |>
  pivot_longer(cols = contains("Water"), values_to = "Water Content")

# Seperate name column into three using a space as the delimiter

LongTempSoil <- LongTempSoil |> 
  separate(name, into = c("Type", "Site", "Quadrat"), sep = " ") |> 
  select(-Type)

LongWaterSoil <- LongWaterSoil |>
  separate(name, into = c("Water", "Content", "Site", "Quadrat"), sep = " ") |>
  select(-Water, -Content)

# Join LongSoil and Earthworm_Abundance by Site and Quad

tot_temp <-  left_join(LongTempSoil, Earthworm_Abundance, by = c("Site", "Quadrat")) |>
  filter(Quadrat != "0306")

tot_water <- left_join(LongWaterSoil, Earthworm_Abundance, by = c("Site", "Quadrat")) |>
  filter(Quadrat != "0306")

# Create the regression plots

plot_temp <- ggplot(tot_temp, aes(x = Temperature, y = Abundance)) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  ) +
  geom_point(color = "#A2B283") +
  geom_smooth(method = "lm", color = "steelblue") +
  labs(
    title = "Earthworm Abundance and Soil Temperature",
    x = "Soil Temperature", y = "Earthworm Abundance",) 

plot_water <- ggplot( tot_water, aes(x = `Water Content`, y = Abundance)) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  ) +
  geom_point(color = "#A2B283") +
  geom_smooth(method = "lm", color = "steelblue") +
  labs(
    title = "Earthworm Abundance and Soil Water Content",
    x = "Soil Water Content", y = "Earthworm Abundance")

regression_plots <- plot_temp + plot_water 

regression_plots  +
  plot_layout(axes = "collect")

# Obtains the R^2 values for the regression plots
tot_temp_lm <- lm(tot_temp$Abundance ~ tot_temp$Temperature)
tot_water_lm <- lm(tot_water$Abundance ~ tot_water$`Water Content`)
 
summary(tot_water_lm)$r.squared 
summary(tot_temp_lm)$r.squared
