# Import of tidyverse, and excel reader

library(tidyverse)
library(patchwork)

# Creates a data frame of the excel containing the earthworm data

# Soil_Data <- read_excel("Worm Soil Data_v8Jul24.excel")

Soil_Data <- read_csv("Worm Soil Data_v8Jul24.csv")


# Fixes dates of data to be just Month/Day/Year

Soil_Data$Date <- lubridate::mdy_hm(Soil_Data$Date) 

Soil_Data <- Soil_Data |> 
  mutate(Date = format(Date, "%m-%d-%Y"))

# Group and averages the dates together

Soil_Data_Clean <- Soil_Data |> 
  select(-`Line#`) |> 
  group_by(Date) |> 
  summarise(across(everything(), mean, na.rm = TRUE))

Soil_Data_Clean$Date <- lubridate::mdy(Soil_Data_Clean$Date) 

# Dual axis scatter line plot for each plot. Also a key to allow the creation of a legend.
key <- c("Temperature" = "#a2b283", "Water Content" = "steelblue")

Plot_0107 <- ggplot(Soil_Data_Clean, aes(x=Date, y=`Temperature TFO 0107`, group = 1)) +
  labs(title = "TFO Plot 0107",
       color = "Legend") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  ) +
  geom_line(aes(color = "Temperature"), size = 1.5) + 
  geom_line(aes(y=`Water Content TFO 0107`*100, group = 1, color = "Water Content"), size = 1.5) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Temperature",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~./100, name="Water Content")
  ) +
  scale_color_manual(values = key)

Plot_0117 <- ggplot(Soil_Data_Clean, aes(x=Date, y=`Temperature TFO 0117`, group = 1)) +
  labs(title = "TFO Plot 0117",
       color = "Legend") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  ) +
  geom_line(aes(color = "Temperature"), size = 1.5) + 
  geom_line(aes(y=`Water Content TFO 0117`*100, group = 1, color = "Water Content"), size = 1.5) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Temperature",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~./100, name="Water Content")
  ) +
  scale_color_manual(values = key)


Plot_0204 <- ggplot(Soil_Data_Clean, aes(x=Date, y=`Temperature TFO 0204`, group = 1)) +
  labs(title = "TFO Plot 0204",
       color = "Legend") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  ) +
  geom_line(aes(color = "Temperature"), size = 1.5) + 
  geom_line(aes(y=`Water Content TFO 0204`*100, group = 1, color = "Water Content"), size = 1.5) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Temperature",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~./100, name="Water Content")
  ) +
 scale_color_manual(values = key)

Plot_0214 <- ggplot(Soil_Data_Clean, aes(x=Date, y=`Temperature TFO 0214`, group = 1)) +
  labs(title = "TFO Plot 0214",
       color = "Legend") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  ) +
  geom_line(aes(color = "Temperature"), size = 1.5) + 
  geom_line(aes(y=`Water Content TFO 0214`*100, group = 1, color = "Water Content"), size = 1.5) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Temperature",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~./100, name="Water Content")
  ) +
  scale_color_manual(values = key)

Plot_0306 <- ggplot(Soil_Data_Clean, aes(x=Date, y=`Temperature TFO 0306`, group = 1)) +
  labs(title = "TFO Plot 0306",
       color = "Legend") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  ) +
  geom_line(aes(color = "Temperature"), size = 1.5) + 
  geom_line(aes(y=`Water Content TFO 0306`*50, group = 1, color = "Water Content"), size = 1.5) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Temperature",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~./50, name="Water Content")
  ) +
  scale_color_manual(values = key)

Plot_0101 <- ggplot(Soil_Data_Clean, aes(x=Date, y=`Temperature RBP 0101`, group = 1)) +
  labs(title = "RBP Plot 0101",
       color = "Legend") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  ) +
  geom_line(aes(color = "Temperature"), size = 1.5) + 
  geom_line(aes(y=`Water Content RBP 0101`*50, group = 1, color = "Water Content"), size = 1.5) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Temperature",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~./50, name="Water Content")
  ) +
  scale_color_manual(values = key)


Plot_0103 <- ggplot(Soil_Data_Clean, aes(x=Date, y=`Temperature RBP 0103`, group = 1)) +
  labs(title = "RBP Plot 0103",
       color = "Legend") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  ) +
  geom_line(aes(color = "Temperature"), size = 1.5) + 
  geom_line(aes(y=`Water Content RBP 0103`*50, group = 1, color = "Water Content"), size = 1.5) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Temperature",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~./50, name="Water Content")
  ) +
  scale_color_manual(values = key)


Plot_0105 <- ggplot(Soil_Data_Clean, aes(x=Date, y=`Temperature RBP 0105`, group = 1)) +
  labs(title = "RBP Plot 0105",
       color = "Legend") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  ) +
  geom_line(aes(color = "Temperature"), size = 1.5) + 
  geom_line(aes(y=`Water Content RBP 0105`*100, group = 1, color = "Water Content"), size = 1.5) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Temperature",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~./100, name="Water Content")
  ) +
  scale_color_manual(values = key)


Plot_0300 <- ggplot(Soil_Data_Clean, aes(x=Date, y=`Temperature RBP 0300`, group = 1)) +
  labs(title = "RBP Plot 0300",
       color = "Legend") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  ) +
  geom_line(aes(color = "Temperature"), size = 1.5) + 
  geom_line(aes(y=`Water Content RBP 0300`*50, group = 1, color = "Water Content"), size = 1.5) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Temperature",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~./50, name="Water Content")
  ) +
  scale_color_manual(values = key)


# Time to practice my patchwork skills

Soil_Sensor_Plots <- Plot_0107 + Plot_0117 + Plot_0204 + Plot_0214 + Plot_0306 + Plot_0101 + Plot_0103 + Plot_0105 + Plot_0300

Soil_Sensor_Plots + plot_annotation(
  title = "Temperature and Water Content over Time in TFO and RBP"
  ) + 
  plot_layout(guides = "collect")

