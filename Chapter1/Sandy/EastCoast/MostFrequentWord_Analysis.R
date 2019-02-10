library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(tidytext)
library(stringr)


# Defining working directory
setwd ("/home/marcela/Coding/MarcesThesis/Chapter1/Sandy/EastCoast")

# Loading data into R
sandy <- read.csv("Sandy_Subset.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Cleaning text a bit
sandy <- distinct(sandy, text, .keep_all = TRUE) # remove duplicate tweets
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_sandy <- sandy %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) 

tidy_sandy$created_at <- ymd_hms(tidy_sandy$created_at, tz ="UTC") # Setting date variable

# Adding column for each most frequent word
tidy_power <- tidy_sandy %>% 
  unnest_tokens(PowerC, text, token = "words") %>%
  filter(PowerC == "power")
tidy_power <- select(tidy_power, one_of(c("id", "PowerC")))
tidy_power <- distinct(tidy_power, id, .keep_all = TRUE) # remove duplicate tweets
tidy_sandy <- left_join(tidy_sandy, tidy_power, by="id")

tidy_storm <- tidy_sandy %>% 
  unnest_tokens(StormC, text, token = "words") %>%
  filter(StormC == "storm")
tidy_storm <- select(tidy_storm, one_of(c("id", "StormC")))
tidy_storm <- distinct(tidy_storm, id, .keep_all = TRUE) # remove duplicate tweets
tidy_sandy <- left_join(tidy_sandy, tidy_storm, by="id")

tidy_wind <- tidy_sandy %>% 
  unnest_tokens(WindC, text, token = "words") %>%
  filter(WindC == "wind")
tidy_wind <- select(tidy_wind, one_of(c("id", "WindC")))
tidy_wind <- distinct(tidy_wind, id, .keep_all = TRUE) # remove duplicate tweets
tidy_sandy <- left_join(tidy_sandy, tidy_wind, by="id")

# tidy_hit <- tidy_sandy %>% 
#   unnest_tokens(HitC, text, token = "words") %>%
#   filter(HitC == "hit")
# tidy_hit <- select(tidy_hit, one_of(c("id", "HitC")))
# tidy_hit <- distinct(tidy_hit, id, .keep_all = TRUE) # remove duplicate tweets
# tidy_sandy <- left_join(tidy_sandy, tidy_hit, by="id")

tidy_rain <- tidy_sandy %>% 
  unnest_tokens(RainC, text, token = "words") %>%
  filter(RainC == "rain")
tidy_rain <- select(tidy_rain, one_of(c("id", "RainC")))
tidy_rain <- distinct(tidy_rain, id, .keep_all = TRUE) # remove duplicate tweets
tidy_sandy <- left_join(tidy_sandy, tidy_rain, by="id")

tidy_flood <- tidy_sandy %>% 
  unnest_tokens(Flood, text, token = "words") %>%
  filter(Flood == "flood")
tidy_flood <- select(tidy_flood, one_of(c("id", "Flood")))
tidy_flood <- distinct(tidy_flood, id, .keep_all = TRUE) # remove duplicate tweets
tidy_sandy <- left_join(tidy_sandy, tidy_flood, by="id")

# Preparing datasets to plot
tidy_storm_plot <- tidy_sandy[!is.na(tidy_sandy$StormC),]
Date_Storm <- tidy_storm_plot$created_at
tidy_power_plot <- tidy_sandy[!is.na(tidy_sandy$PowerC),]
Date_Power <- tidy_power_plot$created_at
tidy_wind_plot <- tidy_sandy[!is.na(tidy_sandy$WindC),]
Date_Wind <- tidy_wind_plot$created_at
# tidy_hit_plot <- tidy_sandy[!is.na(tidy_sandy$HitC),]
# Date_Hit <- tidy_hit_plot$created_at
tidy_flood_plot <- tidy_sandy[!is.na(tidy_sandy$Flood),]
Date_Flood <- tidy_flood_plot$created_at
tidy_rain_plot <- tidy_sandy[!is.na(tidy_sandy$RainC),]
Date_Rain <- tidy_rain_plot$created_at

# Plotting temporal distribution of most frequent words
mfw <- ggplot() + 
  geom_freqpoly(data = tidy_storm_plot, aes(x = Date_Storm, y = ..count../sum(..count..), color = "Storm")) +
  geom_freqpoly(data = tidy_power_plot, aes(x = Date_Power, y = ..count../sum(..count..), color = "Power")) + 
  geom_freqpoly(data = tidy_wind_plot, aes(x = Date_Wind, y = ..count../sum(..count..), color = "Wind")) + 
  geom_freqpoly(data = tidy_flood_plot, aes(x = Date_Flood, y = ..count../sum(..count..), color = "Flood")) + 
  geom_freqpoly(data = tidy_rain_plot, aes(x = Date_Rain, y = ..count../sum(..count..), color = "Rain")) +
  scale_color_manual("", values = c("#ffff99", "#e78ac3","#fdb462", "#a6cee3","#66c2a5")) +
  xlab("Date") +
  ylab("Percentage of Reports") +
  scale_y_continuous(labels = scales::percent) +
  theme_dark() +
  #theme_stata() + #scale_colour_stata() + 
  theme(legend.title = element_blank(),
        legend.background = element_rect(fill = "grey50", size = 4, colour = "grey50"),
        legend.text = element_text(size = 10),
        plot.background = element_rect(fill = "grey50", colour = NA),
        axis.title = element_text(size = 12, face = "bold",
                                  margin=margin(20,20,0,0)), 
        axis.line = element_line(colour = "Black"),
        legend.position = c(0.85, 0.6),
        axis.text = element_text(size = 11, color = "black"),
        strip.text = element_text(size = 11),
        legend.key.width = unit(1.5, "cm"))  

mfw

ggsave(filename = "MostFreqWordsEvents", plot = mfw, device = "png", 
       path = "Outputs", width=15, height=6, units="in", dpi = 300, bg = "Black")

# Preparing dataset to map Most Frequent Words
# tidy_sandy$mfw = "" # Initialize Variable
# tidy_sandy$mfw[tidy_sandy$HitC != "NA"] = "hit"
# tidy_sandy$mfw[tidy_sandy$mfw != "hit" & tidy_sandy$RainC != "NA"] = "rain"
# tidy_sandy$mfw[tidy_sandy$mfw != "hit" & tidy_sandy$mfw != "rain" & tidy_sandy$WindC != "NA"] = "wind"
# tidy_sandy$mfw[tidy_sandy$mfw != "hit" & tidy_sandy$mfw != "rain" & tidy_sandy$mfw != "wind" & 
#                  tidy_sandy$StormC != "NA"] = "storm"
# tidy_sandy$mfw[tidy_sandy$mfw != "hit" & tidy_sandy$mfw != "rain" & tidy_sandy$mfw != "wind" & 
#                  tidy_sandy$mfw != "storm" & tidy_sandy$PowerC != "NA"] = "power"

