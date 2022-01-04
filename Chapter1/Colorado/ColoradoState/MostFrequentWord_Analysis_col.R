library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(tidytext)
library(stringr)


# Defining working directory
setwd ("/home/marcela/Coding/EWE-reporting/Colorado/ColoradoState")

# Loading data into R
colorado <- read.csv("Colorado_Subset.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Cleaning text a bit
colorado <- distinct(colorado, t_text, .keep_all = TRUE) # remove duplicate tweets
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_colorado <- colorado %>% 
  filter(!str_detect(t_text, "^RT")) %>%
  mutate(text = str_replace_all(t_text, replace_reg, "")) 

tidy_colorado$created_at <- ymd_hms(tidy_colorado$created_at, tz ="UTC") # Setting date variable

# Adding column for each most frequent word

# tidy_cowx <- tidy_colorado %>% 
#   unnest_tokens(COWX, t_text, token = "words") %>%
#   filter(COWX == "cowx")
# tidy_cowx <- select(tidy_cowx, one_of(c("id", "COWX")))
# tidy_cowx <- distinct(tidy_cowx, id, .keep_all = TRUE) # remove duplicate tweets
# tidy_colorado <- left_join(tidy_colorado, tidy_cowx, by="id")

tidy_flash <- tidy_colorado %>% 
  unnest_tokens(FLASH, t_text, token = "words") %>%
  filter(FLASH == "flash")
tidy_flash <- select(tidy_flash, one_of(c("id", "FLASH")))
tidy_flash <- distinct(tidy_flash, id, .keep_all = TRUE) # remove duplicate tweets
tidy_colorado <- left_join(tidy_colorado, tidy_flash, by="id")

tidy_flood <- tidy_colorado %>% 
  unnest_tokens(FLOOD, t_text, token = "words") %>%
  filter(FLOOD== "flood")
tidy_flood <- select(tidy_flood, one_of(c("id", "FLOOD")))
tidy_flood <- distinct(tidy_flood, id, .keep_all = TRUE) # remove duplicate tweets
tidy_colorado <- left_join(tidy_colorado, tidy_flood, by="id")

tidy_rain <- tidy_colorado %>% 
  unnest_tokens(RAIN, t_text, token = "words") %>%
  filter(RAIN == "rain")
tidy_rain <- select(tidy_rain, one_of(c("id", "RAIN")))
tidy_rain <- distinct(tidy_rain, id, .keep_all = TRUE) # remove duplicate tweets
tidy_colorado <- left_join(tidy_colorado, tidy_rain, by="id")

tidy_thunderstorm <- tidy_colorado %>% 
  unnest_tokens(THUNDERSTORM, t_text, token = "words") %>%
  filter(THUNDERSTORM == "thunderstorm")
tidy_thunderstorm <- select(tidy_thunderstorm, one_of(c("id", "THUNDERSTORM")))
tidy_thunderstorm <- distinct(tidy_thunderstorm, id, .keep_all = TRUE) # remove duplicate tweets
tidy_colorado <- left_join(tidy_colorado, tidy_thunderstorm, by="id")

tidy_snow <- tidy_colorado %>% 
  unnest_tokens(SNOW, t_text, token = "words") %>%
  filter(SNOW == "snow")
tidy_snow <- select(tidy_snow, one_of(c("id", "SNOW")))
tidy_snow <- distinct(tidy_snow, id, .keep_all = TRUE) # remove duplicate tweets
tidy_colorado <- left_join(tidy_colorado, tidy_snow, by="id")

tidy_water <- tidy_colorado %>% 
  unnest_tokens(WATER, t_text, token = "words") %>%
  filter(WATER == "cowx")
tidy_water <- select(tidy_water, one_of(c("id", "WATER")))
tidy_water <- distinct(tidy_water, id, .keep_all = TRUE) # remove duplicate tweets
tidy_colorado <- left_join(tidy_colorado, tidy_water, by="id")

tidy_storm <- tidy_colorado %>% 
  unnest_tokens(STORM, t_text, token = "words") %>%
  filter(STORM == "storm")
tidy_storm <- select(tidy_storm, one_of(c("id", "STORM")))
tidy_storm <- distinct(tidy_storm, id, .keep_all = TRUE) # remove duplicate tweets
tidy_colorado <- left_join(tidy_colorado, tidy_storm, by="id")

# tidy_warn <- tidy_colorado %>% 
#   unnest_tokens(WARN, t_text, token = "words") %>%
#   filter(WARN == "warn")
# tidy_warn <- select(tidy_warn, one_of(c("id", "WARN")))
# tidy_warn <- distinct(tidy_warn, id, .keep_all = TRUE) # remove duplicate tweets
# tidy_colorado <- left_join(tidy_colorado, tidy_warn, by="id")
# 
# tidy_help <- tidy_colorado %>% 
#   unnest_tokens(HELP, t_text, token = "words") %>%
#   filter(HELP == "help")
# tidy_help <- select(tidy_help, one_of(c("id", "HELP")))
# tidy_help <- distinct(tidy_help, id, .keep_all = TRUE) # remove duplicate tweets
# tidy_colorado <- left_join(tidy_colorado, tidy_help, by="id")
# 
# tidy_victim <- tidy_colorado %>% 
#   unnest_tokens(VICTIM, t_text, token = "words") %>%
#   filter(VICTIM == "victim")
# tidy_victim <- select(tidy_victim, one_of(c("id", "VICTIM")))
# tidy_victim <- distinct(tidy_victim, id, .keep_all = TRUE) # remove duplicate tweets
# tidy_colorado <- left_join(tidy_colorado, tidy_victim, by="id")

# Preparing datasets to plot

# tidy_flash_plot <- tidy_colorado[!is.na(tidy_colorado$FLASH),]
# Date_FLASH <- tidy_flash_plot$created_at
# 
# tidy_warn_plot <- tidy_colorado[!is.na(tidy_colorado$WARN),]
# Date_WARN <- tidy_warn_plot$created_at
# 
# tidy_help_plot <- tidy_colorado[!is.na(tidy_colorado$HELP),]
# Date_HELP <- tidy_help_plot$created_at
# 
# tidy_victim_plot <- tidy_colorado[!is.na(tidy_colorado$VICTIM),]
# Date_VICTIM <- tidy_victim_plot$created_at
# 
# tidy_cowx_plot <- tidy_colorado[!is.na(tidy_colorado$COWX),]
# Date_COWX <- tidy_cowx_plot$created_at

tidy_flash_plot <- tidy_colorado[!is.na(tidy_colorado$FLASH),]
Date_FLASH <- tidy_flash_plot$created_at

tidy_flood_plot <- tidy_colorado[!is.na(tidy_colorado$FLOOD),]
Date_FLOOD <- tidy_flood_plot$created_at

tidy_rain_plot <- tidy_colorado[!is.na(tidy_colorado$RAIN),]
Date_RAIN <- tidy_rain_plot$created_at

tidy_thunderstorm_plot <- tidy_colorado[!is.na(tidy_colorado$THUNDERSTORM),]
Date_THUNDERSTORM <- tidy_thunderstorm_plot$created_at

tidy_snow_plot <- tidy_colorado[!is.na(tidy_colorado$SNOW),]
Date_SNOW <- tidy_snow_plot$created_at

tidy_water_plot <- tidy_colorado[!is.na(tidy_colorado$WATER),]
Date_WATER <- tidy_water_plot$created_at

tidy_storm_plot <- tidy_colorado[!is.na(tidy_colorado$STORM),]
Date_STORM <- tidy_storm_plot$created_at

# Plotting temporal distribution of most frequent words

mfw <- ggplot() + 
  geom_freqpoly(data = tidy_flash_plot, aes(x = Date_FLASH, y = ..count../sum(..count..), color = "Flash Flood")) +
  geom_freqpoly(data = tidy_rain_plot, aes(x = Date_RAIN, y = ..count../sum(..count..), color = "Rain")) + 
  geom_freqpoly(data = tidy_thunderstorm_plot, aes(x = Date_THUNDERSTORM, y = ..count../sum(..count..), color = "Thunderstorm")) +
  geom_freqpoly(data = tidy_snow_plot, aes(x = Date_SNOW, y = ..count../sum(..count..), color = "Snow")) + 
  geom_freqpoly(data = tidy_water_plot, aes(x = Date_WATER, y = ..count../sum(..count..), color = "Water")) +
  geom_freqpoly(data = tidy_storm_plot, aes(x = Date_STORM, y = ..count../sum(..count..), color = "Storm")) +
  scale_color_manual("", values = c("#ae017e","#9ebcda", "#f7fbff","#a6d96a","#ffff33","#253494")) +
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
        legend.position = c(0.85, 0.7),
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

