library(ggplot2)
library(tidyverse)

# Defining working directory
setwd ("Chapter1/Colorado/ColoradoState")

# Data to create basemap
coloradoPol <- readRDS(file = "Outputs/colorado_state_poly.rds") # Colorado State Polygon with counties subdivision
colo_state_tweets <- read_csv("Outputs/Colorado_State_Subset.csv") # Tweets sent from Colorado state

ReportsInColorado <- ggplot(data = coloradoPol) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="grey20", colour="grey35", alpha=1)+
  geom_point(data = colo_state_tweets, aes(x=x, y=y), color="Purple", alpha = 1 / 8, size=0.5) +
  theme(legend.position = "none") +
  coord_equal(ratio=1) +
  labs(x="", y="", title="")+
  coord_equal(ratio=1) +
  annotate(geom = "text", 
           x=getSpPPolygonsLabptSlots(coloradoPol)[,1], 
           y=getSpPPolygonsLabptSlots(coloradoPol)[,2],
           label = coloradoPol$NAME,
           size = 1.2,
           color = "white") +
  theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),# get rid of x and y ticks
        axis.text.y = element_blank(),
        axis.text.x = element_blank(), legend.position = "none", 
        panel.background = element_rect(fill = "grey12", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

ReportsInColorado

ggsave(filename = "ColoradoStateMap", plot = ReportsInColorado, device = "png", 
       path = "Outputs", width=5, height=6, units="in", dpi = 300, bg = "Black")
