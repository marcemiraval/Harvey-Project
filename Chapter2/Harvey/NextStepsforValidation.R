ani <- autoplot(basemap) +
  geom_point(data = allReports, 
             aes(x = lon, 
                 y = lat, 
                 colour = source,
                 size = source,
                 alpha = .7)) +
  scale_color_manual(name = "", # No title in the legend
                    values = c("Validated NWS" = "#7fbf7b",
                               "Twitter" = "#af8dc3")) +
  labs(title = "{(current_frame)}",
       caption = "Source: @marcemiraval 2019") + 
  xlab("") +
  ylab("") +
  theme(legend.title = element_blank(),
        plot.caption = element_text(size = 8)) +
  transition_manual(date, cumulative = TRUE) +
  exit_fade() 

ani



# 
# # This is not working
image <- animate(animm)
library(magick)
image_write(ani, 'Harvey/Outputs/ani.gif')


############

FEMADamage<- st_read("Data/FEMADamage/FEMADamage.shp")

st_crs(FEMADamage)

ggplot() +
  geom_point(data = FEMADamage, 
             aes(x = LONGITUDE,
                 y = LATITUDE,
                 color = DMG_LEVEL))
