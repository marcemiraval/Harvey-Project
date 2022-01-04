
create_histo = function(InputFile, HistoColor = NA, HistoBinWidth, HistoName, SavePath){
  gen_hist <- ggplot(InputFile, aes(x = InputFile$created_at,  fill = InputFile$event_stage)) +
  geom_histogram(aes(y = ..count..), colour = HistoColor, binwidth = HistoBinWidth, 
                 position="identity", alpha =0.6) +
  scale_x_datetime(name = "", breaks = date_breaks("12 hour"),
                   labels = date_format("%m/%d %H:%M", tz = "America/Toronto"), expand = c(0.01,0),
                   limits = c(
                     as.POSIXct(min_datetime - 7200, tz ="UTC"),
                     as.POSIXct(max_datetime, tz ="UTC")),
                   timezone = "UTC") +
  scale_y_continuous(name = "Count") + ggtitle("") +
    theme(axis.line = element_line(size=0.5, colour = "black"),
        plot.title=element_text(size = 15, family="Trebuchet MS", face = "bold"),
        text=element_text(size = 12, family="Trebuchet MS"),
        axis.text.x=element_text(colour="#636363", size = 8),
        axis.text.y=element_text(colour="#636363", size = 8),
        legend.position = c(1, 0.7), legend.justification = c(1, 0))+ labs(fill="")+
    scale_fill_manual(values = c("#fed976", "#d53e4f","#fc8d59", "#3288bd","#fee08b","#762a83"),
                      limits = c("Pre-Calm", "Pre-Peak", "Peak", "Post-Peak", "Post-Calm"),
                      breaks = c("Pre-Calm", "Pre-Peak", "Peak", "Post-Peak", "Post-Calm")) +
    labs(fill="")

  ggsave(filename = HistoName, path = SavePath, plot = gen_hist, device = "jpeg",width = 40, height = 20,
       units = "cm")
}


