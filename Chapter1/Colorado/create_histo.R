
create_histo = function(InputFile, HistoColor = NA, HistoBinWidth, HistoName, SavePath){
  gen_hist <- ggplot(InputFile, aes(x = InputFile$date,  fill = InputFile$flood_stage)) +
  geom_histogram(aes(y = ..count..), colour = HistoColor, binwidth = HistoBinWidth, 
                 position="identity", alpha =0.9) +
  scale_x_datetime(name = "Date", breaks = date_breaks("2 day"),
                   labels = date_format("%m/%d"), expand = c(0.01,0),
                   limits = c(
                     as.POSIXct(min_datetime),
                     as.POSIXct(max_datetime)
                   )) + scale_y_continuous(name = "Count") +
  ggtitle("") +
  theme(axis.line = element_line(size=0.5, colour = "black"),
        plot.title=element_text(size = 15, face = "bold"),
        text=element_text(size = 12),
        axis.text.x=element_text(colour="#636363", size = 10),
        axis.text.y=element_text(colour="#636363", size = 10),
        legend.position = c(1, 0.7), legend.justification = c(1, 0))+
  scale_fill_manual(values = c("#2ca25f", "#d53e4f","#fc8d59", "#3288bd","#fee08b"), 
                    limits = c("Pre_flood", "Flood", "Immediate_Aftermath", "Post_Flood"),
                    breaks = c("Pre_flood", "Flood", "Immediate_Aftermath", "Post_Flood"))+
  labs(fill="") 

  ggsave(filename = HistoName, path = SavePath, plot = gen_hist, device = "png",width = 40, height = 20,
       units = "cm")
}


