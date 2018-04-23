library(ggplot2)
library(gganimate)

setwd('/home/marcela/Coding/EWE-reporting/Colorado')
Colorado=read.csv("Colorado_Clean.csv")
Colorado<-Colorado[!(Colorado$longitude<=-145 | Colorado$latitude>=50 | Colorado$latitude<=28),]
Colorado$date=as.Date(Colorado$created_at)
Colorado$date_time=as.POSIXct(Colorado$created_at, format="%Y-%m-%d %H")

p <-ggplot(Colorado, aes(x=longitude, y=latitude, color = "firebrick4",frame=date))+
  borders("state",fill="#252525",size=0.1)+
  geom_point(size=0.2)+
  coord_map("lambert", lat0=20, lat=50)+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_rect(fill="black"),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_rect(fill="black"),
        plot.title = element_text(family="Garamond", hjust=0.5,lineheight=.8, 
                                  colour = "white",size=16))

gganimate(p,ani.width=1200, ani.height=800,interval=0.2,"Colorado.gif", title_frame = TRUE)  

