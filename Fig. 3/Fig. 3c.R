library(ggplot2)
library(reshape2)
data <- read.table('step_site.rela',header = T)
colnames(data) <- c("path",c(1:90))
dataf <- melt(data,id.vars = c("path"))
dataf$path <- factor(dataf$path,levels = rev(data$path))
dataf$path <- as.factor(dataf$path)
dataf$variable <- as.numeric(dataf$variable)
ggplot(dataf) + geom_area(aes(variable,value,fill = path),alpha = 0.7,position = "stack")+
  scale_fill_manual(values=c(rgb(81,68,154,max=255),rgb(54,81,161,max=255),rgb(145,25,52,max=255),
                             rgb(2,40,86,max=255),rgb(0,167,64,max=255),rgb(204,255,51,max=255),
                             rgb(255,255,25,max=255),rgb(255,37,0,max=255),rgb(0,255,255,max=255),
                             rgb(195,19,27,max=255),rgb(224,110,18,max=255),rgb(80,15,26,max=255),
                             rgb(13,67,146,max=255),rgb(154,153,183,max=255),rgb(85,183,150,max=255),
                             rgb(242,204,142,max=255),rgb(255,194,75,max=255),rgb(129,184,223,max=255),
                             rgb(6,223,6,max=255),rgb(15,153,183,max=255),rgb(246,111,105,max=255),
                             rgb(250,192,15,max=255),rgb(255,59,59,max=255),rgb(14,96,107,max=255),
                             rgb(252,140,90,max=255),rgb(92,35,102,max=255),rgb(144,201,230,max=255),
                             rgb(130,178,154,max=255),rgb(78,171,144,max=255),rgb(21,151,165,max=255)
  ))+
  labs(x="Sites",y="Culmulative relative abundance")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
        axis.text.x = element_text(hjust =0.5,size=12,angle = 0, colour = 'black'),
        axis.text.y=element_text(size=12,colour = 'black'),
        axis.line = element_line(size=0.5,colour="black"),
        legend.text = element_text(size=6),legend.position = "bottom")+
  scale_x_continuous(limits = c(0,90),breaks=seq(0,90,15))+
  scale_y_continuous(limits = c(0,12),breaks=seq(0,12,3))