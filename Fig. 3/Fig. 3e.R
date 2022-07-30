data<-read.table('step.aveaai_relavar',header = T)
library(ggplot2)
library(reshape2)
data2 <-melt(data,id.vars = "path")
data2$path <- factor(data2$path,levels = data$path)
ggplot(data2)+geom_point(aes(path,value,color=variable,group=variable),size=2,shape=16)+
  geom_line(aes(path,value,color=variable,group=variable))+
  scale_color_manual(values = c(rgb(255,59,59,max=255),rgb(129,184,223,max=255)))+
  labs(x="Phyla",y='Abundance variability/Average AAI',fill='')+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,angle = 0,size=10,colour = 'black'),
        axis.text.y=element_text(size=15,angle = 0,hjust=1,colour = 'black'),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+
  scale_y_continuous(breaks = seq(0,3.5,0.5))+theme(legend.position = 'none')+
  coord_flip()