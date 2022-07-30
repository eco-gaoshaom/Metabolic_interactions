#pathnum_binnum_barplot
data<-read.table('element.pathnum_binnum',header = T)
data2<-melt(data,id.vars = c("pathnum"))
ggplot(data2,aes(pathnum,value,fill=variable))+
  geom_bar(stat = 'identity',position = 'dodge',width =0.7)+
  scale_fill_manual(values = c(rgb(255,59,59,max=255),
                               rgb(15,153,183,max=255),rgb(6,223,6,max=255),
                               rgb(250,192,15,max=255),rgb(242,204,142,max=255)))+
  labs(x="Number of pathways",y='Number of bins')+
  theme_bw()+
  scale_x_continuous(breaks = seq(0,8,1))+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 0.5,size=15,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),panel.border = element_blank(),
        axis.line = element_line(size=1,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')