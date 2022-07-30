data<-read.table('phyla.binnum_aveQuality',header = T)
library(ggplot2)
data$phylum <- factor(data$phylum,levels = data$phylum)
ggplot(data)+geom_bar(aes(phylum,binnum),stat = 'identity',fill=rgb(220,220,220,max=255),width = 0.5)+
  labs(x="Phyla",y='Number of genomes (x 5)/Quality of MAGs',fill='')+
  geom_point(aes(phylum,com.4con_av),size=2,shape=16,color=rgb(255,59,59,max=255))+
  geom_line(aes(phylum,com.4con_av),color=rgb(255,59,59,max=255),group = 1)+
  #geom_hline(yintercept=50,linetype=4,color="grey",size=1)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
        axis.text.x = element_text(hjust = 1,angle = 45,size=10,colour = 'black'),
        axis.text.y=element_text(size=15,colour = 'black'),
        axis.line = element_line(size=0.5,colour="black"),legend.text = element_text(size=13),
        legend.position = 'right')+
  scale_y_continuous(breaks = seq(0,120,20))+theme(legend.position = 'none')+
  coord_cartesian(ylim = c(0,120))
