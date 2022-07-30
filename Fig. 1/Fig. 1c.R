library(vegan)
library(ggplot2)
popu<-read.table('pop_site.table',header = T)
asv<-read.table('ASV_site.table',header = T)

popu<-decostand(t(popu),method = 'hellinger')
asv<-decostand(t(asv),method = 'hellinger')

popu_dist<-vegdist(popu,method = 'bray')
asv_dist<-vegdist(asv,method = 'bray')

mantel(popu_dist,asv_dist,method = 'pearson')

popu_dist_num<-as.numeric(popu_dist)
asv_dist_num<-as.numeric(asv_dist)

data<-data.frame(popu_dist_num,asv_dist_num)
ggplot(data)+geom_point(aes(x=asv_dist_num, y=popu_dist_num),size=1,color="grey")+
  geom_smooth(method="lm",fill= NA,level=0,aes(asv_dist_num,popu_dist_num))+
  #scale_color_manual(values=colorvalue)+scale_shape_manual(values=shapevalue)+
  #geom_text_repel(aes(Dim1,Dim2,label=rownames(votu),color=factor(Mine)),size=3,box.padding=unit(0.1, "lines"),point.padding=unit(0.1, "lines"),arrow = arrow(length=unit(0, "npc")),force = 1, max.iter = 3e3)+
  theme_bw()+
  labs(x="pop_pop(0.94)",y='asv_asv',fill='')+
  scale_y_continuous(breaks = seq(0,1,0.25),limits=c(0,1))+
  #coord_cartesian(ylim = c(0,1))+
  scale_x_continuous(breaks = seq(0,1,0.25),limits=c(0,1))+
  #coord_cartesian(xlim = c(0,1))+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),axis.line.x = element_line(size = 0.5),
        axis.line.y = element_line(size=0.5),axis.text.x = element_text(size=18,color = "black"),
        axis.text.y = element_text(size=18,color="black"),axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),legend.text = element_text(size=18))