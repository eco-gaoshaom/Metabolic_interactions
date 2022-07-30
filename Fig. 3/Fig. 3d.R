path<-read.table('step_site.rela',header = T)
rownames(path) <- path$path
path <- t(path[,-1])
path<-decostand(path,method='hellinger')
distp<-vegdist(path,method = 'bray')
distp_num<-as.numeric(distp)

phylum<-read.table('phyla_site.rela',header = T) 
phylum<-decostand(t(phylum),method='hellinger')
dist_phylum<-vegdist(phylum,method = 'bray')
distphylum_num<-as.numeric(dist_phylum)

order<-read.table('order_site.rela',header = T)
order<-decostand(t(order),method='hellinger')
dist_order<-vegdist(order,method = 'bray')
distorder_num<-as.numeric(dist_order)

pop<-read.table('pop_site.rela',header = T) 
pop<-decostand(t(pop),method='hellinger')
dist_pop<-vegdist(pop,method = 'bray')
distpop_num<-as.numeric(dist_pop)

sim <- data.frame(distp_num,distphylum_num,distorder_num,distpop_num)
library(reshape2)
data <- melt(sim)
data$variable <- factor(data$variable,levels=c("distp_num","distphylum_num","distorder_num","distpop_num"))
ggplot(data)+
  #stat_boxplot(geom = "errorbar",width=0.3,aes(variable,value,color=variable),position = position_dodge(1))+
  geom_boxplot(aes(x=variable,y=value,color=variable),width=0.15,position = position_dodge(1),outlier.shape = NA)+
  geom_violin(aes(x=variable,y=value,fill=variable),alpha=0.5,width=0.9)+
  #geom_jitter(aes(x=variable,y=value,color=variable),shape = 16,width = 0.2,alpha = 0.5)+
  scale_color_manual(values=c(rgb(92,35,102,max=255),rgb(6,223,6,max=255),
                              rgb(255,59,59,max=255),rgb(14,96,107,max=255)
  ))+
  labs(x="Group",y="Similarity")+
  scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.2))+
  theme_bw()+
  theme(
    legend.position = "right",
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.margin=margin(0,0,0,0,"mm"),
    axis.text.x=element_text(size=rel(2),face="bold",angle = 45,hjust = 1),
    axis.text.y = element_text(size=rel(2),face="bold"),
    axis.title.x = element_text(size=rel(1.4),face="bold"),
    axis.title.y = element_text(size=rel(1.4),face="bold"),
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    legend.text=element_text(size=rel(1.1)),
    panel.grid = element_blank()
  )
