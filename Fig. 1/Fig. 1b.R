library(ggplot2)
library(reshape2)
data<-read.table('site.binnum_relabun',header = T)
data <- data[,-1]
data <- melt(data,id.vars = c(data$binnum))

data$variable <- factor(data$variable,levels=c("binnum","rela"))
ggplot(data)+
  geom_boxplot(aes(x=variable,y=value,color=variable), width=0.5,position = position_dodge(1),outlier.shape = NA)+
  #geom_point(aes(x=variable,y=value,color=variable))+
  geom_jitter(aes(x=variable,y=value,color=variable),shape = 16,width = 0.2)+
  scale_color_manual(values=c(rgb(255,59,59,max=255),rgb(14,96,107,max=255)
  ))+
  labs(x=" ",y="Number of genomes (x 2000)/Relative abundance of genomes")+
  scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.2))+
  theme_bw()+
  theme(
    legend.position = "right",
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.margin=margin(0,0,0,0,"mm"),
    axis.text.x=element_text(size=rel(1),face="bold"),
    axis.text.y = element_text(size=rel(1),face="bold"),
    axis.title.x = element_text(size=rel(1),face="bold"),
    axis.title.y = element_text(size=rel(1),face="bold"),
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    legend.text=element_text(size=rel(1.1)),
    panel.grid = element_blank()
  )