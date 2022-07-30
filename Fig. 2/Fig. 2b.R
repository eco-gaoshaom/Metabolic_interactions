data<-read.table('order_step.proportion',header = T)
library(gplots)
order<-rownames(x)
step<-colnames(x)
order_n<-as.character(replicate(order,n=length(step)))
step_n<-as.character(t(replicate(step,n=length(order))))
proportion<-as.numeric(x)
sig <- c()

data_n<-data.frame(step_n,order_n,proportion)
data_n$step_n <- factor(data_n$step_n,levels = step)
data_n$order_n <- factor(data_n$order_n,levels = order)
ggplot(data_n)+
  geom_point(aes(step_n,order_n,size=proportion,color=proportion),alpha=1,shape=16)+
  #theme_classic(base_line_size = 1)+
  scale_size_area(max_size = 6)+
  theme_bw()+coord_flip()+
  scale_color_gradient2(high = rgb(56,89,137,max=255),low = rgb(255,255,255,max=255))+
  #scale_color_gradient2(low = rgb(56,89,137,max=255),mid = rgb(255,255,255,max=255),high= rgb(255,59,59,max=255))+
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(hjust = 1,angle = 45,size=10,vjust = 1),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))