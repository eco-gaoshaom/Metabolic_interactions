library(igraph)
sp.ra <- read.table("2625pop_site.rela",header = T)
data<-t(sp.ra)
corr<-rcorr(as.matrix(data),type='pearson')
r <- corr$r
p <- corr$P
p[is.na(p)]<-0


p_adjust<-p.adjust(p,method="BH")
for (i in 1:ncol(p)){
  for (j in 1:nrow(p)){
    p[j,i]=p_adjust[(i-1)*nrow(p)+j]
  }
}

for (i in 1:nrow(r)) {
  for (j in 1:nrow(r)) {
    if (i!=j) {
      if (p[i, j]>=0.05) {
        r[i, j]=0
      }
    }
  }
}
r<-as.matrix(r)

cormatrix <- r

cormatrix[is.na(cormatrix)]<-0
diag(cormatrix)<-0    #no links for self-self    
cormatrix[abs(cormatrix)>0]<-1

site = colnames(sp.ra)

site_magname = list()
site_g <- list()
site_g2 <- list()

site_net_gd <- c()

for (i in 1:90){
  site_magname[[i]] <- c(rownames(sp.ra)[sp.ra[,i]>0])
  site_g[[i]] <- graph_from_adjacency_matrix(as.matrix(cormatrix[site_magname[[i]],site_magname[[i]]]), 
                                             mode="undirected", weighted = NULL, diag = FALSE, add.colnames = NULL)
  iso_node_id = which(degree(site_g[[i]])==0)# remove isolated nodes
  site_g2[[i]] <- delete.vertices(site_g[[i]], iso_node_id)
  site_net_gd[i] <- graph.density(site_g2[[i]])
}


cormatrix2 <- r

cormatrix2[is.na(cormatrix2)]<-0
diag(cormatrix2)<-0    #no links for self-self    

site_cormatrix2 <- list()
site_network.raw = list()
site_sp.ra2 <- list()

for (i in 1:90){
  site_magname[[i]] <- c(rownames(sp.ra)[sp.ra[,i]>0])
  site_cormatrix2[[i]] <- cormatrix2[site_magname[[i]],site_magname[[i]]]
  
  print(sum(abs(site_cormatrix2[[i]])>0)/2)  #this should be the number of links. 
  print(sum(colSums(abs(site_cormatrix2[[i]]))>0))  # node number: number of species with at least one linkage with others.
  
  site_network.raw[[i]] <- site_cormatrix2[[i]][colSums(abs(site_cormatrix2[[i]]))>0,colSums(abs(site_cormatrix2[[i]]))>0]
  site_sp.ra2[[i]] <- sp.ra[site_magname[[i]],]
  
}

rand.remov.once<-function(netRaw, rm.percent, sp.ra, abundance.weighted=T){
  id.rm<-sample(1:nrow(netRaw), round(nrow(netRaw)*rm.percent))
  net.Raw=netRaw #don't want change netRaw
  net.Raw[id.rm,]=0;  net.Raw[,id.rm]=0;   ##remove all the links to these species
  if (abundance.weighted){
    net.stength= net.Raw*sp.ra
  } else {
    net.stength= net.Raw
  }
  
  sp.meanInteration<-colMeans(net.stength)
  
  id.rm2<- which(sp.meanInteration<=0)  ##remove species have negative interaction or no interaction with others
  remain.percent<-(nrow(netRaw)-length(id.rm2))/nrow(netRaw)
  #for simplicity, I only consider the immediate effects of removing the
  #'id.rm' species; not consider the sequential effects of extinction of
  # the 'id.rm2' species.
  
  #you can write out the network pruned
  #  net.Raw[id.rm2,]=0;  net.Raw[,id.rm2]=0;
  # write.csv( net.Raw,"network pruned.csv")
  
  remain.percent
}

rm.p.list=seq(0.05,0.2,by=0.05)
rmsimu<-function(netRaw, rm.p.list, sp.ra, abundance.weighted=T,nperm=100){
  t(sapply(rm.p.list,function(x){
    remains=sapply(1:nperm,function(i){
      rand.remov.once(netRaw=netRaw, rm.percent=x, sp.ra=sp.ra, abundance.weighted=abundance.weighted)
    })
    remain.mean=mean(remains)
    remain.sd=sd(remains)
    remain.se=sd(remains)/(nperm^0.5)
    result<-c(remain.mean,remain.sd,remain.se)
    names(result)<-c("remain.mean","remain.sd","remain.se")
    result
  }))
}


site_Weighted.simu <- list()
site_Unweighted.simu <- list()
site_Weighted.simu.0.5rm <- matrix(0,90,3)
site_Unweighted.simu.0.5rm <- matrix(0,90,3)

for (i in 1:90){
  print(i)
  site_Weighted.simu[[i]] <- rmsimu(netRaw=site_network.raw[[i]], rm.p.list=seq(0.05,1,by=0.05), 
                                    sp.ra=site_sp.ra2[[i]], abundance.weighted=T,nperm=100)
  site_Unweighted.simu[[i]] <- rmsimu(netRaw=site_network.raw[[i]], rm.p.list=seq(0.05,1,by=0.05),
                                      sp.ra=site_sp.ra2[[i]], abundance.weighted=F,nperm=100)
  site_Weighted.simu.0.5rm[i,] <- site_Weighted.simu[[i]][10,]
  site_Unweighted.simu.0.5rm[i,] <- site_Unweighted.simu[[i]][10,]
}

rownames(site_Weighted.simu.0.5rm) <- site
rownames(site_Unweighted.simu.0.5rm) <- site
colnames(site_Weighted.simu.0.5rm) <- c("weighted_mean_robustness","weighted_sd_robustness","weighted_se_robustness")
colnames(site_Unweighted.simu.0.5rm) <- c("unweighted_mean_robustness","unweighted_sd_robustness","unweighted_se_robustness")

result <- data.frame(site_net_gd,site_Weighted.simu.0.5rm,site_Unweighted.simu.0.5rm)
write.table(result,"site.density_robustness",sep="\t")
