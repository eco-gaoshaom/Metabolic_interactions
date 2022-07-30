#calculate cooccurrence-score
data<-read.table('2625pop_site.rela',header = T)
library(vegan)
library(Hmisc)

data<-t(data)
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
write.table(r,"2625.co-occurrence.rscore",sep = "\t")

pet_mean <- read.table("2625.competition.mean.index",header = T)
pel_mean <- read.table("2625.complementarity.mean.index",header = T)

rownames(r) <- colnames(r)
rownames(pet_mean) <- rownames(r)
colnames(pet_mean) <- colnames(r)
rownames(pel_mean) <- rownames(r)
colnames(pel_mean) <- colnames(r)
pet_mean<-as.matrix(pet_mean)
pel_mean<-as.matrix(pel_mean)

mantel(r,pet_mean,method = 'pearson',permutations = 999)
mantel(r,pel_mean,method = 'pearson',permutations = 999)