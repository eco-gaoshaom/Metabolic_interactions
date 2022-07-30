data <- read.table("2625pop_site.rela",header = T)
rownames(data) <- rownames(r)
data<-t(data)

pet_mean <- read.table("2625.competition.mean.index",header = T)
pel_mean <- read.table("2625.complementarity.mean.index",header = T)
colnames(pet_mean) <- rownames(pet_mean)
colnames(pel_mean) <- rownames(pel_mean)

#site specific correlations between co-occurrence score and metabolic interaction indices
site_mag <- list()
site_mag_num <- c()
site_petm <- list()
site_pelm <- list()
site_r <- list()

site_petm.upper <- list()
site_pelm.upper <- list()
site_r.upper <- list()

site_r_petm_cor <- list()
site_r_pelm_cor <- list()

site_r_petm_r <- c()
site_r_petm_p <- c()
site_r_pelm_r <- c()
site_r_pelm_p <- c()

site_mro <- c()
site_mip <- c()

for (i in 1:90){
  print(i)
  site_mag[[i]] <- c(rownames(t(data))[t(data)[,i]>0])
  #site_seed_overlap[[i]] <- Reduce(intersect,seed_list[site_mag[[i]]])
  site_mag_num[i] <- length(site_mag[[i]])
  site_petm[[i]] <- as.matrix(pet_mean[site_mag[[i]],site_mag[[i]]])
  site_pelm[[i]] <- as.matrix(pel_mean[site_mag[[i]],site_mag[[i]]])
  site_r[[i]] <- as.matrix(r[site_mag[[i]],site_mag[[i]]])
  site_petm.upper[[i]] <- site_petm[[i]][upper.tri(site_petm[[i]])]
  site_pelm.upper[[i]] <- site_pelm[[i]][upper.tri(site_pelm[[i]])]
  site_r.upper[[i]] <- site_r[[i]][upper.tri(site_r[[i]])]
  site_r_petm_cor[[i]] <- cor.test(site_petm.upper[[i]],site_r.upper[[i]],method='pearson')
  site_r_pelm_cor[[i]] <- cor.test(site_pelm.upper[[i]],site_r.upper[[i]],method='pearson')
  site_r_petm_r[i] <- site_r_petm_cor[[i]]$estimate
  site_r_petm_p[i] <- site_r_petm_cor[[i]]$p.value
  site_r_pelm_r[i] <- site_r_pelm_cor[[i]]$estimate
  site_r_pelm_p[i] <- site_r_pelm_cor[[i]]$p.value
  
  site_mro[i] <- mean(site_petm.upper[[i]])
  site_mip[i] <- mean(site_pelm.upper[[i]])
}

#randomization of the correlations
site_r_petm_ran <- matrix(0,999,90)
site_r_pelm_ran <- matrix(0,999,90)
for (i in 1:90){
  site_mag_ran <- list()
  site_petm_ran <- list()
  site_pelm_ran <- list()
  site_r_ran <- list()
  site_petm_ran.upper <- list()
  site_pelm_ran.upper <- list()
  site_r_ran.upper <- list()
  site_r_petm_cor_ran <- list()
  site_r_pelm_cor_ran <- list()
  site_r_petm_r_ran <- c()
  site_r_petm_p_ran <- c()
  site_r_pelm_r_ran <- c()
  site_r_pelm_p_ran <- c()
  for (j in 1:999){
    site_mag_ran[[j]] <- rownames(t(data))[sample(1:2625,site_mag_num[i])]
    site_petm_ran[[j]] <- as.matrix(pet_mean[site_mag_ran[[j]],site_mag_ran[[j]]])
    site_pelm_ran[[j]] <- as.matrix(pel_mean[site_mag_ran[[j]],site_mag_ran[[j]]])
    site_r_ran[[j]] <- as.matrix(r[site_mag_ran[[j]],site_mag_ran[[j]]])
    site_petm_ran.upper[[j]] <- site_petm_ran[[j]][upper.tri(site_petm_ran[[j]])]
    site_pelm_ran.upper[[j]] <- site_pelm_ran[[j]][upper.tri(site_pelm_ran[[j]])]
    site_r_ran.upper[[j]] <- site_r_ran[[j]][upper.tri(site_r_ran[[j]])]
    site_r_petm_cor_ran[[j]] <- cor.test(site_petm_ran.upper[[j]],site_r_ran.upper[[j]],method='pearson')
    site_r_pelm_cor_ran[[j]] <- cor.test(site_pelm_ran.upper[[j]],site_r_ran.upper[[j]],method='pearson')
    site_r_petm_r_ran[j] <- site_r_petm_cor_ran[[j]]$estimate
    site_r_petm_p_ran[j] <- site_r_petm_cor_ran[[j]]$p.value
    site_r_pelm_r_ran[j] <- site_r_pelm_cor_ran[[j]]$estimate
    site_r_pelm_p_ran[j] <- site_r_pelm_cor_ran[[j]]$p.value
    print((i-1)*90+j)
  }
  site_r_petm_ran[,i] <- site_r_petm_r_ran
  site_r_pelm_ran[,i] <- site_r_pelm_r_ran
}

site_pet_pel_r_cor<-data.frame(site_r_petm_r,site_r_pelm_r,site_r_petm_p,site_r_pelm_p,site_mro,site_mip)
colnames(site_pet_pel_r_cor) <- c("petm_r","pelm_r","petm_r_p","pelm_r_p","site_mean_mro","site_mean_mip")
write.table(site_pet_pel_r_cor,"site_pet_pel_r_cor",sep="\t")

write.table(site_r_petm_ran,"site_r_petm_ran",sep="\t")
write.table(site_r_pelm_ran,"site_r_pelm_ran",sep="\t")
