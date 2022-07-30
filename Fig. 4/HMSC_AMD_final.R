library(Hmsc)
library(tidyverse)

# ---- read data ----
bin_abun <- read_tsv("410bin_site.abun")
env <- read_tsv("90site.chem_climate_loca")

YData <- bin_abun %>%
  column_to_rownames(var = "BinID") %>%
  t()

# reduce env dimension
env_pca <- env %>%
  column_to_rownames(var = "SampleID") %>%
  select(pH:MAP) %>%
  rda(., scale = T)

XData <- as.data.frame(scores(env_pca, display = "sites", choices = 1:7, scaling = 1))
XData$lng <- env$lng
XData$lat <- env$lat

XFormula = ~.

studyDesign <- data.frame(sample = as_factor(rownames(YData)), 
                          plot = as_factor(str_replace(rownames(YData), pattern = "\\d.*", "")))

rL.sample = HmscRandomLevel(units = levels(studyDesign$sample))
rL.plot = HmscRandomLevel(units = levels(studyDesign$plot))

m6 = Hmsc(Y = YData, XData = XData, XFormula = XFormula, studyDesign = studyDesign,
          ranLevels = list(sample = rL.sample),
          distr = "lognormal")

m6 <- sampleMcmc(m6, thin = 10, samples = 500, transient = 5000,
                 nChains = 10, nParallel = 10, verbose = 0)

save(m6, file = "m6.RData")

m6post = convertToCodaObject(m6)
par(mfrow=c(2,2))
ess.beta = effectiveSize(m6post$Beta)
psrf.beta = gelman.diag(m6post$Beta, multivariate=FALSE)$psrf

sppairs = matrix(sample(x = 1:410^2, size = 100))
tmp = m6post$Omega[[1]]
for (chain in 1:length(tmp)){
  tmp[[chain]] = tmp[[chain]][,sppairs]
}
ess.omega = effectiveSize(tmp)
psrf.omega = gelman.diag(tmp, multivariate=FALSE)$psrf

# Model fit and variance partitioning
preds = computePredictedValues(m6)
MF = evaluateModelFit(hM=m6, predY=preds)
hist(MF$SR2, xlim = c(0,1), main=paste0("Mean = ", round(mean(MF$SR2),2)))

VP = computeVariancePartitioning(m6, group = c(1,1,1,1,1,1,1,1, 2, 2), groupnames=c("env", "spat"))
plotVariancePartitioning(m6, VP = VP)

tmp <- as.data.frame(t(VP$vals))
tmp$R2 <- MF$SR2
write.table(tmp, "m6_vp.tsv", sep = "\t", quote = F)


OmegaCor = computeAssociations(m6)
supportLevel = 0.95
toPlot = ((OmegaCor[[1]]$support>supportLevel)
          + (OmegaCor[[1]]$support<(1-supportLevel))>0)*OmegaCor[[1]]$mean
corrplot(toPlot, method = "color",
         col=colorRampPalette(c("blue","white","red"))(200),
         tl.cex=.6, tl.col="black",
         title=paste("random effect level:", m6$rLNames[1]), mar=c(0,0,1,0))

write.table(toPlot, "m6_OmegaCor_sample.tsv", sep = "\t")
