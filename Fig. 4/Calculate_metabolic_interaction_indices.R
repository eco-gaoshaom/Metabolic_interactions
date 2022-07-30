library(XML)
library(methods)
library(xml2)
library(RevEcoR)
library(vegan)
xml_name <- read.table("2625.xmlname")#name of 2,625 xml file produced by CarveMe software
convert_sbml_to_edgeList <- function(xml_file){
  if(!require(xml2)) stop("Please install xml2 package!")
  xml_ns_strip(xml_file)
  reactions <- as_list(xml_find_all(xml_file, "//reaction"))
  res <- lapply(reactions, function(reac){
    reactants <- sapply(reac$listOfReactants, function(x) attr(x, "species"))
    products <- sapply(reac$listOfProducts, function(x) attr(x, "species"))
    
    rev <- attr(reac, "reversible")
    
    r2p <- data.frame(source = rep(reactants, each = length(products)),
                      target = rep(products, times = length(reactants)))
    p2r <- data.frame(source = rep(products, each = length(reactants)),
                      target = rep(reactants, times = length(products)))
    
    res <- if(rev == "false") r2p else rbind(r2p, p2r)
    return(res)
  })
  final_res <- do.call(rbind, res)
  return(final_res)
}

xmlfile = list()
xml_res = list()
xml_res_2 = list()
xml_net = list()

for (i in 1:2625){
  print(i)
  xmlfile[[i]] <- read_xml(as.character(xml_name$V1[i]))
  xml_res[[i]] <- convert_sbml_to_edgeList(xmlfile[[i]])
  xml_res_2[[i]] <- cbind(rownames(xml_res[[i]]),xml_res[[i]][,1:2])
  colnames(xml_res_2[[i]]) <- c(".attrs.name","substrate.name","product.name")
  rownames(xml_res_2[[i]]) <- c(1:nrow(xml_res_2[[i]]))
  xml_net[[i]] <- reconstructGsMN(xml_res_2[[i]], RefData = RefDbCache) 
}

save.image("1-2625.xml_net.RData")


interactions <- calculateCooperationIndex(xml_net)
compet_index <- interactions$competition.index
compel_index <- interactions$complementarity.index

pet_mean <- (compet_index + t(compet_index))/2
pel_mean <- (compel_index + t(compel_index))/2

write.table(pet_mean,"2625.competition.mean.index",sep="\t")
write.table(pel_mean,"2625.complementarity.mean.index",sep="\t")