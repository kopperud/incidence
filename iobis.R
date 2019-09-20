library(vegan)
library(RcppCNPy)
library(readr)
library(rjson)
library(ggplot2)
library(tibble)

setwd("~/incidence")

grab <- function(var, metadata){
  res <- lapply(metadata, function(x) sapply(x[[var]], function(e) e[[1]]))
  return(res)
} 



################ Same for OBIS
fpaths <- Sys.glob("iobis_species/incidence*.npy")
fpaths <- fpaths[order(parse_number(fpaths))] ## reorder them
incidence <- lapply(fpaths, npyLoad)
filename <- "iobis_species/incidence_metadata.json"
metadata <- fromJSON(readChar(filename, file.info(filename)$size), simplify = FALSE)


taxa <- grab("taxa", metadata)
columns <- 1:60 ## No toponyms here, just 60 bins
mid_lats <- unlist(grab("mid_lat", metadata))

## add in rownames, column names
for (i in seq_along(incidence)){
  if(is.matrix(incidence[[i]])){
    #print(i)
    dimnames(incidence[[i]]) <- list(1:36, taxa[[i]]) 
    
  }
}


richness <- t(sapply(incidence, specpool))
## This "specpool" returns a very silly object
richness1 <- cbind(richness, "mid_lat" = mid_lats)[-c(1:3, 30, 59:60),]
df <- tibble("dummy" = 1:54)
for (i in seq_along(colnames(richness1))){
  df[colnames(richness1)[i]] <- unlist(richness1[,i])
}

df
df$chao_upper <- df$chao + df$chao.se*1.96 # No idea what distribution these estimators have
df$chao_lower <- df$chao - df$chao.se*1.96
df$jack1_upper <- df$jack1 + df$jack1.se*1.96
df$jack1_lower <- df$jack1 - df$jack1.se*1.96

write.csv2(df, "output/obis.csv")


