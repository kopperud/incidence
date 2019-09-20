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

## The sampling units are text-mined toponyms (e.g. "Weddell Sea"). I excluded all toponyms that were bigger than 0.66*(earth_radius)^2
## Load the incidence matrices. One incidence matrix for each latitudinal bin (3 degree differences)
fpaths <- Sys.glob("tdm_species/incidence*.npy")
fpaths <- fpaths[order(parse_number(fpaths))] ## reorder them
incidence <- lapply(fpaths, npyLoad)

## Metadata, the taxa names and toponym names
filename <- "tdm_species/incidence_metadata.json"
s <- readChar(filename, file.info(filename)$size)
metadata <- fromJSON(s, simplify = FALSE)


taxa <- grab("taxa", metadata)
toponyms <- grab("toponyms", metadata)
mid_lats <- unlist(grab("mid_lat", metadata))

## add in rownames, column names
for (i in seq_along(incidence)){
  #print(i)
  dimnames(incidence[[i]]) <- list(toponyms[[i]], taxa[[i]])
}



richness <- t(sapply(incidence, specpool))
## This "specpool" returns a very silly object
richness1 <- cbind(richness, "mid_lat" = mid_lats)[-c(1:3, 59:60),]
df <- tibble("dummy" = 1:55)
for (i in seq_along(colnames(richness1))){
  df[colnames(richness1)[i]] <- unlist(richness1[,i])
}


df$chao_upper <- df$chao + df$chao.se*1.96 # No idea what distribution these estimators have
df$chao_lower <- df$chao - df$chao.se*1.96
df$jack1_upper <- df$jack1 + df$jack1.se*1.96
df$jack1_lower <- df$jack1 - df$jack1.se*1.96


write.csv2(df, "output/tdm.csv")

