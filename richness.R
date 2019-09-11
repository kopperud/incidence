library(vegan)
library(RcppCNPy)
library(readr)
library(rjson)
library(ggplot2)

setwd("~/incidence")

## The sampling units are text-mined toponyms (e.g. "Weddell Sea"). I excluded all toponyms that were bigger than 0.66*(earth_radius)^2
## Load the incidence matrices. One incidence matrix for each latitudinal bin (3 degree differences)
fpaths <- Sys.glob("tdm_species/incidence*.npy")
fpaths <- fpaths[order(parse_number(fpaths))] ## reorder them
incidence <- lapply(fpaths, npyLoad)

## Metadata, the taxa names and toponym names
filename <- "tdm_species/incidence_metadata.json"
s <- readChar(filename, file.info(filename)$size)
metadata <- fromJSON(s, simplify = FALSE)

grab <- function(var){
  res <- lapply(metadata, function(x) sapply(x[[var]], function(e) e[[1]]))
  return(res)
} 
taxa <- grab("taxa")
toponyms <- grab("toponyms")
mid_lats <- unlist(grab("mid_lat"))

## add in rownames, column names
for (i in seq_along(incidence)){
  print(i)
  dimnames(incidence[[i]]) <- list(toponyms[[i]], taxa[[i]])
}


richness <- t(sapply(incidence, specpool))
## This "specpool" returns a very silly object
richness1 <- cbind(richness, "mid_lat" = mid_lats)[-c(1:3, 59:60),]
df <- tibble("dummy" = 1:55)
for (i in seq_along(colnames(richness1))){
  df[colnames(richness1)[i]] <- unlist(richness1[,i])
}

df

df$chao_upper <- df$chao + df$chao.se*1.96 # No idea what distribution these estimators have
df$chao_lower <- df$chao - df$chao.se*1.96
df$jack1_upper <- df$jack1 + df$jack1.se*1.96
df$jack1_lower <- df$jack1 - df$jack1.se*1.96

plot(df$mid_lat, df$chao, "l", col = "orange", lwd = 2)
points(df$mid_lat, df$Species, "l", lwd = 2)

## Plot across space

ggplot(df, aes(x = mid_lat)) +
  theme_bw() +
  geom_ribbon(aes(ymin = chao_lower,
                  ymax = chao_upper), fill = "orange", alpha = 0.2) +
  geom_ribbon(aes(ymin = jack1_lower,
                  ymax = jack1_upper), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = Species, color = "black")) +
  geom_line(aes(y = chao, color = "orange")) +
  geom_line(aes(y = jack1, color = "blue")) +
  scale_color_manual(name = "richness estimates", 
                     labels = c("Raw", "Jackknife1", "Chao"),
                     values = c("black", "blue", "orange"))


