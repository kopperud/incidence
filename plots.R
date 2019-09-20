library(gridExtra)
library(latex2exp)
library(ggplot2)


setwd("~/incidence")
obis <- read.csv2("output/obis.csv")
tdm <- read.csv2("output/tdm.csv")


## Plot across space
f <- function(df, title){
  p <- ggplot(df, aes(x = mid_lat)) +
    theme_bw() +
    geom_ribbon(aes(ymin = chao_lower,
                    ymax = chao_upper), fill = "orange", alpha = 0.2) +
    geom_ribbon(aes(ymin = jack1_lower,
                    ymax = jack1_upper), fill = "blue", alpha = 0.2) +
    geom_line(aes(y = Species, color = "black")) +
    geom_line(aes(y = chao, color = "orange")) +
    geom_line(aes(y = jack1, color = "blue")) +
    scale_color_manual(name = "richness estimates", 
                       labels = c("Observed", "Jackknife1", "Chao"),
                       values = c("black", "blue", "orange")) +
    ggtitle(title)
}

obis_plot <- f(obis, TeX("OBIS data. Chao1 = $S_{obs} + Q_1^2 /(Q_2)$. Jackknife1 = $S_{obs} + Q_1(m-1)/m$. $Q_1$: singletons. $Q_2$: doubletons. m: sampling units."))
tdm_plot <- f(tdm, TeX("TDM data. Bands are 95% CI, assuming estimation variance is gaussian"))


grid.arrange(obis_plot, tdm_plot)



