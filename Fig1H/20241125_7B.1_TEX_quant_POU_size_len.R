
###########################################################################################
## Celine Roelse                                                                         ##
## 2024-11-25                                                                            ##
## Cyst characterization - panel TEX14 combined with POU5F1 and DDX4                   ##
###########################################################################################

library(ggplot2)
library(ggthemes)
library(readxl)
library(Rmisc)
library(lattice)
library(plyr)
library(ggpubr)
library(dplyr)
library(ggsignif)
library(xlsx)


## Specify color-blind-compatible palette:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## Set working directory
setwd("D:/R-projects/7B.1_bridges")

## Read data files
p2 <- read_excel("20241125_7B.1_TEX_quant_tidy_POU.xlsx")                   
p2 <- as.data.frame(p2)


longestlen.sum <- summarySE(p2, measurevar='longestlen_cyst', groupvars = 'age', na.rm = T)

longestlen_plot_mean <- ggplot(p2, aes(y=longestlen_cyst, x=age)) +
  geom_jitter(width=0.3) +
  ylab("Longest length per cyst") +
  geom_crossbar(data=longestlen.sum, aes(ymin = longestlen_cyst, ymax = longestlen_cyst),
                linewidth=0.5,col="red", width = .5)+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=rel(1.4)), 
        axis.text.x = element_text(size=rel(1.5), vjust = 0.5), 
        axis.text.y = element_text(size=rel(1.5), hjust = 0.5))

longestlen_plot_mean

ggsave("./Plots_pou/20241125_longestlength_mean_pou.pdf", width=3, height=3.5)
ggsave("./Plots_pou/20241125_longestlength_mean_pou.png", width=3, height=3.5)


################


cystsize.sum <- summarySE(p2, measurevar='cystsize', groupvars = 'age')

cystsizeplot_mean <- ggplot(p2, aes(y=cystsize, x=age)) +
  geom_jitter(width=0.3) +
  ylab("Cyst size") +
  geom_crossbar(data=cystsize.sum, aes(ymin = cystsize, ymax = cystsize),
                size=0.5,col="red", width = .5)+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=rel(1.4)), 
        axis.text.x = element_text(size=rel(1.5), vjust = 0.5), 
        axis.text.y = element_text(size=rel(1.5), hjust = 0.5))

cystsizeplot_mean

ggsave("./Plots_pou/20241202_cystsize_mean_pou.pdf", width=3, height=3.5)
ggsave("./Plots_pou/20241202_cystsize_mean_pou.png", width=3, height=3.5)


