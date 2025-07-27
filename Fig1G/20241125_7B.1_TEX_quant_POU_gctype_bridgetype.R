
###########################################################################################
## Celine Roelse                                                                         ##
## 2024-11-25                                                                            ##
## Bridge characterization - panel TEX14 combined with POU5F1 and DDX4                   ##
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


WG19 <- subset(p2, age=="WG19")

bridgetypesum_19 <- data.frame("Bridge_category" = c('4_4', '4_5', '4_6', '4_7', '5_5', '5_6', '5_7', '6_6','6_7', '7_7'), 
                               "amount" = c(sum(WG19$'4_4'), sum(WG19$'4_5'), sum(WG19$'4_6'), sum(WG19$'4_7'), sum(WG19$'5_5'), 
                                            sum(WG19$'5_6'), sum(WG19$'5_7'), sum(WG19$'6_6'), sum(WG19$'6_7'), sum(WG19$'7_7')))

bridgetypeplot_19 <- ggplot(bridgetypesum_19, aes(y=amount, x="", fill=Bridge_category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(as.integer(amount/sum(amount)*100), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("WG19")

bridgetypeplot_19

ggsave("./Plots_pou/20241202_bridgetype_WG19_pou.pdf", width=5, height=3.5)
ggsave("./Plots_pou/20241202_bridgetype_WG19_pou.png", width=5, height=3.5)


################

WG20 <- subset(p2, age=="WG20")

bridgetypesum_20 <- data.frame("Bridge_category" = c('4_4', '4_5', '4_6', '4_7', '5_5', '5_6', '5_7', '6_6','6_7', '7_7'), 
                               "amount" = c(sum(WG20$'4_4'), sum(WG20$'4_5'), sum(WG20$'4_6'), sum(WG20$'4_7'), sum(WG20$'5_5'), 
                                            sum(WG20$'5_6'), sum(WG20$'5_7'), sum(WG20$'6_6'), sum(WG20$'6_7'), sum(WG20$'7_7')))

bridgetypeplot_20 <- ggplot(bridgetypesum_20, aes(y=amount, x="", fill=Bridge_category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(as.integer(amount/sum(amount)*100), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("WG20")

bridgetypeplot_20

ggsave("./Plots_pou/20241202_bridgetype_WG20_pou.pdf", width=5, height=3.5)
ggsave("./Plots_pou/20241202_bridgetype_WG20_pou.png", width=5, height=3.5)


##################################################

GCtypesum_19 <- data.frame("GCtype" = c('4', '5', '6', '7'), 
                        "amount" = c(sum(WG19$'n4'), sum(WG19$'n5'), sum(WG19$'n6'), sum(WG19$'n7')))

GCtypeplot_19 <- ggplot(GCtypesum_19, aes(y=amount, x="", fill=GCtype)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Set2") +
  ggtitle("WG19") +
  labs(fill="GC type")

GCtypeplot_19

ggsave("./Plots_pou/20241202_GCtype_WG19_pou.pdf", width=5, height=3.5)
ggsave("./Plots_pou/20241202_GCtype_WG19_pou.png", width=5, height=3.5)


################

GCtypesum_20 <- data.frame("GCtype" = c('4', '5', '6', '7'), 
                           "amount" = c(sum(WG20$'n4'), sum(WG20$'n5'), sum(WG20$'n6'), sum(WG20$'n7')))

GCtypeplot_20 <- ggplot(GCtypesum_20, aes(y=amount, x="", fill=GCtype)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Set2") +
  ggtitle("WG20") +
  labs(fill="GC type")

GCtypeplot_20

ggsave("./Plots_pou/20241202_GCtype_WG20_pou.pdf", width=5, height=3.5)
ggsave("./Plots_pou/20241202_GCtype_WG20_pou.png", width=5, height=3.5)

