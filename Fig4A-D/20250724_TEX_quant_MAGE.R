
####################################################################################################
## Celine Roelse                                                                                  ##
## 2024-10-21                                                                                     ##
## Bridge characterization - panel KIF23 or TEX14 combined with MAGEA3 and DDX4                   ##
####################################################################################################

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
p1 <- read_excel("20250318_7B.1_TEX_quant_tidy_MAGE.xlsx")                   
p1 <- as.data.frame(p1)


p1$age <- factor(p1$age, levels = c("WG15", "WG16", "WG17", "WG18", "WG19", "WG20", "WG21", "Prepub", "Prepub2", "Adult", "Adult2"))

longestlen.sum <- summarySE(p1, measurevar='longestlen_cyst', groupvars = 'age', na.rm = T)

longestlen_plot_mean <- ggplot(p1, aes(y=longestlen_cyst, x=age)) +
  geom_jitter(width=0.3) +
  ylab("Longest length per cyst") +
  ylim (0, max(p1$longestlen_cyst))+
  geom_crossbar(data=longestlen.sum, aes(ymin = longestlen_cyst, ymax = longestlen_cyst),
                linewidth=0.5,col="red", width = .5)+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=rel(1.4)), 
        axis.text.x = element_text(size=rel(1.5), vjust = 0.5), 
        axis.text.y = element_text(size=rel(1.5), hjust = 0.5))

longestlen_plot_mean

ggsave("./Plots_mage/20250331_longestlength_mean.pdf", width=9, height=3.5)
ggsave("./Plots_mage/20250331_longestlength_mean.png", width=9, height=3.5)


p1 <- p1 %>% mutate(binlonglen = cut(longestlen_cyst, breaks=c(0, 2, 5, 10, 20)))

longlenbins_age <- prop.table(table(p1$age, p1$binlonglen), 1)
longlenbins_age <- as.data.frame(longlenbins_age)


longlenbinsplot <- ggplot(longlenbins_age, aes(y=Freq, x=Var1, fill=Var2)) + 
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  geom_text(aes(label = paste0(round(Freq*100), "%")), position = position_stack(vjust=0.5, reverse=TRUE)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=rel(1.4)), 
        axis.text.x = element_text(size=rel(1.5), vjust = 0.5), 
        axis.text.y = element_text(size=rel(1.5), hjust = 0.5)) +
  scale_fill_manual(values=cbPalette[c(1, 2, 3, 4)])

longlenbinsplot

ggsave("./Plots_mage/20250401_longestlenbinned.pdf", width=9, height=3.5)
ggsave("./Plots_mage/20250401_longestlenbinned.png", width=9, height=3.5)


##################################################
## General cyst size

cystsize.sum <- summarySE(p1, measurevar='cystsize', groupvars = 'age')

cystsizeplot_mean <- ggplot(p1, aes(y=cystsize, x=age)) +
  geom_jitter(width=0.3) +
  ylab("Cyst size") +
  ylim (0, max(p1$cystsize))+
  geom_crossbar(data=cystsize.sum, aes(ymin = cystsize, ymax = cystsize),
                linewidth=0.5,col="red", width = .5)+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=rel(1.4)), 
        axis.text.x = element_text(size=rel(1.5), vjust = 0.5), 
        axis.text.y = element_text(size=rel(1.5), hjust = 0.5))

cystsizeplot_mean

ggsave("./Plots_mage/20250331_cystsize_mean.pdf", width=9, height=3.5)
ggsave("./Plots_mage/20250331_cystsize_mean.png", width=9, height=3.5)



p1 <- p1 %>% mutate(bincysize = cut(cystsize, breaks=c(0, 2, 5, 10, 20, 30)))


cystsize_age <- prop.table(table(p1$age, p1$bincysize), 1)
cystsize_age <- as.data.frame(cystsize_age)


cystsizebinsplot <- ggplot(cystsize_age, aes(y=Freq, x=Var1, fill=Var2)) + 
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  geom_text(aes(label = paste0(round(Freq*100), "%")), position = position_stack(vjust=0.5, reverse=TRUE)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=rel(1.4)), 
        axis.text.x = element_text(size=rel(1.5), vjust = 0.5), 
        axis.text.y = element_text(size=rel(1.5), hjust = 0.5)) +
  scale_fill_manual(values=cbPalette[c(1, 2, 3, 4, 5)])

cystsizebinsplot

ggsave("./Plots_mage/20250401_cystsizebinned.pdf", width=9, height=3.5)
ggsave("./Plots_mage/20250401_cystsizebinned.png", width=9, height=3.5)



#############
## General branch index

p1$branchindex <- p1$cystsize / p1$longestlen_cyst

branchindex_plot <- ggplot(p1, aes(y=branchindex, x=age)) +
  geom_jitter(width=0.3) +
  scale_y_continuous(n.breaks = 8) +
  ylab("Branch index") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=rel(1.4)), 
        axis.text.x = element_text(size=rel(1.5), vjust = 0.5), 
        axis.text.y = element_text(size=rel(1.5), hjust = 0.5))

branchindex_plot

ggsave("./Plots_mage/20250331_branchindex.pdf", width=9, height=3.5)
ggsave("./Plots_mage/20250331_branchindex.png", width=9, height=3.5)



p1 <- p1 %>% mutate(binbi = cut(branchindex, breaks=c(0, 1, 1.25, 1.5, 1.75, 2.5)))

bi_age <- prop.table(table(p1$age, p1$binbi), 1)
bi_age <- as.data.frame(bi_age)


branchindexbinsplot <- ggplot(bi_age, aes(y=Freq, x=Var1, fill=Var2)) + 
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  geom_text(aes(label = paste0(round(Freq*100), "%")), position = position_stack(vjust=0.5, reverse=TRUE)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=rel(1.4)), 
        axis.text.x = element_text(size=rel(1.5), vjust = 0.5), 
        axis.text.y = element_text(size=rel(1.5), hjust = 0.5)) +
  scale_fill_manual(values=cbPalette[c(1, 2, 3, 4, 5)])

branchindexbinsplot

ggsave("./Plots_mage/20250401_branchindexbinned.pdf", width=9, height=3.5)
ggsave("./Plots_mage/20250401_branchindexbinned.png", width=9, height=3.5)



########################## Bridgetype - synchronous/asynchronous

p1$homobridge <- with(p1, b00 + b11 + b22 + b33)

p1$heterobridge <- with(p1, b01 + b02 + b03 + b12 + b13 + b23)

homhet <- data.frame("Bridge_cat" = c('homogenbridge', 'heterogenbridge'), 
                            "amount" = c(sum(p1$'homobridge'), 
                                         sum(p1$'heterobridge')))


homhetplot <- ggplot(homhet, aes(y=amount, x="", fill=Bridge_cat)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("All samples")

homhetplot

ggsave("./Plots_mage/20250318_homhetbridgetype.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_homhetbridgetype.png", width=5, height=3.5)


##########

wg15homhet <- data.frame("Bridge_cat" = c('homogenbridge', 'heterogenbridge'), 
                         "amount" = c(sum(WG15$'homobridge'), 
                                      sum(WG15$'heterobridge')))

wg15homhetplot <- ggplot(wg15homhet, aes(y=amount, x="", fill=Bridge_cat)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("WG15")

wg15homhetplot

ggsave("./Plots_mage/20250318_wg15homhetbridgetype.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_wg15homhetbridgetype.png", width=5, height=3.5)


##########

wg16homhet <- data.frame("Bridge_cat" = c('homogenbridge', 'heterogenbridge'), 
                         "amount" = c(sum(WG16$'homobridge'), 
                                      sum(WG16$'heterobridge')))

wg16homhetplot <- ggplot(wg16homhet, aes(y=amount, x="", fill=Bridge_cat)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("WG16")

wg16homhetplot

ggsave("./Plots_mage/20250318_wg16homhetbridgetype.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_wg16homhetbridgetype.png", width=5, height=3.5)


##########

wg17homhet <- data.frame("Bridge_cat" = c('homogenbridge', 'heterogenbridge'), 
                         "amount" = c(sum(WG17$'homobridge'), 
                                      sum(WG17$'heterobridge')))

wg17homhetplot <- ggplot(wg17homhet, aes(y=amount, x="", fill=Bridge_cat)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("WG17")

wg17homhetplot

ggsave("./Plots_mage/20250318_wg17homhetbridgetype.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_wg17homhetbridgetype.png", width=5, height=3.5)


##########

wg18homhet <- data.frame("Bridge_cat" = c('homogenbridge', 'heterogenbridge'), 
                         "amount" = c(sum(WG18$'homobridge'), 
                                      sum(WG18$'heterobridge')))

wg18homhetplot <- ggplot(wg18homhet, aes(y=amount, x="", fill=Bridge_cat)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("WG18")

wg18homhetplot

ggsave("./Plots_mage/20250318_wg18homhetbridgetype.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_wg18homhetbridgetype.png", width=5, height=3.5)


##########

wg19homhet <- data.frame("Bridge_cat" = c('homogenbridge', 'heterogenbridge'), 
                         "amount" = c(sum(WG19$'homobridge'), 
                                      sum(WG19$'heterobridge')))

wg19homhetplot <- ggplot(wg19homhet, aes(y=amount, x="", fill=Bridge_cat)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("WG19")

wg19homhetplot

ggsave("./Plots_mage/20250318_wg19homhetbridgetype.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_wg19homhetbridgetype.png", width=5, height=3.5)

##########

wg20homhet <- data.frame("Bridge_cat" = c('homogenbridge', 'heterogenbridge'), 
                         "amount" = c(sum(WG20$'homobridge'), 
                                      sum(WG20$'heterobridge')))

wg20homhetplot <- ggplot(wg20homhet, aes(y=amount, x="", fill=Bridge_cat)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("WG20")

wg20homhetplot

ggsave("./Plots_mage/20250318_wg20homhetbridgetype.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_wg20homhetbridgetype.png", width=5, height=3.5)


##########

wg21homhet <- data.frame("Bridge_cat" = c('homogenbridge', 'heterogenbridge'), 
                         "amount" = c(sum(WG21$'homobridge'), 
                                      sum(WG21$'heterobridge')))

wg21homhetplot <- ggplot(wg21homhet, aes(y=amount, x="", fill=Bridge_cat)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("WG21")

wg21homhetplot

ggsave("./Plots_mage/20250318_wg21homhetbridgetype.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_wg21homhetbridgetype.png", width=5, height=3.5)


##########

prephomhet <- data.frame("Bridge_cat" = c('homogenbridge', 'heterogenbridge'), 
                         "amount" = c(sum(prepub$'homobridge'), 
                                      sum(prepub$'heterobridge')))

prephomhetplot <- ggplot(prephomhet, aes(y=amount, x="", fill=Bridge_cat)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("Prepub")

prephomhetplot

ggsave("./Plots_mage/20250318_Prepub_homhetbridgetype.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_Prepub_homhetbridgetype.png", width=5, height=3.5)

##########

prep2homhet <- data.frame("Bridge_cat" = c('homogenbridge', 'heterogenbridge'), 
                         "amount" = c(sum(prepub2$'homobridge'), 
                                      sum(prepub2$'heterobridge')))

prep2homhetplot <- ggplot(prep2homhet, aes(y=amount, x="", fill=Bridge_cat)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("Prepub2")

prep2homhetplot

ggsave("./Plots_mage/20250318_Prepub2_homhetbridgetype.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_Prepub2_homhetbridgetype.png", width=5, height=3.5)

##########

adhomhet <- data.frame("Bridge_cat" = c('homogenbridge', 'heterogenbridge'), 
                         "amount" = c(sum(adult$'homobridge'), 
                                      sum(adult$'heterobridge')))

adhomhetplot <- ggplot(adhomhet, aes(y=amount, x="", fill=Bridge_cat)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("Adult")

adhomhetplot

ggsave("./Plots_mage/20250318_Adult_homhetbridgetype.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_Adult_homhetbridgetype.png", width=5, height=3.5)


##########

ad2homhet <- data.frame("Bridge_cat" = c('homogenbridge', 'heterogenbridge'), 
                       "amount" = c(sum(adult2$'homobridge'), 
                                    sum(adult2$'heterobridge')))

ad2homhetplot <- ggplot(ad2homhet, aes(y=amount, x="", fill=Bridge_cat)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("Adult2")

ad2homhetplot

ggsave("./Plots_mage/20250318_Adult2_homhetbridgetype.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_Adult2_homhetbridgetype.png", width=5, height=3.5)


