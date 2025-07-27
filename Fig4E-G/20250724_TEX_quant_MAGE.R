
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


longestlen_plot <- ggplot(p1, aes(y=longestlen_cyst, x=age, fill=bridgemarker)) +
  geom_jitter(aes(color = bridgemarker), width=0.3) +
  ylab("Longest length per cyst") +
  ylim (0, max(p1$longestlen_cyst))+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=rel(1.4)), 
        axis.text.x = element_text(size=rel(1.5), vjust = 0.5), 
        axis.text.y = element_text(size=rel(1.5), hjust = 0.5), 
        legend.text = element_text(size=rel(1)))

longestlen_plot

ggsave("./Plots_mage/20250331_longestlength.pdf", width=9, height=3.5)
ggsave("./Plots_mage/20250331_longestlength.png", width=9, height=3.5)


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

ggsave("./Plots_mage/20250401_longestlenbinned_CORRECT.pdf", width=9, height=3.5)
ggsave("./Plots_mage/20250401_longestlenbinned_CORRECT.png", width=9, height=3.5)


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

ggsave("./Plots_mage/20250401_cystsizebinned_CORRECT.pdf", width=9, height=3.5)
ggsave("./Plots_mage/20250401_cystsizebinned_CORRECT.png", width=9, height=3.5)


#cystsizebinsplot <- ggplot(p1, aes(y=cystsize, x=age, fill=bincysize)) + 
#  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
#  theme_classic() +
#  theme(plot.title = element_text(hjust = 0.5), 
#        axis.title.x = element_blank(), 
#        axis.title.y = element_text(size=rel(1.4)), 
#        axis.text.x = element_text(size=rel(1.5), vjust = 0.5), 
#        axis.text.y = element_text(size=rel(1.5), hjust = 0.5)) +
#  scale_fill_manual(values=cbPalette[c(1, 2, 3, 4, 5)])

#cystsizebinsplot

#ggsave("./Plots_mage/20250318_cystsizebinned.pdf", width=9, height=3.5)
#ggsave("./Plots_mage/20250318_cystsizebinned.png", width=9, height=3.5)



cystsizeplot_mean <- ggplot(p1, aes(y=cystsize, x=age, color=bincysize)) +
  geom_jitter(width=0.3) +
  ylab("Cyst size") +
  geom_crossbar(data=cystsize.sum, aes(ymin = cystsize, ymax = cystsize),
                linewidth=0.5,col="red", width = .5)+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=rel(1.4)), 
        axis.text.x = element_text(size=rel(1.5), vjust = 0.5), 
        axis.text.y = element_text(size=rel(1.5), hjust = 0.5)) 

cystsizeplot_mean

ggsave("./Plots_mage/20250318_cystsize-colors_mean.pdf", width=9, height=3.5)
ggsave("./Plots_mage/20250318_cystsize-colors_mean.png", width=9, height=3.5)



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

ggsave("./Plots_mage/20250401_branchindexbinned_CORRECT.pdf", width=9, height=3.5)
ggsave("./Plots_mage/20250401_branchindexbinned_CORRECT.png", width=9, height=3.5)


#branchindexbinsplot <- ggplot(p1, aes(y=branchindex, x=age, fill=binbi)) + 
#  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
#  theme_classic() +
#  theme(plot.title = element_text(hjust = 0.5), 
#        axis.title.x = element_blank(), 
#        axis.title.y = element_text(size=rel(1.4)), 
#        axis.text.x = element_text(size=rel(1.5), vjust = 0.5), 
#        axis.text.y = element_text(size=rel(1.5), hjust = 0.5)) +
#  scale_fill_manual(values=cbPalette[c(1, 2, 3, 4, 5)])

#branchindexbinsplot

#ggsave("./Plots_mage/20250318_branchindexbinned.pdf", width=9, height=3.5)
#ggsave("./Plots_mage/20250318_branchindexbinned.png", width=9, height=3.5)



BI.sum <- summarySE(p1, measurevar='branchindex', groupvars = 'age')


branchindexbinsplot_mean <- ggplot(p1, aes(y=branchindex, x=age, color=binbi)) +
  geom_jitter(width=0.3) +
  ylab("Branch index") +
  geom_crossbar(data=BI.sum, aes(ymin = branchindex, ymax = branchindex),
                linewidth=0.5,col="red", width = .5)+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=rel(1.4)), 
        axis.text.x = element_text(size=rel(1.5), vjust = 0.5), 
        axis.text.y = element_text(size=rel(1.5), hjust = 0.5)) 

branchindexbinsplot_mean

ggsave("./Plots_mage/20250318_branchindex-colors_mean.pdf", width=9, height=3.5)
ggsave("./Plots_mage/20250318_branchindex-colors_mean.png", width=9, height=3.5)



############################################## 
## Bridge type (and by age)

bridgetypesum <- data.frame("Bridge_category" = c('b00', 'b01', 'b02', 'b03', 'b11', 'b12', 'b13', 'b22','b23', 'b33'), 
                           "amount" = c(sum(p1$'b00'), sum(p1$'b01'), sum(p1$'b02'), sum(p1$'b03'), sum(p1$'b11'), 
                                        sum(p1$'b12'), sum(p1$'b13'), sum(p1$'b22'), sum(p1$'b23'), sum(p1$'b33')))


bridgetypeplot <- ggplot(bridgetypesum, aes(y=amount, x="", fill=Bridge_category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Set3") +
  ggtitle("All samples")

bridgetypeplot

ggsave("./Plots_mage/20250401_bridgetype_colors.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250401_bridgetype_colors.png", width=5, height=3.5)



###############

WG15 <- subset(p1, age=="WG15")

bridgetypesum_15 <- data.frame("Bridge_category" = c('b00', 'b01', 'b02', 'b03', 'b11', 'b12', 'b13', 'b22','b23', 'b33'), 
                            "amount" = c(sum(WG15$'b00'), sum(WG15$'b01'), sum(WG15$'b02'), sum(WG15$'b03'), sum(WG15$'b11'), 
                                         sum(WG15$'b12'), sum(WG15$'b13'), sum(WG15$'b22'), sum(WG15$'b23'), sum(WG15$'b33')))

bridgetypeplot_15 <- ggplot(bridgetypesum_15, aes(y=amount, x="", fill=Bridge_category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("WG15")

bridgetypeplot_15

ggsave("./Plots_mage/20250116_bridgetype_WG15.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250116_bridgetype_WG15.png", width=5, height=3.5)


###############

WG16 <- subset(p1, age=="WG16")

bridgetypesum_16 <- data.frame("Bridge_category" = c('b00', 'b01', 'b02', 'b03', 'b11', 'b12', 'b13', 'b22','b23', 'b33'), 
                               "amount" = c(sum(WG16$'b00'), sum(WG16$'b01'), sum(WG16$'b02'), sum(WG16$'b03'), sum(WG16$'b11'), 
                                            sum(WG16$'b12'), sum(WG16$'b13'), sum(WG16$'b22'), sum(WG16$'b23'), sum(WG16$'b33')))

bridgetypeplot_16 <- ggplot(bridgetypesum_16, aes(y=amount, x="", fill=Bridge_category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("WG16")

bridgetypeplot_16

ggsave("./Plots_mage/20250210_bridgetype_WG16.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250210_bridgetype_WG16.png", width=5, height=3.5)


###############

WG17 <- subset(p1, age=="WG17")

bridgetypesum_17 <- data.frame("Bridge_category" = c('b00', 'b01', 'b02', 'b03', 'b11', 'b12', 'b13', 'b22','b23', 'b33'), 
                               "amount" = c(sum(WG17$'b00'), sum(WG17$'b01'), sum(WG17$'b02'), sum(WG17$'b03'), sum(WG17$'b11'), 
                                            sum(WG17$'b12'), sum(WG17$'b13'), sum(WG17$'b22'), sum(WG17$'b23'), sum(WG17$'b33')))

bridgetypeplot_17 <- ggplot(bridgetypesum_17, aes(y=amount, x="", fill=Bridge_category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("WG17")

bridgetypeplot_17

ggsave("./Plots_mage/20250116_bridgetype_WG17.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250116_bridgetype_WG17.png", width=5, height=3.5)


###############

WG18 <- subset(p1, age=="WG18")

bridgetypesum_18 <- data.frame("Bridge_category" = c('b00', 'b01', 'b02', 'b03', 'b11', 'b12', 'b13', 'b22','b23', 'b33'), 
                               "amount" = c(sum(WG18$'b00'), sum(WG18$'b01'), sum(WG18$'b02'), sum(WG18$'b03'), sum(WG18$'b11'), 
                                            sum(WG18$'b12'), sum(WG18$'b13'), sum(WG18$'b22'), sum(WG18$'b23'), sum(WG18$'b33')))

bridgetypeplot_18 <- ggplot(bridgetypesum_18, aes(y=amount, x="", fill=Bridge_category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(amount/sum(amount)*100), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("WG18")

bridgetypeplot_18

ggsave("./Plots_mage/20250116_bridgetype_WG18.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250116_bridgetype_WG18.png", width=5, height=3.5)

################

WG19 <- subset(p1, age=="WG19")

bridgetypesum_19 <- data.frame("Bridge_category" = c('b00', 'b01', 'b02', 'b03', 'b11', 'b12', 'b13', 'b22','b23', 'b33'), 
                               "amount" = c(sum(WG19$'b00'), sum(WG19$'b01'), sum(WG19$'b02'), sum(WG19$'b03'), sum(WG19$'b11'), 
                                            sum(WG19$'b12'), sum(WG19$'b13'), sum(WG19$'b22'), sum(WG19$'b23'), sum(WG19$'b33')))

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

ggsave("./Plots_mage/20250116_bridgetype_WG19.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250116_bridgetype_WG19.png", width=5, height=3.5)


################

WG20 <- subset(p1, age=="WG20")

bridgetypesum_20 <- data.frame("Bridge_category" = c('b00', 'b01', 'b02', 'b03', 'b11', 'b12', 'b13', 'b22','b23', 'b33'), 
                               "amount" = c(sum(WG20$'b00'), sum(WG20$'b01'), sum(WG20$'b02'), sum(WG20$'b03'), sum(WG20$'b11'), 
                                            sum(WG20$'b12'), sum(WG20$'b13'), sum(WG20$'b22'), sum(WG20$'b23'), sum(WG20$'b33')))

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

ggsave("./Plots_mage/20250116_bridgetype_WG20.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250116_bridgetype_WG20.png", width=5, height=3.5)


################

WG21 <- subset(p1, age=="WG21")

bridgetypesum_21 <- data.frame("Bridge_category" = c('b00', 'b01', 'b02', 'b03', 'b11', 'b12', 'b13', 'b22','b23', 'b33'), 
                               "amount" = c(sum(WG21$'b00'), sum(WG21$'b01'), sum(WG21$'b02'), sum(WG21$'b03'), sum(WG21$'b11'), 
                                            sum(WG21$'b12'), sum(WG21$'b13'), sum(WG21$'b22'), sum(WG21$'b23'), sum(WG21$'b33')))

bridgetypeplot_21 <- ggplot(bridgetypesum_21, aes(y=amount, x="", fill=Bridge_category)) +
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
  ggtitle("WG21")

bridgetypeplot_21

ggsave("./Plots_mage/20250116_bridgetype_WG21.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250116_bridgetype_WG21.png", width=5, height=3.5)


################

prepub <- subset(p1, age=="Prepub")

bridgetypesum_pre <- data.frame("Bridge_category" = c('b00', 'b01', 'b02', 'b03', 'b11', 'b12', 'b13', 'b22','b23', 'b33'), 
                               "amount" = c(sum(prepub$'b00'), sum(prepub$'b01'), sum(prepub$'b02'), sum(prepub$'b03'), sum(prepub$'b11'), 
                                            sum(prepub$'b12'), sum(prepub$'b13'), sum(prepub$'b22'), sum(prepub$'b23'), sum(prepub$'b33')))

bridgetypeplot_pre <- ggplot(bridgetypesum_pre, aes(y=amount, x="", fill=Bridge_category)) +
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
  ggtitle("Prepubertal")

bridgetypeplot_pre

ggsave("./Plots_mage/20250116_bridgetype_prepub.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250116_bridgetype_prepub.png", width=5, height=3.5)


################

adult <- subset(p1, age=="Adult")

bridgetypesum_ad <- data.frame("Bridge_category" = c('b00', 'b01', 'b02', 'b03', 'b11', 'b12', 'b13', 'b22','b23', 'b33'), 
                                "amount" = c(sum(adult$'b00'), sum(adult$'b01'), sum(adult$'b02'), sum(adult$'b03'), sum(adult$'b11'), 
                                             sum(adult$'b12'), sum(adult$'b13'), sum(adult$'b22'), sum(adult$'b23'), sum(adult$'b33')))

bridgetypeplot_ad <- ggplot(bridgetypesum_ad, aes(y=amount, x="", fill=Bridge_category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(as.integer(amount/sum(amount)*100), "%")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("Adult")

bridgetypeplot_ad

ggsave("./Plots_mage/20250116_bridgetype_adult.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250116_bridgetype_adult.png", width=5, height=3.5)



################

prepub2 <- subset(p1, age=="Prepub2")

bridgetypesum_pre2 <- data.frame("Bridge_category" = c('b00', 'b01', 'b02', 
                                                       'b03', 'b11', 'b12', 
                                                       'b13', 'b22','b23', 'b33'), 
                                "amount" = c(sum(prepub2$'b00'), sum(prepub2$'b01'), 
                                             sum(prepub2$'b02'), sum(prepub2$'b03'), 
                                             sum(prepub2$'b11'), sum(prepub2$'b12'), 
                                             sum(prepub2$'b13'), sum(prepub2$'b22'), 
                                             sum(prepub2$'b23'), sum(prepub2$'b33')))

bridgetypeplot_pre2 <- ggplot(bridgetypesum_pre2, aes(y=amount, x="", fill=Bridge_category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(as.integer(amount/sum(amount)*100), "%")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("Prepubertal2")

bridgetypeplot_pre2

ggsave("./Plots_mage/20250318_bridgetype_prepub2.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_bridgetype_prepub2.png", width=5, height=3.5)


################

adult2 <- subset(p1, age=="Adult2")

bridgetypesum_ad2 <- data.frame("Bridge_category" = c('b00', 'b01', 'b02', 
                                                      'b03', 'b11', 'b12', 
                                                      'b13', 'b22','b23', 'b33'), 
                               "amount" = c(sum(adult2$'b00'), sum(adult2$'b01'), 
                                            sum(adult2$'b02'), sum(adult2$'b03'), 
                                            sum(adult2$'b11'), sum(adult2$'b12'), 
                                            sum(adult2$'b13'), sum(adult2$'b22'), 
                                            sum(adult2$'b23'), sum(adult2$'b33')))

bridgetypeplot_ad2 <- ggplot(bridgetypesum_ad2, aes(y=amount, x="", 
                                                    fill=Bridge_category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(as.integer(amount/sum(amount)*100), "%")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("Adult2")

bridgetypeplot_ad2

ggsave("./Plots_mage/20250318_bridgetype_adult.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_bridgetype_adult.png", width=5, height=3.5)




########################## Bridgetype - homo/hetero

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




##########################

p1 <- data.frame(lapply(p1, function(x) { gsub("NA", NA, x) }))


unbridge <- subset(p1, !is.na(Unconn_bridge_id))

unbridge$Unconn_bridge_id <- as.character(unbridge$Unconn_bridge_id)

uncon_count <- as.data.frame(table(unbridge$Unconn_bridge_id))

uncon_plot <- ggplot(uncon_count, aes(y=Freq, x="", fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(Freq/sum(Freq)*100), "%")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Set2") +
  ggtitle("GCs in cysts with unconnected bridges") + 
  labs(fill="GC type")

uncon_plot

ggsave("./Plots_mage/20250318_unconbridges.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_unconbridges.png", width=5, height=3.5)



##############################################
## GC types (and by age)


GCtypesum_all <- data.frame("GCtype" = c('0', '1', '2', '3'), 
                           "amount" = c(sum(p1$'n0'), sum(p1$'n1'), 
                                        sum(p1$'n2'), sum(p1$'n3')))

GCtypeplot_all <- ggplot(GCtypesum_all, aes(y=amount, x="", fill=GCtype)) +
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
  scale_fill_brewer(palette="Set2") +
  ggtitle("All samples") +
  labs(fill="GC type")

GCtypeplot_all

ggsave("./Plots_mage/20250318_GCtype_all.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_GCtype_all.png", width=5, height=3.5)


###############

GCtypesum_15 <- data.frame("GCtype" = c('0', '1', '2', '3'), 
                               "amount" = c(sum(WG15$'n0'), sum(WG15$'n1'), 
                                            sum(WG15$'n2'), sum(WG15$'n3')))

GCtypeplot_15 <- ggplot(GCtypesum_15, aes(y=amount, x="", fill=GCtype)) +
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
  scale_fill_brewer(palette="Set2") +
  ggtitle("WG15") +
  labs(fill="GC type")

GCtypeplot_15

ggsave("./Plots_mage/20250116_GCtype_WG15.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250116_GCtype_WG15.png", width=5, height=3.5)


###############

GCtypesum_16 <- data.frame("GCtype" = c('0', '1', '2', '3'), 
                           "amount" = c(sum(WG16$'n0'), sum(WG16$'n1'), 
                                        sum(WG16$'n2'), sum(WG16$'n3')))

GCtypeplot_16 <- ggplot(GCtypesum_16, aes(y=amount, x="", fill=GCtype)) +
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
  scale_fill_brewer(palette="Set2") +
  ggtitle("WG16") +
  labs(fill="GC type")

GCtypeplot_16

ggsave("./Plots_mage/20250210_GCtype_WG16.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250210_GCtype_WG16.png", width=5, height=3.5)


###############

GCtypesum_17 <- data.frame("GCtype" = c('0', '1', '2', '3'), 
                           "amount" = c(sum(WG17$'n0'), sum(WG17$'n1'), 
                                        sum(WG17$'n2'), sum(WG17$'n3')))

GCtypeplot_17 <- ggplot(GCtypesum_17, aes(y=amount, x="", fill=GCtype)) +
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
  scale_fill_brewer(palette="Set2") +
  ggtitle("WG17") +
  labs(fill="GC type")

GCtypeplot_17

ggsave("./Plots_mage/20250116_GCtype_WG17.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250116_GCtype_WG17.png", width=5, height=3.5)


###############

GCtypesum_18 <- data.frame("GCtype" = c('0', '1', '2', '3'), 
                           "amount" = c(sum(WG18$'n0'), sum(WG18$'n1'), 
                                        sum(WG18$'n2'), sum(WG18$'n3')))

GCtypeplot_18 <- ggplot(GCtypesum_18, aes(y=amount, x="", fill=GCtype)) +
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
  scale_fill_brewer(palette="Set2") +
  ggtitle("WG18") +
  labs(fill="GC type")

GCtypeplot_18

ggsave("./Plots_mage/20250116_GCtype_WG18.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250116_GCtype_WG18.png", width=5, height=3.5)

################

GCtypesum_19 <- data.frame("GCtype" = c('0', '1', '2', '3'), 
                        "amount" = c(sum(WG19$'n0'), sum(WG19$'n1'), 
                                     sum(WG19$'n2'), sum(WG19$'n3')))

GCtypeplot_19 <- ggplot(GCtypesum_19, aes(y=amount, x="", fill=GCtype)) +
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
  scale_fill_brewer(palette="Set2") +
  ggtitle("WG19") +
  labs(fill="GC type")

GCtypeplot_19

ggsave("./Plots_mage/20250116_GCtype_WG19.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250116_GCtype_WG19.png", width=5, height=3.5)


################

GCtypesum_20 <- data.frame("GCtype" = c('0', '1', '2', '3'), 
                           "amount" = c(sum(WG20$'n0'), sum(WG20$'n1'), 
                                        sum(WG20$'n2'), sum(WG20$'n3')))

GCtypeplot_20 <- ggplot(GCtypesum_20, aes(y=amount, x="", fill=GCtype)) +
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
  scale_fill_brewer(palette="Set2") +
  ggtitle("WG20") +
  labs(fill="GC type")

GCtypeplot_20

ggsave("./Plots_mage/20250116_GCtype_WG20.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250116_GCtype_WG20.png", width=5, height=3.5)


################

GCtypesum_21 <- data.frame("GCtype" = c('0', '1', '2', '3'), 
                           "amount" = c(sum(WG21$'n0'), sum(WG21$'n1'), 
                                        sum(WG21$'n2'), sum(WG21$'n3')))

GCtypeplot_21 <- ggplot(GCtypesum_21, aes(y=amount, x="", fill=GCtype)) +
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
  scale_fill_brewer(palette="Set2") +
  ggtitle("WG21") +
  labs(fill="GC type")

GCtypeplot_21

ggsave("./Plots_mage/20250116_GCtype_WG21.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250116_GCtype_WG21.png", width=5, height=3.5)


################

GCtypesum_pre <- data.frame("GCtype" = c('0', '1', '2', '3'), 
                           "amount" = c(sum(prepub$'n0'), sum(prepub$'n1'), 
                                        sum(prepub$'n2'), sum(prepub$'n3')))

GCtypeplot_pre <- ggplot(GCtypesum_pre, aes(y=amount, x="", fill=GCtype)) +
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
  scale_fill_brewer(palette="Set2") +
  ggtitle("Prepubertal") +
  labs(fill="GC type")

GCtypeplot_pre

ggsave("./Plots_mage/20250116_GCtype_prepub.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250116_GCtype_prepub.png", width=5, height=3.5)


################

GCtypesum_ad <- data.frame("GCtype" = c('0', '1', '2', '3'), 
                            "amount" = c(sum(adult$'n0'), sum(adult$'n1'), 
                                         sum(adult$'n2'), sum(adult$'n3')))

GCtypeplot_ad <- ggplot(GCtypesum_ad, aes(y=amount, x="", fill=GCtype)) +
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
  scale_fill_brewer(palette="Set2") +
  ggtitle("Adult") +
  labs(fill="GC type")

GCtypeplot_ad

ggsave("./Plots_mage/20250116_GCtype_adult.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250116_GCtype_adult.png", width=5, height=3.5)




################

GCtypesum_pre2 <- data.frame("GCtype" = c('0', '1', '2', '3'), 
                            "amount" = c(sum(prepub2$'n0'), sum(prepub2$'n1'), 
                                         sum(prepub2$'n2'), sum(prepub2$'n3')))

GCtypeplot_pre2 <- ggplot(GCtypesum_pre2, aes(y=amount, x="", fill=GCtype)) +
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
  scale_fill_brewer(palette="Set2") +
  ggtitle("Prepubertal2") +
  labs(fill="GC type")

GCtypeplot_pre2

ggsave("./Plots_mage/20250318_GCtype_prepub2.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_GCtype_prepub2.png", width=5, height=3.5)


################

GCtypesum_ad2 <- data.frame("GCtype" = c('0', '1', '2', '3'), 
                           "amount" = c(sum(adult2$'n0'), sum(adult2$'n1'), 
                                        sum(adult2$'n2'), sum(adult2$'n3')))

GCtypeplot_ad2 <- ggplot(GCtypesum_ad2, aes(y=amount, x="", fill=GCtype)) +
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
  scale_fill_brewer(palette="Set2") +
  ggtitle("Adult2") +
  labs(fill="GC type")

GCtypeplot_ad2

ggsave("./Plots_mage/20250318_GCtype_adult2.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250318_GCtype_adult2.png", width=5, height=3.5)


##################################################



postbirth <- subset(p1, age == "Prepub" | age == "Prepub2" | age == "Adult" | age == "Adult2")

GCtypesum_all_pb <- data.frame("GCtype" = c('0', '1', '2', '3'), 
                            "amount" = c(sum(postbirth$'n0'), sum(postbirth$'n1'), 
                                         sum(postbirth$'n2'), sum(postbirth$'n3')))

GCtypeplot_all_pb <- ggplot(GCtypesum_all_pb, aes(y=amount, x="", fill=GCtype)) +
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
  scale_fill_brewer(palette="Set2") +
  ggtitle("All GCs_pb") +
  labs(fill="GC type")

GCtypeplot_all_pb

ggsave("./Plots_mage/20250331_GCtype_all_pb.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250331_GCtype_all_pb.png", width=5, height=3.5)




fetal <- subset(p1, !(age %in% c("Prepub", "Prepub2", "Adult", "Adult2")))


GCtypesum_f <- data.frame("GCtype" = c('0', '1', '2', '3'), 
                            "amount" = c(sum(fetal$'n0'), sum(fetal$'n1'), 
                                         sum(fetal$'n2'), sum(fetal$'n3')))

GCtypeplot_f <- ggplot(GCtypesum_f, aes(y=amount, x="", fill=GCtype)) +
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
  scale_fill_brewer(palette="Set2") +
  ggtitle("All GCs_F") +
  labs(fill="GC type")

GCtypeplot_f

ggsave("./Plots_mage/20250417_GCtype_fetal.pdf", width=5, height=3.5)
ggsave("./Plots_mage/20250417_GCtype_fetal.png", width=5, height=3.5)
